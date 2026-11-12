#include <cassert>
#include <iostream>

#include "globals/global_types.h"

#include "memory/memory.param.h"

#include "kernel_factory.h"
#include "sampler.h"
#include "synthetic_basic_apis.h"

// Flushes the kernel buffer into a map at the end of kernel generation. **
void flush_kernel_buffer(std::vector<ctype_pin_inst>& buffer, std::map<uns64, ctype_pin_inst>& kernel_map) {
  for (auto item : buffer) {
    auto pc = item.instruction_addr;
    kernel_map.insert({pc, item});
  }
}

// Generates leading nops for CF workloads.
uns64 Kernel_Factory::generate_nop_sequence(std::vector<ctype_pin_inst>& kernel_buffer, Sampler& uid_sequence,
                                            uns num_of_nops, uns64 starting_pc) {
  Sampler nop_pcs(UNIFORM_SEQUENTIAL, 1, num_of_nops + 1, starting_pc, get_nop_size());
  for (uns i{0}; i < num_of_nops; i++) {
    auto current_pc{nop_pcs.get_next_element()};
    kernel_buffer.push_back(
        ctype_pin_inst{generate_nop(current_pc, uid_sequence.get_next_element(), get_nop_size(), false)});
  }
  return nop_pcs.get_next_element();
}

// Generates tail end unconditional branch and NOP PADs at the end of each kernel
void Kernel_Factory::generate_tail_end_insts(std::vector<ctype_pin_inst>& kernel_buffer, Sampler& uid_sequence,
                                             Sampler& targets_pool, uns pad_to_next_inst,
                                             uns unconditional_branch_size) {
  uns64 current_pc = targets_pool.get_last_element_in_sorted_order() + pad_to_next_inst;
  current_pc = generate_nop_sequence(kernel_buffer, uid_sequence, DISPATCH_WIDTH, current_pc);
  auto next_inst = generate_unconditional_branch(current_pc, uid_sequence.get_next_element(), get_start_pc(),
                                                 unconditional_branch_size);
  kernel_buffer.push_back(next_inst);
  current_pc += unconditional_branch_size;
  generate_nop_sequence(kernel_buffer, uid_sequence, pad_length + 1500, current_pc);
}

/************  PreChecks For CF_Workloads ******************
These prechecks warns the user beforehand if
1. Issue packets will cross ICACHE_BOUNDARIES
2. The chosen size for NOPs is sufficient to fill out the remaining cache line given the size of issue packets
3. If the configuration tuple; target_stride, nop_size and cf_instr_size (CF_BR, CF_CBR and CF_IBR0) are appopriate for
correct operation

Parameter Definitions
1. nop_count_in_packet: How many NOPS are in an issue_packet
2. cf_op_count_in_packet: How many cf ops are in an issue packet. CBR worklads include NOPS+CBR+BR, IBR and UBR consist
of NOPS+IBR/UBR respectively
3. target_stride: This indicates the distance between taken targets. If the starting pc is a multiple of the
icache_line_size, then every issuepacket starts at an ICACHE_BOUNDARY
4. cf_inst_size: Size of control flow instructions; CBR, UBR or IBR according to the chosen kernel
5. nop_inst_size: Size of a NOP
***************************************************************/

void prechecks_for_CF_kernels(uns nop_count_in_packet, uns cf_op_count_in_packet, uns target_stride, uns cf_inst_size,
                              uns nop_inst_size) {
  // total size of packet
  uns cf_kernel_issue_packet_size = (nop_count_in_packet * nop_inst_size) + (cf_op_count_in_packet * cf_inst_size);

  // if packet is greater than branch target distance, packets will overlap into each other
  assert((cf_kernel_issue_packet_size <= target_stride && (target_stride % cf_kernel_issue_packet_size == 0)) &&
         "ERROR: Invalid tuple; target_stride, nop_size and cf_inst size, packets overwrite each other");

  if (cf_kernel_issue_packet_size > ICACHE_LINE_SIZE)
    std::cerr << "WARNING: CF Packets cross ICACHE_BOUNDARIES, Frontend Bound Atrribtion in TMA possible";
  else if (cf_kernel_issue_packet_size < ICACHE_LINE_SIZE)
    // The issue packet is not fully utilizing icahce lines but we need to make sure NOP size is appopriate to fill out
    // this gap so offpath execution will work properly
    assert((ICACHE_LINE_SIZE - cf_kernel_issue_packet_size) % nop_inst_size == 0 &&
           "Issue packets for this kernel < ICACHE_INE_SIZE but NOP size is inappopriate to fill out the leftover");
};

/* ==================================================================================================
 * CBR_LIMITED KERNEL (includes cbr_20t, cbr_50t, cbr_80t)
 * --------------------------------------------------------------------------------------------------
 * The kernel consists of a sequence of conditional branches with fixed targets but randomized branch
 directions.
 * The direction_sampler generates the randomized sequence according to the weights assigned each
 unique sample; {0, 1}. 0 = not-taken, 1 = taken. The weight is defined using the t_nt_ratio.
 With t_nt_ratio = 0.5, weight = {50, 50} for unique samples {0, 1}.
 * To ensure topdown, reflects the bottleneck as Bad Speculation. Each CBR is preceeded by a sequence of
 NOPs such that, the frontend can reliably deliver the full issue width every cycle. In addition,
 the sequience of NOPs+CBR in each issue packet is sized to fit the ICACHE_LINE_SIZE.
 * The kernel is essentially an unrolled loop, that ends with backward unconditional branch back to the
 beginning of the program. The branch direction sequence is rerandomized when control returns to the beginning.
 * The target_pool is generated randomly initially. But every iteration of the loop uses the same fixed targets
 according to the same sequence.
 * ================================================================================================*/

std::map<uns64, ctype_pin_inst> Kernel_Factory::generate_cbr_kernel() {
  uid_sequence =
      Sampler(UNIFORM_SEQUENTIAL, 1, ((2 * DISPATCH_WIDTH * target_pool_size) + pad_length + 1), start_uid, 1);

  std::map<uns64, ctype_pin_inst> kernel_map;
  std::vector<ctype_pin_inst> kernel_buffer;
  // branch direction distribution for both onpath and offpath branches
  uns64 taken_ratio = static_cast<uns64>(t_nt_ratio * 100);
  uns64 not_Taken_ratio = 100 - taken_ratio;
  Sampler direction_sequence(direction_strategy, 1, {0, 1}, ((target_pool_size)), {taken_ratio, not_Taken_ratio});
  uns64 current_pc{0}, current_uid{0}, _target{0};
  ctype_pin_inst next_inst;
  prechecks_for_CF_kernels(DISPATCH_WIDTH, 2, target_stride, get_inst_size(), get_nop_size());

  // Generate insts for every possible pc, includes possible offpath insts
  for (uns i{0}; i < (target_pool_size); i++) {
    // generate leading nops and return next pc
    current_pc = target_pool.get_next_element();
    auto temp = current_pc;  // use temp to determine if we exhausted a full cache line later.
    current_pc = generate_nop_sequence(kernel_buffer, uid_sequence, DISPATCH_WIDTH, current_pc);
    current_uid = uid_sequence.get_next_element();
    // every taken target is the next element in the shuffled sequence
    _target = target_pool.peek_next_element();
    next_inst = generate_conditional_branch(current_pc, current_uid, _target, direction_sequence.get_next_element(),
                                            get_inst_size());
    kernel_buffer.push_back(next_inst);
    // generate unconditional branch to target on fall through
    current_uid = uid_sequence.get_next_element();
    current_pc += get_inst_size();
    next_inst = generate_unconditional_branch(current_pc, current_uid, _target + get_nop_size(), get_inst_size());
    current_pc += get_inst_size();
    kernel_buffer.push_back(next_inst);
    // if we arent filling the full icache line with issue_packets, fill the remaining line with NOPS
    // if this value is negative it means the config allows packets to span across cache line
    int empty_distance = ICACHE_LINE_SIZE - ((current_pc + 1) - temp);

    if (target_stride == ICACHE_LINE_SIZE && (empty_distance > 0)) {
      // we can fill spaces
      generate_nop_sequence(kernel_buffer, uid_sequence, empty_distance, current_pc);
    }
  }
  uns packet_size = (DISPATCH_WIDTH * NOP_SIZE) + (2 * get_inst_size());
  generate_tail_end_insts(kernel_buffer, uid_sequence, target_pool, packet_size, get_nop_size());
  flush_kernel_buffer(kernel_buffer, kernel_map);
  return kernel_map;
}

/* ===============================================================================================
 * UBR_LIMITED KERNEL (includes cbr_20t, cbr_50t, cbr_80t)
 * -----------------------------------------------------------------------------------------------
 * The kernel consists of a sequence of unconditional branches with a set of targets.
 * The target pool is generated uniformly, shuffled and sampled for each UBR in the program.
 * Depending on the target pool pattern, the kernel ensures either 100% HIT or MISS or in between.
 * To ensure topdown, reflects the bottleneck as Bad Speculation. Each UBR is preceeded by a sequence of
 NOPs such that, the frontend can reliably deliver the full issue width every cycle. In addition,
 the sequience of NOPs+UBR in each issue packet is sized to fit the ICACHE_LINE_SIZE.
 * The kernel is a loop, that ends with backward unconditional branch to the beginning of the program.
 * ================================================================================================*/

std::map<uns64, ctype_pin_inst> Kernel_Factory::generate_ubr_kernel() {
  uid_sequence = Sampler(UNIFORM_SEQUENTIAL, 1, (target_pool_size + pad_length + 1), start_uid, 1);

  std::map<uns64, ctype_pin_inst> kernel_map;
  std::vector<ctype_pin_inst> kernel_buffer;
  ctype_pin_inst next_inst;
  prechecks_for_CF_kernels(DISPATCH_WIDTH - 1, 1, target_stride, get_inst_size(), get_nop_size());
  uns64 current_pc = 0, current_uid = 0;
  for (uns i{0}; i < (target_pool_size); i++) {
    // generate leading nops
    current_pc = target_pool.get_next_element();
    current_pc = generate_nop_sequence(kernel_buffer, uid_sequence, DISPATCH_WIDTH - 1, current_pc);
    current_uid = uid_sequence.get_next_element();

    // target is the next element in shuffled sequence
    uns64 next_target = target_pool.peek_next_element();

    next_inst = generate_unconditional_branch(current_pc, current_uid, next_target, get_inst_size());
    kernel_buffer.push_back(next_inst);
  }

  uns packet_size = ((DISPATCH_WIDTH - 1) * NOP_SIZE) + (get_inst_size());
  generate_tail_end_insts(kernel_buffer, uid_sequence, target_pool, packet_size, get_inst_size());

  flush_kernel_buffer(kernel_buffer, kernel_map);
  return kernel_map;
}

/* ===========================================================================================================
 * IBR_LIMITED KERNEL (includes IBR_RR_4TGT, IBR_RANDOM_2TGT, IBR_RANDOM_4TGT)
 * ------------------------------------------------------------------------------------------------------------
 * The kernel consists of a sequence of indirect branches that branch to a set of indirect targets.
 * The targets are obtained from a fixed memory address sourced indirectly from a fixed source register operand.
 * The target pool is generated uniformly, shuffled and sampled for each IBR in the program.
 * To ensure topdown, reflects the bottleneck as Bad Speculation. Each IBR is preceeded by a sequence of
 NOPs such that, the frontend can reliably deliver the full issue width every cycle. In addition,
 the sequience of NOPs+IBR in each issue packet is sized to fit the ICACHE_LINE_SIZE.
 * The kernel is a loop, that ends with backward unconditional branch to the beginning of the program.
 * ===========================================================================================================*/
std::map<uns64, ctype_pin_inst> Kernel_Factory::generate_random_ibr_kernel() {
  assert(starting_target <= get_start_pc() && "Random IBR Targets must not include starting_pc");
  uid_sequence =
      Sampler(UNIFORM_SEQUENTIAL, 1, ((2 * DISPATCH_WIDTH * target_pool_size + pad_length) + 1), start_uid, 1);

  prechecks_for_CF_kernels(DISPATCH_WIDTH - 1, 1, target_stride, get_inst_size(), get_nop_size());

  std::map<uns64, ctype_pin_inst> kernel_map;
  std::vector<ctype_pin_inst> kernel_buffer;
  // fixed mem address
  uns64 memaddress = 0x123456;
  ctype_pin_inst next_inst;
  uns64 current_pc = start_pc, current_uid = 0;
  // generate leading nops
  current_pc = generate_nop_sequence(kernel_buffer, uid_sequence, DISPATCH_WIDTH - 1, current_pc);
  current_uid = uid_sequence.get_next_element();
  // target is the next element in shuffled sequence
  uns64 next_target = target_pool.get_next_element();
  next_inst = generate_indirect_branch(current_pc, current_uid, next_target, memaddress, get_inst_size());
  kernel_buffer.push_back(next_inst);
  // generate tail-end instructions
  current_pc += get_inst_size();
  for (uns i = 0; i < pad_length; i++) {
    current_pc = generate_nop_sequence(kernel_buffer, uid_sequence, DISPATCH_WIDTH - 1, current_pc);
    current_uid = uid_sequence.get_next_element();
    next_inst = generate_unconditional_branch(current_pc, current_uid, start_pc, get_inst_size());
    kernel_buffer.push_back(next_inst);
    current_pc += get_inst_size();
  }
  flush_kernel_buffer(kernel_buffer, kernel_map);
  return kernel_map;
}

std::map<uns64, ctype_pin_inst> Kernel_Factory::generate_round_robin_ibr_kernel() {
  std::map<uns64, ctype_pin_inst> kernel_map;
  std::vector<ctype_pin_inst> kernel_buffer;
  // fixed mem address
  uns64 memaddress = 0x123456;
  ctype_pin_inst next_inst;

  uns64 current_uid = 0, current_pc = 0;
  for (uns i{0}; i < target_pool_size; i++) {
    // generate leading nops
    current_pc = target_pool.get_next_element();
    current_pc = generate_nop_sequence(kernel_buffer, uid_sequence, DISPATCH_WIDTH - 1, current_pc);

    current_uid = uid_sequence.get_next_element();
    // target is the next element in shuffled sequence
    uns64 next_target = target_pool.peek_next_element();
    next_inst = generate_indirect_branch(current_pc, current_uid, next_target, memaddress, get_inst_size());
    kernel_buffer.push_back(next_inst);
  }
  uns packet_size = ((DISPATCH_WIDTH - 1) * NOP_SIZE) + (get_inst_size());
  generate_tail_end_insts(kernel_buffer, uid_sequence, target_pool, packet_size, get_inst_size());
  flush_kernel_buffer(kernel_buffer, kernel_map);
  return kernel_map;
}

/* ===========================================================================================================
 * ILP_LIMITED KERNEL (includes ILP_DEP_0_CHAIN, ILP_DEP_1_CHAIN, ILP_DEP_2_CHAIN, ILP_DEP_4_CHAIN,)
 * ------------------------------------------------------------------------------------------------------------
 * The kernel consists of a sequence of alu_type instructions that are orchestrated to generate ilp bottlenecks.
 * The sequence of instructions create a dependence chain through source and destination operands.
 * The number of dependence chains is configurable. With 1 dependence chain, every succeeding instruction is
 orchestrated to stall before execute so that the preceeding one completes.
 * Configuring > 1 dependence chains, generates a set of mutually exclusive dependence chain. Thus, an instruction
 from each dependence chain can be in execute concurrently.
 * The kernel is an unrolled loop, that ends with backward unconditional branch to the beginning of the program.
 * ===========================================================================================================*/
std::map<uns64, ctype_pin_inst> Kernel_Factory::generate_ilp_kernel(uns dependence_chain_length, uns workload_length) {
  std::map<uns64, ctype_pin_inst> kernel_map;
  std::vector<ctype_pin_inst> kernel_buffer;
  if (dependence_chain_length != 0)
    assert((workload_length % dependence_chain_length) == 0 &&
           "workload_length must be a multiple of dependence chain length");
  uns64 current_pc = 0;
  // zero dependence chain length means no carried loop dependence
  if (dependence_chain_length == 0) {
    for (uns i{0}; i < (workload_length); i++) {
      current_pc = pc_sequence.get_next_element();
      kernel_buffer.push_back(
          generate_alu_type_inst(current_pc, uid_sequence.get_next_element(), get_inst_size(), 1, 2, 3));
      // append unconditional branch to end of the kernel
    }
  } else {
    for (uns i{0}; i < ((workload_length / dependence_chain_length)); i++) {
      for (uns j{0}; j < dependence_chain_length; j++) {
        current_pc = pc_sequence.get_next_element();
        kernel_buffer.push_back(
            generate_alu_type_inst(current_pc, uid_sequence.get_next_element(), get_inst_size(), j + 1, j + 1, j + 1));
      }
    }
  }
  generate_tail_end_insts(kernel_buffer, uid_sequence, pc_sequence, get_inst_size(), get_inst_size());
  flush_kernel_buffer(kernel_buffer, kernel_map);
  return kernel_map;
}

/* ===========================================================================================================
 * MEMORY KERNELS (LATENCY, BANDWIDTH)
     * LATENCY_KERNELS (DCACHE_LIMITED, MLC_LIMITED, LLC_LIMITED, MAIN_MEM_LIMITED)
     * BANDWIDTH_KERNELS (MEM_BAND_1FU, MEM_BAND_2FU, MEM_BAND_4FU,)
 * ------------------------------------------------------------------------------------------------------------
 * The memory kernels orchestrate memory latency and bandwidth bottlenecks.

 * The latency workloads are intended to limit IPC through the latency of the various levels in the cache hierachy.
 * To enable latency bottlenecks, an unrolled loop-carried dependence chain including load instructions is utilised.
 * Each subsequent Load accesses a memory address that is gauranteed to hit at a particular level but miss in all
 preceeding levels, if any. The memory address pattern is defined in a target_pool and generated by the sampler.
 * The level is configurable as any level in the memory hierachy.

 * The bandwidth kernels orchestrate memory bandwidth bottlenecks.
 * Here the instructions stream consists of a sequence of independent loads that can execute concurrently.However,
 the actual number of loads that can actually execute is limited by the number of available load FUs
 which is configurable in PARAMS.in
 * ===========================================================================================================*/
std::map<uns64, ctype_pin_inst> Kernel_Factory::generate_load_kernel(Load_Kernel_Type type) {
  // stride should be enough to cause hits at a level but misses in the precceding levels if any
  target_stride = [&]() -> uns64 {
    switch (level) {
      case DCACHE_LEVEL: {
        return 0;
      };
      case MLC_LEVEL: {
        return DCACHE_SIZE / (DCACHE_ASSOC);
      };
      case LLC_LEVEL: {
        return MLC_SIZE / (MLC_ASSOC);
      };
      case MEM_LEVEL: {
        return L1_SIZE / L1_ASSOC;
      }
      default:
        return 0;
    }
  }();

  /*
  The target_pool consists of a set of load addresses that map to the same set and are enough to cause 100%
  conflict misses in all preceeding levels.
  */
  target_pool_size = [&]() -> uns64 {
    switch (level) {
      case DCACHE_LEVEL: {
        return workload_length;
      };
      case MLC_LEVEL: {
        return 2 * DCACHE_ASSOC;
      };

      case LLC_LEVEL: {
        return 2 * MLC_ASSOC;
      };

      case MEM_LEVEL: {
        return 2 * L1_ASSOC;
      };

      default:
        return workload_length;
    }
  }();

  target_pool = Sampler(target_strategy, 1, target_pool_size, starting_target, target_stride);

  std::vector<ctype_pin_inst> kernel_buffer;
  std::map<uns64, ctype_pin_inst> kernel_map;
  for (uns i{0}; i < (workload_length); i++) {
    auto current_pc = pc_sequence.get_next_element();
    auto mem_addr = target_pool.get_next_element();
    switch (type) {
      case LATENCY: {
        kernel_buffer.push_back(generate_generic_load(current_pc, uid_sequence.get_next_element(), mem_addr,
                                                      get_inst_size(), Reg_Id::REG_RAX, Reg_Id::REG_RAX));
        break;
      }
      case BANDWIDTH: {
        kernel_buffer.push_back(generate_generic_load(current_pc, uid_sequence.get_next_element(), mem_addr,
                                                      get_inst_size(), Reg_Id::REG_RAX, Reg_Id::REG_RBX));
        break;
      }
      default:
        break;
    }
  }
  generate_tail_end_insts(kernel_buffer, uid_sequence, pc_sequence, get_inst_size(), get_inst_size());
  flush_kernel_buffer(kernel_buffer, kernel_map);
  return kernel_map;
}

/* ===========================================================================================================
 * ICACHE_LIMITED
 * ------------------------------------------------------------------------------------------------------------
 * The kernel stresses the capacity of the ICACHE. By ensuring the loop to be run exceeds ICACHE_SIZE, the return
 to the start of the program is gauranteed 100% ICACHE_MISS.
 * The size for every instruction = ICACHE_LINE_SIZE, moreover, the workload required FDIP/frontend prefetcher be
 turned off.
 * ===========================================================================================================*/
std::map<uns64, ctype_pin_inst> Kernel_Factory::generate_icache_kernel() {
  std::map<uns64, ctype_pin_inst> kernel_map;
  std::vector<ctype_pin_inst> kernel_buffer;

  for (uns64 i{0}; i < workload_length; i++) {
    auto current_pc = pc_sequence.get_next_element();

    kernel_buffer.push_back(
        generate_alu_type_inst(current_pc, uid_sequence.get_next_element(), ICACHE_LINE_SIZE, 1, 2, 3));
  }
  generate_tail_end_insts(kernel_buffer, uid_sequence, pc_sequence, ICACHE_LINE_SIZE, get_inst_size());
  flush_kernel_buffer(kernel_buffer, kernel_map);
  return kernel_map;
}