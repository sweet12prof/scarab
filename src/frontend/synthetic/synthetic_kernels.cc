#include "frontend/synthetic/synthetic_kernels.h"

#include <iostream>
#include <random>

#include "bp/bp.param.h"
#include "memory/memory.param.h"

#include "isa/isa.h"

#define NOP_SIZE ICACHE_LINE_SIZE / (ISSUE_WIDTH)
#define BRANCH_SIZE ICACHE_LINE_SIZE - (NOP_SIZE * (ISSUE_WIDTH - 1))

// names of bottlenecks
const char* bottleneckNames[] = {
#define BOTTLENECK_IMPL(id, name) name,
#include "bottlenecks_table.def"
#undef BOTTLENECK_IMPL
    "invalid"};
BottleNeck_enum bottleneck;
//
extern uns64 synth_start_pc;
extern uns64 synth_start_uid;
// random Function
static std::mt19937_64 engine(1234);
static std::uniform_int_distribution<uns64> dist64{0, std::numeric_limits<uns64>::max()};
static std::bernoulli_distribution distBool(0.5);
// mem workload vars
static const uns64 start_ld_vaddr{0xf800000};
static uns64 ld_vaddr{start_ld_vaddr};
// branch workload vars
static uns64 tgtAddr{0};
static bool direction{false};
uns cf_count = 0;
static std::vector<uint64_t> branch_targets;
static uint64_t branch_targets_index{0};
static bool loopback_ibr[MAX_NUM_PROCS];

ctype_pin_inst generate_loop_carried_dependence_load(uns64 ip, uns64 uid) {
  ctype_pin_inst inst;
  memset(&inst, 0, sizeof(inst));
  inst.inst_uid = uid;
  inst.instruction_addr = ip;
  inst.instruction_next_addr = ip + 7;
  inst.size = 7;
  inst.op_type = OP_MOV;
  inst.fake_inst = 0;
  strcpy(inst.pin_iclass, "SYNTHETIC LOAD");
  inst.num_simd_lanes = 1;
  inst.lane_width_bytes = 1;
  inst.is_move = 1;
  inst.has_immediate = 0;
  inst.num_ld1_addr_regs = 1;
  inst.ld1_addr_regs[0] = Reg_Id::REG_RAX;
  inst.num_dst_regs = 1;
  inst.dst_regs[0] = Reg_Id::REG_RAX;
  inst.ld_vaddr[0] = (dist64(engine) % 0x800000000000);
  inst.num_ld = 1;
  inst.num_st = 0;
  inst.ld_size = 8;
  return inst;
}

ctype_pin_inst generate_independent_operand_load(uns64 ip, uns64 uid, uns64 vaddr) {
  ctype_pin_inst inst;
  memset(&inst, 0, sizeof(inst));
  inst.inst_uid = uid;
  inst.instruction_addr = ip;
  inst.instruction_next_addr = ip + 9;
  inst.size = 9;
  inst.op_type = OP_MOV;
  inst.fake_inst = 0;
  strcpy(inst.pin_iclass, "SYNTHETIC LOAD");
  inst.num_simd_lanes = 1;
  inst.lane_width_bytes = 1;
  inst.is_move = 1;
  inst.has_immediate = 0;
  inst.num_ld1_addr_regs = 1;
  inst.ld1_addr_regs[0] = Reg_Id::REG_RBX;
  inst.num_dst_regs = 1;
  inst.dst_regs[0] = Reg_Id::REG_RAX;
  inst.ld_vaddr[0] = (vaddr % 0x800000000000);
  inst.num_ld = 1;
  inst.num_st = 0;
  inst.ld_size = 8;
  return inst;
}

ctype_pin_inst create_ILP_limited(uns64 ip, uns64 uid) {
  ctype_pin_inst inst;
  memset(&inst, 0, sizeof(inst));
  inst.inst_uid = uid;
  inst.instruction_addr = ip;
  inst.instruction_next_addr = ip + 7;
  inst.size = 7;
  inst.op_type = OP_IMUL;
  inst.fake_inst = 0;
  strcpy(inst.pin_iclass, "SYNTHETIC ADD");
  inst.num_simd_lanes = 1;
  inst.lane_width_bytes = 1;
  inst.num_src_regs = 2;
  inst.num_dst_regs = 1;
  inst.src_regs[0] = Reg_Id::REG_RAX;
  inst.src_regs[1] = Reg_Id::REG_RBX;
  inst.dst_regs[0] = Reg_Id::REG_RAX;
  return inst;
}

ctype_pin_inst generate_conditional_branch(uns64 ip, uns64 uid, uns64 tgtAddr, bool direction, uns8 inst_size) {
  ctype_pin_inst inst;
  memset(&inst, 0, sizeof(inst));
  inst.inst_uid = uid;
  inst.instruction_addr = ip;
  inst.instruction_next_addr = direction ? tgtAddr : (ip + inst_size);
  inst.size = inst_size;
  inst.op_type = OP_IADD;
  inst.cf_type = CF_CBR;
  inst.num_simd_lanes = 1;
  inst.lane_width_bytes = 1;
  inst.branch_target = tgtAddr;
  inst.actually_taken = direction ? TAKEN : NOT_TAKEN;
  inst.fake_inst = 0;
  return inst;
}

ctype_pin_inst generate_unconditional_branch(uns64 ip, uns64 uid, uns64 tgt, uns8 inst_size) {
  ctype_pin_inst inst;
  memset(&inst, 0, sizeof(inst));
  inst.instruction_addr = ip;
  inst.inst_uid = uid;
  inst.instruction_next_addr = tgt;
  inst.size = inst_size;
  inst.op_type = OP_CF;
  inst.cf_type = CF_BR;
  inst.num_simd_lanes = 1;
  inst.lane_width_bytes = 1;
  inst.branch_target = tgt;
  inst.actually_taken = TAKEN;
  inst.fake_inst = 0;
  strcpy(inst.pin_iclass, "DUMMY_JMP");
  return inst;
}

ctype_pin_inst create_icache_limited(uns64 ip, uns64 uid) {
  ctype_pin_inst inst;
  memset(&inst, 0, sizeof(inst));
  inst.instruction_addr = ip;
  inst.inst_uid = uid;
  inst.instruction_next_addr = ip + 64;
  inst.size = 64;
  inst.op_type = OP_IADD;
  inst.fake_inst = 0;
  strcpy(inst.pin_iclass, "SYNTHETIC ADD");
  inst.num_simd_lanes = 1;
  inst.lane_width_bytes = 1;
  inst.num_src_regs = 2;
  inst.num_dst_regs = 1;
  inst.src_regs[0] = Reg_Id::REG_RAX;
  inst.src_regs[1] = Reg_Id::REG_RBX;
  inst.dst_regs[0] = Reg_Id::REG_RCX;
  return inst;
}

ctype_pin_inst generate_indirect_branch(uns64 ip, uns64 uid, uns64 tgtAddr, uns64 vaddr, uns8 inst_size) {
  ctype_pin_inst inst;
  memset(&inst, 0, sizeof(inst));
  inst.instruction_addr = ip;
  inst.inst_uid = uid;
  inst.instruction_next_addr = tgtAddr;
  inst.size = inst_size;
  inst.op_type = OP_CF;
  inst.cf_type = CF_IBR;
  inst.num_simd_lanes = 1;
  inst.lane_width_bytes = 1;
  inst.branch_target = tgtAddr;
  inst.actually_taken = 1;
  inst.fake_inst = 0;
  inst.num_src_regs = 1;
  inst.num_ld1_addr_regs = 1;
  // inst.ld1_addr_regs[0] = REG_RAX;
  inst.num_ld = 1;
  inst.ld_vaddr[0] = vaddr % 0x800000000000;  // ensure its in unprivileged space
  strcpy(inst.pin_iclass, "DUMMY_JMP");
  return inst;
}

ctype_pin_inst generate_synthetic_microkernel(uns proc_id, BottleNeck_enum bottleneck_type, uns64 ip, uns64 uid) {
  switch (bottleneck_type) {
    case MEM_BANDWIDTH_LIMITED: {
      ctype_pin_inst inst;
      if (ip >= 2000) {
        inst = generate_unconditional_branch(ip, synth_start_uid, synth_start_pc, BRANCH_SIZE);
        ld_vaddr = start_ld_vaddr;
      } else {
        inst = generate_independent_operand_load(ip, uid, ld_vaddr);
        ld_vaddr += 8;
      }
      return inst;
    }

    case MEM_LATENCY_LIMITED: {
      ctype_pin_inst inst;
      if (ip >= 2000) {
        inst = generate_unconditional_branch(ip, synth_start_uid, synth_start_pc, BRANCH_SIZE);
      } else
        inst = generate_loop_carried_dependence_load(ip, uid);
      return inst;
    }

    case BRANCH_PREDICTOR_LIMITED: {
      ctype_pin_inst inst;
      if (cf_count == ISSUE_WIDTH - 1) {
        if (ip > 1200) {
          inst = generate_unconditional_branch(ip, synth_start_uid, synth_start_pc, BRANCH_SIZE);
          tgtAddr = synth_start_pc;
          cf_count = 0;
        } else {
          tgtAddr = ip + ICACHE_LINE_SIZE + BRANCH_SIZE;
          inst = generate_conditional_branch(ip, uid, tgtAddr, direction, BRANCH_SIZE);
          cf_count = 0;
        }
      } else {
        inst = create_dummy_nop(ip, WPNM_REASON_REDIRECT_TO_NOT_INSTRUMENTED);
        inst.size = NOP_SIZE;
        inst.instruction_next_addr = ip + NOP_SIZE;
        inst.inst_uid = uid;
        cf_count++;
        inst.fake_inst = 0;
      }
      direction = distBool(engine);  // randomize direction for next time conditional branch to be generated
      return inst;
    }

    case BTB_LIMITED: {
      ctype_pin_inst inst;
      if (cf_count == ISSUE_WIDTH - 1) {
        if (branch_targets_index == 2 * TC_ASSOC) {
          inst = generate_unconditional_branch(ip, synth_start_uid, synth_start_pc, BRANCH_SIZE);
          branch_targets_index = 0;
          cf_count = 0;
        } else {
          inst = generate_unconditional_branch(ip, uid, branch_targets[branch_targets_index], BRANCH_SIZE);
          branch_targets_index++;
          cf_count = 0;
        }
      } else {
        inst = create_dummy_nop(ip, WPNM_REASON_REDIRECT_TO_NOT_INSTRUMENTED);
        inst.size = NOP_SIZE;
        inst.instruction_next_addr = ip + NOP_SIZE;
        inst.inst_uid = uid;
        cf_count++;
      }
      return inst;
    }

    case ICACHE_LIMITED:
      return create_icache_limited(ip, uid);
      break;

    case INDIRECT_BRANCH_LIMITED: {
      ctype_pin_inst inst;
      if (cf_count == ISSUE_WIDTH - 1) {
        if (branch_targets_index == 2 * TC_ASSOC) {
          inst = generate_unconditional_branch(ip, synth_start_uid, synth_start_pc, BRANCH_SIZE);
          branch_targets_index = 0;
          cf_count = 0;
        } else {
          if (loopback_ibr[proc_id]) {
            inst = generate_unconditional_branch(ip, synth_start_uid, synth_start_pc, BRANCH_SIZE);
            loopback_ibr[proc_id] = false;
            cf_count = 0;
          } else {
            inst = generate_indirect_branch(ip, uid, branch_targets[branch_targets_index], ld_vaddr, BRANCH_SIZE);
            branch_targets_index++;
            ld_vaddr = (inst.ld_vaddr[0] + 8) % 0x800000000000;
            cf_count = 0;
            loopback_ibr[proc_id] = false;
          }
        }
      } else {
        inst = create_dummy_nop(ip, WPNM_REASON_REDIRECT_TO_NOT_INSTRUMENTED);
        inst.size = NOP_SIZE;
        inst.instruction_next_addr = ip + NOP_SIZE;
        cf_count++;
      }
      return inst;
    }

    case ILP_LIMITED: {
      ctype_pin_inst inst;
      if (ip >= 2000) {
        inst = generate_unconditional_branch(ip, synth_start_uid, synth_start_pc, BRANCH_SIZE);
      } else
        inst = create_ILP_limited(ip, uid);
      return inst;
    }

    default:
      return create_dummy_nop(ip, WPNM_NOT_IN_WPNM);
  }
}

void synthetic_kernel_init() {
  uns64 tgt = synth_start_pc + (BTB_ENTRIES / BTB_ASSOC);
  for (uns i{0}; i < BTB_ENTRIES; i++) {
    branch_targets.push_back(tgt);
    tgt += (BTB_ENTRIES / BTB_ASSOC);
  };
}