extern "C" {
#include "globals/assert.h"
#include "globals/global_defs.h"
#include "globals/global_types.h"
#include "globals/global_vars.h"
#include "globals/utils.h"

#include "debug/debug.param.h"
#include "debug/debug_macros.h"

#include "isa/isa.h"
}

#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <limits>
#include <random>
#include <unordered_map>
#include <vector>

#include "../../../ctype_pin_inst.h"
#include "./pin/pin_lib/uop_generator.h"
#include "bp/bp.h"
#include "frontend/synth_fe.h"
#include "pin/pin_lib/uop_generator.h"
#include "pt_memtrace/memtrace_fe.h"

#include "ctype_pin_inst.h"
#include "statistics.h"
#include "xed-iclass-enum.h"
//#define PRINT_INFO

#define DEBUG(proc_id, args...) _DEBUG(proc_id, DEBUG_SYNTHETIC_INST, ##args)

// enum class
enum class BottleNeck_enum {
  MEM_LATENCY_LIMITED,
  MEM_BANDWIDTH_LIMITED,
  ILP_LIMITED,
  BRANCH_PREDICTOR_LIMITED,
  BTB_LIMITED,
  ICACHE_LIMITED,
  INDIRECT_BRANCH_LIMITED
};

// utility functions and variables
std::random_device rd;
std::mt19937_64 engine(1234);
std::uniform_int_distribution<uint64_t> dist64{0, std::numeric_limits<uint64_t>::max()};
std::bernoulli_distribution distBool(0.5);
//functions to generate predefined targets, used in BTB_limited workload
void gen_addr(std::vector<uint16_t>&);
void shuffleAddrs(std::vector<uint16_t>&);

ctype_pin_inst generatesyntheticInstr(BottleNeck_enum, uint64_t, uint64_t);
ctype_pin_inst create_latencyBound_load(uint64_t, uint64_t);
ctype_pin_inst create_bandwidthBound_load(uint64_t, uint64_t);
ctype_pin_inst create_ILP_limited_add(uint64_t, uint64_t);
ctype_pin_inst create_bp_limited(uint64_t, uint64_t, uint64_t, bool);
ctype_pin_inst create_btb_limited(uint64_t , uint64_t , uint64_t );
ctype_pin_inst create_icache_limited(uint64_t, uint64_t);
ctype_pin_inst create_indirect_jmp(uint64_t, uint64_t, uint64_t, uint64_t);
void off_path_generate_inst(uns proc_id, uint64_t* off_path_addr, ctype_pin_inst* inst);

BottleNeck_enum bottleneck{BottleNeck_enum::INDIRECT_BRANCH_LIMITED};

ctype_pin_inst dummyinst = {};
ctype_pin_inst* next_pi;
uint64_t jmp_ip = 2000;
uint64_t static_jmp_ip {jmp_ip};
// bool genReturnJmp{false}, isoffpath{false};
uint64_t tgtAddr{jmp_ip + 4};
uint64_t ld_vaddr{0xf800000};
bool direction{false}, at_loopback{false};
std::vector<uint16_t> btbAddresses(8192);
uint16_t btbaddrs_index_count{0};
std::unordered_map<uint64_t, uint64_t> uid_tgtMap;

void synth_init() {
  uop_generator_init(NUM_CORES);
  next_pi = (ctype_pin_inst*)malloc(NUM_CORES * sizeof(ctype_pin_inst));
  gen_addr(btbAddresses);
  dummyinst = generatesyntheticInstr(bottleneck, 1001, 1000);
  for (uns proc_id{0}; proc_id < NUM_CORES; proc_id++)
    next_pi[proc_id] = dummyinst;
}

void synth_done() {
}

Addr synth_next_fetch_addr(uns proc_id) {
  return convert_to_cmp_addr(proc_id, (next_pi[proc_id].instruction_addr));
}

Flag synth_can_fetch_op(uns proc_id) {
  return !(uop_generator_get_eom(proc_id) && trace_read_done[proc_id]);
}

void synth_fetch_op(uns proc_id, Op* op) {
  DEBUG(proc_id, "Fetch Op begin:\n");
  if (uop_generator_get_bom(proc_id)) {
    ASSERT(proc_id, !trace_read_done[proc_id] && !reached_exit[proc_id]);
    uop_generator_get_uop(proc_id, op, &next_pi[proc_id]);
  #ifdef PRINT_INFO
      std::cout << " ip " << next_pi[proc_id].instruction_addr << " Next " << next_pi[proc_id].instruction_next_addr
                << " size " << (uint32_t)next_pi[proc_id].size << " target " << next_pi[proc_id].branch_target << " size "
                << (uint32_t)next_pi[proc_id].size << " taken " << (uint32_t)next_pi[proc_id].actually_taken << std::endl;
  #endif
  } else {
    uop_generator_get_uop(proc_id, op, NULL);
  }
  if (uop_generator_get_eom(proc_id)) {
    dummyinst = generatesyntheticInstr(bottleneck, dummyinst.instruction_next_addr, ++dummyinst.inst_uid);
    next_pi[proc_id] = dummyinst;
  }
  // #ifdef PRINT_INFO
  //   std::cout << "bom eom simultaneous ? " << std::boolalpha << (uop_generator_get_bom(proc_id) == 1 && uop_generator_get_eom(proc_id) == 1) << std::endl;
  // #endif
}

void synth_redirect(uns proc_id, uns64 inst_uid, Addr fetch_addr) {
  #ifdef PRINT_INFO 
    std::cout << "Redirect happended here: " << " current jmp_ip " << dummyinst.instruction_addr << " pred addr "
              << fetch_addr << std::endl;
  #endif
  jmp_ip = fetch_addr;
  tgtAddr = jmp_ip + 4;
  direction = distBool(engine);
  // For direct CBR/BR, fetch_op returns only 1 uop, thus bom and eom are reached simultaneously
  // At eom, fetch_op generates the next instr on the correct path. However next instr should be at pred_addr from bp.
  // the following is required to overwrite  next_pi pc to pred_addr
  dummyinst = generatesyntheticInstr(bottleneck, 0, ++dummyinst.inst_uid);
  next_pi[proc_id] = dummyinst;
}

void synth_recover(uns proc_id, uns64 inst_uid) {
  // Without defining this, the branch resteer to the correct path does not work.
  // suspect it has to do with how eom is reached in the same iteration as bom for direct CBR/BR
  // this forces the next off+path instruction to be generated by fetch_op function
  // Thus we must overwrite the generated off-path op
  jmp_ip = uid_tgtMap.at(inst_uid);
  uid_tgtMap.clear();    
    if(jmp_ip != static_jmp_ip)
    at_loopback = true;
  tgtAddr = jmp_ip + 4;
  direction = distBool(engine);
  dummyinst = generatesyntheticInstr(bottleneck, 0, ++dummyinst.inst_uid);
  next_pi[proc_id] = dummyinst;
  #ifdef PRINT_INFO
    std::cout << "Recovery Happened Here" << std::endl;
  #endif
}

void synth_retire(uns proc_id, uns64 inst_uid) {
}

void synth_fill_in_dynamic_info(uns proc_id) {
}

// generate and shuffle synthetic indirect jump addresses
void gen_addr(std::vector<uint16_t>& btbAddrs) {
  uint16_t i{1000};
  for (auto& item : btbAddrs)
    item = i += 4;
}

void shuffleAddrs(std::vector<uint16_t>& btbAddrs) {
  std::shuffle(btbAddrs.begin(), btbAddrs.end(), engine);
}

ctype_pin_inst create_latencyBound_load(uint64_t ip, uint64_t uid) {
  ctype_pin_inst inst;
  memset(&inst, 0, sizeof(inst));
  inst.inst_uid = uid;
  inst.instruction_addr = ip;
  inst.instruction_next_addr = ip + 7;
  inst.size = 7;
  inst.op_type = OP_MOV;
  inst.fake_inst = 1;
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

ctype_pin_inst create_bandwidthBound_load(uint64_t ip, uint64_t uid) {
  ctype_pin_inst inst;
  memset(&inst, 0, sizeof(inst));
  inst.inst_uid = uid;
  inst.instruction_addr = ip;
  inst.instruction_next_addr = ip + 7;
  inst.size = 7;
  inst.op_type = OP_MOV;
  inst.fake_inst = 1;
  strcpy(inst.pin_iclass, "SYNTHETIC LOAD");
  inst.num_simd_lanes = 1;
  inst.lane_width_bytes = 1;
  inst.is_move = 1;
  inst.has_immediate = 0;
  inst.num_ld1_addr_regs = 1;
  inst.ld1_addr_regs[0] = Reg_Id::REG_RBX;
  inst.num_dst_regs = 1;
  inst.dst_regs[0] = Reg_Id::REG_RAX;
  inst.ld_vaddr[0] = (dist64(engine) % 0x800000000000);
  inst.num_ld = 1;
  inst.num_st = 0;
  inst.ld_size = 8;
  return inst;
}

ctype_pin_inst create_ILP_limited_add(uint64_t ip, uint64_t uid) {
  ctype_pin_inst inst;  // creating add rax, rbx  -- rax = rax + rbx
  memset(&inst, 0, sizeof(inst));
  inst.inst_uid = uid;
  inst.instruction_addr = ip;
  inst.instruction_next_addr = ip + 7;
  inst.size = 7;
  inst.op_type = OP_IADD;
  inst.fake_inst = 1;
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

ctype_pin_inst create_bp_limited(uint64_t ip, uint64_t uid, uint64_t tgtAddr, bool direction) {
  ctype_pin_inst inst;
  memset(&inst, 0, sizeof(inst));
  inst.inst_uid = uid;
  inst.instruction_addr = ip;
  inst.instruction_next_addr = direction ? tgtAddr : (ip + 2);
  inst.size = 2;
  inst.op_type = OP_CF;
  inst.cf_type = CF_CBR;
  inst.num_simd_lanes = 1;
  inst.lane_width_bytes = 1;
  inst.branch_target = direction ? tgtAddr : (ip + 2);
  inst.actually_taken = direction ? TAKEN : NOT_TAKEN;
  inst.fake_inst = 1;
  //  std::cout << " ip " << inst.instruction_addr << " Next " << inst.instruction_next_addr << " size "
  //             << (uint32_t)inst.size << " target " << inst.branch_target << inst.size << " taken "
  //             << (uint32_t)inst.actually_taken << std::endl;
  return inst;
}

ctype_pin_inst create_btb_limited(uint64_t ip, uint64_t uid, uint64_t tgtAddr) {
  ctype_pin_inst inst;
  memset(&inst, 0, sizeof(inst));
  inst.instruction_addr = ip;
  inst.inst_uid = uid;
  inst.instruction_next_addr = tgtAddr;
  inst.size = 2;
  inst.op_type = OP_CF;
  inst.cf_type = CF_BR;
  inst.num_simd_lanes = 1;
  inst.lane_width_bytes = 1;
  inst.branch_target = tgtAddr;
  inst.actually_taken = 1;
  inst.fake_inst = 1;
  strcpy(inst.pin_iclass, "DUMMY_JMP");
  return inst;
}

ctype_pin_inst create_icache_limited(uint64_t ip, uint64_t uid){
  ctype_pin_inst inst;
  memset(&inst, 0, sizeof(inst));
  inst.instruction_addr = ip;
  inst.inst_uid = uid;
  inst.instruction_next_addr = ip + 255;
  inst.size = 255;
  inst.op_type = OP_NOP;
  inst.num_simd_lanes = 1;
  inst.lane_width_bytes = 1;
  inst.fake_inst = 1;
  strcpy(inst.pin_iclass, "DUMMY_NOP");
  return inst;
}

ctype_pin_inst create_indirect_jmp(uint64_t ip, uint64_t uid, uint64_t tgtAddr, uint64_t vaddr){
    ctype_pin_inst inst;
    memset(&inst, 0, sizeof(inst));
    inst.instruction_addr = ip;
    inst.inst_uid = uid;
    inst.instruction_next_addr = tgtAddr;
    inst.size = 2;
    inst.op_type = OP_CF;
    inst.cf_type = CF_IBR;
    inst.num_simd_lanes = 1;
    inst.lane_width_bytes = 1;
    inst.branch_target = tgtAddr;
    inst.actually_taken = 1;
    inst.num_ld1_addr_regs = 1;
    inst.ld1_addr_regs[0] = REG_RAX;
    inst.fake_inst = 0;
    inst.num_ld  = 1;
    inst.ld_vaddr[0] = vaddr % 0x800000000000;      //ensure its in unprivileged space
    strcpy(inst.pin_iclass, "DUMMY_JMP");
    return inst;
}

ctype_pin_inst generatesyntheticInstr(BottleNeck_enum bottleneck_type, uint64_t ip, uint64_t uid) {
  switch (bottleneck_type) {
    case BottleNeck_enum::MEM_BANDWIDTH_LIMITED:
      return create_bandwidthBound_load(ip, uid);

    case BottleNeck_enum::MEM_LATENCY_LIMITED:
      return create_latencyBound_load(ip, uid);

    case BottleNeck_enum::BRANCH_PREDICTOR_LIMITED: {
      auto inst = create_bp_limited(jmp_ip, uid, tgtAddr, direction);
      uid_tgtMap.insert({uid, (uint64_t)inst.instruction_next_addr});
      tgtAddr = inst.instruction_next_addr + 4;  // tgtAddr +4 everytime
      jmp_ip = inst.instruction_next_addr;       // ip for next instr is either  target or fallthru
      direction = distBool(engine);              // randomize direction for next time conditional branch to be generated
      if (tgtAddr > 3000)                        //restart from IP 2000 after every 3000 branches |-> makes approx 100% btb hit
        tgtAddr = 2000;
      return inst;
    }

    case BottleNeck_enum::BTB_LIMITED: {
      auto inst = create_btb_limited(jmp_ip, uid, btbAddresses[btbaddrs_index_count]);
      uid_tgtMap.insert({uid, (uint64_t)inst.instruction_next_addr});
      jmp_ip = inst.instruction_next_addr;
      btbaddrs_index_count++;
      if (btbaddrs_index_count == 8192) {
        shuffleAddrs(btbAddresses);
        btbaddrs_index_count = 0;
      }
      return inst;
    }

      case BottleNeck_enum::ICACHE_LIMITED :
        return create_icache_limited(ip, uid);
      break;

      // case BottleNeck_enum::INDIRECT_BRANCH_LIMITED: {
      //   auto inst = create_indirect_jmp(jmp_ip, uid, tgtAddr, ld_vaddr); 
      //   uid_tgtMap.insert({uid, (uint64_t)inst.instruction_next_addr});
      //   jmp_ip    = inst.instruction_next_addr;
      //   tgtAddr   = inst.instruction_next_addr + 4;
      //   ld_vaddr  = inst.ld_vaddr[0] + 4;
      //   return inst;
      // }

       case BottleNeck_enum::INDIRECT_BRANCH_LIMITED: {
        ctype_pin_inst inst;
          //tgtAddr = at_loopback ? static_jmp_ip : ;
          if(!at_loopback){
            //std::cout << "not loop back" << std::endl;
            inst = create_indirect_jmp(jmp_ip, uid, btbAddresses[btbaddrs_index_count], ld_vaddr); } //this sends us to backward branch
          else{
            inst = create_btb_limited(jmp_ip, uid, static_jmp_ip);               //we are at the backward branch
            at_loopback = false;
          }
          uid_tgtMap.insert({uid, (uint64_t)inst.instruction_next_addr});
          btbaddrs_index_count++;
          jmp_ip    = inst.instruction_next_addr;
          ld_vaddr  = (inst.ld_vaddr[0] + 4) %  0x800000000000;
          if (btbaddrs_index_count == 8192) {
            shuffleAddrs(btbAddresses);
            btbaddrs_index_count = 0;
          };
        
          // inst = create_btb_limited(jmp_ip, uid, static_jmp_ip);
          // uid_tgtMap.insert({uid, (uint64_t)inst.instruction_next_addr});
          // jmp_ip        = static_jmp_ip;
          // // genReturnJmp  = false;
          // std::cout << "Return to indirect jump" << std::endl;
        
        return inst;
      }

    default:
      return create_dummy_nop(ip, WPNM_NOT_IN_WPNM);
  }
}


