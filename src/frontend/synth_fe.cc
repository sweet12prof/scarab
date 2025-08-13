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
std::mt19937_64 engine(3238);
std::uniform_int_distribution<uint64_t> dist64{0, std::numeric_limits<uint64_t>::max()};
std::bernoulli_distribution distBool(0.5);

void gen_and_shuffle_btb_addrs(std::vector<uint16_t>& btbAddrs);

ctype_pin_inst generatesyntheticInstr(BottleNeck_enum, uint64_t, uint64_t);
ctype_pin_inst create_latencyBound_load(uint64_t, uint64_t);
ctype_pin_inst create_bandwidthBound_load(uint64_t, uint64_t);
ctype_pin_inst create_ILP_limited_add(uint64_t, uint64_t);
ctype_pin_inst create_bp_limited_je(uint64_t, uint64_t, uint64_t, bool);
ctype_pin_inst create_btb_limited(uint64_t ip, uint64_t uid, uint64_t tgtAddr);

BottleNeck_enum bottleneck{BottleNeck_enum::BRANCH_PREDICTOR_LIMITED};

ctype_pin_inst dummyinst = {};
ctype_pin_inst* next_pi;
uint64_t jmp_ip = 4000;
uint64_t tgtAddr{jmp_ip + 65};
bool direction{distBool(engine)};
bool olddirection = direction;
std::vector<uint16_t> btbAddresses(65536);
uint16_t btbaddrs_index_count{0};

void synth_init() {
  // ASSERTM(0, !FETCH_OFF_PATH_OPS,
  //           "Trace frontend does not support wrong path. Turn off "
  //           "FETCH_OFF_PATH_OPS\n");

  uop_generator_init(NUM_CORES);
  next_pi = (ctype_pin_inst*)malloc(NUM_CORES * sizeof(ctype_pin_inst));
  dummyinst = generatesyntheticInstr(bottleneck, 1001, 1000);
  for (uns proc_id{0}; proc_id < NUM_CORES; proc_id++)
    next_pi[proc_id] = dummyinst;
  gen_and_shuffle_btb_addrs(btbAddresses);
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
  } else {
    uop_generator_get_uop(proc_id, op, NULL);
  }

  if (uop_generator_get_eom(proc_id)) {
    dummyinst = generatesyntheticInstr(bottleneck, dummyinst.instruction_next_addr, ++dummyinst.inst_uid);
    next_pi[proc_id] = dummyinst;
    op->exit = FALSE;
    trace_read_done[proc_id] = FALSE;
    reached_exit[proc_id] = FALSE;
  }
}

void synth_redirect(uns proc_id, uns64 inst_uid, Addr fetch_addr) {
}

void synth_recover(uns proc_id, uns64 inst_uid) {
}

void synth_retire(uns proc_id, uns64 inst_uid) {
}

void synth_fill_in_dynamic_info(uns proc_id) {
}

// generate and shuffle synthetic indirect jump addresses
void gen_and_shuffle_btb_addrs(std::vector<uint16_t>& btbAddrs) {
  uint16_t i{0};
  for (auto& item : btbAddrs)
    item = ++i;
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

ctype_pin_inst create_bp_limited_je(uint64_t ip, uint64_t uid, uint64_t tgtAddr, bool direction) {
  ctype_pin_inst inst;
  memset(&inst, 0, sizeof(inst));
  // inst.inst_uid = uid;
  inst.instruction_addr = ip;
  // inst.instruction_next_addr = direction  ? tgtAddr : ip + 1;

  if(direction)
    inst.instruction_next_addr = tgtAddr;
  else 
    inst.instruction_next_addr = ip + 1;

  inst.size = 1;
  inst.op_type = OP_CF;
  inst.cf_type = CF_CBR;
  inst.num_simd_lanes = 1;
  inst.lane_width_bytes = 1;
  inst.branch_target = tgtAddr;
  inst.actually_taken = static_cast<uint8_t> (direction);
  inst.fake_inst = 1;
  return inst;
}

ctype_pin_inst create_btb_limited(uint64_t ip, uint64_t uid, uint64_t tgtAddr) {
  ctype_pin_inst inst;
  memset(&inst, 0, sizeof(inst));
  inst.instruction_addr = ip;
  inst.instruction_next_addr = ip + 1;
  inst.size = 1;
  inst.op_type = OP_CF;
  inst.cf_type = CF_IBR;
  inst.num_simd_lanes = 1;
  inst.lane_width_bytes = 1;
  inst.branch_target = tgtAddr;
  inst.actually_taken = 1;
  inst.fake_inst = 1;
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
      std::cout << "direction is " << direction << " jmp_ip is " << jmp_ip << " tgt is " << tgtAddr << std::endl;
      auto inst = create_bp_limited_je(jmp_ip, uid, tgtAddr, direction);
      jmp_ip = direction ? tgtAddr : jmp_ip + 1;  // ip for next instr is either fall through(+6) or target
      tgtAddr += 65;                               // tgtAddr +2 everytime
      direction = distBool(engine); // randomize direction for next time conditional branch to be generated
      return inst;
    }

    case BottleNeck_enum::BTB_LIMITED: {
      auto inst = create_btb_limited(jmp_ip, uid, btbAddresses[btbaddrs_index_count]);
      jmp_ip = btbAddresses[btbaddrs_index_count];
      btbaddrs_index_count++;
      return inst;
    }

      // case BottleNeck_enum::ICACHE_LIMITED :{}
      // break;

      // case BottleNeck_enum::INDIRECT_BRANCH_LIMITED: {}
      // break;

    default:
      return create_dummy_nop(ip, WPNM_NOT_IN_WPNM);
  }
}
