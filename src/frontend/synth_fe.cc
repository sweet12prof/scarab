#include "frontend/synth_fe.h"
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
#include "pin/pin_lib/uop_generator.h"
#include "pt_memtrace/memtrace_fe.h"

#include "ctype_pin_inst.h"
#include "statistics.h"
#include "xed-iclass-enum.h"

//#define PRINT_INFO
#define DEBUG(proc_id, args...) _DEBUG(proc_id, DEBUG_SYNTHETIC_INST, ##args)

// names of bottlenecks
const char* bottleneckNames[] = {
#define BOTTLENECK_IMPL(id, name) name,
#include "bottlenecks_table.def"
#undef BOTTLENECK_IMPL
    "invalid"};

// intrinsic frontend variables
ctype_pin_inst next_onpath_pi[MAX_NUM_PROCS];
ctype_pin_inst next_offpath_pi[MAX_NUM_PROCS];
bool off_path_mode[MAX_NUM_PROCS] = {false};
uint64_t off_path_addr[MAX_NUM_PROCS] = {0};
BottleNeck_enum bottleneck;
ctype_pin_inst dummyinst = {};

// For generting Random branch targets and CBRs
uint64_t start_pc{256};
uint64_t start_uid{1000};
uint64_t tgtAddr{start_pc + 32};
uint64_t ld_vaddr{0xf800000};
bool direction{false}, at_loopback{false};
std::vector<uint64_t> btbAddresses(100);
uint64_t btbaddrs_index_count{0};
std::unordered_map<uint64_t, uint64_t> uid_tgtMap;
std::random_device rd;
std::mt19937_64 engine(1234);
std::uniform_int_distribution<uint64_t> dist64{0, std::numeric_limits<uint64_t>::max()};
std::bernoulli_distribution distBool(0.5);
void gen_addr();
void shuffleAddrs();
uns round_trip = 0;
uns cf_count = 0;

/* Synthetic generator Functions*/
ctype_pin_inst generatesyntheticInstr(uns, BottleNeck_enum, uint64_t, uint64_t);
ctype_pin_inst create_latencyBound(uint64_t, uint64_t);
ctype_pin_inst create_bandwidthBound(uint64_t, uint64_t);
ctype_pin_inst create_ILP_limited(uint64_t, uint64_t);
ctype_pin_inst create_bp_limited(uint64_t, uint64_t, uint64_t, bool);
ctype_pin_inst create_btb_limited(uint64_t, uint64_t, uint64_t);
ctype_pin_inst create_icache_limited(uint64_t, uint64_t);
ctype_pin_inst create_indirect_jmp(uint64_t, uint64_t, uint64_t, uint64_t);

void synth_init() {
  bottleneck = static_cast<BottleNeck_enum>(BOTTLENECK);
  uop_generator_init(NUM_CORES);
  gen_addr();
  std::cout << "bottleneck is " << bottleneck << std::endl;
  std::cout << "Simulating synthetic " << bottleneckNames[BOTTLENECK] << " bottleneck" << std::endl;
  dummyinst = generatesyntheticInstr(0, bottleneck, start_pc, start_uid);
  for (uns proc_id{0}; proc_id < NUM_CORES; proc_id++) {
    next_onpath_pi[proc_id] = dummyinst;
    // generate initial instruction for other cores if any
    if ((proc_id + 1) != NUM_CORES)
      dummyinst = generatesyntheticInstr(proc_id, bottleneck, dummyinst.instruction_next_addr, ++dummyinst.inst_uid);
#ifdef PRINT_INFO
    std::cout << " ip " << next_onpath_pi[proc_id].instruction_addr << " Next "
              << next_onpath_pi[proc_id].instruction_next_addr << " size " << (uint32_t)next_onpath_pi[proc_id].size
              << " target " << next_onpath_pi[proc_id].branch_target << " size "
              << (uint32_t)next_onpath_pi[proc_id].size << " taken " << (uint32_t)next_onpath_pi[proc_id].actually_taken
              << " cycle count " << cycle_count << std::endl;
#endif
  }
}

void synth_done() {
}

Addr synth_next_fetch_addr(uns proc_id) {
  return next_onpath_pi[proc_id].instruction_addr;
}

Flag synth_can_fetch_op(uns proc_id) {
  return !(uop_generator_get_eom(proc_id) && trace_read_done[proc_id]);
}

void synth_fetch_op(uns proc_id, Op* op) {
#ifdef PRINT_INFO
  ctype_pin_inst next_pi = off_path_mode[proc_id] ? next_offpath_pi[proc_id] : next_onpath_pi[proc_id];
  std::cout << " ip " << next_pi.instruction_addr << " Next " << next_pi.instruction_next_addr << " size "
            << (uint32_t)next_pi.size << " target " << next_pi.branch_target << " size " << (uint32_t)next_pi.size
            << " taken " << (uint32_t)next_pi.actually_taken << " cycle count " << cycle_count << std::endl;
#endif
  if (uop_generator_get_bom(proc_id)) {
    if (!off_path_mode[proc_id]) {
      uop_generator_get_uop(proc_id, op, &next_onpath_pi[proc_id]);
    } else {
      uop_generator_get_uop(proc_id, op, &next_offpath_pi[proc_id]);
    }
  } else {
    uop_generator_get_uop(proc_id, op, NULL);
  }

  if (uop_generator_get_eom(proc_id)) {
    if (!off_path_mode[proc_id]) {
      dummyinst = generatesyntheticInstr(proc_id, bottleneck, next_onpath_pi[proc_id].instruction_next_addr,
                                         ++dummyinst.inst_uid);
      next_onpath_pi[proc_id] = dummyinst;
    } else {
      dummyinst = generatesyntheticInstr(proc_id, bottleneck, next_offpath_pi[proc_id].instruction_next_addr,
                                         ++dummyinst.inst_uid);
      next_offpath_pi[proc_id] = dummyinst;
    }
  }
}

void synth_redirect(uns proc_id, uns64 inst_uid, Addr fetch_addr) {
  off_path_mode[proc_id] = true;
  off_path_addr[proc_id] = fetch_addr;
  dummyinst = generatesyntheticInstr(proc_id, bottleneck, fetch_addr, ++dummyinst.inst_uid);
  next_offpath_pi[proc_id] = dummyinst;
  DEBUG(proc_id, "Redirect on-path:%lx off-path:%lx", next_onpath_pi[proc_id].instruction_addr,
        next_offpath_pi[proc_id].instruction_addr);
#ifdef PRINT_INFO
  std::cout << " Redirect happened here predicted bp addr is " << fetch_addr << std::endl;
  // std::cout << "Redirect happened at " << cycle_count << " cycles " << std::endl;
#endif
}

void synth_recover(uns proc_id, uns64 inst_uid) {
  Op dummy_op;
  off_path_mode[proc_id] = false;
  // Finish decoding of the current off-path inst before switching to on-path
  while (!uop_generator_get_eom(proc_id)) {
    uop_generator_get_uop(proc_id, &dummy_op, &next_offpath_pi[proc_id]);
  }
  DEBUG(proc_id, "Recover CF:%lx ", next_onpath_pi[proc_id].instruction_addr);
#ifdef PRINT_INFO
  // std::cout << " Recovery happended here " << std::endl;
  std::cout << " Recover happened at " << cycle_count << " cycle " << std::endl;
#endif
}

void synth_retire(uns proc_id, uns64 inst_uid) {
}

ctype_pin_inst generatesyntheticInstr(uns proc_id, BottleNeck_enum bottleneck_type, uint64_t ip, uint64_t uid) {
  switch (bottleneck_type) {
    case MEM_BANDWIDTH_LIMITED: {
      ctype_pin_inst inst;
      if (!off_path_mode[proc_id]) {
        if (ip >= 2000) {
          inst = create_btb_limited(ip, uid, start_pc);
        } else
          inst = create_bandwidthBound(ip, uid);
      } else {
         inst = create_dummy_nop(ip, WPNM_REASON_REDIRECT_TO_NOT_INSTRUMENTED);
         inst.size = 16;
         inst.instruction_next_addr = ip + 16;
         cf_count++;
      }
      return inst;
    }

    case MEM_LATENCY_LIMITED: {
      ctype_pin_inst inst;
      if (!off_path_mode[proc_id]) {
        if (ip >= 2000) {
          inst = create_btb_limited(ip, uid, start_pc);
        } else
          inst = create_latencyBound(ip, uid);
      } else {
         inst = create_dummy_nop(ip, WPNM_REASON_REDIRECT_TO_NOT_INSTRUMENTED);
         inst.size = 16;
         inst.instruction_next_addr = ip + 16;
         cf_count++;
      }
      return inst;
    }

    case BRANCH_PREDICTOR_LIMITED: {
      ctype_pin_inst inst;
      if (!off_path_mode[proc_id]) {
        if (tgtAddr > 1200) {
          inst = create_btb_limited(ip, uid, start_pc);
          tgtAddr = start_pc;
        } else if (cf_count == 5) {
          inst = create_bp_limited(ip, uid, tgtAddr, direction);
          cf_count = 0;
        } else {
          inst = create_dummy_nop(ip, WPNM_REASON_REDIRECT_TO_NOT_INSTRUMENTED);
          inst.size = 16;
          inst.instruction_next_addr = ip + 16;
          cf_count++;
        }

        tgtAddr = inst.instruction_next_addr + 32;  // tgtAddr +2 everytime
        direction = distBool(engine);  // randomize direction for next time conditional branch to be generated

      } else {
        inst = create_dummy_nop(ip, WPNM_REASON_REDIRECT_TO_NOT_INSTRUMENTED);
        inst.size = 16;
        inst.instruction_next_addr = ip + 16;
        // inst = create_bp_limited(ip, uid, tgtAddr, NOT_TAKEN);
      }

      return inst;
    }

    case BTB_LIMITED: {
      ctype_pin_inst inst;
      if (!off_path_mode[proc_id]) {
        if (cf_count == 5) {
          inst = create_btb_limited(ip, uid, btbAddresses[btbaddrs_index_count]);
          btbaddrs_index_count++;
          cf_count = 0;
          if (btbaddrs_index_count == 5 ) {
            inst = create_btb_limited(ip, uid, start_pc);
            btbaddrs_index_count = 0;
          }
        } else {
          inst = create_dummy_nop(ip, WPNM_REASON_REDIRECT_TO_NOT_INSTRUMENTED);
          inst.size = 16;
          inst.instruction_next_addr = ip + 16;
          cf_count++;
        }
      } else {
        inst = create_dummy_nop(ip, WPNM_REASON_REDIRECT_TO_NOT_INSTRUMENTED);
        inst.size = 16;
        inst.instruction_next_addr = ip + 16;
      }

      return inst;
    }

    case ICACHE_LIMITED:
      return create_icache_limited(ip, uid);
      break;

    case INDIRECT_BRANCH_LIMITED: {
      ctype_pin_inst inst;
      if (!off_path_mode[proc_id]) {
        if (cf_count == 5) {
          inst = create_indirect_jmp(ip, uid, btbAddresses[btbaddrs_index_count], ld_vaddr);
          btbaddrs_index_count++;
          ld_vaddr = (inst.ld_vaddr[0] + 8) % 0x800000000000;
          cf_count = 0;
          if (btbaddrs_index_count == 5 ) {
            inst = create_btb_limited(ip, uid, start_pc);
            btbaddrs_index_count = 0;
          }
        } else {
          inst = create_dummy_nop(ip, WPNM_REASON_REDIRECT_TO_NOT_INSTRUMENTED);
          inst.size = 16;
          inst.instruction_next_addr = ip + 16;
          cf_count++;
        }
      } else {
        inst = create_dummy_nop(ip, WPNM_REASON_REDIRECT_TO_NOT_INSTRUMENTED);
        inst.size = 16;
        inst.instruction_next_addr = ip + 16;
      }

      return inst;
    }

    case ILP_LIMITED: {
      ctype_pin_inst inst;
      if (!off_path_mode[proc_id]) {
        if (ip >= 2000) {
          inst = create_btb_limited(ip, uid, start_pc);
        } else
          inst = create_ILP_limited(ip, uid);
      } else {
         inst = create_dummy_nop(ip, WPNM_REASON_REDIRECT_TO_NOT_INSTRUMENTED);
         inst.size = 16;
         inst.instruction_next_addr = ip + 16;
         cf_count++;
      }
      return inst;
    }

    default:
      return create_dummy_nop(ip, WPNM_NOT_IN_WPNM);
  }
}

ctype_pin_inst create_latencyBound(uint64_t ip, uint64_t uid) {
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

ctype_pin_inst create_bandwidthBound(uint64_t ip, uint64_t uid) {
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
  inst.ld1_addr_regs[0] = Reg_Id::REG_RBX;
  inst.num_dst_regs = 1;
  inst.dst_regs[0] = Reg_Id::REG_RAX;
  inst.ld_vaddr[0] = (dist64(engine) % 0x800000000000);
  inst.num_ld = 1;
  inst.num_st = 0;
  inst.ld_size = 8;
  return inst;
}

ctype_pin_inst create_ILP_limited(uint64_t ip, uint64_t uid) {
  ctype_pin_inst inst;  // creating add rax, rbx  -- rax = rax + rbx
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


ctype_pin_inst create_bp_limited(uint64_t ip, uint64_t uid, uint64_t tgtAddr, bool direction) {
  ctype_pin_inst inst;
  memset(&inst, 0, sizeof(inst));
  inst.inst_uid = uid;
  inst.instruction_addr = ip;
  inst.instruction_next_addr = direction ? tgtAddr : (ip + 16);
  inst.size = 16;
  inst.op_type = OP_CF;
  inst.cf_type = CF_CBR;
  inst.num_simd_lanes = 1;
  inst.lane_width_bytes = 1;
  inst.branch_target = tgtAddr;
  inst.actually_taken = direction ? TAKEN : NOT_TAKEN;
  inst.fake_inst = 0;
  return inst;
}

ctype_pin_inst create_btb_limited(uint64_t ip, uint64_t uid, uint64_t tgt) {
  ctype_pin_inst inst;
  memset(&inst, 0, sizeof(inst));
  inst.instruction_addr = ip;
  inst.inst_uid = uid;
  inst.instruction_next_addr = tgt;
  inst.size = 16;
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

ctype_pin_inst create_icache_limited(uint64_t ip, uint64_t uid) {
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

ctype_pin_inst create_indirect_jmp(uint64_t ip, uint64_t uid, uint64_t tgtAddr, uint64_t vaddr) {
  ctype_pin_inst inst;
  memset(&inst, 0, sizeof(inst));
  inst.instruction_addr = ip;
  inst.inst_uid = uid;
  inst.instruction_next_addr = tgtAddr;
  inst.size = 16;
  inst.op_type = OP_CF;
  inst.cf_type = CF_IBR;
  inst.num_simd_lanes = 1;
  inst.lane_width_bytes = 1;
  inst.branch_target = tgtAddr;
  inst.actually_taken = 1;
  inst.fake_inst = 0;
  inst.num_src_regs = 1;
  inst.num_ld1_addr_regs = 1;
  inst.ld1_addr_regs[0] = REG_RAX;
  inst.num_ld = 1;
  inst.ld_vaddr[0] = vaddr % 0x800000000000;  // ensure its in unprivileged space
  strcpy(inst.pin_iclass, "DUMMY_JMP");
  return inst;
}

// generate and shuffle synthetic indirect jump addresses
void gen_addr() {
  uint64_t i{256};
  for (auto& item : btbAddresses) {
    i = (i + 2048);
    item = i;
  }
  // btbAddresses.back() = start_pc;
}

void shuffleAddrs() {
  std::rotate(btbAddresses.rbegin(), btbAddresses.rbegin() + 1, btbAddresses.rend());
  //std::rotate(btbAddresses.begin(), btbAddresses.begin() + 1, btbAddresses.end());
}
