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

#include <cstdlib>
#include <iostream>
#include <limits>
#include <random>

#include "../../../ctype_pin_inst.h"
#include "./pin/pin_lib/uop_generator.h"
#include "bp/bp.h"
#include "frontend/synth_fe.h"
#include "pin/pin_lib/uop_generator.h"
#include "pt_memtrace/memtrace_fe.h"

#include "ctype_pin_inst.h"
#include "statistics.h"

#define DEBUG(proc_id, args...) _DEBUG(proc_id, DEBUG_SYNTHETIC_INST, ##args)

// private member functions
ctype_pin_inst create_generic_load(uint64_t, uint64_t, Reg_Id, Reg_Id);
ctype_pin_inst create_dummy_load(uint64_t, uint64_t);
void genLoad();

ctype_pin_inst* next_pi;
uint64_t synth_inst_run_count = 0;  // counter for synthetic run length
uint64_t loadAddr = 0;
ctype_pin_inst dummyinst = {};

std::mt19937_64 engine;
std::uniform_int_distribution<uint64_t> dist64{0, std::numeric_limits<uint64_t>::max()};
void synth_init() {
  ASSERTM(0, !FETCH_OFF_PATH_OPS,
          "Trace frontend does not support wrong path. Turn off "
          "FETCH_OFF_PATH_OPS\n");

  uop_generator_init(NUM_CORES);
  next_pi = (ctype_pin_inst*)malloc(NUM_CORES * sizeof(ctype_pin_inst));
  dummyinst = create_dummy_load(1001, 1000);
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
  } else {
    uop_generator_get_uop(proc_id, op, NULL);
  }

  if (uop_generator_get_eom(proc_id)) {
    dummyinst = create_dummy_load(dummyinst.instruction_next_addr, ++dummyinst.inst_uid);
    next_pi[proc_id] = dummyinst;
    synth_inst_run_count++;
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

//----Load latency limited----------------------//
ctype_pin_inst create_dummy_load(uint64_t ip, uint64_t uid) {
  ctype_pin_inst inst;
  memset(&inst, 0, sizeof(inst));
  genLoad();
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
  inst.ld_vaddr[0] = loadAddr;
  inst.num_ld = 1;
  inst.num_st = 0;
  inst.ld_size = 8;
  return inst;
}
void genLoad() {
  loadAddr = (dist64(engine) % 0x800000000000);
}

//--genericLoad ---//
ctype_pin_inst create_generic_load(uint64_t, uint64_t, Reg_Id, Reg_Id){
  
}