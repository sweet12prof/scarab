extern "C" {
#include "globals/assert.h"
#include "globals/global_defs.h"
#include "globals/global_types.h"
#include "globals/global_vars.h"
#include "globals/utils.h"

#include "debug/debug.param.h"
#include "debug/debug_macros.h"
#include "debug/debug_print.h"

#include "bp/bp.param.h"
#include "memory/memory.param.h"
}

#include <iostream>

#include "bp/bp.h"
#include "frontend/synthetic/synth_fe.h"
#include "frontend/synthetic/synthetic_kernels.h"
#include "pin/pin_lib/uop_generator.h"

#include "ctype_pin_inst.h"

#define PRINT_INFO
#define DEBUG(proc_id, args...) _DEBUG(proc_id, DEBUG_SYNTHETIC_INST, ##args)

// intrinsic frontend variables
static uint NOP_SIZE;
static uint BRANCH_SIZE;
static ctype_pin_inst next_onpath_pi[MAX_NUM_PROCS];
static ctype_pin_inst next_offpath_pi[MAX_NUM_PROCS];
static bool off_path_mode[MAX_NUM_PROCS] = {false};
static uint64_t off_path_addr[MAX_NUM_PROCS] = {0};
static ctype_pin_inst dummyinst = {};

// For generting Random branch targets and branch directions
uint64_t synth_start_pc{256};
uint64_t synth_start_uid{1000};
extern uns cf_count;

// void gen_addr();
// void shuffleAddrs();

void synth_init() {
  bottleneck = static_cast<BottleNeck_enum>(BOTTLENECK);
  uop_generator_init(NUM_CORES);
  synthetic_kernel_init();
  NOP_SIZE = ICACHE_LINE_SIZE / ISSUE_WIDTH;
  BRANCH_SIZE = ICACHE_LINE_SIZE - (NOP_SIZE * (ISSUE_WIDTH - 1));

  std::cout << "Simulating synthetic " << bottleneckNames[BOTTLENECK] << " bottleneck" << std::endl;
  std::cout << " NOP SIZE " << NOP_SIZE << " BRANCH SIZE " << BRANCH_SIZE << std::endl;
  dummyinst = generate_synthetic_microkernel(0, bottleneck, synth_start_pc, synth_start_uid);

  for (uns proc_id{0}; proc_id < NUM_CORES; proc_id++) {
    next_onpath_pi[proc_id] = dummyinst;
    // generate initial instruction for other cores if any
    if ((proc_id + 1) != NUM_CORES)
      dummyinst = generate_synthetic_microkernel(proc_id, bottleneck, dummyinst.instruction_next_addr,
                                                 ++dummyinst.inst_uid);
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
  if (uop_generator_get_bom(proc_id)) {
    if (!off_path_mode[proc_id]) {
      uop_generator_get_uop(proc_id, op, &next_onpath_pi[proc_id]);
    } else {
      uop_generator_get_uop(proc_id, op, &next_offpath_pi[proc_id]);
    }
#ifdef PRINT_INFO
    ctype_pin_inst next_pi = off_path_mode[proc_id] ? next_offpath_pi[proc_id] : next_onpath_pi[proc_id];
    std::cout << disasm_op(op, TRUE) << ": ip " << next_pi.instruction_addr << " Next " << next_pi.instruction_next_addr
              << " size " << (uint32_t)next_pi.size << " target " << next_pi.branch_target << " size "
              << (uint32_t)next_pi.size << " taken " << (uint32_t)next_pi.actually_taken << " uid " << next_pi.inst_uid
              << std::endl;
    if(cf_count == 0)
        std::cout << std::endl;
#endif
  } else {
    uop_generator_get_uop(proc_id, op, NULL);
  }

  if (uop_generator_get_eom(proc_id)) {
    if (!off_path_mode[proc_id]) {
      dummyinst = generate_synthetic_microkernel(proc_id, bottleneck, next_onpath_pi[proc_id].instruction_next_addr,
                                                 ++dummyinst.inst_uid);
      next_onpath_pi[proc_id] = dummyinst;
    } else {
      dummyinst = generate_synthetic_microkernel(proc_id, bottleneck, next_offpath_pi[proc_id].instruction_next_addr,
                                                 ++dummyinst.inst_uid);
      next_offpath_pi[proc_id] = dummyinst;
    }
  }
}

void synth_redirect(uns proc_id, uns64 inst_uid, Addr fetch_addr) {
  off_path_mode[proc_id] = true;
  off_path_addr[proc_id] = fetch_addr;
  dummyinst =
      generate_synthetic_microkernel(proc_id, bottleneck, fetch_addr, inst_uid);
  next_offpath_pi[proc_id] = dummyinst;
  DEBUG(proc_id, "Redirect on-path:%lx off-path:%lx", next_onpath_pi[proc_id].instruction_addr,
        next_offpath_pi[proc_id].instruction_addr);

#ifdef PRINT_INFO
  std::cout << "Redirect happened here predicted addr:  " << fetch_addr << std::endl;
#endif

  cf_count = 1;
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
  std::cout << " Recover happened at " << cycle_count << " cycle " << std::endl;
#endif
}

void synth_retire(uns proc_id, uns64 inst_uid) {
}
