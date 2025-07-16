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

#include "../../../ctype_pin_inst.h"
#include <cstdlib>
#include "pin/pin_lib/uop_generator.h"
#include "frontend/synth_fe.h"
#include "pt_memtrace/memtrace_fe.h"
#include <iostream>




#include "./pin/pin_lib/uop_generator.h"
#include "bp/bp.h"

#include "ctype_pin_inst.h"
#include "statistics.h"

#include <random>
#include <limits>


#define DR_DO_NOT_DEFINE_int64
#define DEBUG(proc_id, args...) _DEBUG(proc_id, DEBUG_SYNTHETIC_INST , ##args)
#define SYNTHETIC_RUN_LENGTH 1000000
#define SYNTHETIC_LOAD_INSTR_SIZE 7




//private member functions
ctype_pin_inst create_dummy_load(std::uint64_t ip );

ctype_pin_inst* next_pi;
uns64 synth_inst_run_count = 0; //counter for synthetic run length
ctype_pin_inst dummyinst = create_dummy_load(0x4010011);;

std::default_random_engine engine;
std::uniform_int_distribution<uns64> dist64{0, std::numeric_limits<uns64>::max()};

void synth_init(){
    ASSERTM(0, !FETCH_OFF_PATH_OPS,
          "Trace frontend does not support wrong path. Turn off "
          "FETCH_OFF_PATH_OPS\n");

    uop_generator_init(NUM_CORES);
    next_pi = (ctype_pin_inst*) malloc (NUM_CORES * sizeof(ctype_pin_inst));
}

void synth_done(){}

Addr synth_next_fetch_addr(uns proc_id){
  return convert_to_cmp_addr(proc_id,( next_pi[proc_id].instruction_addr));
   // return dummyinst.instruction_next_addr;
 }

Flag synth_can_fetch_op(uns proc_id){
   
   return !(uop_generator_get_eom(proc_id) && trace_read_done[proc_id]);
 }

void synth_fetch_op(uns proc_id, Op* op){
    DEBUG(proc_id, "Fetch Op begin:\n");
 next_pi = &dummyinst;
if (uop_generator_get_bom(proc_id)) {
    ASSERT(proc_id, !trace_read_done[proc_id] && !reached_exit[proc_id]);
    uop_generator_get_uop(proc_id, op, &next_pi[proc_id]);
  } else {
    uop_generator_get_uop(proc_id, op, NULL);
  }

  if (uop_generator_get_eom(proc_id)) {
    int success = (synth_inst_run_count == SYNTHETIC_RUN_LENGTH);
     dummyinst = create_dummy_load(dummyinst.instruction_next_addr);
     synth_inst_run_count++;
    if(success){
      trace_read_done[proc_id] = TRUE;
      reached_exit[proc_id] = TRUE;
      /* this flag is supposed to be set in uop_generator_get_uop() but there
       * is a circular dependency on trace_read_done to be set. So, we set
       * op->exit here. */
        op->exit = TRUE;
    }
  }

 }

void synth_redirect(uns proc_id, uns64 inst_uid, Addr fetch_addr){ }

void synth_recover(uns proc_id, uns64 inst_uid){ }

void synth_retire(uns proc_id, uns64 inst_uid){ }

void synth_fill_in_dynamic_info(uns proc_id){
 
}

ctype_pin_inst create_dummy_load(std::uint64_t ip){
    ctype_pin_inst inst;
    memset(&inst, 0, sizeof(inst));
    
    inst.inst_uid = 1011;

    inst.instruction_addr = ip;
    inst.instruction_next_addr = ip + SYNTHETIC_LOAD_INSTR_SIZE;
    
    inst.size = SYNTHETIC_LOAD_INSTR_SIZE;
    inst.op_type = OP_MOV;
   
    inst.fake_inst = 1;
    strcpy(inst.pin_iclass, "SYNTHETIC LOAD");
   
    inst.num_simd_lanes = 1;
    inst.lane_width_bytes = 1;
    
    inst.is_move = 1;
    inst.has_immediate = 0;

    inst.num_ld1_addr_regs = 1;
    inst.ld1_addr_regs[0] = Reg_Id::REG_RIP;

    inst.num_dst_regs = 1;
    inst.dst_regs[0] = Reg_Id::REG_RAX;

    inst.ld_vaddr[0] = inst.instruction_addr + 0x40000000;

    inst.num_ld = 1;
    inst.num_st = 0;

    inst.ld_size = 8;
    return inst;
}
