extern "C" {
#include "globals/assert.h"
#include "globals/global_defs.h"
#include "globals/global_types.h"
#include "globals/global_vars.h"
#include "globals/utils.h"

#include "debug/debug.param.h"
#include "debug/debug_macros.h"
}

#include "../../../ctype_pin_inst.h"
#include "pin/pin_lib/x86_decoder.h"
#include <cstdlib>
#include "pin/pin_lib/uop_generator.h"
#include "frontend/synth_fe.h"
#include "pt_memtrace/memtrace_fe.h"
#include <iostream>


#define DEBUG(proc_id, args...) _DEBUG(proc_id, DEBUG_SYNTHETIC_INST , ##args)


xed_decoded_inst_t gen_decoded_xed_uncacheable_load_inst(uns64); //helper func prototype not in header synth_fe_h

void synth_init(){}
void synth_done(){}

//remember to edit num of cores later, no spec given. 
ctype_pin_inst* next_pi = (ctype_pin_inst*) malloc (NUM_CORES * sizeof(ctype_pin_inst));

Addr synth_next_fetch_addr(uns proc_id){ return 0; }

Flag synth_can_fetch_op(uns proc_id){return 1; }

void synth_fetch_op(uns proc_id, struct Op_struct* op){
    DEBUG(proc_id, "Fetch Op begin:\n");
    uns64 uncacheable_mem_address = 0xF0000000;
      
    xed_decoded_inst_t decoded_ins = gen_decoded_xed_uncacheable_load_inst(uncacheable_mem_address);
    
    fill_in_basic_info(&next_pi[proc_id], &decoded_ins);
    
    fill_in_simd_info(&next_pi[proc_id], &decoded_ins, 0); //if there are defaults this may not be necessary
    fill_in_cf_info(&next_pi[proc_id], &decoded_ins);

 }

void synth_redirect(uns proc_id, uns64 inst_uid, Addr fetch_addr){ }

void synth_recover(uns proc_id, uns64 inst_uid){ }

void synth_retire(uns proc_id, uns64 inst_uid){ }

xed_decoded_inst_t gen_decoded_xed_uncacheable_load_inst(uns64 uncacheable_mem_address){
   //  DEBUG(proc_id, "Fetch Op begin:\n");
    //init xed
    xed_tables_init();

    uns8 encoded_x64_inst[5];
    //set opcode
    encoded_x64_inst[0] = 0xA1;

    for(size_t i = 1; i < 5; i++){
        encoded_x64_inst[i] = (uns8) uncacheable_mem_address % 256; //get lsbyte
        uncacheable_mem_address >>= 8; //shift to next ls byte
    }

    //set machine info
    xed_state_t xed_state;
    xed_state_zero(&xed_state);

    xed_state.mmode = XED_MACHINE_MODE_LEGACY_32;
    xed_state.stack_addr_width = XED_ADDRESS_WIDTH_32b;

    xed_decoded_inst_t decoded_uncacheable_load_inst;
    xed_decoded_inst_zero_set_mode(&decoded_uncacheable_load_inst, &xed_state);

    xed_error_enum_t err = xed_decode(&decoded_uncacheable_load_inst, encoded_x64_inst, 5);
    assert(err == XED_ERROR_NONE && "error creating uncacheable load");
    xed_reg_enum_t reg = xed_decoded_inst_get_reg(&decoded_uncacheable_load_inst, XED_OPERAND_REG0);
    assert(reg == XED_REG_EAX);
    return decoded_uncacheable_load_inst;
}