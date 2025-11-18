/* Synthetic generator Functions*/
#ifndef __SYNTHETIC_KERNELS_H__
#define __SYNTHETIC_KERNELS_H__

#include "globals/global_types.h"
#include "ctype_pin_inst.h"
#include <stdbool.h>

// enum class
typedef enum BottleNeck_Id_enum {
  #define BOTTLENECK_IMPL(id, name) id, 
  #include "frontend/synthetic/bottlenecks_table.def"
  #undef BOTTLENECK_IMPL
  INVALID
} BottleNeck_enum;

// top_level function to generate synthetic workload
ctype_pin_inst generate_synthetic_microkernel(uns, BottleNeck_enum, uns64, uns64, bool);  

// Very basic One Line Instructions
ctype_pin_inst generate_loop_carried_dependence_load(uns64, uns64, uns64);            //for latency limited workload       
ctype_pin_inst generate_independent_operand_load(uns64, uns64, uns64 );        //for bandwidth limited workload, normal load no dependences
ctype_pin_inst generate_conditional_branch(uns64, uns64, uns64, bool, uns8);
ctype_pin_inst generate_unconditional_branch(uns64, uns64, uns64, uns8);
ctype_pin_inst generate_indirect_branch (uns64, uns64, uns64, uns64, uns8);
//misc 

//--------------Workloads------------------------//
ctype_pin_inst create_ILP_limited(uns64, uns64);
ctype_pin_inst create_icache_limited(uns64, uns64);
ctype_pin_inst generate_load_latency_limited_microkernel(uns64, uns64);
ctype_pin_inst generate_load_bandwidth_limited_microkernel(uns64, uns64);
ctype_pin_inst generate_cbr_limited_microkernel(uns64, uns64, uns64, uns64);
ctype_pin_inst generate_br_limited_microkernel(uns64, uns64, uns64, bool, uns8);
ctype_pin_inst generate_ibr_limited_microkernel(uns64, uns64, uns64, bool, uns8);

//----------------synthetic_kernel_init-------------------//
void synthetic_kernel_init();

extern const char * bottleneckNames[];
extern BottleNeck_enum bottleneck;

#endif //SYNTHETIC KERNELS