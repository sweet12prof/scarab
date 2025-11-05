#ifndef __SYNTH_FE_H__
#define __SYNTH_FE_H__
#include "globals/global_types.h"
#ifdef __cplusplus
extern "C" {
#endif
struct Op_struct;

void synth_init();
void synth_done();


/*Front End Interface*/
Addr synth_next_fetch_addr(uns proc_id);
Flag synth_can_fetch_op(uns proc_id);
void synth_fetch_op(uns proc_id, Op* op);
void synth_redirect(uns proc_id, uns64 inst_uid, Addr fetch_addr);
void synth_recover(uns proc_id, uns64 inst_uid);
void synth_retire(uns proc_id, uns64 inst_uid);

// enum class
typedef enum BottleNeck_Id_enum {
  #define BOTTLENECK_IMPL(id, name) id, 
  #include "bottlenecks_table.def"
  #undef BOTTLENECK_IMPL
  INVALID
} BottleNeck_enum;

extern const char * bottleneckNames[];
extern BottleNeck_enum bottleneck;

#ifdef __cplusplus
}
#endif


#endif