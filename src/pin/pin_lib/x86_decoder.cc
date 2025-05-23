/* Copyright 2020 HPS/SAFARI Research Groups
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include <iostream>
#include <set>
#include <string>
#include <unordered_map>

#include "pin/pin_lib/pin_scarab_common_lib.h"
#include "frontend/frontend_intf.h"

using namespace std;

/************************** Data Type Definitions *****************************/
#define REG(x) SCARAB_REG_##x,
typedef enum Reg_Id_struct {
#include "isa/x86_regs.def"
  SCARAB_NUM_REGS
} Reg_Id;
#undef REG

#include "gather_scatter_addresses.h"
#include "pin/pin_lib/x86_decoder.h"

struct Reg_Array_Info {
  size_t  array_offset;
  uint8_t ctype_pin_inst::*num;
  uint8_t                  max_num;
};

/**************************** Global Variables ********************************/
// helper array used by add_reg()
static Reg_Array_Info reg_array_infos[NUM_REG_ARRAYS] = {
  {offsetof(ctype_pin_inst, src_regs), &ctype_pin_inst::num_src_regs,
   MAX_SRC_REGS_NUM},
  {offsetof(ctype_pin_inst, dst_regs), &ctype_pin_inst::num_dst_regs,
   MAX_DST_REGS_NUM},
  {offsetof(ctype_pin_inst, ld1_addr_regs), &ctype_pin_inst::num_ld1_addr_regs,
   MAX_MEM_ADDR_REGS_NUM},
  {offsetof(ctype_pin_inst, ld2_addr_regs), &ctype_pin_inst::num_ld2_addr_regs,
   MAX_MEM_ADDR_REGS_NUM},
  {offsetof(ctype_pin_inst, st_addr_regs), &ctype_pin_inst::num_st_addr_regs,
   MAX_MEM_ADDR_REGS_NUM},
};

// maps for translation from pin to scarab
uint8_t reg_compress_map[(int)XED_REG_LAST + 1] = {0};  // Assuming REG_INV is 0
// Assuming OP_INV is 0
struct iclass_to_scarab iclass_to_scarab_map[XED_ICLASS_LAST] = {{0}};

struct iclass_to_scarab iclass_to_scarab(xed_iclass_enum_t iclass) {
  return iclass_to_scarab_map[iclass];
}
std::ostream* dec_err_ostream;

/********************* Private Functions Prototypes ***************************/

static uint8_t reg_compress(xed_reg_enum_t pin_reg, ADDRINT ip);

/**************************** Public Functions ********************************/
void init_x86_decoder(std::ostream* err_ostream) {
  init_reg_compress_map();
  init_pin_opcode_convert();
  if(err_ostream) {
    dec_err_ostream = err_ostream;
  } else {
    dec_err_ostream = &std::cout;
  }
}

int dump_marker_type(const xed_decoded_inst_t* ins) {
  // not caring about the actual start/end specified in the app
  if(XED_INS_Opcode(ins) == XED_ICLASS_XCHG &&
     XED_INS_OperandReg(ins, 0) == XED_REG_RBX &&
     XED_INS_OperandReg(ins, 1) == XED_REG_RBX) {
    return 1;
  }
  if(XED_INS_Opcode(ins) == XED_ICLASS_XCHG &&
     XED_INS_OperandReg(ins, 0) == XED_REG_RDX &&
     XED_INS_OperandReg(ins, 1) == XED_REG_RDX) {
    return 2;
  }
  return 0;
}

void fill_in_basic_info(ctype_pin_inst* info, const xed_decoded_inst_t* ins) {
  int category = XED_INS_Category(ins);
  info->size   = XED_INS_Size(ins);

  info->true_op_type = XED_INS_Opcode(ins);
  info->op_type      = iclass_to_scarab_map[XED_INS_Opcode(ins)].opcode;
  assert(XED_INS_Mnemonic(ins).size() < sizeof(info->pin_iclass));
  strcpy(info->pin_iclass, XED_INS_Mnemonic(ins).c_str());

  if((category == XED_CATEGORY_SSE) || (category == XED_CATEGORY_FCMOV) ||
     (category == XED_CATEGORY_X87_ALU)) {
    info->is_fp = 1;
  }
  info->is_string         = (category == XED_CATEGORY_STRINGOP);
  info->is_call           = (category == XED_CATEGORY_CALL);
  info->is_move           = (category == XED_CATEGORY_DATAXFER ||
                   info->op_type == OP_MOV);
  info->is_prefetch       = (category == XED_CATEGORY_PREFETCH);
  info->has_push          = (category == XED_CATEGORY_PUSH ||
                    category == XED_CATEGORY_CALL);
  info->has_pop           = (category == XED_CATEGORY_POP ||
                   category == XED_CATEGORY_RET);
  info->is_lock           = XED_INS_LockPrefix(ins);
  info->is_repeat         = XED_INS_HasRealRep(ins);
  info->is_gather_scatter = XED_INS_IsVgather(ins) || XED_INS_IsVscatter(ins);

  for (int ii = 0; (ii < 8) && (ii < info->size); ii++) {
    info->inst_binary_lsb = (info->inst_binary_lsb << 8) + XED_INS_Byte(ins, ii);
  }
  for (int ii = 0; (ii < 16) && (ii < info->size); ii++) {
    info->inst_binary_msb = (info->inst_binary_msb << 8) + XED_INS_Byte(ins, ii);
  }

  if (dump_marker_type(ins) == 1) {
    info->scarab_marker_roi_begin = true;
    info->scarab_marker_roi_end = false;
  } else if (dump_marker_type(ins) == 2) {
    info->scarab_marker_roi_end = true;
    info->scarab_marker_roi_begin = false;
  }
}

uint32_t add_dependency_info(ctype_pin_inst*           info,
                             const xed_decoded_inst_t* ins) {
  uint32_t      max_op_width = 0;
  const ADDRINT iaddr        = info->instruction_addr;
  bool is_gather_or_scatter = XED_INS_IsVgather(ins) || XED_INS_IsVscatter(ins);
  info->ld_size             = 0;
  info->st_size             = 0;
  if(info->op_type != OP_NOP) {
    // Iterate over reg operands separately
    for(uint32_t ii = 0; ii < XED_INS_OperandCount(ins); ii++) {
      if(XED_INS_OperandIsReg(ins, ii)) {
        xed_reg_enum_t xed_reg        = XED_INS_OperandReg(ins, ii);
        uint8_t        scarab_reg     = (uint8_t)reg_compress(xed_reg, iaddr);
        bool           operandRead    = XED_INS_OperandRead(ins, ii);
        bool           operandWritten = XED_INS_OperandWritten(ins, ii);
        if(operandRead) {
          add_reg(info, SRC_REGS, scarab_reg);
        }
        if(operandWritten) {
          add_reg(info, DST_REGS, scarab_reg);
        }
        if(scarab_reg >= SCARAB_REG_FP0 && scarab_reg <= SCARAB_REG_FP7)
          info->is_fp = TRUE;
        if(scarab_reg != SCARAB_REG_ZPS) {
          max_op_width = std::max(max_op_width, XED_INS_OperandWidth(ins, ii));
        }
        if(is_gather_or_scatter) {
	  set_gather_scatter_reg_operand_info(iaddr, xed_reg, operandRead,
                                              operandWritten);
        }
      }
      info->has_immediate |= XED_INS_OperandIsImmediate(ins, ii);
      if(is_gather_or_scatter) {
        assert(!(info->has_immediate));
      }
    }
    // Memory Operands (iterate of Memory operands)
    for(unsigned int ii = 0; ii < XED_INS_MemoryOperandCount(ins); ii++) {
      // LEA
      if(!XED_INS_MemoryOperandIsRead(ins, ii) &&
         !XED_INS_MemoryOperandIsWritten(ins, ii)) {
        assert(!is_gather_or_scatter);
        uint8_t scarab_base_reg = (uint8_t)reg_compress(
          XED_INS_OperandMemoryBaseReg(ins, ii), iaddr);
        uint8_t scarab_index_reg = (uint8_t)reg_compress(
          XED_INS_OperandMemoryIndexReg(ins, ii), iaddr);
        add_reg(info, SRC_REGS, scarab_base_reg);
        add_reg(info, SRC_REGS, scarab_index_reg);
      } else {
        xed_reg_enum_t pin_base_reg = XED_INS_OperandMemoryBaseReg(ins, ii);
        uint8_t scarab_base_reg = (uint8_t)reg_compress(pin_base_reg, iaddr);
        xed_reg_enum_t pin_index_reg = XED_INS_OperandMemoryIndexReg(ins, ii);
        uint8_t scarab_index_reg = (uint8_t)reg_compress(pin_index_reg, iaddr);
        if(is_gather_or_scatter) {
          set_gather_scatter_memory_operand_info(
            iaddr, pin_base_reg, pin_index_reg,
            // XED_INS_OperandMemoryDisplacement(ins, ii),
            XED_INS_OperandMemoryScale(ins, ii),
            XED_INS_OperandReadOnly(ins, ii),
            XED_INS_OperandWrittenOnly(ins, ii));
        }
        if(XED_INS_MemoryOperandIsRead(ins, ii)) {
          assert(info->num_ld < MAX_LD_NUM);
          add_reg(info, (Reg_Array_Id)(LD1_ADDR_REGS + info->num_ld),
                  scarab_base_reg);
          add_reg(info, (Reg_Array_Id)(LD1_ADDR_REGS + info->num_ld),
                  scarab_index_reg);
          info->num_ld++;
          // if the instruction has multiple mem accesses check if they have the
          // same size
          assert(info->ld_size == 0 ||
                 info->ld_size == XED_INS_MemoryReadSize(ins, ii));
          info->ld_size = XED_INS_MemoryReadSize(ins, ii);
        }
        if(XED_INS_MemoryOperandIsWritten(ins, ii)) {
          assert(info->num_st < MAX_ST_NUM);
          add_reg(info, (Reg_Array_Id)(ST_ADDR_REGS + info->num_st),
                  scarab_base_reg);
          add_reg(info, (Reg_Array_Id)(ST_ADDR_REGS + info->num_st),
                  scarab_index_reg);
          info->num_st++;
          // if the instruction has multiple mem accesses check if they have the
          // same size
          assert(info->st_size == 0 ||
                 info->st_size == XED_INS_MemoryWriteSize(ins, ii));
          info->st_size = XED_INS_MemoryWriteSize(ins, ii);
        }
      }
      info->has_immediate |= XED_INS_OperandIsImmediate(ins, ii);
      if(is_gather_or_scatter) {
        assert(!(info->has_immediate));
      }
    }
  }
  return max_op_width;
}

void fill_in_simd_info(ctype_pin_inst* info, const xed_decoded_inst_t* ins,
                       uint32_t max_op_width) {
  struct iclass_to_scarab iclass_info = iclass_to_scarab_map[XED_INS_Opcode(ins)];
  if(info->op_type != OP_INV) {
    assert(max_op_width % 8 == 0);
    int lane_width_bytes = iclass_info.lane_width_bytes;
    int num_simd_lanes   = iclass_info.num_simd_lanes;
    if(lane_width_bytes == -1) {
      info->lane_width_bytes = max_op_width / 8;
      info->num_simd_lanes   = 1;
    } else if(num_simd_lanes == -1) {
      info->lane_width_bytes = lane_width_bytes;
      assert((max_op_width / 8) % lane_width_bytes == 0);
      info->num_simd_lanes = max_op_width / 8 / lane_width_bytes;
    } else {
      info->lane_width_bytes = lane_width_bytes;
      info->num_simd_lanes   = num_simd_lanes;
    }
  }

  info->is_simd = 0;
  for(int i = 0; i < info->num_src_regs; ++i) {
    if(info->src_regs[i] >= SCARAB_REG_ZMM0 &&
       info->src_regs[i] <= SCARAB_REG_ZMM31) {
      info->is_simd = 1;
      return;
    }
  }
  for(int i = 0; i < info->num_dst_regs; ++i) {
    if(info->dst_regs[i] >= SCARAB_REG_ZMM0 &&
       info->dst_regs[i] <= SCARAB_REG_ZMM31) {
      info->is_simd = 1;
      return;
    }
  }
}

void apply_x87_bug_workaround(ctype_pin_inst*           info,
                              const xed_decoded_inst_t* ins) {
  // Workaround for a bug in Pin 2.8, see
  // http://tech.groups.yahoo.com/group/pinheads/message/6082
  if(pops_x87_stack(XED_INS_Opcode(ins)) && info->num_dst_regs == 1 &&
     info->dst_regs[0] == SCARAB_REG_FP0) {
    int other_reg = SCARAB_REG_FP0;
    for(int i = 0; i < info->num_src_regs; ++i) {
      if(info->src_regs[i] != SCARAB_REG_FP0) {
        assert(other_reg == SCARAB_REG_FP0);
        other_reg = info->src_regs[i];
      }
    }
    // not checking for other_reg != SCARAB_REG_FP0 because sometimes the
    // compiler may use FSTP ST0 or smth to simply pop the x87 stack
    info->dst_regs[0] = other_reg;
  }
}

void fill_in_cf_info(ctype_pin_inst* info, const xed_decoded_inst_t* ins) {
  int category  = XED_INS_Category(ins);
  info->cf_type = NOT_CF;


  if(XED_INS_IsSyscall(ins) || XED_INS_IsSysret(ins) ||
     XED_INS_IsInterrupt(ins)) {
    info->cf_type = CF_SYS;
  } else if(XED_INS_IsRet(ins)) {
    info->cf_type = CF_RET;
  } else if(XED_INS_IsIndirectBranchOrCall(ins)) {
    // indirect
    if(category == XED_CATEGORY_UNCOND_BR)
      info->cf_type = CF_IBR;
    else if(category == XED_CATEGORY_COND_BR)
      info->cf_type = CF_ICO;  // ICBR not supported by Scarab, so map it to ICO
    else if(category == XED_CATEGORY_CALL)
      info->cf_type = CF_ICALL;
  } else if(XED_INS_IsDirectBranchOrCall(ins)) {
    // direct
    if(category == XED_CATEGORY_UNCOND_BR)
      info->cf_type = CF_BR;
    else if(category == XED_CATEGORY_COND_BR)
      info->cf_type = CF_CBR;
    else if(category == XED_CATEGORY_CALL)
      info->cf_type = CF_CALL;
    info->branch_target = XED_INS_DirectBranchOrCallTargetAddress(info->instruction_addr, ins);
  }
  info->is_ifetch_barrier = is_ifetch_barrier(ins);
}

void print_err_if_invalid(ctype_pin_inst* info, const xed_decoded_inst_t* ins) {
  bool invalid = info->op_type == OP_INV;
  bool correct = true;
#ifdef ENABLE_PT_MEMTRACE
  if (FRONTEND == FE_MEMTRACE) {
    correct = info->cf_type != NOT_CF || XED_INS_DirectBranchOrCallTargetAddress(info->instruction_addr, ins) == info->branch_target;
    if (info->is_repeat) correct = true;  // Ignore rep instr since the target is the same instr.
    if (info->last_inst_from_trace) {
      // if last intruction is direct, branch target is overwritten, but npc shall be itself
      // if it is indirect or NOT_CF, branch target shall equal npc, and is itself
      assert(info->instruction_next_addr == info->instruction_addr);
      if (!correct) {
        assert(info->cf_type == NOT_CF);
        // ignore incorrect npc for the last instruction from the trace
        correct = true;
      }
    }
  }
#endif
  if(invalid || !correct) {
    if(invalid) {
      std::cout << "Invalid inst! " << std::endl;
    }
    if(!correct) {
      std::cout
        << "Not correct inst: " << +info->cf_type << ", " << std::hex << info->instruction_addr << ' ' << std::dec << +info->size << ' ' << xed_operand_values_get_branch_displacement_int32(ins) << ' ' << std::hex << info->branch_target << ' ' << std::endl;;
    }
    std::cout
      << "Unmapped instruction at "
      << "EIP: " << std::hex << info->instruction_addr
      << ", opcode: " << XED_INS_Mnemonic(ins) << ", category: "
      << std::string(xed_category_enum_t2str(XED_INS_Category(ins))) << std::dec
      << ", opcode index: " << XED_INS_Opcode(ins)
      << ", hasrealrep: " << (int)info->is_repeat
      << ", is_lock: " << (int)info->is_lock
      << ", num_ld: " << (int)info->num_ld << ", num_st: " << (int)info->num_st
      << ", num_src_regs: " << (int)info->num_src_regs
      << ", num_dst_regs: " << (int)info->num_dst_regs
      << ", num_ld1_addr_regs: " << (int)info->num_ld1_addr_regs
      << ", num_ld2_addr_regs: " << (int)info->num_ld2_addr_regs
      << ", num_st_addr_regs: " << (int)info->num_st_addr_regs
      << ", num_simd_lanes: " << (int)info->num_simd_lanes
      << ", lane_width_bytes: " << (int)info->lane_width_bytes
      << ". Look at README in pin/pin_lib on how to map new instructions" << std::endl;;
    //dec_err_ostream->flush();
  }
}

uint8_t is_ifetch_barrier(const xed_decoded_inst_t* ins) {
  int category = XED_INS_Category(ins);
  int opcode   = XED_INS_Opcode(ins);
  return (category == XED_CATEGORY_IO) ||
         (category == XED_CATEGORY_INTERRUPT) ||
         (category == XED_CATEGORY_VTX) || (category == XED_CATEGORY_SYSTEM) ||
         (category == XED_CATEGORY_SYSCALL) ||
         (category == XED_CATEGORY_SYSRET) || (opcode == XED_ICLASS_PAUSE);
}

static compressed_reg_t reg_compress(xed_reg_enum_t pin_reg, ADDRINT ip) {
  compressed_reg_t result = reg_compress_map[pin_reg];
  if(result == SCARAB_REG_INV && (int)pin_reg != 0) {
    (*dec_err_ostream) << "Invalid register operand "
                       << std::string(xed_reg_enum_t2str(pin_reg))
                       << " at: " << std::hex << ip << std::endl;
  }
  return result;
}

/*************************** Private Functions  *******************************/
void add_reg(ctype_pin_inst* info, Reg_Array_Id id, uint8_t reg) {
  if(reg == SCARAB_REG_INV)
    return;
  if(reg >= SCARAB_REG_CS && reg <= SCARAB_REG_RIP)
    return;  // ignoring EIP and segment regs

  uint8_t* array = ((uint8_t*)info) + reg_array_infos[id].array_offset;
  uint8_t ctype_pin_inst::*num_ptr = reg_array_infos[id].num;
  //  uint8_t * num   = &(info->*num_ptr);
  uint8_t max_num = reg_array_infos[id].max_num;
  assert(id < NUM_REG_ARRAYS);
  assert(info->*num_ptr < max_num);
  array[info->*num_ptr] = reg;
  (info->*num_ptr)++;
}

void init_reg_compress_map(void) {
  assert(SCARAB_REG_INV == 0);

  // Makes sure src_regs is wide enough for Scarab registers.
  assert(SCARAB_NUM_REGS < (1 << (sizeof(compressed_reg_t) * 8)));

  reg_compress_map[0]                = 0;
  reg_compress_map[(int)XED_REG_RDI] = SCARAB_REG_RDI;
  reg_compress_map[(int)XED_REG_EDI] = SCARAB_REG_RDI;
  reg_compress_map[(int)XED_REG_ESI] = SCARAB_REG_RSI;
  reg_compress_map[(int)XED_REG_RSI] = SCARAB_REG_RSI;
  reg_compress_map[(int)XED_REG_EBP] = SCARAB_REG_RBP;
  reg_compress_map[(int)XED_REG_RBP] = SCARAB_REG_RBP;
  reg_compress_map[(int)XED_REG_ESP] = SCARAB_REG_RSP;
  reg_compress_map[(int)XED_REG_RSP] = SCARAB_REG_RSP;
  reg_compress_map[(int)XED_REG_EBX] = SCARAB_REG_RBX;
  reg_compress_map[(int)XED_REG_RBX] = SCARAB_REG_RBX;
  reg_compress_map[(int)XED_REG_EDX] = SCARAB_REG_RDX;
  reg_compress_map[(int)XED_REG_RDX] = SCARAB_REG_RDX;
  reg_compress_map[(int)XED_REG_ECX] = SCARAB_REG_RCX;
  reg_compress_map[(int)XED_REG_RCX] = SCARAB_REG_RCX;
  reg_compress_map[(int)XED_REG_EAX] = SCARAB_REG_RAX;
  reg_compress_map[(int)XED_REG_RAX] = SCARAB_REG_RAX;
  //  reg_compress_map[(int)XED_REG_GR_LAST]   = SCARAB_REG_RAX;
  reg_compress_map[(int)XED_REG_FSBASE] = SCARAB_REG_CS;
  // todo: Not sure about the next
  reg_compress_map[(int)XED_REG_GSBASE] = SCARAB_REG_CS;
  reg_compress_map[(int)XED_REG_CS]     = SCARAB_REG_CS;
  reg_compress_map[(int)XED_REG_SS]     = SCARAB_REG_SS;
  reg_compress_map[(int)XED_REG_DS]     = SCARAB_REG_DS;
  reg_compress_map[(int)XED_REG_ES]     = SCARAB_REG_ES;
  reg_compress_map[(int)XED_REG_FS]     = SCARAB_REG_FS;
  reg_compress_map[(int)XED_REG_GS]     = SCARAB_REG_GS;
  // reg_compress_map[(int)XED_REG_SEG_LAST]  = SCARAB_REG_GS;
  // Treating any flag dependency as ZPS because we could not
  // get finer grain dependicies from PIN
  reg_compress_map[(int)XED_REG_EFLAGS]    = SCARAB_REG_ZPS;
  reg_compress_map[(int)XED_REG_RFLAGS]    = SCARAB_REG_ZPS;
  reg_compress_map[(int)XED_REG_EIP]       = SCARAB_REG_RIP;
  reg_compress_map[(int)XED_REG_RIP]       = SCARAB_REG_RIP;
  reg_compress_map[(int)XED_REG_AL]        = SCARAB_REG_RAX;
  reg_compress_map[(int)XED_REG_AH]        = SCARAB_REG_RAX;
  reg_compress_map[(int)XED_REG_AX]        = SCARAB_REG_RAX;
  reg_compress_map[(int)XED_REG_CL]        = SCARAB_REG_RCX;
  reg_compress_map[(int)XED_REG_CH]        = SCARAB_REG_RCX;
  reg_compress_map[(int)XED_REG_CX]        = SCARAB_REG_RCX;
  reg_compress_map[(int)XED_REG_DL]        = SCARAB_REG_RDX;
  reg_compress_map[(int)XED_REG_DH]        = SCARAB_REG_RDX;
  reg_compress_map[(int)XED_REG_DX]        = SCARAB_REG_RDX;
  reg_compress_map[(int)XED_REG_BL]        = SCARAB_REG_RBX;
  reg_compress_map[(int)XED_REG_BH]        = SCARAB_REG_RBX;
  reg_compress_map[(int)XED_REG_BX]        = SCARAB_REG_RBX;
  reg_compress_map[(int)XED_REG_BP]        = SCARAB_REG_RBX;
  reg_compress_map[(int)XED_REG_SI]        = SCARAB_REG_RSI;
  reg_compress_map[(int)XED_REG_DI]        = SCARAB_REG_RDI;
  reg_compress_map[(int)XED_REG_SP]        = SCARAB_REG_RSP;
  reg_compress_map[(int)XED_REG_FLAGS]     = SCARAB_REG_ZPS;
  reg_compress_map[(int)XED_REG_IP]        = SCARAB_REG_RIP;
  reg_compress_map[(int)XED_REG_MMX_FIRST] = SCARAB_REG_ZMM0;
  reg_compress_map[(int)XED_REG_MMX0]      = SCARAB_REG_ZMM0;
  reg_compress_map[(int)XED_REG_MMX1]      = SCARAB_REG_ZMM1;
  reg_compress_map[(int)XED_REG_MMX2]      = SCARAB_REG_ZMM2;
  reg_compress_map[(int)XED_REG_MMX3]      = SCARAB_REG_ZMM3;
  reg_compress_map[(int)XED_REG_MMX4]      = SCARAB_REG_ZMM4;
  reg_compress_map[(int)XED_REG_MMX5]      = SCARAB_REG_ZMM5;
  reg_compress_map[(int)XED_REG_MMX6]      = SCARAB_REG_ZMM6;
  reg_compress_map[(int)XED_REG_MMX7]      = SCARAB_REG_ZMM7;
  reg_compress_map[(int)XED_REG_MMX_LAST]  = SCARAB_REG_ZMM7;

  reg_compress_map[(int)XED_REG_XMM_FIRST] = SCARAB_REG_ZMM0;
  reg_compress_map[(int)XED_REG_XMM0]      = SCARAB_REG_ZMM0;
  reg_compress_map[(int)XED_REG_XMM1]      = SCARAB_REG_ZMM1;
  reg_compress_map[(int)XED_REG_XMM2]      = SCARAB_REG_ZMM2;
  reg_compress_map[(int)XED_REG_XMM3]      = SCARAB_REG_ZMM3;
  reg_compress_map[(int)XED_REG_XMM4]      = SCARAB_REG_ZMM4;
  reg_compress_map[(int)XED_REG_XMM5]      = SCARAB_REG_ZMM5;
  reg_compress_map[(int)XED_REG_XMM6]      = SCARAB_REG_ZMM6;
  reg_compress_map[(int)XED_REG_XMM7]      = SCARAB_REG_ZMM7;
  // reg_compress_map[(int)XED_REG_XMM_SSE_LAST]    = SCARAB_REG_ZMM7;
  // reg_compress_map[(int)XED_REG_XMM_AVX_LAST]    = SCARAB_REG_ZMM7;
  // reg_compress_map[(int)XED_REG_XMM_AVX512_LAST] = SCARAB_REG_ZMM7;
  reg_compress_map[(int)XED_REG_XMM_LAST] = SCARAB_REG_ZMM7;

  reg_compress_map[(int)XED_REG_YMM_FIRST] = SCARAB_REG_ZMM0;
  reg_compress_map[(int)XED_REG_YMM0]      = SCARAB_REG_ZMM0;
  reg_compress_map[(int)XED_REG_YMM1]      = SCARAB_REG_ZMM1;
  reg_compress_map[(int)XED_REG_YMM2]      = SCARAB_REG_ZMM2;
  reg_compress_map[(int)XED_REG_YMM3]      = SCARAB_REG_ZMM3;
  reg_compress_map[(int)XED_REG_YMM4]      = SCARAB_REG_ZMM4;
  reg_compress_map[(int)XED_REG_YMM5]      = SCARAB_REG_ZMM5;
  reg_compress_map[(int)XED_REG_YMM6]      = SCARAB_REG_ZMM6;
  reg_compress_map[(int)XED_REG_YMM7]      = SCARAB_REG_ZMM7;
  // reg_compress_map[(int)XED_REG_YMM_AVX_LAST]    = SCARAB_REG_ZMM7;
  // reg_compress_map[(int)XED_REG_YMM_AVX512_LAST] = SCARAB_REG_ZMM7;
  reg_compress_map[(int)XED_REG_YMM_LAST] = SCARAB_REG_ZMM7;

  reg_compress_map[(int)XED_REG_ZMM_FIRST] = SCARAB_REG_ZMM0;
  reg_compress_map[(int)XED_REG_ZMM0]      = SCARAB_REG_ZMM0;
  reg_compress_map[(int)XED_REG_ZMM1]      = SCARAB_REG_ZMM1;
  reg_compress_map[(int)XED_REG_ZMM2]      = SCARAB_REG_ZMM2;
  reg_compress_map[(int)XED_REG_ZMM3]      = SCARAB_REG_ZMM3;
  reg_compress_map[(int)XED_REG_ZMM4]      = SCARAB_REG_ZMM4;
  reg_compress_map[(int)XED_REG_ZMM5]      = SCARAB_REG_ZMM5;
  reg_compress_map[(int)XED_REG_ZMM6]      = SCARAB_REG_ZMM6;
  reg_compress_map[(int)XED_REG_ZMM7]      = SCARAB_REG_ZMM7;
  // reg_compress_map[(int)XED_REG_ZMM_AVX512_SPLIT_LAST] = SCARAB_REG_ZMM7;
  // reg_compress_map[(int)XED_REG_ZMM_AVX512_LAST]       = SCARAB_REG_ZMM7;
  reg_compress_map[(int)XED_REG_ZMM_LAST] = SCARAB_REG_ZMM7;

  // reg_compress_map[(int)XED_REG_MASK_FIRST]             = SCARAB_REG_K0;
  // reg_compress_map[(int)XED_REG_IMPLICIT_FULL_MASK] = SCARAB_REG_K0;
  reg_compress_map[(int)XED_REG_K0] = SCARAB_REG_K0;
  reg_compress_map[(int)XED_REG_K1] = SCARAB_REG_K1;
  reg_compress_map[(int)XED_REG_K2] = SCARAB_REG_K2;
  reg_compress_map[(int)XED_REG_K3] = SCARAB_REG_K3;
  reg_compress_map[(int)XED_REG_K4] = SCARAB_REG_K4;
  reg_compress_map[(int)XED_REG_K5] = SCARAB_REG_K5;
  reg_compress_map[(int)XED_REG_K6] = SCARAB_REG_K6;
  reg_compress_map[(int)XED_REG_K7] = SCARAB_REG_K7;
  // reg_compress_map[(int)XED_REG_MASK_LAST]             = SCARAB_REG_K7;

  reg_compress_map[(int)XED_REG_MXCSR] = SCARAB_REG_OTHER;
  // reg_compress_map[(int)XED_REG_MXCSRMASK] = SCARAB_REG_OTHER;

  reg_compress_map[(int)XED_REG_X87CONTROL] = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_X87STATUS]  = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_X87TAG]     = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_X87PUSH]    = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_X87POP]     = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_X87POP2]    = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_X87OPCODE]  = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_X87LASTCS]  = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_X87LASTIP]  = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_X87LASTDS]  = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_X87LASTDP]  = SCARAB_REG_OTHER;
  /*
  reg_compress_map[(int)XED_REG_FPST_BASE]     = SCARAB_REG_FPCW;
  reg_compress_map[(int)XED_REG_FPSTATUS_BASE] = SCARAB_REG_FPCW;
  reg_compress_map[(int)XED_REG_FPCW]          = SCARAB_REG_FPCW;
  reg_compress_map[(int)XED_REG_FPSW]          = SCARAB_REG_FPST;
  reg_compress_map[(int)XED_REG_FPTAG]         = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_FPIP_OFF]      = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_FPIP_SEL]      = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_FPOPCODE]      = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_FPDP_OFF]      = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_FPDP_SEL]      = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_FPSTATUS_LAST] = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_FPTAG_FULL]    = SCARAB_REG_OTHER;
  */
  //  reg_compress_map[(int)XED_REG_ST_BASE]   = SCARAB_REG_FP0;
  reg_compress_map[(int)XED_REG_ST0] = SCARAB_REG_FP0;
  reg_compress_map[(int)XED_REG_ST1] = SCARAB_REG_FP1;
  reg_compress_map[(int)XED_REG_ST2] = SCARAB_REG_FP2;
  reg_compress_map[(int)XED_REG_ST3] = SCARAB_REG_FP3;
  reg_compress_map[(int)XED_REG_ST4] = SCARAB_REG_FP4;
  reg_compress_map[(int)XED_REG_ST5] = SCARAB_REG_FP5;
  reg_compress_map[(int)XED_REG_ST6] = SCARAB_REG_FP6;
  reg_compress_map[(int)XED_REG_ST7] = SCARAB_REG_FP7;
  // reg_compress_map[(int)XED_REG_ST_LAST]   = SCARAB_REG_FP7;
  // reg_compress_map[(int)XED_REG_FPST_LAST] = SCARAB_REG_FP7;

  // reg_compress_map[(int)XED_REG_DR_BASE] = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_DR0] = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_DR1] = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_DR2] = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_DR3] = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_DR4] = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_DR5] = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_DR6] = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_DR7] = SCARAB_REG_OTHER;
  // reg_compress_map[(int)XED_REG_DR_LAST] = SCARAB_REG_OTHER;

  // reg_compress_map[(int)XED_REG_CR_BASE] = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_TSC]    = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_TSCAUX] = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_XCR0]   = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_CR0]    = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_CR1]    = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_CR2]    = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_CR3]    = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_CR4]    = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_CR5]    = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_CR6]    = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_CR7]    = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_CR8]    = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_CR9]    = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_CR10]   = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_CR11]   = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_CR12]   = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_CR13]   = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_CR14]   = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_CR15]   = SCARAB_REG_OTHER;
  // reg_compress_map[(int)XED_REG_CR_LAST] = SCARAB_REG_OTHER;
  // reg_compress_map[(int)XED_REG_TSSR]    = SCARAB_REG_OTHER;
  reg_compress_map[(int)XED_REG_LDTR] = SCARAB_REG_OTHER;
  // reg_compress_map[(int)XED_REG_TR_BASE] = SCARAB_REG_OTHER;
  // reg_compress_map[(int)XED_REG_TR]      = SCARAB_REG_OTHER;
  // reg_compress_map[(int)XED_REG_TR3]     = SCARAB_REG_OTHER;
  // reg_compress_map[(int)XED_REG_TR4]     = SCARAB_REG_OTHER;
  // reg_compress_map[(int)XED_REG_TR5]     = SCARAB_REG_OTHER;
  // reg_compress_map[(int)XED_REG_TR6]     = SCARAB_REG_OTHER;
  // reg_compress_map[(int)XED_REG_TR7]     = SCARAB_REG_OTHER;
  // reg_compress_map[(int)XED_REG_TR_LAST] = SCARAB_REG_OTHER;

  reg_compress_map[(int)XED_REG_STACKPUSH] = SCARAB_REG_RSP;
  reg_compress_map[(int)XED_REG_STACKPOP]  = SCARAB_REG_RSP;
  reg_compress_map[(int)XED_REG_RDI]       = SCARAB_REG_RDI;
  reg_compress_map[(int)XED_REG_RSI]       = SCARAB_REG_RSI;
  reg_compress_map[(int)XED_REG_RBP]       = SCARAB_REG_RBP;
  reg_compress_map[(int)XED_REG_RSP]       = SCARAB_REG_RSP;
  reg_compress_map[(int)XED_REG_RBX]       = SCARAB_REG_RBX;
  reg_compress_map[(int)XED_REG_RDX]       = SCARAB_REG_RDX;
  reg_compress_map[(int)XED_REG_RCX]       = SCARAB_REG_RCX;
  reg_compress_map[(int)XED_REG_RAX]       = SCARAB_REG_RAX;
  reg_compress_map[(int)XED_REG_R8]        = SCARAB_REG_R8;
  reg_compress_map[(int)XED_REG_R9]        = SCARAB_REG_R9;
  reg_compress_map[(int)XED_REG_R10]       = SCARAB_REG_R10;
  reg_compress_map[(int)XED_REG_R11]       = SCARAB_REG_R11;
  reg_compress_map[(int)XED_REG_R12]       = SCARAB_REG_R12;
  reg_compress_map[(int)XED_REG_R13]       = SCARAB_REG_R13;
  reg_compress_map[(int)XED_REG_R14]       = SCARAB_REG_R14;
  reg_compress_map[(int)XED_REG_R15]       = SCARAB_REG_R15;
  // reg_compress_map[(int)XED_REG_GR_LAST] = SCARAB_REG_R15;
  reg_compress_map[(int)XED_REG_RFLAGS] = SCARAB_REG_ZPS;
  reg_compress_map[(int)XED_REG_RIP]    = SCARAB_REG_RIP;

  reg_compress_map[(int)XED_REG_DIL]  = SCARAB_REG_RDI;
  reg_compress_map[(int)XED_REG_SIL]  = SCARAB_REG_RSI;
  reg_compress_map[(int)XED_REG_BPL]  = SCARAB_REG_RBP;
  reg_compress_map[(int)XED_REG_SPL]  = SCARAB_REG_RSP;
  reg_compress_map[(int)XED_REG_R8B]  = SCARAB_REG_R8;
  reg_compress_map[(int)XED_REG_R8W]  = SCARAB_REG_R8;
  reg_compress_map[(int)XED_REG_R8D]  = SCARAB_REG_R8;
  reg_compress_map[(int)XED_REG_R9B]  = SCARAB_REG_R9;
  reg_compress_map[(int)XED_REG_R9W]  = SCARAB_REG_R9;
  reg_compress_map[(int)XED_REG_R9D]  = SCARAB_REG_R9;
  reg_compress_map[(int)XED_REG_R10B] = SCARAB_REG_R10;
  reg_compress_map[(int)XED_REG_R10W] = SCARAB_REG_R10;
  reg_compress_map[(int)XED_REG_R10D] = SCARAB_REG_R10;
  reg_compress_map[(int)XED_REG_R11B] = SCARAB_REG_R11;
  reg_compress_map[(int)XED_REG_R11W] = SCARAB_REG_R11;
  reg_compress_map[(int)XED_REG_R11D] = SCARAB_REG_R11;
  reg_compress_map[(int)XED_REG_R12B] = SCARAB_REG_R12;
  reg_compress_map[(int)XED_REG_R12W] = SCARAB_REG_R12;
  reg_compress_map[(int)XED_REG_R12D] = SCARAB_REG_R12;
  reg_compress_map[(int)XED_REG_R13B] = SCARAB_REG_R13;
  reg_compress_map[(int)XED_REG_R13W] = SCARAB_REG_R13;
  reg_compress_map[(int)XED_REG_R13D] = SCARAB_REG_R13;
  reg_compress_map[(int)XED_REG_R14B] = SCARAB_REG_R14;
  reg_compress_map[(int)XED_REG_R14W] = SCARAB_REG_R14;
  reg_compress_map[(int)XED_REG_R14D] = SCARAB_REG_R14;
  reg_compress_map[(int)XED_REG_R15B] = SCARAB_REG_R15;
  reg_compress_map[(int)XED_REG_R15W] = SCARAB_REG_R15;
  reg_compress_map[(int)XED_REG_R15D] = SCARAB_REG_R15;

  reg_compress_map[(int)XED_REG_XMM8]  = SCARAB_REG_ZMM8;
  reg_compress_map[(int)XED_REG_XMM9]  = SCARAB_REG_ZMM9;
  reg_compress_map[(int)XED_REG_XMM10] = SCARAB_REG_ZMM10;
  reg_compress_map[(int)XED_REG_XMM11] = SCARAB_REG_ZMM11;
  reg_compress_map[(int)XED_REG_XMM12] = SCARAB_REG_ZMM12;
  reg_compress_map[(int)XED_REG_XMM13] = SCARAB_REG_ZMM13;
  reg_compress_map[(int)XED_REG_XMM14] = SCARAB_REG_ZMM14;
  reg_compress_map[(int)XED_REG_XMM15] = SCARAB_REG_ZMM15;
  // reg_compress_map[(int)XED_REG_XMM_SSE_LAST]          = SCARAB_REG_ZMM15;
  // reg_compress_map[(int)XED_REG_XMM_AVX_LAST]          = SCARAB_REG_ZMM15;
  reg_compress_map[(int)XED_REG_XMM16] = SCARAB_REG_ZMM16;
  // reg_compress_map[(int)XED_REG_XMM_AVX512_HI16_FIRST] = SCARAB_REG_ZMM16;
  reg_compress_map[(int)XED_REG_XMM17] = SCARAB_REG_ZMM17;
  reg_compress_map[(int)XED_REG_XMM18] = SCARAB_REG_ZMM18;
  reg_compress_map[(int)XED_REG_XMM19] = SCARAB_REG_ZMM19;
  reg_compress_map[(int)XED_REG_XMM20] = SCARAB_REG_ZMM20;
  reg_compress_map[(int)XED_REG_XMM21] = SCARAB_REG_ZMM21;
  reg_compress_map[(int)XED_REG_XMM22] = SCARAB_REG_ZMM22;
  reg_compress_map[(int)XED_REG_XMM23] = SCARAB_REG_ZMM23;
  reg_compress_map[(int)XED_REG_XMM24] = SCARAB_REG_ZMM24;
  reg_compress_map[(int)XED_REG_XMM25] = SCARAB_REG_ZMM25;
  reg_compress_map[(int)XED_REG_XMM26] = SCARAB_REG_ZMM26;
  reg_compress_map[(int)XED_REG_XMM27] = SCARAB_REG_ZMM27;
  reg_compress_map[(int)XED_REG_XMM28] = SCARAB_REG_ZMM28;
  reg_compress_map[(int)XED_REG_XMM29] = SCARAB_REG_ZMM29;
  reg_compress_map[(int)XED_REG_XMM30] = SCARAB_REG_ZMM30;
  reg_compress_map[(int)XED_REG_XMM31] = SCARAB_REG_ZMM31;
  // reg_compress_map[(int)XED_REG_XMM_AVX512_HI16_LAST]  = SCARAB_REG_ZMM31;
  // reg_compress_map[(int)XED_REG_XMM_AVX512_LAST]       = SCARAB_REG_ZMM31;
  reg_compress_map[(int)XED_REG_XMM_LAST] = SCARAB_REG_ZMM31;

  reg_compress_map[(int)XED_REG_YMM8]  = SCARAB_REG_ZMM8;
  reg_compress_map[(int)XED_REG_YMM9]  = SCARAB_REG_ZMM9;
  reg_compress_map[(int)XED_REG_YMM10] = SCARAB_REG_ZMM10;
  reg_compress_map[(int)XED_REG_YMM11] = SCARAB_REG_ZMM11;
  reg_compress_map[(int)XED_REG_YMM12] = SCARAB_REG_ZMM12;
  reg_compress_map[(int)XED_REG_YMM13] = SCARAB_REG_ZMM13;
  reg_compress_map[(int)XED_REG_YMM14] = SCARAB_REG_ZMM14;
  reg_compress_map[(int)XED_REG_YMM15] = SCARAB_REG_ZMM15;
  // reg_compress_map[(int)XED_REG_YMM_AVX_LAST]          = SCARAB_REG_ZMM15;
  reg_compress_map[(int)XED_REG_YMM16] = SCARAB_REG_ZMM16;
  // reg_compress_map[(int)XED_REG_YMM_AVX512_HI16_FIRST] = SCARAB_REG_ZMM16;
  reg_compress_map[(int)XED_REG_YMM17] = SCARAB_REG_ZMM17;
  reg_compress_map[(int)XED_REG_YMM18] = SCARAB_REG_ZMM18;
  reg_compress_map[(int)XED_REG_YMM19] = SCARAB_REG_ZMM19;
  reg_compress_map[(int)XED_REG_YMM20] = SCARAB_REG_ZMM20;
  reg_compress_map[(int)XED_REG_YMM21] = SCARAB_REG_ZMM21;
  reg_compress_map[(int)XED_REG_YMM22] = SCARAB_REG_ZMM22;
  reg_compress_map[(int)XED_REG_YMM23] = SCARAB_REG_ZMM23;
  reg_compress_map[(int)XED_REG_YMM24] = SCARAB_REG_ZMM24;
  reg_compress_map[(int)XED_REG_YMM25] = SCARAB_REG_ZMM25;
  reg_compress_map[(int)XED_REG_YMM26] = SCARAB_REG_ZMM26;
  reg_compress_map[(int)XED_REG_YMM27] = SCARAB_REG_ZMM27;
  reg_compress_map[(int)XED_REG_YMM28] = SCARAB_REG_ZMM28;
  reg_compress_map[(int)XED_REG_YMM29] = SCARAB_REG_ZMM29;
  reg_compress_map[(int)XED_REG_YMM30] = SCARAB_REG_ZMM30;
  reg_compress_map[(int)XED_REG_YMM31] = SCARAB_REG_ZMM31;
  // reg_compress_map[(int)XED_REG_YMM_AVX512_HI16_LAST]  = SCARAB_REG_ZMM31;
  // reg_compress_map[(int)XED_REG_YMM_AVX512_LAST]       = SCARAB_REG_ZMM31;
  reg_compress_map[(int)XED_REG_YMM_LAST] = SCARAB_REG_ZMM31;

  reg_compress_map[(int)XED_REG_ZMM8]  = SCARAB_REG_ZMM8;
  reg_compress_map[(int)XED_REG_ZMM9]  = SCARAB_REG_ZMM9;
  reg_compress_map[(int)XED_REG_ZMM10] = SCARAB_REG_ZMM10;
  reg_compress_map[(int)XED_REG_ZMM11] = SCARAB_REG_ZMM11;
  reg_compress_map[(int)XED_REG_ZMM12] = SCARAB_REG_ZMM12;
  reg_compress_map[(int)XED_REG_ZMM13] = SCARAB_REG_ZMM13;
  reg_compress_map[(int)XED_REG_ZMM14] = SCARAB_REG_ZMM14;
  reg_compress_map[(int)XED_REG_ZMM15] = SCARAB_REG_ZMM15;
  // reg_compress_map[(int)XED_REG_ZMM_AVX512_SPLIT_LAST] = SCARAB_REG_ZMM15;
  reg_compress_map[(int)XED_REG_ZMM16] = SCARAB_REG_ZMM16;
  // reg_compress_map[(int)XED_REG_ZMM_AVX512_HI16_FIRST] = SCARAB_REG_ZMM16;
  reg_compress_map[(int)XED_REG_ZMM17] = SCARAB_REG_ZMM17;
  reg_compress_map[(int)XED_REG_ZMM18] = SCARAB_REG_ZMM18;
  reg_compress_map[(int)XED_REG_ZMM19] = SCARAB_REG_ZMM19;
  reg_compress_map[(int)XED_REG_ZMM20] = SCARAB_REG_ZMM20;
  reg_compress_map[(int)XED_REG_ZMM21] = SCARAB_REG_ZMM21;
  reg_compress_map[(int)XED_REG_ZMM22] = SCARAB_REG_ZMM22;
  reg_compress_map[(int)XED_REG_ZMM23] = SCARAB_REG_ZMM23;
  reg_compress_map[(int)XED_REG_ZMM24] = SCARAB_REG_ZMM24;
  reg_compress_map[(int)XED_REG_ZMM25] = SCARAB_REG_ZMM25;
  reg_compress_map[(int)XED_REG_ZMM26] = SCARAB_REG_ZMM26;
  reg_compress_map[(int)XED_REG_ZMM27] = SCARAB_REG_ZMM27;
  reg_compress_map[(int)XED_REG_ZMM28] = SCARAB_REG_ZMM28;
  reg_compress_map[(int)XED_REG_ZMM29] = SCARAB_REG_ZMM29;
  reg_compress_map[(int)XED_REG_ZMM30] = SCARAB_REG_ZMM30;
  reg_compress_map[(int)XED_REG_ZMM31] = SCARAB_REG_ZMM31;
  // reg_compress_map[(int)XED_REG_ZMM_AVX512_HI16_LAST]  = SCARAB_REG_ZMM31;
  // reg_compress_map[(int)XED_REG_ZMM_AVX512_LAST]       = SCARAB_REG_ZMM31;
  reg_compress_map[(int)XED_REG_ZMM_LAST] = SCARAB_REG_ZMM31;
};

void init_pin_opcode_convert(void) {
  assert(OP_INV == 0);

  iclass_to_scarab_map[XED_ICLASS_ADC]      = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_ADCX]     = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_ADC_LOCK] = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_ADD]      = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_ADDPD]    = {OP_FADD, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_ADDPS]    = {OP_FADD, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_ADDSD]    = {OP_FADD, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_ADDSS]    = {OP_FADD, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_ADDSUBPD] = {OP_FADD, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_ADDSUBPS] = {OP_FADD, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_ADD_LOCK] = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_ADOX] = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_AESENC]   = {OP_PIPELINED_MEDIUM, -1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_AESENCLAST]   = {OP_PIPELINED_MEDIUM, -1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_AND]      = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_ANDN]     = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_ANDNPD]   = {OP_LOGIC, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_ANDNPS]   = {OP_LOGIC, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_ANDPD]    = {OP_LOGIC, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_ANDPS]    = {OP_LOGIC, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_AND_LOCK] = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_BLSI]     = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_BLSMSK]   = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_BLSR]     = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_BSF] = {OP_NOTPIPELINED_MEDIUM, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_BSR] = {OP_NOTPIPELINED_MEDIUM, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_BSWAP]     = {OP_MOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_BT]        = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_BTC]       = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_BTC_LOCK]  = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_BTR]       = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_BTR_LOCK]  = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_BTS]       = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_BTS_LOCK]  = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CALL_FAR]  = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CALL_NEAR] = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CBW]       = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CDQ]       = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CDQE]      = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CLAC]   = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CLC]       = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CLD]       = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CLI]       = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CMC]       = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CMOVB]     = {OP_CMOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CMOVBE]    = {OP_CMOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CMOVL]     = {OP_CMOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CMOVLE]    = {OP_CMOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CMOVNB]    = {OP_CMOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CMOVNBE]   = {OP_CMOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CMOVNL]    = {OP_CMOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CMOVNLE]   = {OP_CMOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CMOVNO]    = {OP_CMOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CMOVNP]    = {OP_CMOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CMOVNS]    = {OP_CMOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CMOVNZ]    = {OP_CMOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CMOVO]     = {OP_CMOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CMOVP]     = {OP_CMOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CMOVS]     = {OP_CMOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CMOVZ]     = {OP_CMOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CMP]       = {OP_ICMP, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CMPPD]     = {OP_FCMP, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CMPPS]     = {OP_FCMP, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CMPSB]     = {OP_ICMP, 1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CMPSD]     = {OP_ICMP, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CMPSD_XMM] = {OP_FCMP, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CMPSQ]     = {OP_ICMP, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CMPSS]     = {OP_FCMP, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CMPSW]     = {OP_ICMP, 2, 1, NONE};
  // TODO: make sure Scarab produces reasonable micro-ops for CMPXCHG.
  // Also, should not be a Scarab ifetch barrier.
  iclass_to_scarab_map[XED_ICLASS_CMPXCHG]         = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CMPXCHG16B]      = {OP_IADD, 2, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CMPXCHG16B_LOCK] = {OP_IADD, 2, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CMPXCHG8B]       = {OP_IADD, 1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CMPXCHG8B_LOCK]  = {OP_IADD, 1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CMPXCHG_LOCK]    = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_COMISD]          = {OP_ICMP, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_COMISS]          = {OP_ICMP, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CRC32]           = {OP_PIPELINED_MEDIUM, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CVTDQ2PD]        = {OP_FCVT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CVTDQ2PS]        = {OP_FCVT, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CVTPD2DQ]        = {OP_FCVT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CVTPD2PI]        = {OP_FCVT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CVTPD2PS]        = {OP_FCVT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CVTPI2PD]        = {OP_FCVT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CVTPI2PS]        = {OP_FCVT, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CVTPS2DQ]        = {OP_FCVT, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CVTPS2PD]        = {OP_FCVT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CVTPS2PI]        = {OP_FCVT, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CVTSD2SI]        = {OP_FCVT, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CVTSD2SS]        = {OP_FCVT, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CVTSI2SD]        = {OP_FCVT, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CVTSI2SS]        = {OP_FCVT, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CVTSS2SD]        = {OP_FCVT, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CVTSS2SI]        = {OP_FCVT, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CVTTPD2DQ]       = {OP_FCVT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CVTTPD2PI]       = {OP_FCVT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CVTTPS2DQ]       = {OP_FCVT, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CVTTPS2PI]       = {OP_FCVT, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CVTTSD2SI]       = {OP_FCVT, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CVTTSS2SI]       = {OP_FCVT, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CPUID]    = {OP_NOTPIPELINED_VERY_SLOW, -1, 1,
                                            NONE};
  iclass_to_scarab_map[XED_ICLASS_CQO]      = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CWD]      = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_CWDE]     = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_DEC]      = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_DEC_LOCK] = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_DIV]      = {OP_IDIV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_DIVPD]    = {OP_FDIV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_DIVPS]    = {OP_FDIV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_DIVSD]    = {OP_FDIV, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_DIVSS]    = {OP_FDIV, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_ENTER]    = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FABS]     = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FADD]     = {OP_FADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FADDP]    = {OP_FADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FCHS]     = {OP_LOGIC, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FCMOVB]   = {OP_FCMOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FCMOVBE]  = {OP_FCMOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FCMOVE]   = {OP_FCMOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FCMOVNB]  = {OP_FCMOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FCMOVNBE] = {OP_FCMOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FCMOVNE]  = {OP_FCMOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FCMOVNU]  = {OP_FCMOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FCMOVU]   = {OP_FCMOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FCOM]     = {OP_FCMP, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FCOMI]    = {OP_FCMP, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FCOMIP]   = {OP_FCMP, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FCOMP]    = {OP_FCMP, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FCOMPP]   = {OP_FCMP, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FCOS]     = {OP_NOTPIPELINED_VERY_SLOW, 8, 1,
                                           NONE};
  iclass_to_scarab_map[XED_ICLASS_FDISI8087_NOP] = {OP_NOP, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FDIV]          = {OP_FDIV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FDIVP]         = {OP_FDIV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FDIVR]         = {OP_FDIV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FDIVRP]        = {OP_FDIV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FENI8087_NOP]  = {OP_NOP, -1, 1, NONE};
  // Most FI* opcodes are mapped to FMUL to account for longer latency because
  // they involve an integer/float convertion AND operate.
  iclass_to_scarab_map[XED_ICLASS_FIADD]   = {OP_FMUL, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FICOM]   = {OP_FMUL, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FICOMP]  = {OP_FMUL, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FIDIV]   = {OP_FDIV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FIDIVR]  = {OP_FDIV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FILD]    = {OP_FCVT, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FIMUL]   = {OP_FMUL, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FINCSTP] = {OP_NOP, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FIST]    = {OP_FCVT, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FISTP]   = {OP_FCVT, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FISTTP]  = {OP_FCVT, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FISUB]   = {OP_FMUL, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FISUBR]  = {OP_FMUL, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FLD]     = {OP_MOV, -1, 1,
                                          NONE};  // Potential FMOV
  iclass_to_scarab_map[XED_ICLASS_FLD1]    = {OP_MOV, -1, 1,
                                           NONE};  // Potential FMOV
  iclass_to_scarab_map[XED_ICLASS_FLDCW]   = {OP_NOTPIPELINED_MEDIUM, -1, 1,
                                            NONE};
  iclass_to_scarab_map[XED_ICLASS_FLDENV]   = {OP_NOTPIPELINED_MEDIUM, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FLDL2E]  = {OP_MOV, -1, 1,
                                             NONE};  // Potential FMOV
  iclass_to_scarab_map[XED_ICLASS_FLDL2T]  = {OP_MOV, -1, 1,
                                             NONE};  // Potential FMOV
  iclass_to_scarab_map[XED_ICLASS_FLDLG2]  = {OP_MOV, -1, 1,
                                             NONE};  // Potential FMOV
  iclass_to_scarab_map[XED_ICLASS_FLDLN2]  = {OP_MOV, -1, 1,
                                             NONE};  // Potential FMOV
  iclass_to_scarab_map[XED_ICLASS_FLDPI]   = {OP_MOV, -1, 1,
                                            NONE};  // Potential FMOV
  iclass_to_scarab_map[XED_ICLASS_FLDZ]    = {OP_MOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FMUL]    = {OP_FMUL, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FMULP]   = {OP_FMUL, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FNCLEX] = {OP_NOTPIPELINED_SLOW, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FNSTCW] = {OP_NOTPIPELINED_MEDIUM, -1, 1,
                                             NONE};
  iclass_to_scarab_map[XED_ICLASS_FNSTENV] = {OP_NOTPIPELINED_SLOW, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FNSTSW] = {OP_NOTPIPELINED_MEDIUM, -1, 1,
                                             NONE};
  iclass_to_scarab_map[XED_ICLASS_FNOP]   = {OP_NOP, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FPREM]  = {OP_FMUL, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FRNDINT]       = {OP_FCVT, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FSETPM287_NOP] = {OP_NOP, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FSIN]    = {OP_NOTPIPELINED_VERY_SLOW, 8, 1,
                                           NONE};
  iclass_to_scarab_map[XED_ICLASS_FSINCOS] = {OP_NOTPIPELINED_VERY_SLOW, 8, 1,
                                              NONE};
  iclass_to_scarab_map[XED_ICLASS_FSQRT]   = {OP_FDIV, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FST]     = {OP_MOV, -1, 1,
                                          NONE};  // Potential FMOV
  iclass_to_scarab_map[XED_ICLASS_FSTP]    = {OP_MOV, -1, 1,
                                           NONE};  // Potential FMOV
  iclass_to_scarab_map[XED_ICLASS_FSUB]    = {OP_FADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FSUBP]   = {OP_FADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FSUBR]   = {OP_FADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FSUBRP]  = {OP_FADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FUCOM]   = {OP_FCMP, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FUCOMI]  = {OP_FCMP, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FUCOMIP] = {OP_FCMP, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FUCOMP]  = {OP_FCMP, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FUCOMPP] = {OP_FCMP, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FWAIT]   = {OP_NOP, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FXAM]    = {OP_FCMP, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FXCH]    = {OP_NOP, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_FYL2X]   = {OP_NOTPIPELINED_VERY_SLOW, 8, 1,
                                            NONE};
  iclass_to_scarab_map[XED_ICLASS_FYL2XP1] = {OP_NOTPIPELINED_VERY_SLOW, 8, 1,
                                              NONE};
  iclass_to_scarab_map[XED_ICLASS_IDIV]    = {OP_IDIV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_IMUL]    = {OP_IMUL, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_IN]     = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_INC]     = {OP_MOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_INT]     = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_INT3]    = {OP_IADD, -1, 1, NONE};
  // TODO: find out what opcodes these iclasses correspond to.
  // iclass_to_scarab_map[XED_ICLASS_INCPPSD] = {OP_IADD, -1, 1, NONE};
  // iclass_to_scarab_map[XED_ICLASS_INCPPSQ] = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_INC_LOCK] = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_INVLPG] = {OP_LOGIC, -1, 1, NONE};

  iclass_to_scarab_map[XED_ICLASS_IRETQ]    = {OP_PIPELINED_MEDIUM, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_JB]       = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_JBE]      = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_JCXZ]     = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_JECXZ]    = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_JL]       = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_JLE]      = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_JMP]      = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_JMP_FAR]  = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_JNB]      = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_JNBE]     = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_JNL]      = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_JNLE]     = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_JNO]      = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_JNP]      = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_JNS]      = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_JNZ]      = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_JO]       = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_JP]       = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_JRCXZ]    = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_JS]       = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_JZ]       = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_KNOTB]    = {OP_LOGIC, 1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_KNOTD]    = {OP_LOGIC, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_KNOTQ]    = {OP_LOGIC, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_KNOTW]    = {OP_LOGIC, 2, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_KMOVB]    = {OP_MOV, 1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_KMOVD]    = {OP_MOV, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_KMOVQ]    = {OP_MOV, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_KMOVW]    = {OP_MOV, 2, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_LDDQU]    = {OP_MOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_LDMXCSR]  = {OP_NOTPIPELINED_MEDIUM, -1, 1,
                                              NONE};
  iclass_to_scarab_map[XED_ICLASS_LEA]      = {OP_LDA, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_LEAVE]    = {OP_IADD, -1, 1, NONE};
  // TODO: this should be a memory barrier if we want to support
  // multi-threaded execution.
  iclass_to_scarab_map[XED_ICLASS_LFENCE] = {OP_NOP, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_LODSB]  = {OP_MOV, 1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_LODSD]  = {OP_MOV, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_LODSQ]  = {OP_MOV, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_LODSW]  = {OP_MOV, 2, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_LOOP]   = {OP_CF,  -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_LSL]    = {OP_LDA, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_LZCNT]  = {OP_PIPELINED_FAST, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MAXPD]  = {OP_FCMP, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MAXPS]  = {OP_FCMP, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MAXSD]  = {OP_FCMP, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MAXSS]  = {OP_FCMP, 4, 1, NONE};
  // TODO: this should be a memory barrier if we want to support
  // multi-threaded execution.
  iclass_to_scarab_map[XED_ICLASS_MFENCE]    = {OP_NOP, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MINPD]     = {OP_FCMP, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MINPS]     = {OP_FCMP, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MINSD]     = {OP_FCMP, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MINSS]     = {OP_FCMP, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MONITOR]   = {OP_LDA, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MOV]       = {OP_MOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MOVAPD]    = {OP_MOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MOVAPS]    = {OP_MOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MOV_CR]    = {OP_MOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MOV_DR]    = {OP_MOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MOVBE]     = {OP_MOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MOVD]      = {OP_PIPELINED_FAST, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MOVDDUP]   = {OP_MOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MOVDQ2Q]   = {OP_PIPELINED_FAST, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MOVDQA]    = {OP_MOV, 8, 2, NONE};
  iclass_to_scarab_map[XED_ICLASS_MOVDQU]    = {OP_MOV, 8, 2, NONE};
  iclass_to_scarab_map[XED_ICLASS_MOVHLPS]   = {OP_MOV, 4, 2, NONE};
  iclass_to_scarab_map[XED_ICLASS_MOVHPD]    = {OP_MOV, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MOVHPS]    = {OP_MOV, 4, 2, NONE};
  iclass_to_scarab_map[XED_ICLASS_MOVLHPS]   = {OP_MOV, 4, 2, NONE};
  iclass_to_scarab_map[XED_ICLASS_MOVLPD]    = {OP_MOV, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MOVLPS]    = {OP_MOV, 4, 2, NONE};
  iclass_to_scarab_map[XED_ICLASS_MOVMSKPD]  = {OP_PIPELINED_FAST, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MOVMSKPS]  = {OP_PIPELINED_FAST, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MOVNTDQ]   = {OP_MOV, 8, 2, NT};
  iclass_to_scarab_map[XED_ICLASS_MOVNTDQA]  = {OP_MOV, 8, 2, NT};
  iclass_to_scarab_map[XED_ICLASS_MOVNTI]    = {OP_MOV, -1, 1, NT};
  iclass_to_scarab_map[XED_ICLASS_MOVNTPD]   = {OP_MOV, 8, -1, NT};
  iclass_to_scarab_map[XED_ICLASS_MOVNTPS]   = {OP_MOV, 4, -1, NT};
  iclass_to_scarab_map[XED_ICLASS_MOVNTQ]    = {OP_MOV, 8, 1, NT};
  iclass_to_scarab_map[XED_ICLASS_MOVNTSD]   = {OP_MOV, 8, 1, NT};
  iclass_to_scarab_map[XED_ICLASS_MOVNTSS]   = {OP_MOV, 4, 1, NT};
  iclass_to_scarab_map[XED_ICLASS_MOVQ]      = {OP_MOV, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MOVQ2DQ]   = {OP_PIPELINED_FAST, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MOVSB]     = {OP_MOV, 1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MOVSD]     = {OP_MOV, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MOVSD_XMM] = {OP_MOV, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MOVSHDUP]  = {OP_MOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MOVSLDUP]  = {OP_MOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MOVSQ]     = {OP_MOV, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MOVSS]     = {OP_MOV, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MOVSW]     = {OP_MOV, 2, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MOVSX]     = {OP_MOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MOVSXD]    = {OP_MOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MOVUPD]    = {OP_PIPELINED_FAST, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MOVUPS]    = {OP_PIPELINED_FAST, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MOVZX]     = {OP_MOV, -1, 1, NONE};
  // Moves to debug and control registers, left unmapped
  // iclass_to_scarab_map[XED_ICLASS_MOV_CR] = {OP_MOV, -1, 1, NONE};
  // iclass_to_scarab_map[XED_ICLASS_MOV_DR] = {OP_MOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MPSADBW]   = {OP_NOTPIPELINED_MEDIUM, 1, 16,
                                              NONE};
  iclass_to_scarab_map[XED_ICLASS_MUL]       = {OP_IMUL, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MULPD]     = {OP_FMUL, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MULPS]     = {OP_FMUL, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MULSD]     = {OP_FMUL, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MULSS]     = {OP_FMUL, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MULX]      = {OP_IMUL, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_MWAIT]      = {OP_NOTPIPELINED_MEDIUM, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_NEG]       = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_NEG_LOCK]  = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_NOP]       = {OP_NOP, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_NOP2]      = {OP_NOP, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_NOP3]      = {OP_NOP, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_NOP4]      = {OP_NOP, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_NOP5]      = {OP_NOP, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_NOP6]      = {OP_NOP, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_NOP7]      = {OP_NOP, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_NOP8]      = {OP_NOP, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_NOP9]      = {OP_NOP, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_NOT]       = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_NOT_LOCK]  = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_OR]        = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_ORPD]      = {OP_LOGIC, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_ORPS]      = {OP_LOGIC, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_OR_LOCK]   = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_OUT]   = {OP_MOV, 2, 1, NONE};

  iclass_to_scarab_map[XED_ICLASS_PABSB]     = {OP_LOGIC, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PABSD]     = {OP_LOGIC, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PACKSSDW]  = {OP_MOV, -1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PACKUSWB]  = {OP_MOV, -1, -1, NONE}; // I think output lane bytes will be different than input lane bytes, but not sure.
  iclass_to_scarab_map[XED_ICLASS_PABSW]     = {OP_LOGIC, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PADDB]     = {OP_IADD, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PADDD]     = {OP_IADD, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PADDQ]     = {OP_IADD, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PADDSB]    = {OP_IADD, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PADDSW]    = {OP_IADD, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PADDUSB]   = {OP_IADD, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PADDUSW]   = {OP_IADD, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PADDW]     = {OP_IADD, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PALIGNR]   = {OP_SHIFT, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PAND]      = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PANDN]     = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PAUSE]     = {OP_NOTPIPELINED_VERY_SLOW, 1, 1,
                                            NONE};
  iclass_to_scarab_map[XED_ICLASS_PAVGB]     = {OP_LOGIC, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PAVGW]     = {OP_LOGIC, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PBLENDVB]  = {OP_CMOV, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PBLENDW]   = {OP_CMOV, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PCLMULQDQ] = {OP_IMUL, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PCMPEQB]   = {OP_ICMP, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PCMPEQD]   = {OP_ICMP, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PCMPEQQ]   = {OP_ICMP, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PCMPEQW]   = {OP_ICMP, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PCMPESTRI] = {OP_NOTPIPELINED_SLOW, 1, -1,
                                                NONE};
  iclass_to_scarab_map[XED_ICLASS_PCMPESTRM] = {OP_NOTPIPELINED_SLOW, 1, -1,
                                                NONE};
  iclass_to_scarab_map[XED_ICLASS_PCMPGTB]   = {OP_ICMP, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PCMPGTD]   = {OP_ICMP, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PCMPGTQ]   = {OP_ICMP, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PCMPGTW]   = {OP_ICMP, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PCMPISTRI] = {OP_NOTPIPELINED_SLOW, 1, -1,
                                                NONE};
  iclass_to_scarab_map[XED_ICLASS_PCMPISTRM] = {OP_NOTPIPELINED_SLOW, 1, -1,
                                                NONE};
  iclass_to_scarab_map[XED_ICLASS_PEXTRB]    = {OP_PIPELINED_FAST, 1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PEXTRD]    = {OP_PIPELINED_FAST, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PEXTRQ]    = {OP_PIPELINED_FAST, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PEXTRW]    = {OP_PIPELINED_FAST, 2, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PEXTRW_SSE4] = {OP_PIPELINED_FAST, 2, 1,
                                                  NONE};
  iclass_to_scarab_map[XED_ICLASS_PINSRB]      = {OP_MOV, 1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PINSRD]      = {OP_MOV, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PINSRQ]      = {OP_MOV, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PINSRW]      = {OP_MOV, 2, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PMOVMSKB]  = {OP_PIPELINED_FAST, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PMADDUBSW] = {OP_IMUL, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PMADDWD]   = {OP_IMUL, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PMAXSB]    = {OP_ICMP, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PMAXSD]    = {OP_ICMP, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PMAXSW]    = {OP_ICMP, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PMAXUB]    = {OP_ICMP, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PMAXUD]    = {OP_ICMP, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PMAXUW]    = {OP_ICMP, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PMINSB]    = {OP_ICMP, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PMINSD]    = {OP_ICMP, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PMINSW]    = {OP_ICMP, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PMINUB]    = {OP_ICMP, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PMINUD]    = {OP_ICMP, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PMINUW]    = {OP_ICMP, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PMOVMSKB]  = {OP_PIPELINED_FAST, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PMOVSXBD]  = {OP_CMOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PMOVSXBQ]  = {OP_CMOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PMOVSXBW]  = {OP_CMOV, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PMOVSXDQ]  = {OP_CMOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PMOVSXWD]  = {OP_CMOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PMOVSXWQ]  = {OP_CMOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PMOVZXBD]  = {OP_CMOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PMOVZXBQ]  = {OP_CMOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PMOVZXBW]  = {OP_CMOV, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PMOVZXDQ]  = {OP_CMOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PMOVZXWD]  = {OP_CMOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PMOVZXWQ]  = {OP_CMOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PMULDQ]    = {OP_IMUL, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PMULHRSW]  = {OP_IMUL, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PMULHRW]   = {OP_IMUL, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PMULHUW]   = {OP_IMUL, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PMULHW]    = {OP_IMUL, 2, -1, NONE};
  // according to Agner Fog, PMULLD is twice the latency of the other PMUL*
  // instructions...
  iclass_to_scarab_map[XED_ICLASS_PMULLD]  = {OP_PIPELINED_MEDIUM, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PMULLW]  = {OP_IMUL, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PMULUDQ] = {OP_IMUL, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_POP]     = {OP_IADD, -1, 1, NONE};
  // POPA and POPAD should create multiple micro-ops
  // iclass_to_scarab_map[XED_ICLASS_POPA] = {OP_IADD, -1, 1, NONE};
  // iclass_to_scarab_map[XED_ICLASS_POPAD] = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_POPCNT] = {OP_PIPELINED_MEDIUM, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_POPF]   = {OP_NOTPIPELINED_SLOW, 2, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_POPFD]  = {OP_NOTPIPELINED_SLOW, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_POPFQ]  = {OP_NOTPIPELINED_SLOW, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_POR]    = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PREFETCHNTA] = {OP_MOV, -1, 1, NT};
  iclass_to_scarab_map[XED_ICLASS_PREFETCHT0]  = {OP_MOV, -1, 1, T0};
  iclass_to_scarab_map[XED_ICLASS_PREFETCHT1]  = {OP_MOV, -1, 1, T1};
  iclass_to_scarab_map[XED_ICLASS_PREFETCHT2]  = {OP_MOV, -1, 1, T2};
  iclass_to_scarab_map[XED_ICLASS_PREFETCHW]   = {OP_MOV, -1, 1, W};
  iclass_to_scarab_map[XED_ICLASS_PREFETCHWT1] = {OP_MOV, -1, 1, WT1};
  // iclass_to_scarab_map[XED_ICLASS_PREFETCH_EXCLUSIVE] = {OP_MOV, -1, 1,
  // EXCLUSIVE};
  // iclass_to_scarab_map[XED_ICLASS_PREFETCH_RESERVED] = {OP_MOV, -1, 1,
  // RESERVED};
  iclass_to_scarab_map[XED_ICLASS_PSADBW]    = {OP_PIPELINED_FAST, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PSHUFB]    = {OP_MOV, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PSHUFD]    = {OP_MOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PSHUFHW]   = {OP_MOV, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PSHUFLW]   = {OP_MOV, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PSHUFW]    = {OP_MOV, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PSLLD]     = {OP_SHIFT, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PSLLDQ]    = {OP_SHIFT, 16, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PSLLQ]     = {OP_SHIFT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PSLLW]     = {OP_SHIFT, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PSRAD]     = {OP_SHIFT, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PSRAW]     = {OP_SHIFT, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PSRLD]     = {OP_SHIFT, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PSRLDQ]    = {OP_SHIFT, 16, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PSRLQ]     = {OP_SHIFT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PSRLW]     = {OP_SHIFT, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PSUBB]     = {OP_IADD, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PSUBD]     = {OP_IADD, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PSUBQ]     = {OP_IADD, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PSUBSB]    = {OP_IADD, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PSUBSW]    = {OP_IADD, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PSUBUSB]   = {OP_IADD, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PSUBUSW]   = {OP_IADD, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PSUBW]     = {OP_IADD, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PTEST]     = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PUNPCKHBW] = {OP_MOV, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PUNPCKHDQ] = {OP_MOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PUNPCKHQDQ] = {OP_MOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PUNPCKHWD]  = {OP_MOV, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PUNPCKLBW]  = {OP_MOV, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PUNPCKLDQ]  = {OP_MOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PUNPCKLQDQ] = {OP_MOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PUNPCKLWD]  = {OP_MOV, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PUSH]       = {OP_IADD, -1, 1, NONE};
  // PUSHA and PUSHAD should create multiple micro-ops
  // iclass_to_scarab_map[XED_ICLASS_PUSHA] = {OP_IADD, -1, 1, NONE};
  // iclass_to_scarab_map[XED_ICLASS_PUSHAD] = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PUSHF]  = {OP_NOTPIPELINED_SLOW, 2, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PUSHFD] = {OP_NOTPIPELINED_SLOW, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PUSHFQ] = {OP_NOTPIPELINED_SLOW, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PXOR]   = {OP_LOGIC, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_RCPPS]  = {OP_FMUL, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_RCL]      = {OP_SHIFT, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_RCR]      = {OP_SHIFT, -1, 1, NONE};

  iclass_to_scarab_map[XED_ICLASS_RDTSC]  = {OP_NOTPIPELINED_SLOW, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_RDTSCP]  = {OP_NOTPIPELINED_SLOW, 8, 1, NONE}; // 4 bytes for time-stamp (somehow), 4 for processor id
  iclass_to_scarab_map[XED_ICLASS_RDPKRU] = {OP_MOV, 1, 2, NONE};
  // INS_Opcode() never returns REPEAT variants of the iclasses
  iclass_to_scarab_map[XED_ICLASS_REPE_CMPSB]  = {OP_ICMP, 1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_REPE_CMPSD]  = {OP_ICMP, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_REPE_CMPSQ]  = {OP_ICMP, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_REPE_CMPSW]  = {OP_ICMP, 2, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_REPNE_CMPSB] = {OP_ICMP, 1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_REPNE_CMPSD] = {OP_ICMP, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_REPNE_CMPSQ] = {OP_ICMP, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_REPNE_CMPSW] = {OP_ICMP, 2, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_REPNE_SCASB] = {OP_ICMP, 1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_REPNE_SCASD] = {OP_ICMP, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_REPNE_SCASQ] = {OP_ICMP, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_REPNE_SCASW] = {OP_ICMP, 2, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_REP_MOVSB]   = {OP_MOV, 1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_REP_MOVSD]   = {OP_MOV, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_REP_MOVSQ]   = {OP_MOV, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_REP_MOVSW]   = {OP_MOV, 2, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_REP_STOSB]   = {OP_MOV, 1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_REP_STOSD]   = {OP_MOV, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_REP_STOSQ]   = {OP_MOV, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_REP_STOSW]   = {OP_MOV, 2, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_RET_FAR]     = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_RET_NEAR]    = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_ROL]         = {OP_SHIFT, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_ROR]         = {OP_SHIFT, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_RORX]        = {OP_SHIFT, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_ROUNDPD]     = {OP_FCVT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_ROUNDPS]     = {OP_FCVT, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_ROUNDSD]     = {OP_FCVT, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_ROUNDSS]     = {OP_FCVT, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_RSQRTPS] = {OP_PIPELINED_MEDIUM, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_RSQRTSS] = {OP_PIPELINED_MEDIUM, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SAHF]    = {OP_MOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SAR]     = {OP_SHIFT, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SARX]    = {OP_SHIFT, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SBB]     = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SBB_LOCK] = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SCASB]    = {OP_ICMP, 1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SCASD]    = {OP_ICMP, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SCASQ]    = {OP_ICMP, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SCASW]    = {OP_ICMP, 2, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SETB]     = {OP_MOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SETBE]    = {OP_MOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SETL]     = {OP_MOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SETLE]    = {OP_MOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SETNB]    = {OP_MOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SETNBE]   = {OP_MOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SETNL]    = {OP_MOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SETNLE]   = {OP_MOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SETNO]    = {OP_MOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SETNP]    = {OP_MOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SETNS]    = {OP_MOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SETNZ]    = {OP_MOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SETO]     = {OP_MOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SETP]     = {OP_MOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SETS]     = {OP_MOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SETZ]     = {OP_MOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SHL]      = {OP_SHIFT, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SHLD]     = {OP_SHIFT, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SHLX]     = {OP_SHIFT, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SHR]      = {OP_SHIFT, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SHRD]     = {OP_SHIFT, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SHRX]     = {OP_SHIFT, -1, 1, NONE};
  // TODO: this should be a memory barrier if we want to support
  // multi-threaded execution.
  iclass_to_scarab_map[XED_ICLASS_SFENCE]  = {OP_NOP, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SHUFPD]  = {OP_MOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SHUFPS]  = {OP_MOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SQRTPD]  = {OP_FDIV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SQRTPS]  = {OP_FDIV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SQRTSD]  = {OP_FDIV, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SQRTSS]  = {OP_FDIV, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_STAC]   = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_STC]   = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_STD]   = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_STI]   = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_STMXCSR] = {OP_PIPELINED_MEDIUM, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_STOSB]   = {OP_MOV, 1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_STOSD]   = {OP_MOV, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_STOSQ]   = {OP_MOV, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_STOSW]   = {OP_MOV, 2, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SUB]     = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SUBPD]   = {OP_FADD, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SUBPS]   = {OP_FADD, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SUBSD]   = {OP_FADD, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SUBSS]   = {OP_FADD, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SUB_LOCK] = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SYSCALL]  = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SYSENTER] = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SYSRET] = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_SYSRET64] = {OP_IADD, -1, 1, NONE};

  iclass_to_scarab_map[XED_ICLASS_SWAPGS]  = {OP_MOV,-1,1,NONE};
  iclass_to_scarab_map[XED_ICLASS_TEST]     = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_TZCNT]    = {OP_NOTPIPELINED_MEDIUM, -1, 1,
                                            NONE};
  iclass_to_scarab_map[XED_ICLASS_UCOMISD]  = {OP_FCMP, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_UCOMISS]  = {OP_FCMP, 4, -1, NONE};
  // TODO: It works only for trace front-end now
  iclass_to_scarab_map[XED_ICLASS_UD0]             = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_UD1]             = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_UD2]             = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_UNPCKHPD]        = {OP_MOV, -1, 8, NONE};
  iclass_to_scarab_map[XED_ICLASS_UNPCKHPS]        = {OP_MOV, -1, 4, NONE};
  iclass_to_scarab_map[XED_ICLASS_UNPCKLPD]        = {OP_MOV, -1, 8, NONE};
  iclass_to_scarab_map[XED_ICLASS_UNPCKLPS]        = {OP_MOV, -1, 4, NONE};
  iclass_to_scarab_map[XED_ICLASS_VADDPD]          = {OP_FADD, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VADDPS]          = {OP_FADD, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VADDSD]          = {OP_FADD, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VADDSS]          = {OP_FADD, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VADDSUBPD]       = {OP_FADD, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VADDSUBPS]       = {OP_FADD, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VAESENC]         = {OP_PIPELINED_MEDIUM, -1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VAESENCLAST]     = {OP_PIPELINED_MEDIUM, -1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VANDNPD]         = {OP_LOGIC, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VANDNPS]         = {OP_LOGIC, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VANDPD]          = {OP_LOGIC, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VANDPS]          = {OP_LOGIC, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VBLENDMPD]       = {OP_CMOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VBLENDMPS]       = {OP_CMOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VBLENDPD]        = {OP_CMOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VBLENDPS]        = {OP_CMOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VBLENDVPD]       = {OP_CMOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VBLENDVPS]       = {OP_CMOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VBROADCASTF128]  = {OP_MOV, 16, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VBROADCASTF32X2] = {OP_MOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VBROADCASTF32X4] = {OP_MOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VBROADCASTF32X8] = {OP_MOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VBROADCASTF64X2] = {OP_MOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VBROADCASTF64X4] = {OP_MOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VBROADCASTI128]  = {OP_MOV, 16, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VBROADCASTI32X2] = {OP_MOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VBROADCASTI32X4] = {OP_MOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VBROADCASTI32X8] = {OP_MOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VBROADCASTI64X2] = {OP_MOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VBROADCASTI64X4] = {OP_MOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VBROADCASTSD]    = {OP_MOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VBROADCASTSS]    = {OP_MOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCOMISD]         = {OP_ICMP, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCOMISS]         = {OP_ICMP, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCMPPD]          = {OP_FCMP, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCMPPS]          = {OP_FCMP, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCMPSD]          = {OP_FCMP, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCMPSS]          = {OP_FCMP, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTDQ2PD]       = {OP_FCVT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTDQ2PS]       = {OP_FCVT, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTPD2DQ]       = {OP_FCVT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTPD2PS]       = {OP_FCVT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTPD2QQ]       = {OP_FCVT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTPD2UDQ]      = {OP_FCVT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTPD2UQQ]      = {OP_FCVT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTPH2PS]       = {OP_FCVT, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTPS2DQ]       = {OP_FCVT, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTPS2PD]       = {OP_FCVT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTPS2PH]       = {OP_FCVT, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTPS2QQ]       = {OP_FCVT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTPS2UDQ]      = {OP_FCVT, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTPS2UQQ]      = {OP_FCVT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTSD2SI]       = {OP_FCVT, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTSD2SS]       = {OP_FCVT, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTSD2USI]      = {OP_FCVT, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTSI2SD]       = {OP_FCVT, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTSI2SS]       = {OP_FCVT, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTSS2SD]       = {OP_FCVT, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTSS2SI]       = {OP_FCVT, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTSS2USI]      = {OP_FCVT, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTTPD2DQ]      = {OP_FCVT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTTPD2QQ]      = {OP_FCVT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTTPD2UDQ]     = {OP_FCVT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTTPD2UQQ]     = {OP_FCVT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTTPS2DQ]      = {OP_FCVT, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTTPS2QQ]      = {OP_FCVT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTTPS2UDQ]     = {OP_FCVT, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTTPS2UQQ]     = {OP_FCVT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTTSD2SI]      = {OP_FCVT, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTTSD2USI]     = {OP_FCVT, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTTSS2SI]      = {OP_FCVT, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTTSS2USI]     = {OP_FCVT, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTUDQ2PD]      = {OP_FCVT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTUDQ2PS]      = {OP_FCVT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTUQQ2PD]      = {OP_FCVT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTUQQ2PS]      = {OP_FCVT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTUSI2SD]      = {OP_FCVT, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VCVTUSI2SS]      = {OP_FCVT, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VDBPSADBW] = {OP_PIPELINED_FAST, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VDIVPD]    = {OP_FDIV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VDIVPS]    = {OP_FDIV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VDIVSD]    = {OP_FDIV, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VDIVSS]    = {OP_FDIV, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VERR]  = {OP_LOGIC, 2, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VERW]  = {OP_LOGIC, 2, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VEXTRACTF128]  = {OP_MOV, 16, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VEXTRACTF32X4] = {OP_MOV, 16, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VEXTRACTF32X8] = {OP_MOV, 32, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VEXTRACTF64X2] = {OP_MOV, 16, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VEXTRACTF64X4] = {OP_MOV, 32, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VEXTRACTI128]  = {OP_MOV, 16, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VEXTRACTI32X4] = {OP_MOV, 16, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VEXTRACTI32X8] = {OP_MOV, 32, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VEXTRACTI64X2] = {OP_MOV, 16, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VEXTRACTI64X4] = {OP_MOV, 32, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VEXTRACTPS]    = {OP_PIPELINED_FAST, 4, -1,
                                                 NONE};
  iclass_to_scarab_map[XED_ICLASS_VFPCLASSPD]    = {OP_FCMP, 8, 8, NONE};
  // FMA instructions
  iclass_to_scarab_map[XED_ICLASS_VFMADD132PD]    = {OP_FMA, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMADD132PS]    = {OP_FMA, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMADD132SD]    = {OP_FMA, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMADD132SS]    = {OP_FMA, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMADD213PD]    = {OP_FMA, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMADD213PS]    = {OP_FMA, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMADD213SD]    = {OP_FMA, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMADD213SS]    = {OP_FMA, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMADD231PD]    = {OP_FMA, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMADD231PS]    = {OP_FMA, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMADD231SD]    = {OP_FMA, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMADD231SS]    = {OP_FMA, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMADDPD]       = {OP_FMA, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMADDPS]       = {OP_FMA, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMADDSD]       = {OP_FMA, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMADDSS]       = {OP_FMA, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMADDSUB132PD] = {OP_FMA, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMADDSUB132PS] = {OP_FMA, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMADDSUB213PD] = {OP_FMA, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMADDSUB213PS] = {OP_FMA, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMADDSUB231PD] = {OP_FMA, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMADDSUB231PS] = {OP_FMA, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMADDSUBPD]    = {OP_FMA, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMADDSUBPS]    = {OP_FMA, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMSUB132PD]    = {OP_FMA, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMSUB132PS]    = {OP_FMA, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMSUB132SD]    = {OP_FMA, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMSUB132SS]    = {OP_FMA, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMSUB213PD]    = {OP_FMA, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMSUB213PS]    = {OP_FMA, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMSUB213SD]    = {OP_FMA, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMSUB213SS]    = {OP_FMA, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMSUB231PD]    = {OP_FMA, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMSUB231PS]    = {OP_FMA, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMSUB231SD]    = {OP_FMA, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMSUB231SS]    = {OP_FMA, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMSUBADD132PD] = {OP_FMA, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMSUBADD132PS] = {OP_FMA, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMSUBADD213PD] = {OP_FMA, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMSUBADD213PS] = {OP_FMA, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMSUBADD231PD] = {OP_FMA, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMSUBADD231PS] = {OP_FMA, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMSUBADDPD]    = {OP_FMA, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMSUBADDPS]    = {OP_FMA, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMSUBPD]       = {OP_FMA, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMSUBPS]       = {OP_FMA, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMSUBSD]       = {OP_FMA, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFMSUBSS]       = {OP_FMA, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFNMADD132PD]   = {OP_FMA, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFNMADD132PS]   = {OP_FMA, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFNMADD132SD]   = {OP_FMA, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFNMADD132SS]   = {OP_FMA, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFNMADD213PD]   = {OP_FMA, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFNMADD213PS]   = {OP_FMA, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFNMADD213SD]   = {OP_FMA, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFNMADD213SS]   = {OP_FMA, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFNMADD231PD]   = {OP_FMA, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFNMADD231PS]   = {OP_FMA, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFNMADD231SD]   = {OP_FMA, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFNMADD231SS]   = {OP_FMA, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFNMADDPD]      = {OP_FMA, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFNMADDPS]      = {OP_FMA, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFNMADDSD]      = {OP_FMA, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFNMADDSS]      = {OP_FMA, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFNMSUB132PD]   = {OP_FMA, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFNMSUB132PS]   = {OP_FMA, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFNMSUB132SD]   = {OP_FMA, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFNMSUB132SS]   = {OP_FMA, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFNMSUB213PD]   = {OP_FMA, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFNMSUB213PS]   = {OP_FMA, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFNMSUB213SD]   = {OP_FMA, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFNMSUB213SS]   = {OP_FMA, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFNMSUB231PD]   = {OP_FMA, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFNMSUB231PS]   = {OP_FMA, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFNMSUB231SD]   = {OP_FMA, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFNMSUB231SS]   = {OP_FMA, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFNMSUBPD]      = {OP_FMA, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFNMSUBPS]      = {OP_FMA, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFNMSUBSD]      = {OP_FMA, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VFNMSUBSS]      = {OP_FMA, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VGATHERDPD]     = {OP_GATHER, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VGATHERDPS]     = {OP_GATHER, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VGATHERPF0DPD]  = {OP_GATHER, 4, 16, T0};
  iclass_to_scarab_map[XED_ICLASS_VGATHERPF0DPS]  = {OP_GATHER, 4, 16, T0};
  iclass_to_scarab_map[XED_ICLASS_VGATHERPF0QPD]  = {OP_GATHER, 8, 8, T0};
  iclass_to_scarab_map[XED_ICLASS_VGATHERPF0QPS]  = {OP_GATHER, 8, 8, T0};
  iclass_to_scarab_map[XED_ICLASS_VGATHERPF1DPD]  = {OP_GATHER, 4, 16, T1};
  iclass_to_scarab_map[XED_ICLASS_VGATHERPF1DPS]  = {OP_GATHER, 4, 16, T1};
  iclass_to_scarab_map[XED_ICLASS_VGATHERPF1QPD]  = {OP_GATHER, 8, 8, T1};
  iclass_to_scarab_map[XED_ICLASS_VGATHERPF1QPS]  = {OP_GATHER, 8, 8, T1};
  iclass_to_scarab_map[XED_ICLASS_VGATHERQPD]     = {OP_GATHER, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VGATHERQPS]     = {OP_GATHER, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VGETEXPPD]      = {OP_FCVT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VGETEXPPS]      = {OP_FCVT, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VGETEXPSD]      = {OP_FCVT, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VGETEXPSS]      = {OP_FCVT, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VGETMANTPD]     = {OP_FCVT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VGETMANTPS]     = {OP_FCVT, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VGETMANTSD]     = {OP_FCVT, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VGETMANTSS]     = {OP_FCVT, 4, 1, NONE};

  iclass_to_scarab_map[XED_ICLASS_VINSERTF128]  = {OP_MOV, 16, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VINSERTF32X4] = {OP_MOV, 16, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VINSERTF32X8] = {OP_MOV, 32, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VINSERTF64X2] = {OP_MOV, 16, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VINSERTF64X4] = {OP_MOV, 32, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VINSERTI128]  = {OP_MOV, 16, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VINSERTI32X4] = {OP_MOV, 16, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VINSERTI32X8] = {OP_MOV, 32, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VINSERTI64X2] = {OP_MOV, 16, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VINSERTI64X4] = {OP_MOV, 32, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VINSERTPS]    = {OP_MOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VLDMXCSR]    = {OP_NOTPIPELINED_MEDIUM, -1, 1,
                                               NONE};
  iclass_to_scarab_map[XED_ICLASS_VMASKMOVDQU] = {OP_PIPELINED_FAST, 1, -1,
                                                  NONE};
  iclass_to_scarab_map[XED_ICLASS_VMASKMOVPD]  = {OP_PIPELINED_FAST, 8, -1,
                                                 NONE};
  iclass_to_scarab_map[XED_ICLASS_VMASKMOVPS]  = {OP_PIPELINED_FAST, 4, -1,
                                                 NONE};
  iclass_to_scarab_map[XED_ICLASS_VMAXPD]      = {OP_FCMP, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VMAXPS]      = {OP_FCMP, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VMAXSD]      = {OP_FCMP, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VMAXSS]      = {OP_FCMP, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VMINPD]      = {OP_FCMP, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VMINPS]      = {OP_FCMP, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VMINSD]      = {OP_FCMP, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VMINSS]      = {OP_FCMP, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VMOVAPD]     = {OP_MOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VMOVAPS]     = {OP_MOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VMOVD]       = {OP_MOV, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VMOVDDUP]    = {OP_MOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VMOVDDUP]    = {OP_MOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VMOVDQA]     = {OP_MOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VMOVDQA32]   = {OP_MOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VMOVDQA64]   = {OP_MOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VMOVDQU]     = {OP_MOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VMOVDQU16]   = {OP_MOV, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VMOVDQU32]   = {OP_MOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VMOVDQU64]   = {OP_MOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VMOVDQU8]    = {OP_MOV, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VMOVHLPS]    = {OP_MOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VMOVHPD]     = {OP_MOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VMOVHPS]     = {OP_MOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VMOVLHPS]    = {OP_MOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VMOVLPD]     = {OP_MOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VMOVLPS]     = {OP_MOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VMOVMSKPD] = {OP_PIPELINED_FAST, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VMOVMSKPS] = {OP_PIPELINED_FAST, -1, 1, NONE};
  // TODO: Scarab currently does not support stores with non-temporal hints.
  // It will simply produces a store micro-op that does not bypass the cache.
  // Should we model this as a simple OP_NOTPIPELINED_VERY_SLOW?
  iclass_to_scarab_map[XED_ICLASS_VMOVNTDQ]  = {OP_MOV, -1, 1, NT};
  iclass_to_scarab_map[XED_ICLASS_VMOVNTDQA] = {OP_MOV, -1, 1, NT};
  iclass_to_scarab_map[XED_ICLASS_VMOVNTPD]  = {OP_MOV, 8, -1, NT};
  iclass_to_scarab_map[XED_ICLASS_VMOVNTPS]  = {OP_MOV, 4, -1, NT};
  iclass_to_scarab_map[XED_ICLASS_VMOVQ]     = {OP_MOV, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VMOVSD]    = {OP_MOV, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VMOVSHDUP] = {OP_MOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VMOVSLDUP] = {OP_MOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VMOVSS]    = {OP_MOV, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VMOVUPD]   = {OP_PIPELINED_FAST, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VMOVUPS]   = {OP_PIPELINED_FAST, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VMPSADBW]  = {OP_NOTPIPELINED_MEDIUM, 1, -1,
                                               NONE};
  iclass_to_scarab_map[XED_ICLASS_VMULPD]    = {OP_FMUL, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VMULPS]    = {OP_FMUL, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VMULSD]    = {OP_FMUL, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VMULSS]    = {OP_FMUL, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VORPD]     = {OP_LOGIC, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VORPS]     = {OP_LOGIC, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPABSB]    = {OP_LOGIC, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPABSD]    = {OP_LOGIC, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPABSQ]    = {OP_LOGIC, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPABSW]    = {OP_LOGIC, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPACKSSDW] = {OP_MOV, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPACKSSWB] = {OP_MOV, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPACKUSDW] = {OP_MOV, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPACKUSWB] = {OP_MOV, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPADDB]    = {OP_IADD, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPADDD]    = {OP_IADD, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPADDQ]    = {OP_IADD, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPADDSB]   = {OP_IADD, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPADDSW]   = {OP_IADD, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPADDUSB]  = {OP_IADD, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPADDUSW]  = {OP_IADD, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPADDW]    = {OP_IADD, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPALIGNR]  = {OP_SHIFT, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPAND]     = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPANDD]    = {OP_LOGIC, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPANDN]    = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPANDND]   = {OP_LOGIC, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPANDNQ]   = {OP_LOGIC, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPANDQ]    = {OP_LOGIC, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPAVGB]    = {OP_LOGIC, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPAVGW]    = {OP_LOGIC, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPBLENDD]  = {OP_CMOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPBLENDMB] = {OP_CMOV, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPBLENDMD] = {OP_CMOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPBLENDMQ] = {OP_CMOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPBLENDMW] = {OP_CMOV, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPBLENDVB] = {OP_CMOV, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPBLENDW]  = {OP_CMOV, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPBROADCASTB]    = {OP_MOV, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPBROADCASTD]    = {OP_MOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPBROADCASTMB2Q] = {OP_PIPELINED_MEDIUM, 8,
                                                      -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPBROADCASTMW2D] = {OP_PIPELINED_MEDIUM, 4,
                                                      -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPBROADCASTQ]    = {OP_MOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPBROADCASTW]    = {OP_MOV, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPCLMULQDQ]      = {OP_IMUL, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPCMPB]          = {OP_ICMP, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPCMPD]          = {OP_ICMP, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPCMPEQB]        = {OP_ICMP, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPCMPEQD]        = {OP_ICMP, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPCMPEQQ]        = {OP_ICMP, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPCMPEQW]        = {OP_ICMP, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPCMPESTRI] = {OP_NOTPIPELINED_SLOW, 1, -1,
                                                 NONE};
  iclass_to_scarab_map[XED_ICLASS_VPCMPESTRM] = {OP_NOTPIPELINED_SLOW, 1, -1,
                                                 NONE};
  iclass_to_scarab_map[XED_ICLASS_VPCMPGTB]   = {OP_ICMP, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPCMPGTD]   = {OP_ICMP, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPCMPGTQ]   = {OP_ICMP, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPCMPGTW]   = {OP_ICMP, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPCMPISTRI] = {OP_NOTPIPELINED_SLOW, 1, -1,
                                                 NONE};
  iclass_to_scarab_map[XED_ICLASS_VPCMPISTRM] = {OP_NOTPIPELINED_SLOW, 1, -1,
                                                 NONE};
  iclass_to_scarab_map[XED_ICLASS_VPCMPQ]     = {OP_ICMP, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPERM2F128] = {OP_MOV, 16, 1,
                                                 NONE};  // TODO: Move or shift?
  iclass_to_scarab_map[XED_ICLASS_VPERM2I128] = {OP_MOV, 16, 1,
                                                 NONE};  // TODO: Move or shift?
  iclass_to_scarab_map[XED_ICLASS_VPERMB]     = {OP_MOV, 1, -1,
                                             NONE};  // TODO: Move or shift?
  iclass_to_scarab_map[XED_ICLASS_VPERMD]     = {OP_MOV, 4, -1,
                                             NONE};  // TODO: Move or shift?
  iclass_to_scarab_map[XED_ICLASS_VPERMQ]     = {OP_MOV, 8, -1,
                                             NONE};  // TODO: Move or shift?
  iclass_to_scarab_map[XED_ICLASS_VPERMILPS]  = {OP_MOV, 8, -1,
                                                NONE};  // TODO: Move or shift?
  iclass_to_scarab_map[XED_ICLASS_VPERMILPD]  = {OP_MOV, 4, -1,
                                                NONE};  // TODO: Move or shift?
  iclass_to_scarab_map[XED_ICLASS_VPERMT2B]  = {OP_PIPELINED_FAST, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPERMT2D]  = {OP_PIPELINED_FAST, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPERMT2PD] = {OP_PIPELINED_FAST, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPERMT2PS] = {OP_PIPELINED_FAST, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPERMT2Q]  = {OP_PIPELINED_FAST, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPERMT2W]  = {OP_PIPELINED_FAST, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPERMW]    = {OP_MOV, 2, -1,
                                             NONE};  // TODO: Move or shift?
  iclass_to_scarab_map[XED_ICLASS_VPERMPD]   = {OP_PIPELINED_FAST, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPERMPS]   = {OP_PIPELINED_FAST, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPERMI2W]  = {OP_MOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPEXTRB]   = {OP_PIPELINED_FAST, 1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPEXTRD]   = {OP_PIPELINED_FAST, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPEXTRQ]   = {OP_PIPELINED_FAST, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPEXTRW]   = {OP_PIPELINED_FAST, 2, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPGATHERDD] = {OP_GATHER, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPGATHERDQ] = {OP_GATHER, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPGATHERQD] = {OP_GATHER, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPGATHERQQ] = {OP_GATHER, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPINSRB]    = {OP_MOV, 1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPINSRD]    = {OP_MOV, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPINSRQ]    = {OP_MOV, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPINSRW]    = {OP_MOV, 2, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMADDUBSW] = {OP_IMUL, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMADDWD]   = {OP_IMUL, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMASKMOVD] = {OP_PIPELINED_MEDIUM, 4, -1,
                                                 NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMASKMOVQ] = {OP_PIPELINED_MEDIUM, 8, -1,
                                                 NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMAXSB]    = {OP_ICMP, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMAXSD]    = {OP_ICMP, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMAXSQ]    = {OP_ICMP, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMAXSW]    = {OP_ICMP, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMAXUB]    = {OP_ICMP, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMAXUD]    = {OP_ICMP, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMAXUQ]    = {OP_ICMP, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMAXUW]    = {OP_ICMP, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMINSB]    = {OP_ICMP, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMINSD]    = {OP_ICMP, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMINSQ]    = {OP_ICMP, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMINSW]    = {OP_ICMP, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMINUB]    = {OP_ICMP, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMINUD]    = {OP_ICMP, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMINUQ]    = {OP_ICMP, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMINUW]    = {OP_ICMP, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVB2M]  = {OP_PIPELINED_FAST, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVD2M]  = {OP_PIPELINED_FAST, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVDB]   = {OP_CMOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVDW]   = {OP_CMOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVM2B]  = {OP_PIPELINED_FAST, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVM2D]  = {OP_PIPELINED_FAST, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVM2Q]  = {OP_PIPELINED_FAST, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVM2W]  = {OP_PIPELINED_FAST, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVMSKB] = {OP_PIPELINED_FAST, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVQ2M]  = {OP_PIPELINED_FAST, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVQB]   = {OP_CMOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVQD]   = {OP_CMOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVQW]   = {OP_CMOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVSDB]  = {OP_CMOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVSDW]  = {OP_CMOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVSQB]  = {OP_CMOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVSQD]  = {OP_CMOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVSQW]  = {OP_CMOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVSWB]  = {OP_CMOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVSXBD] = {OP_CMOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVSXBQ] = {OP_CMOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVSXBW] = {OP_CMOV, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVSXDQ] = {OP_CMOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVSXWD] = {OP_CMOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVSXWQ] = {OP_CMOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVUSDB] = {OP_CMOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVUSDW] = {OP_CMOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVUSQB] = {OP_CMOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVUSQD] = {OP_CMOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVUSQW] = {OP_CMOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVUSWB] = {OP_CMOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVW2M]  = {OP_PIPELINED_FAST, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVWB]   = {OP_CMOV, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVZXBD] = {OP_CMOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVZXBQ] = {OP_CMOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVZXBW] = {OP_CMOV, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVZXDQ] = {OP_CMOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVZXWD] = {OP_CMOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMOVZXWQ] = {OP_CMOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMULDQ]   = {OP_IMUL, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMULHRSW] = {OP_IMUL, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMULHUW]  = {OP_IMUL, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMULHW]   = {OP_IMUL, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMULLD] = {OP_PIPELINED_MEDIUM, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMULLQ] = {OP_PIPELINED_SLOW, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPMULLW] = {OP_IMUL, 2, -1, NONE};
  // each destination lane of VPMULUDQ is 8 bytes, even though each source lane
  // is only 4 bytes.
  iclass_to_scarab_map[XED_ICLASS_VPMULUDQ] = {OP_IMUL, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPOR]     = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPORD]    = {OP_LOGIC, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPORQ]    = {OP_LOGIC, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSADBW]  = {OP_PIPELINED_FAST, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSCATTERDD] = {OP_SCATTER, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSCATTERDQ] = {OP_SCATTER, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSCATTERQD] = {OP_SCATTER, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSCATTERQQ] = {OP_SCATTER, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSHUFB]     = {OP_MOV, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSHUFD]     = {OP_MOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSHUFHW]    = {OP_MOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSHUFLW]    = {OP_MOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSIGNB]     = {OP_LOGIC, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSLLD]      = {OP_SHIFT, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSLLDQ]     = {OP_SHIFT, 16, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSLLQ]      = {OP_SHIFT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSLLVD]     = {OP_SHIFT, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSLLVQ]     = {OP_SHIFT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSLLVW]     = {OP_SHIFT, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSLLW]      = {OP_SHIFT, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSRAD]      = {OP_SHIFT, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSRAQ]      = {OP_SHIFT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSRAVD]     = {OP_SHIFT, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSRAVQ]     = {OP_SHIFT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSRAVW]     = {OP_SHIFT, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSRAW]      = {OP_SHIFT, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSRLD]      = {OP_SHIFT, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSRLDQ]     = {OP_SHIFT, 16, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSRLQ]      = {OP_SHIFT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSRLVD]     = {OP_SHIFT, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSRLVQ]     = {OP_SHIFT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSRLVW]     = {OP_SHIFT, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSRLW]      = {OP_SHIFT, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSUBB]      = {OP_IADD, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSUBD]      = {OP_IADD, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSUBQ]      = {OP_IADD, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSUBSB]     = {OP_IADD, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSUBSW]     = {OP_IADD, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSUBUSB]    = {OP_IADD, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSUBUSW]    = {OP_IADD, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSUBW]      = {OP_IADD, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPTERNLOGD]  = {OP_LOGIC, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPTERNLOGQ]  = {OP_LOGIC, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPTEST]      = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPUNPCKHBW]  = {OP_MOV, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPUNPCKHDQ]  = {OP_MOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPUNPCKHQDQ] = {OP_MOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPUNPCKHWD]  = {OP_MOV, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPUNPCKLBW]  = {OP_MOV, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPUNPCKLDQ]  = {OP_MOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPUNPCKLQDQ] = {OP_MOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPUNPCKLWD]  = {OP_MOV, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPXOR]       = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPXORD]      = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPXORQ]      = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VRCP14PD]    = {OP_NOTPIPELINED_MEDIUM, 8, -1,
                                               NONE};
  iclass_to_scarab_map[XED_ICLASS_VRCP14PS]    = {OP_NOTPIPELINED_MEDIUM, 4, -1,
                                               NONE};
  iclass_to_scarab_map[XED_ICLASS_VRCP14SD]    = {OP_NOTPIPELINED_MEDIUM, 8, 1,
                                               NONE};
  iclass_to_scarab_map[XED_ICLASS_VRCP14SS]    = {OP_NOTPIPELINED_MEDIUM, 4, 1,
                                               NONE};
  iclass_to_scarab_map[XED_ICLASS_VRCP28PD]    = {OP_NOTPIPELINED_MEDIUM, 8, -1,
                                               NONE};
  iclass_to_scarab_map[XED_ICLASS_VRCP28PS]    = {OP_NOTPIPELINED_MEDIUM, 4, -1,
                                               NONE};
  iclass_to_scarab_map[XED_ICLASS_VRCP28SD]    = {OP_NOTPIPELINED_MEDIUM, 8, 1,
                                               NONE};
  iclass_to_scarab_map[XED_ICLASS_VRCP28SS]    = {OP_NOTPIPELINED_MEDIUM, 4, 1,
                                               NONE};
  iclass_to_scarab_map[XED_ICLASS_VRCPPS]      = {OP_NOTPIPELINED_MEDIUM, 4, -1,
                                             NONE};
  iclass_to_scarab_map[XED_ICLASS_VRCPSS]      = {OP_NOTPIPELINED_MEDIUM, 4, 1,
                                             NONE};
  iclass_to_scarab_map[XED_ICLASS_VREDUCEPD]   = {OP_FCVT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VREDUCEPS]   = {OP_FCVT, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VREDUCESD]   = {OP_FCVT, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VREDUCESS]   = {OP_FCVT, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VRNDSCALEPD] = {OP_FCVT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VRNDSCALEPS] = {OP_FCVT, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VRNDSCALESD] = {OP_FCVT, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VRNDSCALESS] = {OP_FCVT, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VROUNDPD]    = {OP_FCVT, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VROUNDPS]    = {OP_FCVT, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VROUNDSD]    = {OP_FCVT, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VROUNDSS]    = {OP_FCVT, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VRSQRT14PD]  = {OP_PIPELINED_MEDIUM, 8, -1,
                                                 NONE};
  iclass_to_scarab_map[XED_ICLASS_VRSQRT14PS]  = {OP_PIPELINED_MEDIUM, 4, -1,
                                                 NONE};
  iclass_to_scarab_map[XED_ICLASS_VRSQRT14SD]  = {OP_PIPELINED_MEDIUM, 8, 1,
                                                 NONE};
  iclass_to_scarab_map[XED_ICLASS_VRSQRT14SS]  = {OP_PIPELINED_MEDIUM, 4, 1,
                                                 NONE};
  iclass_to_scarab_map[XED_ICLASS_VRSQRT28PD]  = {OP_PIPELINED_MEDIUM, 8, -1,
                                                 NONE};
  iclass_to_scarab_map[XED_ICLASS_VRSQRT28PS]  = {OP_PIPELINED_MEDIUM, 4, -1,
                                                 NONE};
  iclass_to_scarab_map[XED_ICLASS_VRSQRT28SD]  = {OP_PIPELINED_MEDIUM, 8, 1,
                                                 NONE};
  iclass_to_scarab_map[XED_ICLASS_VRSQRT28SS]  = {OP_PIPELINED_MEDIUM, 4, 1,
                                                 NONE};
  iclass_to_scarab_map[XED_ICLASS_VRSQRTPS]    = {OP_PIPELINED_MEDIUM, 4, -1,
                                               NONE};
  iclass_to_scarab_map[XED_ICLASS_VRSQRTSS] = {OP_PIPELINED_MEDIUM, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VSCATTERDPD]    = {OP_SCATTER, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VSCATTERDPS]    = {OP_SCATTER, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VSCATTERPF0DPD] = {OP_SCATTER, 4, 16, T0};
  iclass_to_scarab_map[XED_ICLASS_VSCATTERPF0DPS] = {OP_SCATTER, 4, 16, T0};
  iclass_to_scarab_map[XED_ICLASS_VSCATTERPF0QPD] = {OP_SCATTER, 8, 8, T0};
  iclass_to_scarab_map[XED_ICLASS_VSCATTERPF0QPS] = {OP_SCATTER, 8, 8, T0};
  iclass_to_scarab_map[XED_ICLASS_VSCATTERPF1DPD] = {OP_SCATTER, 4, 16, T1};
  iclass_to_scarab_map[XED_ICLASS_VSCATTERPF1DPS] = {OP_SCATTER, 4, 16, T1};
  iclass_to_scarab_map[XED_ICLASS_VSCATTERPF1QPD] = {OP_SCATTER, 8, 8, T1};
  iclass_to_scarab_map[XED_ICLASS_VSCATTERPF1QPS] = {OP_SCATTER, 8, 8, T1};
  iclass_to_scarab_map[XED_ICLASS_VSCATTERQPD]    = {OP_SCATTER, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VSCATTERQPS]    = {OP_SCATTER, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VSHUFPD]        = {OP_MOV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VSHUFPS]        = {OP_MOV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VSHUFI32X4]     = {OP_MOV, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VSQRTPD]        = {OP_FDIV, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VSQRTPS]        = {OP_FDIV, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VSQRTSD]        = {OP_FDIV, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VSQRTSS]        = {OP_FDIV, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VSTMXCSR]   = {OP_NOTPIPELINED_MEDIUM, -1, 1,
                                               NONE};
  iclass_to_scarab_map[XED_ICLASS_VSUBPD]     = {OP_FADD, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VSUBPS]     = {OP_FADD, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VSUBSD]     = {OP_FADD, 8, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VSUBSS]     = {OP_FADD, 4, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VUCOMISD]   = {OP_FCMP, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VUCOMISS]   = {OP_FCMP, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VUNPCKHPD]  = {OP_MOV, -1, 8, NONE};
  iclass_to_scarab_map[XED_ICLASS_VUNPCKHPS]  = {OP_MOV, -1, 4, NONE};
  iclass_to_scarab_map[XED_ICLASS_VUNPCKLPD]  = {OP_MOV, -1, 8, NONE};
  iclass_to_scarab_map[XED_ICLASS_VUNPCKLPS]  = {OP_MOV, -1, 4, NONE};
  iclass_to_scarab_map[XED_ICLASS_VXORPD]     = {OP_LOGIC, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VXORPS]     = {OP_LOGIC, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VZEROALL]   = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VZEROUPPER] = {OP_LOGIC, 16, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_WRMSR]      = {OP_MOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_XADD]       = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_XADD_LOCK]  = {OP_IADD, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_XCHG]       = {OP_MOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_XGETBV] = {OP_NOTPIPELINED_SLOW, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_XOR]    = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_XORPD]  = {OP_LOGIC, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_XORPS]  = {OP_LOGIC, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_XOR_LOCK] = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_XRSTOR]   = {OP_NOTPIPELINED_VERY_SLOW, -1, 1,
                                             NONE};
  iclass_to_scarab_map[XED_ICLASS_XSAVE]    = {OP_NOTPIPELINED_VERY_SLOW, -1, 1,
                                            NONE};
  iclass_to_scarab_map[XED_ICLASS_XSAVE64]    = {OP_NOTPIPELINED_VERY_SLOW, -1, 1,
                                            NONE};
  iclass_to_scarab_map[XED_ICLASS_XSAVEC]   = {OP_NOTPIPELINED_VERY_SLOW, -1, 1,
                                             NONE};
  iclass_to_scarab_map[XED_ICLASS_BZHI] = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPCMPUB] = {OP_LOGIC, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPTESTMB] = {OP_LOGIC, 1, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_RDSEED] = {OP_NOTPIPELINED_VERY_SLOW, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_KORTESTD] = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_KORD] = {OP_LOGIC, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPSIGNW] = {OP_CMOV, 2, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPHADDD] = {OP_IADD, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPDPBUSDS] = {OP_PIPELINED_SLOW, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VHADDPS] = {OP_FADD, 4, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VLDDQU] = {OP_MOV, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VPHMINPOSUW] = {OP_PIPELINED_SLOW, 2, 8, NONE};
  iclass_to_scarab_map[XED_ICLASS_PDEP] = {OP_NOTPIPELINED_SLOW, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_VHADDPD] = {OP_FADD, 8, -1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PEXT] = {OP_NOTPIPELINED_SLOW, -1, 1, NONE};
  iclass_to_scarab_map[XED_ICLASS_PACKSSWB] = {OP_MOV, 1, -1, NONE};
}
