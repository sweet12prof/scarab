#include <variant>
#include <vector>
#include "globals/global_types.h"
#include "ctype_pin_inst.h"

//There is a chance for polymoorphism since the interfaces are the same in each class. std::variant is a
//lightweight option in that case
//Otherwise this is fine. Also any new distribution can follow this format
class Monotonic_Addr64_Distribution { 
  uns64 nextElementPos;
  uns64 distLength;
  std::vector<uns64> distribution;

 public:
  void init_distribution();
  uns64 get_next_element_in_distribution();
};

class Bool_Distribution {  // Can be in its own file
  uns64 nextElementPos;
  uns64 distLength;
  std::vector<uns64> distribution;

 public:
  void init_distribution();
  uns64 get_next_element_in_distribution();
};

template <typename T, uns length>  // Can be in its own file, interestingly we can have generics too like this one
class Generic_Random_Distribution {
  uns64 nextElementPos;
  uns64 distLength;
  std::vector<T> distribution(length);

 public:
  void init_distribution();
  uns64 get_next_element_in_distribution();
};


//------------------ Very basic One Line Instruction generators can be defined in a single file
//ld  [x1] x1
ctype_pin_inst generate_loop_carried_dependence_load(uns64 pc, uns64 uid, uns64 vaddr, uns8 inst_size);
// ld [x2] x1
ctype_pin_inst generate_independent_operand_load(uns64 pc, uns64 uid, uns64 vaddr, uns8 inst_size);
// basic branches
ctype_pin_inst generate_conditional_branch(uns64 pc, uns64 uid, uns64 branchtgt, bool branchdirection, uns8 inst_size);
ctype_pin_inst generate_unconditional_branch(uns64 pc, uns64 uid, uns64 branchtgt, uns8 inst_size);
ctype_pin_inst generate_indirect_branch(uns64 pc, uns64 uid, uns64 branchtgt, uns64 indirect_vaddr, uns8 inst_size);
//nop
ctype_pin_inst generate_nop(uns64 pc, uns64 uid, uns8 inst_size);


// Workloads can be generated using a combination of basic one line instructions and distributions, an example is below
std::vector<ctype_pin_inst> generate_bp_limited_workload() {
  std::vector<ctype_pin_inst> workload;
  Bool_Distribution branch_directions;
  Monotonic_Addr64_Distribution targetAddresses;
  branch_directions.init_distribution();
  targetAddresses.init_distribution();
  uns64 workloadLength{2000};
  uns64 length_counter{0};
  uns64 instr_count{0};
  uns64 pc{0}, uid{0};
  uns nop_size{3}, branch_size{3};

  while (length_counter != workloadLength) {
    if (instr_count != (uns64)ISSUE_WIDTH - 1) {
      // this would generate  leading nops
      workload.push_back(generate_nop(pc, uid, nop_size));
      instr_count++;
      pc += nop_size;
    } else {
        auto tgt_addr{targetAddresses.get_next_element_in_distribution()};
        // this would generate branch at the end of every issue packet
        workload.push_back(generate_conditional_branch(pc, uid, tgt_addr, (bool)branch_directions.get_next_element_in_distribution(), branch_size));
        instr_count = 0;
        pc = tgt_addr;
    }
    length_counter++;
  }
  // final instruction brings the pc back to the beginning
  workload.push_back(generate_unconditional_branch(pc, uid++, 0, branch_size));
  return workload;
}