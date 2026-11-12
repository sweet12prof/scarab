#include "kernel_dispatcher.h"

#include <cassert>

ctype_pin_inst Kernel_Dispatcher::synthetic_fe_generate_next(uns proc_id, bool offpath) {
  switch (kernel) {
    case MEM_FU_LIMITED:
    case LOAD_LATENCY_LIMITED:
    case BTB_LIMITED:
    case ICACHE_LIMITED:
    case ILP_LIMITED:
      return get_next_kernel_inst();
    case CBR_LIMITED:
      return get_next_cbr_kernel_type_inst(proc_id, offpath);

    case IBR_LIMITED: {
      if (TARGET_SAMPLING_STRATEGY == UNIFORM_SEQUENTIAL)
        return get_next_kernel_inst();
      else
        return get_next_ibr_kernel_type_inst(proc_id, offpath);
    }

    default:
      assert(0 && "Invalid Kernel");
  }
}

// dispatches next instruction to be executed the kernel
ctype_pin_inst Kernel_Dispatcher::get_next_kernel_inst() {
  auto inst_pc = get_next_pc();
  auto it = kernel_map.find(inst_pc);

  if (it == kernel_map.end()) {
    for (auto item : kernel_map) {
      std::string op_name;
      switch (item.second.op_type) {
        case OP_CF: {
          if (item.second.cf_type == CF_CBR)
            op_name = " cbr";
          else if (item.second.cf_type == CF_BR)
            op_name = " br";
        } break;

        case OP_NOP:
          op_name = "nop";
          break;

        default:
          op_name = "inv";
          break;
      }
      std::cout << " pc: " << item.first << " op: " << op_name << " op_size " << (uint32_t)item.second.size
                << std::endl;
    }
    std::cout << "Looking for pc: " << inst_pc << std::endl;
  }
  assert(it != kernel_map.end() && "Every inst possible should be in the map");

  auto inst = it->second;
  next_pc = inst.instruction_next_addr;
  return inst;
}

// dipatches next cbr op and rerandomizes the cbr directions if we are at the end of kernel
ctype_pin_inst Kernel_Dispatcher::get_next_cbr_kernel_type_inst(uns proc_id, bool offpath) {
  auto inst = Kernel_Dispatcher::get_next_kernel_inst();
  /* if we execute the workload length or return via the unconditional branch path  */
  if (!offpath && (inst.op_type == OP_CF && inst.cf_type == CF_CBR)) {
    num_of_ops_executed++;

    if (num_of_ops_executed == factory.get_target_pool_size()) {
      num_of_ops_executed = 0;
      factory.reset_target_pool_pointer();
      kernel_map = factory.generate_kernel();
    }
  }
  return inst;
}

// dispatches next ibr op and rerandomizes the ibr targets if we are at the end of random targets kernel
ctype_pin_inst Kernel_Dispatcher::get_next_ibr_kernel_type_inst(uns proc_id, bool offpath) {
  auto inst = get_next_kernel_inst();
  if (!offpath && (inst.op_type == OP_CF && (inst.cf_type == CF_IBR))) {
    num_of_ops_executed++;
    // rerandomize the target pool if we reach the end
    if (num_of_ops_executed == TARGET_POOL_SIZE - 1) {
      num_of_ops_executed = 0;
      factory.regenerate_target_pool();
      factory.reset_target_pool_pointer();
    }
    // for the random ibr kernel, this moves the pointer to the next indirect branch target
    kernel_map = factory.generate_kernel();
  }
  return inst;
}

Kernel_Dispatcher::Kernel_Dispatcher(Kernel_Enum kernel_, uns64 start_pc, uns64 start_uid, uns64 workload_length)
    : factory(kernel_, start_pc, start_uid, workload_length),
      kernel(kernel_),
      kernel_map(factory.generate_kernel()),
      next_pc(start_pc),
      num_of_ops_executed{0} {
}
