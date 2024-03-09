use std::collections::BTreeMap;

use super::{copy_patch::CopyPatchBackend, get_data_type_size, ir::{ConstValue, DataType}};


#[derive(Debug, Clone)]
pub enum CGValue {
    Variable{
        // This type is used for knowing how much data to load or store from/to the stack
        // It is not! used for deciding what kind of operation (what operand size, signed,...) to use
        // TODO: We should probably separate datatypes with operation semantics and size types 
        //       (e.g. Data8 for double, i64 and u64) or just have a size field
        data_type: DataType, 
        stack_pos: usize,
        readonly: bool,
    },
    Free
}

impl CGValue {
    pub fn get_type(&self) -> DataType {
        match self {
            CGValue::Variable{data_type,..} => data_type.clone(),
            _ => panic!("Tried to get the type of a free CGValue")
        }
    }
}

pub(super) struct MemoryManagement<'ctx> {
    args_size: usize,
    pub values: Vec<CGValue>,
    free_slots: Vec<usize>,
    free_stack_pos: BTreeMap<usize, Vec<usize>>,
    // We save whether a register is potentially dirty
    // one value can only be in one register at a time unless it's a readonly/const value
    // as soon as a readonly/const value is dirtied it becomes a different mutable variable
    pub reg_state: [Option<(usize, bool)>; 2], 
    stack_ptr: usize, // TODO: Use actual byte sizes. For now we just use 8 bytes for everything
    pub stack_size: usize,
    cp_backend: &'ctx CopyPatchBackend,
}

impl<'ctx> MemoryManagement<'ctx> {
    pub fn new(cp_backend: &'ctx CopyPatchBackend, arg_types: &[DataType]) -> Self {
        let values = arg_types.iter().enumerate()
            .map(|(i, dt)| CGValue::Variable{ data_type: *dt, stack_pos: i * 8, readonly: true})
            .collect();
        Self {
            args_size: arg_types.len(),
            values,
            free_slots: Vec::new(),
            free_stack_pos: BTreeMap::new(),
            reg_state: [None, None],
            stack_ptr: arg_types.len() * 8,
            stack_size: arg_types.len() * 8,
            cp_backend
        }
    }

    #[allow(dead_code)]
    pub fn reset(&mut self) {
        self.reg_state = [None, None];
        self.values.clear();
        self.free_slots.clear();
        self.stack_ptr = self.args_size * 8;
        self.stack_size = self.args_size * 8;
    }

    pub fn alloc_stack(&mut self, size: usize) -> usize {
        if let Some(next) = self.free_stack_pos.get_mut(&size) {
            let pos = next.pop().unwrap();
            if next.is_empty() {
                self.free_stack_pos.remove(&size);
            }
            pos
        } else {
            let pos = self.stack_ptr;
            self.stack_ptr += size;
            self.stack_size = self.stack_size.max(self.stack_ptr);
            pos
        }
    }

    #[allow(dead_code)]
    pub fn alloc_stack_at_least(&mut self, size: usize) -> usize {
        if let Some((&next_size, next)) = self.free_stack_pos.range_mut(size..).next() {
            let pos = next.pop().unwrap();
            if next.is_empty() {
                self.free_stack_pos.remove(&next_size);
            }
            pos
        } else {
            let pos = self.stack_ptr;
            self.stack_ptr += size;
            self.stack_size = self.stack_size.max(self.stack_ptr);
            pos
        }
    }

    #[allow(dead_code)]
    pub fn free_stack(&mut self, pos: usize, size: usize) {
        self.free_stack_pos.entry(size).or_default().push(pos);
    }

    /// Frees a register, saving its content to the stack if necessary
    pub fn free_reg(&mut self, reg: usize) {
        if let Some((i, dirty)) = &mut self.reg_state[reg] {
            if *dirty {
                let cur_value = &self.values[*i];
                match cur_value {
                    CGValue::Variable{readonly, stack_pos,..} => {
                        if *readonly {
                            panic!("We should have allocated a stack slot before dirtying a readonly value");
                        }
                        // Save it to its designated stack location
                        match reg {
                            0 => self.cp_backend.emit_put_1_stack(*stack_pos),
                            1 => self.cp_backend.emit_put_2_stack(*stack_pos),
                            _ => unreachable!(),
                        }
                        self.reg_state[reg].as_mut().unwrap().1 = false;
                    },
                    CGValue::Free => { /* User doesn't need this anymore so we can just throw it away without writing it back. */},
                }
            }
        }
    }

    pub fn flush_regs(&mut self) {
        for reg in 0..2 {
            self.lose_reg(reg);
        }
    }

    /// Put a single value into a single register. Use "put_in_regs" if you want to set both registers
    /// This might change the state of the other register!
    pub fn put_in_reg(&mut self, reg: usize, v: usize) {
                // Do we already have the correct value?
        if let Some((i, _)) = &self.reg_state[reg] {
            if *i == v {
                return;
            }
        }
        let value = &self.values[v];
        // Is our value in the other register? if so we can just swap the registers
        let other_reg_n = (reg + 1) % 2;
        let other_reg_state = self.reg_state[other_reg_n].clone();
        if let Some((i, _)) = other_reg_state {
            if i == v {
                if let CGValue::Variable{readonly, ..} = &value {
                    if *readonly {
                        // We can just duplicate the value
                        self.free_reg(reg);
                        match other_reg_n {
                            0 => self.cp_backend.emit_duplex1(),
                            1 => self.cp_backend.emit_duplex2(),
                            _ => unreachable!(),
                        }
                        self.reg_state[reg] = self.reg_state[other_reg_n].clone();
                        return;
                    }
                }
                // We just swap the registers so that we don't have to move anything
                self.cp_backend.emit_swap12();
                self.reg_state.swap(0, 1);
                return;
            }
        }
        // We have to go to memory, therefore spill the current value if necessary
        self.free_reg(reg);
        let value: &CGValue = &self.values[v];
        // Load the value to the register
        match value {
            CGValue::Variable{stack_pos, ..} => {
                // TODO: do take stack with datatype
                match reg {
                    0 => self.cp_backend.emit_take_1_stack(*stack_pos),
                    1 => self.cp_backend.emit_take_2_stack(*stack_pos),
                    _ => unreachable!(),
                }
            },
            CGValue::Free => unreachable!("We shouldn't even be able to have a reference to a free value"),
        }
        self.reg_state[reg] = Some((v, false));
    }

    // Get two values into registers in the most efficient way possible
    pub fn put_in_regs(&mut self, reg0_v: usize, reg1_v: usize) {
        let v0_reg_n = self.reg_state.iter()
            .enumerate()
            .find(|(_, r)| r.as_ref().map(|r| r.0 == reg0_v).unwrap_or(false))
            .map(|(i, _)| i);
        let v1_reg_n = self.reg_state.iter()
            .enumerate()
            .find(|(_, r)| r.as_ref().map(|r| r.0 == reg1_v).unwrap_or(false))
            .map(|(i, _)| i);

        // Depending on the different constellations we might have here we can do different things
        // Both values are already in the correct (!) registers
        if v0_reg_n == Some(0) && v1_reg_n == Some(1) {
            return;
        }
        // Both want the same value and it already is in a register
        if v0_reg_n.is_some() && v0_reg_n == v1_reg_n {
            // just duplicate the value
            if let Some(0) = v0_reg_n {
                self.free_reg(1);
                self.cp_backend.emit_duplex1();
                self.reg_state[1] = self.reg_state[0].clone();
            } else {
                self.free_reg(0);
                self.cp_backend.emit_duplex2();
                self.reg_state[0] = self.reg_state[1].clone();
            }
            return;
        }

        // registers are swapped
        if matches!(v0_reg_n, Some(1)) && matches!(v1_reg_n, Some(0)) {
            self.cp_backend.emit_swap12();
            self.reg_state.swap(0, 1);
            return;
        }

        if let Some(0) = v1_reg_n {
            // v2 is in register 0
            // Make sure to get that value into register 1 first
            self.put_in_reg(1, reg1_v);
            self.put_in_reg(0, reg0_v);
        } else {
            // In any other case this will be the most efficient way
            // Either v1 is in register 1 or we have to go to memory for both anyway
            self.put_in_reg(0, reg0_v);
            self.put_in_reg(1, reg1_v);
        }
    }

    pub fn dirty_reg(&mut self, reg: usize) {
        let reg_state = self.reg_state[reg].clone();
        if let Some((i, _)) = reg_state {
            // Check whether the value is readonly and if it is we allocate a new value
            self.reg_state[reg].as_mut().unwrap().1 = true;
            match &self.values[i] {
                CGValue::Variable{readonly,..} => {
                    if *readonly {
                        panic!("tried to dirty a readonly value");
                    }
                },
                _ => unreachable!(),
            }
        } else {
            panic!("tried to dirty a register that is not in use");
        }
    }

    pub fn allocate_stack(&mut self, data_type: DataType) -> usize {
        let stack_pos = self.alloc_stack(get_data_type_size(&data_type));
        if let Some(i) = self.free_slots.pop() {
            self.values[i] = CGValue::Variable{ data_type, stack_pos, readonly: false};
            i
        } else {
            let i = self.values.len();
            self.values.push(CGValue::Variable{ data_type, stack_pos, readonly: false});
            i
        }
    }

    /// Clones the value and returns it. Use the new value first if possible.
    pub fn clone_value(&mut self, v: usize) -> usize {
        let value = self.values[v].clone();
        match value {
            CGValue::Variable{data_type,..} => {
                self.free_reg(0);
                self.put_in_reg(0, v);
                let new_i = self.allocate_stack(data_type);
                self.reg_state[0] = Some((new_i, true));
                new_i
            },
            CGValue::Free => unreachable!("We shouldn't even be able to have a reference to a free value"),
        }
    }

    pub fn free_value(&mut self, v: usize) {
        let value = &self.values[v];
        match value {
            CGValue::Variable{readonly, stack_pos, data_type} => {
                if *readonly {
                    return;
                }
                self.free_slots.push(v);
                self.free_stack(*stack_pos, get_data_type_size(data_type));
                self.values[v] = CGValue::Free;
                // Check reg slots and free them if necessary
                for reg in self.reg_state.iter_mut() {
                    if let Some((i, _)) = reg {
                        if *i == v {
                            *reg = None;
                        }
                    }
                }
            }
            CGValue::Free => {/* TODO: Make sure double frees cannot happen, even internally */},
        }
    }

    pub fn lose_reg(&mut self, reg: usize) {
        self.free_reg(reg);
        self.reg_state[reg] = None;
    }

    pub fn init(&mut self, i: usize, c: ConstValue) {
        self.free_reg(0);
        self.cp_backend.emit_take_1_const(c);
        self.reg_state[0] = Some((i, true));
    }
}