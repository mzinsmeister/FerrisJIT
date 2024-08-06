use std::collections::BTreeMap;

use super::{get_data_type_size, ir::DataType};


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

pub(super) struct MemoryManagement {
    pub values: Vec<CGValue>,
    free_slots: Vec<usize>,
    free_stack_pos: BTreeMap<usize, Vec<usize>>,
    stack_ptr: usize,
    stack_size: usize,
}

impl MemoryManagement {
    pub fn new(arg_types: &[DataType]) -> Self {
        let values = arg_types.iter().enumerate()
            .map(|(i, dt)| CGValue::Variable{ data_type: *dt, stack_pos: i * 8, readonly: true})
            .collect();
        Self {
            values,
            free_slots: Vec::new(),
            free_stack_pos: BTreeMap::new(),
            stack_ptr: arg_types.len() * 8,
            stack_size: arg_types.len() * 8,
        }
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

            }
            CGValue::Free => {/* TODO: Make sure double frees cannot happen, even internally */},
        }
    }
}