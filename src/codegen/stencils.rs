use inkwell::builder::Builder;

use inkwell::context::Context;

use inkwell::memory_buffer::MemoryBuffer;
use inkwell::module::{Linkage, Module};
use inkwell::passes::PassBuilderOptions;
use inkwell::targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine};

use inkwell::types::{BasicMetadataTypeEnum, BasicType, IntType};
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue, PointerValue};
use inkwell::{AddressSpace, OptimizationLevel};


use std::cell::Cell;
use std::collections::BTreeMap;
use std::fmt::{Display, Formatter};

use super::ir::DataType;

// For reference (if you look into the Disassembly). 
// Order of Registers in argument passing of the GHC-CC: R13, RBP, R12, RBX, R14, RSI, RDI, R8, R9, R15

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum StencilOperation {

    // Arithmetic operations
    Add,
    AddConst,
    Sub,
    SubConst,
    Mul,
    MulConst,
    Div,
    DivConst,
    Rem,
    RemConst,

    // Bit-operations
    And,
    AndConst,
    Or,
    OrConst,
    Xor,
    XorConst,
    Shl,
    ShlConst,
    Shr,
    ShrConst,
    Not,

    // Comparison operations
    Eq,
    EqConst,
    Ne,
    NeConst,
    Gt,
    GtConst,
    Ge,
    GeConst,
    Lt,
    LtConst,
    Le,
    LeConst,

    // Control flow operations (If my plan works this should be the only one necessary)
    CondBr,
    UncondBr,

    // These are the technical ones
    Take,
    Take1,
    Take2,
    TakeConst,
    Ret,
    Duplex,
    Put,
    GetStackPtr,
    Load,
    Store,

    // This is a special one that is used to wrap a call to a C(/Rust) function
    CallCFunction,
    // This is a special one that is used to wrap the generated GHC-CC code
    GhcWrapper
}

impl Display for StencilOperation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            StencilOperation::Add => write!(f, "add"),
            StencilOperation::AddConst => write!(f, "add-const"),
            StencilOperation::Sub => write!(f, "sub"),
            StencilOperation::SubConst => write!(f, "sub-const"),
            StencilOperation::Mul => write!(f, "mul"),
            StencilOperation::MulConst => write!(f, "mul-const"),
            StencilOperation::Div => write!(f, "div"),
            StencilOperation::DivConst => write!(f, "div-const"),
            StencilOperation::Rem => write!(f, "rem"),
            StencilOperation::RemConst => write!(f, "rem-const"),
            StencilOperation::And => write!(f, "and"),
            StencilOperation::AndConst => write!(f, "and-const"),
            StencilOperation::Or => write!(f, "or"),
            StencilOperation::OrConst => write!(f, "or-const"),
            StencilOperation::Xor => write!(f, "xor"),
            StencilOperation::XorConst => write!(f, "xor-const"),
            StencilOperation::Shl => write!(f, "shl"),
            StencilOperation::ShlConst => write!(f, "shl-const"),
            StencilOperation::Shr => write!(f, "shr"),
            StencilOperation::ShrConst => write!(f, "shr-const"),
            StencilOperation::Not => write!(f, "not"),
            StencilOperation::Eq => write!(f, "eq"),
            StencilOperation::EqConst => write!(f, "eq-const"),
            StencilOperation::Ne => write!(f, "ne"),
            StencilOperation::NeConst => write!(f, "ne-const"),
            StencilOperation::Gt => write!(f, "ugt"),
            StencilOperation::GtConst => write!(f, "ugt-const"),
            StencilOperation::Ge => write!(f, "uge"),
            StencilOperation::GeConst => write!(f, "uge-const"),
            StencilOperation::Lt => write!(f, "ult"),
            StencilOperation::LtConst => write!(f, "ult-const"),
            StencilOperation::Le => write!(f, "ule"),
            StencilOperation::LeConst => write!(f, "ule-const"),
            StencilOperation::CondBr => write!(f, "cond-br"),
            StencilOperation::UncondBr => write!(f, "uncond-br"),
            StencilOperation::Take => write!(f, "take"),
            StencilOperation::TakeConst => write!(f, "take-const"),
            StencilOperation::Take1 => write!(f, "take1"),
            StencilOperation::Take2 => write!(f, "take2"),
            StencilOperation::Load => write!(f, "load"),
            StencilOperation::Store => write!(f, "store"),
            StencilOperation::Ret => write!(f, "ret"),
            StencilOperation::Duplex => write!(f, "duplex"),
            StencilOperation::Put => write!(f, "put"),
            StencilOperation::GetStackPtr => write!(f, "get-stack-ptr"),
            StencilOperation::CallCFunction => write!(f, "c-func-call"),
            StencilOperation::GhcWrapper => write!(f, "__GHC_CC-CONVERTER__")
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct StencilType {
    pub operation: StencilOperation,
    pub data_type: Option<DataType>
}

impl StencilType {
    pub fn new(operation: StencilOperation, data_type: Option<DataType>) -> Self {
        Self {
            operation,
            data_type
        }
    }
}

impl Display for StencilType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(ty) = &self.data_type {
            write!(f, "{}_{}", ty, self.operation)
        } else {
            write!(f, "{}", self.operation)
        }
    }
}

#[derive(Debug, Clone)]
pub struct Stencil {
    pub s_type: StencilType,
    pub code: Vec<u8>,
    pub holes: Vec<usize>,
    pub tail_holes: Vec<usize>,
    pub large: bool
}

impl Display for Stencil {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.s_type)
    }

}

#[allow(dead_code)]
impl Stencil {
    pub fn code_without_jump(&self) -> &[u8] {
        if self.large {
            &self.code[..self.code.len() - 2]
        } else {
            &self.code[..self.code.len() - 5]
        }
    }

    pub fn get_holes_bytelen(&self) -> usize {
        if self.large {
            8
        } else {
            4
        }
    }

    pub fn get_tailcall_bytelen(&self) -> usize {
        if self.large {
            2
        } else {
            5
        }
    
    }
}

fn get_stencil(s_type: StencilType, elf: &[u8], cut_jmp: bool, large: bool) -> Stencil {
    let gobj = goblin::elf::Elf::parse(elf).unwrap();

    //println!("---------------------------");

    //println!("{:#?}", gobj);

    //println!("-----------------------------");

    // Print relocation record for "PH" symbols

    // TODO: Use this for finding the FPH-0 and FPH-1 function pointer relocs also

    let reltab = gobj.shdr_relocs;
    let strtab = gobj.strtab;
    let symtab = gobj.syms;

    let mut relocs = Vec::new();
    let mut tail_holes = Vec::new();

    for rel in reltab {
        for reloc in rel.1.into_iter() {
            let sym = symtab.get(reloc.r_sym).unwrap();
            let name = &strtab.get_at(sym.st_name);
            if let Some(name) = name {
                if name.starts_with("PH") {
                    //println!("Relocation: {:?}: {:#?}", name, reloc);
                    relocs.push((name.to_string(), reloc));
                }
                if name == &"TAIL" {
                    tail_holes.push(reloc.r_offset as usize);
                }
            }
        }
    }
    // get the binary code of the "sum" function as a byte array from the ELF file

    let mut code = None;
    
    // get .text section offset and size
    for section in gobj.section_headers {
        let name = &strtab.get_at(section.sh_name);
        if let Some(name) = name {
            if name == &".text" {
                let offset = section.sh_offset as usize;
                let size = section.sh_size as usize;
                // print code as hex bytes
                for _i in 0..size {
                    //print!("{:02x} ", res.as_slice()[offset + i]);
                    code = Some(&elf[offset..offset + size]);
                }
            }
        }
    }

    relocs.sort_by(|a, b| a.0.cmp(&b.0));
    tail_holes.sort();

    let holes = relocs.iter().map(|(_, reloc)| reloc.r_offset as usize).collect();

    let mut code = code.unwrap().to_vec();

    if cut_jmp {
        let truncate = if large { 
            // Cut out the last tail hole plus the two bytes before it
            let last_tail_hole = tail_holes.pop().unwrap();
            // Remove the 10 byte range from the code
            code.drain(last_tail_hole - 2..last_tail_hole + 8);
            2
        } else { 5 }; 
        code.truncate(code.len() - truncate);
    }

    Stencil {
        code,
        s_type,
        holes: holes,
        tail_holes: tail_holes,
        large: large
    }
}

struct StencilCodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    ph_counter: Cell<usize>,
    large_ph: Cell<bool>
}

#[allow(dead_code)]
impl<'ctx> StencilCodeGen<'ctx> {

    fn new(context: &'ctx Context) -> Self {
        let module = context.create_module("stencil");
        let builder = context.create_builder();
        Self {
            context,
            module,
            builder,
            ph_counter: Cell::new(1),
            large_ph: Cell::new(false)
        }
    }

    fn init_placeholder(&self, ty: IntType<'ctx>) -> IntValue<'ctx> {
        let i8_array_type = self.context.i8_type().array_type(1048576);

        let global = self.module.add_global(i8_array_type, Some(AddressSpace::default()), format!("PH{}", self.ph_counter.get()).as_str());
        global.set_linkage(Linkage::External);
        global.set_alignment(1);

        self.ph_counter.set(self.ph_counter.get() + 1);

        if ty.get_bit_width() > 4 {
            self.large_ph.set(true);
        }

        let ptr = global.as_pointer_value();

        self.builder.build_ptr_to_int(ptr, ty, "ptrtoint").unwrap()
    }

    fn init_fn_placeholder(&self, args: &[BasicMetadataTypeEnum<'ctx>]) -> FunctionValue {
        let void_type = self.context.void_type();
        let fn_type = void_type.fn_type(args, false);
        let function = self.module.add_function(format!("PH{}", self.ph_counter.get()).as_str(), fn_type, Some(Linkage::External));
        function.set_call_conventions(inkwell::llvm_sys::LLVMCallConv::LLVMGHCCallConv as u32);
        self.ph_counter.set(self.ph_counter.get() + 1);
        self.large_ph.set(true);
        function
    }

    fn get_tailcall_placeholder(&self, args: &[BasicMetadataTypeEnum<'ctx>]) -> FunctionValue {
        let void_type = self.context.void_type();
        let fn_type = void_type.fn_type(args, false);
        let function = self.module.add_function("TAIL", fn_type, Some(Linkage::External));
        function.set_call_conventions(inkwell::llvm_sys::LLVMCallConv::LLVMGHCCallConv as u32);
        function
    }

    fn compile(&self, large: bool) -> MemoryBuffer {
        // Print out the LLVM IR
        //println!("{}", self.module.print_to_string().to_string());

        Target::initialize_all(&InitializationConfig::default());
        let target_triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&target_triple).unwrap();
        let target_machine = target
            .create_target_machine(
                &target_triple,
                "generic",
                "",
                // None will most likely not work here since 
                // it will not inline any basic blocks and not do tail calls for the cond
                OptimizationLevel::Aggressive, 
                RelocMode::Static,
                if large {CodeModel::Large } else { CodeModel::Medium },
            )
            .unwrap();


        let passes: &[&str] = &[
            "instcombine",
            "reassociate",
            "gvn",
            "simplifycfg",
            "loop-simplify",
            "mem2reg"
        ];

        self.module
            .run_passes(passes.join(",").as_str(), &target_machine, PassBuilderOptions::create())
            .unwrap();

        #[cfg(feature = "dump-stencils")]
        {
            // Create folder "stencils-dump" if not exists
            std::fs::create_dir_all("stencils-dump").unwrap();
            target_machine.write_to_file(&self.module, FileType::Object, std::path::Path::new(&format!("stencils-dump/{}.o", self.module.get_name().to_str().unwrap()))).unwrap();
        }
        target_machine.write_to_memory_buffer(&self.module, FileType::Object).unwrap()
    }

    fn compile_ghc_wrapper(&self) -> Stencil {
        let s_type = StencilType::new(StencilOperation::GhcWrapper, None);
        self.module.set_name(&format!("{}", s_type));
        let i8_type = self.context.i8_type();
        let i8_ptr_type = i8_type.ptr_type(AddressSpace::default());
        let void_type = self.context.void_type();
        let fn_type = void_type.fn_type(&[i8_ptr_type.into()], false);
        let function = self.module.add_function("__GHC_CC-CONVERTER__", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");

        let fun = self.init_fn_placeholder(&[i8_ptr_type.into()]);

        self.builder.position_at_end(basic_block);

        let stackptr = function.get_nth_param(0).unwrap().into_pointer_value();

        // Add tail call
        let call = self.builder.build_call(fun, &[stackptr.into()], "call").unwrap();

        call.set_call_convention(inkwell::llvm_sys::LLVMCallConv::LLVMGHCCallConv as u32);

        self.builder.build_return(None).unwrap();

        // Print out the LLVM IR
        //println!("{}", self.module.print_to_string().to_string());

        let elf = self.compile(true);
    
        get_stencil(s_type, elf.as_slice(), false, true)
    }

    // We can also generate stencil variants that take 0-6 64 bit arguments for example
    fn compile_call_c_function(&self) -> Stencil {
        let uint8_ptr = self.context.i8_type().ptr_type(AddressSpace::default());
        self.module.set_name(&format!("{}", StencilType::new(StencilOperation::CallCFunction, None)));
        let void_type = self.context.void_type();
        let fn_type = void_type.fn_type(&[uint8_ptr.into(), uint8_ptr.into()], false);
        let function = self.module.add_function("call-c-func", fn_type, None);
        function.set_call_conventions(inkwell::llvm_sys::LLVMCallConv::LLVMGHCCallConv as u32);
        let basic_block = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(basic_block);

        let stackptr = function.get_nth_param(0).unwrap().into_pointer_value();
        let arg = function.get_nth_param(1).unwrap().into_pointer_value();


        // We go with a function that passes a single pointer as an argument
        // We can then put our arguments somewhere in memory and pass a pointer to that
        // We also get back a single pointer to the result
        let c_function_type = uint8_ptr.fn_type(&[uint8_ptr.into()], false);
        let c_function = self.module.add_function("PH-1", c_function_type, Some(Linkage::External));
        c_function.set_call_conventions(inkwell::llvm_sys::LLVMCallConv::LLVMCCallConv as u32);

        let call = self.builder.build_indirect_call(c_function_type, c_function.as_global_value().as_pointer_value(), &[arg.into()], "call").unwrap();
        call.set_call_convention(inkwell::llvm_sys::LLVMCallConv::LLVMCCallConv as u32);

        let return_value = call.try_as_basic_value().left().unwrap().into_pointer_value();
        
        let tailcall_function = self.get_tailcall_placeholder(&[uint8_ptr.into(), uint8_ptr.into()]);
        let tc = self.builder.build_call(tailcall_function, &[stackptr.into(), return_value.into()], "tailcall").unwrap();
        tc.set_call_convention(inkwell::llvm_sys::LLVMCallConv::LLVMGHCCallConv as u32);
        tc.set_tail_call(true);      
        self.builder.build_return(None).unwrap();

        let elf = self.compile(true);
    
        get_stencil(StencilType::new(StencilOperation::CallCFunction, None), elf.as_slice(), true, true)
    }

    fn compile_stencil<F: Fn(&[BasicValueEnum<'ctx>], PointerValue<'ctx>) -> Vec<BasicValueEnum<'ctx>>>(&self, s_type: StencilType, args: &[BasicMetadataTypeEnum<'ctx>], operation: F) -> Stencil {
        self.module.set_name(&format!("{}", s_type));
        let i8_type = self.context.i8_type();
        let i8_ptr_type = i8_type.ptr_type(AddressSpace::default());

        let void_type = self.context.void_type();

        let mut new_args = Vec::new();
        new_args.push(i8_ptr_type.into());
        for arg in args {
            new_args.push(arg.clone());
        }

        let fn_type = void_type.fn_type(&new_args, false);
        let function = self.module.add_function(&format!("{}", s_type), fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");

        function.set_call_conventions(inkwell::llvm_sys::LLVMCallConv::LLVMGHCCallConv as u32);

        self.builder.position_at_end(basic_block);

        let stackptr = function.get_nth_param(0).unwrap().into_pointer_value();

        let params: Vec<BasicValueEnum<'_>> = function.get_param_iter().skip(1).collect();
        
        let res = operation(&params, stackptr);

        let mut tailcall_args = Vec::new();
        let mut tailcall_args_types = Vec::new();

        tailcall_args.push(stackptr.into());
        tailcall_args_types.push(i8_ptr_type.into());

        for arg in res {
            tailcall_args.push(arg.into());
            tailcall_args_types.push(arg.get_type().into());
        }

        let tailcallfun = self.get_tailcall_placeholder(&tailcall_args_types);

        let tc = self.builder.build_call(tailcallfun, &tailcall_args, "tailcall").unwrap();
        tc.set_call_convention(inkwell::llvm_sys::LLVMCallConv::LLVMGHCCallConv as u32);
        tc.set_tail_call(true);
        self.builder.build_return(None).unwrap();

        let elf = self.compile(self.large_ph.get());
        
        get_stencil(s_type, elf.as_slice(), true, self.large_ph.get())
    }


    fn compile_take_stack(&self) -> Stencil {
        let s_type = StencilType::new(StencilOperation::Take, Some(DataType::I64));
        self.compile_stencil(s_type, &[], |_, stackptr| {
            let i8_type = self.context.i8_type();
            let offset = self.init_placeholder(self.context.i64_type());
            let valueptr = unsafe { self.builder.build_gep(i8_type, stackptr, &[offset], "valueptr").unwrap() };
            let x = self.builder.build_load(self.context.i64_type(), valueptr, "x").unwrap().into_int_value();
            vec![x.into()]
        })
    }

    fn compile_take_first_stack(&self) -> Stencil {
        let i64_type = self.context.i64_type();
        let s_type = StencilType::new(StencilOperation::Take1, Some(DataType::I64));
        self.compile_stencil(s_type, &[i64_type.into(), i64_type.into()], |args, stackptr| {
            let i8_type = self.context.i8_type();
            let offset = self.init_placeholder(self.context.i64_type());
            let valueptr = unsafe { self.builder.build_gep(i8_type, stackptr, &[offset], "valueptr").unwrap() };
            let x = self.builder.build_load(self.context.i64_type(), valueptr, "x").unwrap().into_int_value();
            vec![x.into(), args[1]]
        })
    }

    fn compile_take_second_stack(&self) -> Stencil {
        let i64_type = self.context.i64_type();
        let s_type = StencilType::new(StencilOperation::Take2, Some(DataType::I64));
        self.compile_stencil(s_type,  &[i64_type.into()], |args, stackptr| {
            let i8_type = self.context.i8_type();
            let offset = self.init_placeholder(self.context.i64_type());
            let valueptr = unsafe { self.builder.build_gep(i8_type, stackptr, &[offset], "valueptr").unwrap() };
            let x = self.builder.build_load(self.context.i64_type(), valueptr, "x").unwrap().into_int_value();
            vec![args[0], x.into()]
        })
    }

    fn compile_take_const(&self, data_type: DataType) -> Stencil {
        let s_type = StencilType::new(StencilOperation::TakeConst, Some(data_type.clone()));
        self.compile_stencil(s_type.clone(), &[], |_, _| {
            let same_width_int = data_type.get_same_width_uint().get_llvm_type(self.context).into_int_type();
            let x = self.init_placeholder(same_width_int);
            if data_type.is_float() {
                vec![self.builder.build_bitcast(x, data_type.get_llvm_type(self.context), "cast").unwrap().into()]
            } else {
                vec![x.into()]
            }
        })
    }


    // Compiles a stencil that takes one argument and calls a function with two arguments
    // We could instead also just have one that moves from the first to the second and calls
    // with an undefined first value but this is more flexible and performance should be the same
    fn compile_duplex(&self) -> Stencil {
        let s_type = StencilType::new(StencilOperation::Duplex, None);
        self.compile_stencil(s_type, &[self.context.i64_type().into()], |args, _| {
            let x = args[0].into_int_value();
            vec![x.into(), x.into()]
        })
    }

    // For some weird reason LLVM ignores the "tail" attribute on the call instruction
    // for OptimizationLevel::None. The commented out variant works using a conditional
    // move plus an unconditional jump on the function pointer and should work on any optimization level
    // but is probably a bit less efficient therefore we will rely on the LLVM optimization behaviour for now.
    // To be sure we could also probably just hand assembly this tiny case.
    // This one stencil should be enough to cover all the control flow operations
    // Except switch statements. But we can just use a bunch of if-else statements for that in
    // theory but that's slow for larger numbers of cases. A binary search based one would probably
    // also be possible using just ifs however i don't think a jump table based one would be possible.
    // We could hand assemble that if we really need it or we could precompile stencils with 
    // 1-31 and 2^(5-10). 1024 cases is the most the C99 standard supports so that should be enough.
    // What this stencil would take is the index of the case we want. So we would need to do any
    // necessary calculations before going into this stencil.
    fn compile_cond(&self) -> Stencil {
        let s_type: StencilType = StencilType::new(StencilOperation::CondBr, None);
        self.module.set_name(&format!("{}", s_type));
        let void_type = self.context.void_type();
        let bool_type = self.context.bool_type();
        let i8_ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
        let fn_type = void_type.fn_type(&[i8_ptr_type.into(), bool_type.into()], false);
        let function_head = self.module.add_function("cond", fn_type, None);
        let basic_block = self.context.append_basic_block(function_head, "entry");

        function_head.set_call_conventions(inkwell::llvm_sys::LLVMCallConv::LLVMGHCCallConv as u32);

        let stackptr = function_head.get_nth_param(0).unwrap().into_pointer_value();

        self.builder.position_at_end(basic_block);

        let cond = function_head.get_nth_param(1).unwrap().into_int_value();

        let then_tailcallfun = self.init_fn_placeholder(&[i8_ptr_type.into()]);
        let else_tailcallfun = self.init_fn_placeholder(&[i8_ptr_type.into()]);

        let then_block = self.context.append_basic_block(function_head, "then");
        let else_block = self.context.append_basic_block(function_head, "else");
        //let end_block = self.context.append_basic_block(function_head, "end");

        self.builder.build_conditional_branch(cond, then_block, else_block).unwrap();

        self.builder.position_at_end(then_block);
        let call = self.builder.build_call(then_tailcallfun, &[stackptr.into()], "call").unwrap();
        call.set_call_convention(inkwell::llvm_sys::LLVMCallConv::LLVMGHCCallConv as u32);
        call.set_tail_call(true);
        self.builder.build_return(None).unwrap();
        //self.builder.build_unconditional_branch(end_block).unwrap();

        self.builder.position_at_end(else_block);
        let call = self.builder.build_call(else_tailcallfun, &[stackptr.into()], "call").unwrap();
        call.set_call_convention(inkwell::llvm_sys::LLVMCallConv::LLVMGHCCallConv as u32);
        call.set_tail_call(true);
        self.builder.build_return(None).unwrap();
        //self.builder.build_unconditional_branch(end_block).unwrap();

        /*self.builder.position_at_end(end_block);


        let funptr_type = then_tailcallfun.get_type().ptr_type(AddressSpace::default());
        let phi = self.builder.build_phi(funptr_type, "phi").unwrap();
        phi.add_incoming(&[(&then_tailcallfun.as_global_value(), then_block), (&else_tailcallfun.as_global_value(), else_block)]);

        let call = self.builder.build_indirect_call(then_tailcallfun.get_type(), phi.as_basic_value().into_pointer_value(), &[stackptr.into()], "call").unwrap();
        call.set_call_convention(inkwell::llvm_sys::LLVMCallConv::LLVMGHCCallConv as u32);
        call.set_tail_call(true);
        self.builder.build_return(None).unwrap();*/

        let elf = self.compile(false);
    
        get_stencil(s_type, elf.as_slice(), false, false)    
    }

    fn compile_get_stack_ptr(&self) -> Stencil {
        let s_type = StencilType::new(StencilOperation::GetStackPtr, None);
        self.compile_stencil(s_type, &[], |_, stackptr| {
            let offset = self.init_placeholder(self.context.i64_type());
            let valueptr = unsafe { self.builder.build_gep(self.context.i8_type(), stackptr, &[offset], "valueptr").unwrap() };
            vec![valueptr.into()]
        })
    }

    fn compile_load(&self, d_type: DataType) -> Stencil {
        let s_type = StencilType::new(StencilOperation::Take, Some(d_type.clone()));
        let uint8ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
        self.compile_stencil(s_type, &[uint8ptr_type.into()], |args, stackptr| {
            let valueptr = args[0].into_pointer_value();
            let x = self.builder.build_load(d_type.get_llvm_type(self.context), valueptr, "x").unwrap();
            vec![x.into()]
        })
    }

    fn compile_store(&self, d_type: DataType) -> Stencil {
        let s_type = StencilType::new(StencilOperation::Store, Some(d_type.clone()));
        let uint8ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
        self.compile_stencil(s_type, &[d_type.get_llvm_type(self.context).into(), uint8ptr_type.into()], |args, stackptr| {
            let x = args[0];
            let valueptr = args[1].into_pointer_value();
            self.builder.build_store(valueptr, x).unwrap();
            vec![x, valueptr.into()]
        })
    }

    fn compile_put_stack(&self) -> Stencil {
        let s_type = StencilType::new(StencilOperation::Put, Some(DataType::I64));
        self.compile_stencil(s_type, &[self.context.i64_type().into()], |args, stackptr| {
            let i8_type = self.context.i8_type();
            let x = args[0].into_int_value();
            let offset = self.init_placeholder(self.context.i64_type());
            let valueptr = unsafe { self.builder.build_gep(i8_type, stackptr, &[offset], "valueptr").unwrap() };
            self.builder.build_store(valueptr, x).unwrap();
            vec![x.into()]
        })
    }

    fn compile_int_binop(&self, s_type:StencilType, perform_op: fn(&Builder<'ctx>, DataType, IntValue<'ctx>, IntValue<'ctx>) -> IntValue<'ctx>) -> Stencil {
        let op_type = s_type.data_type.as_ref().unwrap().get_llvm_type(self.context).into_int_type();
        self.compile_stencil(s_type.clone(), &[op_type.into(), op_type.into()], |args, _| {
            let x = args[0].into_int_value();
            let y = args[1].into_int_value();
            let res = perform_op(&self.builder, s_type.data_type.clone().unwrap(), x, y);
            vec![res.into()]
        })
    }

    fn compile_const_int_binop(&self, s_type:StencilType, perform_op: fn(&Builder<'ctx>, DataType, IntValue<'ctx>, IntValue<'ctx>) -> IntValue<'ctx>) -> Stencil {
        let op_type = s_type.data_type.as_ref().unwrap().get_llvm_type(self.context).into_int_type();
        self.compile_stencil(s_type.clone(), &[op_type.into()], |args,  _| {
            let x = args[0].into_int_value();
            let y = self.init_placeholder(op_type);
            let res = perform_op(&self.builder, s_type.data_type.clone().unwrap(), x, y);
            vec![res.into()]
        })
    }

    fn compile_not(&self) -> Stencil {
        let _types = &[DataType::Bool, DataType::U8, DataType::U16, DataType::U32, DataType::U64];
        let s_type = StencilType::new(StencilOperation::Not, Some(DataType::Bool));
        self.compile_stencil(s_type, &[self.context.bool_type().into()], |args, _| {
            let x = args[0].into_int_value();
            let res = self.builder.build_not(x, "not").unwrap();
            vec![res.into()]
        })
    }

    fn compile_uncond_branch(&self) -> Stencil {
        let s_type = StencilType::new(StencilOperation::UncondBr, None);
        self.module.set_name(&format!("{}", s_type));
        let void_type = self.context.void_type();
        let uint8ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
        // We pass through our stack and two argument/scratch registers to be safe
        let fn_type = void_type.fn_type(&[uint8ptr_type.into(), uint8ptr_type.into(), uint8ptr_type.into()], false);
        let function = self.module.add_function("ret", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");
        function.set_call_conventions(inkwell::llvm_sys::LLVMCallConv::LLVMGHCCallConv as u32);
        self.builder.position_at_end(basic_block);

        let stackptr = function.get_nth_param(0).unwrap().into_pointer_value();
        let x = function.get_nth_param(1).unwrap().into_pointer_value();
        let y = function.get_nth_param(2).unwrap().into_pointer_value();
        let tailcallfun = self.get_tailcall_placeholder(&[uint8ptr_type.into(), uint8ptr_type.into(), uint8ptr_type.into()]);
        let tc = self.builder.build_call(tailcallfun, &[stackptr.into(), x.into(), y.into()], "tailcall").unwrap();
        tc.set_call_convention(inkwell::llvm_sys::LLVMCallConv::LLVMGHCCallConv as u32);
        tc.set_tail_call(true);
        self.builder.build_return(None).unwrap();
                
        let elf = self.compile(false);
        get_stencil(s_type, elf.as_slice(), false, false)    
    }


    pub fn compile_ret_stencil(&self) -> Stencil {
        let s_type = StencilType::new(StencilOperation::Ret, None);
        self.module.set_name(&format!("{}", s_type));
        let void_type = self.context.void_type();
        let fn_type = void_type.fn_type(&[], false);
        let function = self.module.add_function("ret", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");

        function.set_call_conventions(inkwell::llvm_sys::LLVMCallConv::LLVMGHCCallConv as u32);

        self.builder.position_at_end(basic_block);

        self.builder.build_return(None).unwrap();
                
        let elf = self.compile(false);
    
        get_stencil(s_type, elf.as_slice(), false, false)    
    }
}

fn compile_all_int_op() -> BTreeMap<StencilType, Stencil> {
    // TODO: This should be some kind of operations in a codegen layer that should
    //       should then be able to be reused for stencil generation
    fn int_add<'ctx>(builder: &Builder<'ctx>, _d_type: DataType, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        builder.build_int_add(x, y, "add").unwrap()
    }
    fn int_sub<'ctx>(builder: &Builder<'ctx>, _d_type: DataType, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        builder.build_int_sub(x, y, "sub").unwrap()
    }
    fn int_mul<'ctx>(builder: &Builder<'ctx>, _d_type: DataType, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        builder.build_int_mul(x, y, "mul").unwrap()
    }
    fn int_div<'ctx>(builder: &Builder<'ctx>, d_type: DataType, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        if d_type.is_signed() {
            builder.build_int_signed_div(x, y, "div").unwrap()
        } else {
            builder.build_int_unsigned_div(x, y, "div").unwrap()
        }
    }
    fn int_rem<'ctx>(builder: &Builder<'ctx>, d_type: DataType, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        if d_type.is_signed() {
            builder.build_int_signed_rem(x, y, "rem").unwrap()
        } else {
            builder.build_int_unsigned_rem(x, y, "rem").unwrap()
        }
    }
    fn int_and<'ctx>(builder: &Builder<'ctx>, _d_type: DataType, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        builder.build_and(x, y, "and").unwrap()
    }
    fn int_or<'ctx>(builder: &Builder<'ctx>, _d_type: DataType, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        builder.build_or(x, y, "or").unwrap()
    }
    fn int_xor<'ctx>(builder: &Builder<'ctx>, _d_type: DataType, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        builder.build_xor(x, y, "xor").unwrap()
    }
    fn int_shl<'ctx>(builder: &Builder<'ctx>, _d_type: DataType, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        builder.build_left_shift(x, y, "shl").unwrap()
    }
    fn int_shr<'ctx>(builder: &Builder<'ctx>, d_type: DataType, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        if d_type.is_signed() {
            builder.build_right_shift(x, y, true, "ashr").unwrap()
        } else {
            builder.build_right_shift(x, y, false, "lshr").unwrap()
        }
    }
    fn int_eq<'ctx>(builder: &Builder<'ctx>, _d_type: DataType, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        builder.build_int_compare(inkwell::IntPredicate::EQ, x, y, "eq").unwrap()
    }
    fn int_ne<'ctx>(builder: &Builder<'ctx>, _d_type: DataType, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        builder.build_int_compare(inkwell::IntPredicate::NE, x, y, "ne").unwrap()
    }
    fn int_gt<'ctx>(builder: &Builder<'ctx>, d_type: DataType, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        if d_type.is_signed() {
            builder.build_int_compare(inkwell::IntPredicate::SGT, x, y, "sgt").unwrap()
        } else {
            builder.build_int_compare(inkwell::IntPredicate::UGT, x, y, "ugt").unwrap()
        }
    }
    fn int_ge<'ctx>(builder: &Builder<'ctx>, d_type: DataType, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        if d_type.is_signed() {
            builder.build_int_compare(inkwell::IntPredicate::SGE, x, y, "sge").unwrap()
        } else {
            builder.build_int_compare(inkwell::IntPredicate::UGE, x, y, "uge").unwrap()
        }
    }
    fn int_lt<'ctx>(builder: &Builder<'ctx>, d_type: DataType, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        if d_type.is_signed() {
            builder.build_int_compare(inkwell::IntPredicate::SLT, x, y, "slt").unwrap()
        } else {
            builder.build_int_compare(inkwell::IntPredicate::ULT, x, y, "ult").unwrap()
        }
    }
    fn int_le<'ctx>(builder: &Builder<'ctx>, d_type: DataType, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        if d_type.is_signed() {
            builder.build_int_compare(inkwell::IntPredicate::SLE, x, y, "sle").unwrap()
        } else {
            builder.build_int_compare(inkwell::IntPredicate::ULE, x, y, "ule").unwrap()
        }
    }
    fn int_not<'ctx>(builder: &Builder<'ctx>, x: IntValue<'ctx>) -> IntValue<'ctx> {
        builder.build_not(x, "not").unwrap()
    }
    let context = Context::create();
    let ops: Vec<(StencilOperation, StencilOperation, for<'a> fn(& Builder<'a>, DataType, IntValue<'a>, IntValue<'a>) -> IntValue<'a>)> = vec![
        (StencilOperation::Add, StencilOperation::AddConst, int_add),  
        (StencilOperation::Sub, StencilOperation::SubConst, int_sub),
        (StencilOperation::Mul, StencilOperation::MulConst, int_mul), 
        (StencilOperation::Div, StencilOperation::DivConst, int_div), 
        (StencilOperation::Rem, StencilOperation::RemConst, int_rem), 
        (StencilOperation::And, StencilOperation::AndConst, int_and), 
        (StencilOperation::Or, StencilOperation::OrConst, int_or), 
        (StencilOperation::Xor, StencilOperation::XorConst, int_xor), 
        (StencilOperation::Shl, StencilOperation::ShlConst, int_shl), 
        (StencilOperation::Shr, StencilOperation::ShrConst, int_shr), 
        (StencilOperation::Eq, StencilOperation::EqConst, int_eq), 
        (StencilOperation::Ne, StencilOperation::NeConst, int_ne), 
        (StencilOperation::Gt, StencilOperation::GtConst, int_gt), 
        (StencilOperation::Ge, StencilOperation::GeConst, int_ge), 
        (StencilOperation::Lt, StencilOperation::LtConst, int_lt), 
        (StencilOperation::Le, StencilOperation::LeConst, int_le), 
    ];
    let types = vec![DataType::Bool, DataType::U8, DataType::U16, DataType::U32, DataType::U64, DataType::I8, DataType::I16, DataType::I32, DataType::I64];

    let mut stencils = BTreeMap::new();

    for (op_type, const_op_type, op) in ops {
        for ty in types.iter() {
            let codegen = StencilCodeGen::new(&context);
            let codegen_const = StencilCodeGen::new(&context);
            let stencil_type = StencilType::new(op_type, Some(ty.clone()));
            let stencil: Stencil = codegen.compile_int_binop(stencil_type.clone(), op);
            let const_stencil_type = StencilType::new(const_op_type, Some(ty.clone()));
            let const_stencil = codegen_const.compile_const_int_binop(const_stencil_type.clone(), op);
            stencils.insert(stencil_type, stencil);
            stencils.insert(const_stencil_type, const_stencil);
        }
    }

    // Compile not for all integer types
    for ty in types.iter() {
        let codegen = StencilCodeGen::new(&context);
        let stencil_type = StencilType::new(StencilOperation::Not, Some(ty.clone()));
        let stencil: Stencil = codegen.compile_stencil(stencil_type.clone(), &[ty.get_llvm_type(&context).into()], |args, _| {
            let x = args[0].into_int_value();
            let res = int_not(&codegen.builder, x);
            vec![res.into()]
        });
        stencils.insert(stencil_type, stencil);
    }

    stencils
}

fn compile_all_take_const(stencil_lib: &mut BTreeMap<StencilType, Stencil>) {
    let context = Context::create();
    let mut result = BTreeMap::new();
    let types = &[DataType::Bool, DataType::U8, DataType::U16, DataType::U32, DataType::U64, DataType::F32, DataType::F64];
    let op = StencilOperation::TakeConst;
    for ty in types {
        let s_type = StencilType::new(op, Some(ty.clone()));
        let codegen = StencilCodeGen::new(&context);
        let stencil = codegen.compile_take_const(s_type.clone().data_type.unwrap());
        result.insert(s_type.clone(), stencil);
    }
    // insert the unsigned stencils again for signed types
    let mut new_stencils = Vec::new();
    for (s_type, stencil) in result.iter() {
    if let Some(ty) = s_type.data_type.as_ref() {
        if let Some(new_ty) = ty.flip_signed() {
            let new_s_type = StencilType::new(s_type.operation, Some(new_ty));
            new_stencils.push((new_s_type, stencil.clone()));
        }
    }
    }
    for (s_type, stencil) in new_stencils {
    result.insert(s_type, stencil);
    }
    stencil_lib.append(&mut result);
}

fn compile_all_load_store() -> BTreeMap<StencilType, Stencil>{
    let context = Context::create();
    let mut result = BTreeMap::new();
    let types = &[DataType::Bool, DataType::U8, DataType::U16, DataType::U32, DataType::U64, DataType::F32, DataType::F64];
    let op = StencilOperation::Load;
    for ty in types {
        let s_type = StencilType::new(op, Some(ty.clone()));
        let codegen = StencilCodeGen::new(&context);
        let stencil = codegen.compile_load(ty.clone());
        result.insert(s_type.clone(), stencil);
    }
    let op = StencilOperation::Store;
    for ty in types {
        let s_type = StencilType::new(op, Some(ty.clone()));
        let codegen = StencilCodeGen::new(&context);
        let stencil = codegen.compile_store(ty.clone());
        result.insert(s_type.clone(), stencil);
    }
    // insert the unsigned stencils again for signed types
    let mut new_stencils = Vec::new();
    for (s_type, stencil) in result.iter() {
        if let Some(ty) = s_type.data_type.as_ref() {
            if let Some(new_ty) = ty.flip_signed() {
                let new_s_type = StencilType::new(s_type.operation, Some(new_ty));
                new_stencils.push((new_s_type, stencil.clone()));
            }
        }
    }
    for (s_type, stencil) in new_stencils {
        result.insert(s_type, stencil);
    }
    result
}

fn compile_stencil(stencil_lib: &mut BTreeMap<StencilType, Stencil>, comp_fn: fn(&StencilCodeGen) -> Stencil) {
    let context = Context::create();
    let codegen = StencilCodeGen::new(&context);

    let stencil = comp_fn(&codegen);
    stencil_lib.insert(stencil.s_type.clone(), stencil);
}

pub fn compile_all_stencils() -> BTreeMap<StencilType, Stencil> {
    let mut stencil_library = BTreeMap::new();

    compile_stencil(&mut stencil_library, |c| c.compile_put_stack());
    compile_stencil(&mut stencil_library,  |c| c.compile_take_stack());
    compile_stencil(&mut stencil_library, |c| c.compile_take_first_stack());
    compile_stencil(&mut stencil_library,  |c| c.compile_take_second_stack());
    compile_stencil(&mut stencil_library, |c| c.compile_duplex());
    compile_stencil(&mut stencil_library, |c| c.compile_ret_stencil());
    compile_stencil(&mut stencil_library, |c| c.compile_ghc_wrapper());
    compile_stencil(&mut stencil_library, |c| c.compile_cond());
    compile_stencil(&mut stencil_library, |c| c.compile_call_c_function());
    compile_stencil(&mut stencil_library, |c| c.compile_get_stack_ptr());
    compile_stencil(&mut stencil_library, |c| c.compile_uncond_branch());

    compile_all_take_const(&mut stencil_library);

    let mut load_store_stencils = compile_all_load_store();

    stencil_library.append(&mut load_store_stencils);

    let mut int_arith_stencils = compile_all_int_op();

    stencil_library.append(&mut int_arith_stencils);

    stencil_library
}

