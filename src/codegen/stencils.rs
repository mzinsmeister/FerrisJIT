use inkwell::builder::Builder;

use inkwell::context::Context;

use inkwell::memory_buffer::MemoryBuffer;
use inkwell::module::{Linkage, Module};
use inkwell::passes::PassBuilderOptions;
use inkwell::targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine};

use inkwell::types::{BasicMetadataTypeEnum, IntType};
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
    SDiv,
    SDivConst,
    SRem,
    SRemConst,
    UDiv,
    UDivConst,
    URem,
    URemConst,
    And,
    AndConst,
    Or,
    OrConst,
    Xor,
    XorConst,
    Shl,
    ShlConst,
    LShr,
    LShrConst,
    AShr,
    AShrConst,

    // Comparison operations
    Eq,
    Ne,
    UGt,
    UGe,
    ULt,
    ULe,
    SGt,
    SGe,
    SLt,
    SLe,

    // Boolean operations
    Not,

    // Control flow operations (If my plan works this should be the only one necessary)
    Cond,

    // These are the technical ones
    Take,
    Take1,
    Take2,
    TakeConst,
    Ret,
    Duplex,
    Put,

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
            StencilOperation::SDiv => write!(f, "sdiv"),
            StencilOperation::SDivConst => write!(f, "sdiv-const"),
            StencilOperation::SRem => write!(f, "srem"),
            StencilOperation::SRemConst => write!(f, "srem-const"),
            StencilOperation::UDiv => write!(f, "udiv"),
            StencilOperation::UDivConst => write!(f, "udiv-const"),
            StencilOperation::URem => write!(f, "urem"),
            StencilOperation::URemConst => write!(f, "urem-const"),
            StencilOperation::And => write!(f, "and"),
            StencilOperation::AndConst => write!(f, "and-const"),
            StencilOperation::Or => write!(f, "or"),
            StencilOperation::OrConst => write!(f, "or-const"),
            StencilOperation::Xor => write!(f, "xor"),
            StencilOperation::XorConst => write!(f, "xor-const"),
            StencilOperation::Shl => write!(f, "shl"),
            StencilOperation::ShlConst => write!(f, "shl-const"),
            StencilOperation::LShr => write!(f, "shr"),
            StencilOperation::LShrConst => write!(f, "shr-const"),
            StencilOperation::AShr => write!(f, "ashr"),
            StencilOperation::AShrConst => write!(f, "ashr-const"),
            StencilOperation::Eq => write!(f, "eq"),
            StencilOperation::Ne => write!(f, "ne"),
            StencilOperation::UGt => write!(f, "ugt"),
            StencilOperation::UGe => write!(f, "uge"),
            StencilOperation::ULt => write!(f, "ult"),
            StencilOperation::ULe => write!(f, "ule"),
            StencilOperation::SGt => write!(f, "sgt"),
            StencilOperation::SGe => write!(f, "sge"),
            StencilOperation::SLt => write!(f, "slt"),
            StencilOperation::SLe => write!(f, "sle"),
            StencilOperation::Not => write!(f, "not"),
            StencilOperation::Cond => write!(f, "cond"),
            StencilOperation::Take => write!(f, "take"),
            StencilOperation::TakeConst => write!(f, "take-const"),
            StencilOperation::Take1 => write!(f, "take1"),
            StencilOperation::Take2 => write!(f, "take2"),
            StencilOperation::Ret => write!(f, "ret"),
            StencilOperation::Duplex => write!(f, "duplex"),
            StencilOperation::Put => write!(f, "put"),
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
    pub large: bool
}

impl Display for Stencil {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.s_type)
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

    for rel in reltab {
        for reloc in rel.1.into_iter() {
            let sym = symtab.get(reloc.r_sym).unwrap();
            let name = &strtab.get_at(sym.st_name);
            if let Some(name) = name {
                if name.starts_with("PH") {
                    //println!("Relocation: {:?}: {:#?}", name, reloc);
                    relocs.push((name.to_string(), reloc));
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

    let holes = relocs.iter().map(|(_, reloc)| reloc.r_offset as usize).collect();


    let mut code = code.unwrap().to_vec();

    if cut_jmp {
        let truncate = if large { 12 } else { 5 };
        code.truncate(code.len() - truncate);
    }



    Stencil {
        code,
        s_type,
        holes: holes,
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

        let tailcalfun_type = void_type.fn_type(&tailcall_args_types, false);

        let tailcallfun = self.module.add_function("tailcall", tailcalfun_type, Some(Linkage::External));

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

    fn compile_take_const(&self) -> Stencil {
        let s_type = StencilType::new(StencilOperation::TakeConst, Some(DataType::I64));
        self.compile_stencil(s_type, &[self.context.i64_type().into()], |_, _| {
            let x = self.init_placeholder(self.context.i64_type());
            vec![x.into()]
        })
    }

    // Compiles a stencil that takes one argument and calls a function with two arguments
    // We could instead also just have one that moves from the first to the second and calls
    // with an undefined first value but this is more flexible and performance should be the same
    fn compile_duplex(&self) -> Stencil {
        let s_type = StencilType::new(StencilOperation::Duplex, Some(DataType::I64));
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
    fn compile_cond(&self) -> Stencil {
        let s_type: StencilType = StencilType::new(StencilOperation::Cond, None);
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

    fn compile_int_arith(&self, s_type:StencilType, op_type: IntType<'ctx>, perform_op: fn(&Builder<'ctx>, IntValue<'ctx>, IntValue<'ctx>) -> IntValue<'ctx>) -> Stencil {
        self.compile_stencil(s_type, &[op_type.into(), op_type.into()], |args, _| {
            let x = args[0].into_int_value();
            let y = args[1].into_int_value();
            let res = perform_op(&self.builder, x, y);
            vec![res.into()]
        })
    }

    fn compile_const_int_arith(&self, s_type:StencilType, op_type: IntType<'ctx>, perform_op: fn(&Builder<'ctx>, IntValue<'ctx>, IntValue<'ctx>) -> IntValue<'ctx>) -> Stencil {
        self.compile_stencil(s_type, &[op_type.into()], |args,  _| {
            let x = args[0].into_int_value();
            let y = self.init_placeholder(op_type);
            let res = perform_op(&self.builder, x, y);
            vec![res.into()]
        })
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

fn compile_all_int_arith() -> BTreeMap<StencilType, Stencil> {
    // TODO: This should be some kind of operations in a codegen layer that should
    //       should then be able to be reused for stencil generation
    fn int_add<'ctx>(builder: &Builder<'ctx>, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        builder.build_int_add(x, y, "add").unwrap()
    }
    fn int_sub<'ctx>(builder: &Builder<'ctx>, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        builder.build_int_sub(x, y, "sub").unwrap()
    }
    fn int_mul<'ctx>(builder: &Builder<'ctx>, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        builder.build_int_mul(x, y, "mul").unwrap()
    }
    fn int_sdiv<'ctx>(builder: &Builder<'ctx>, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        builder.build_int_signed_div(x, y, "div").unwrap()
    }
    fn int_srem<'ctx>(builder: &Builder<'ctx>, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        builder.build_int_signed_rem(x, y, "rem").unwrap()
    }
    fn int_udiv<'ctx>(builder: &Builder<'ctx>, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        builder.build_int_unsigned_div(x, y, "div").unwrap()
    }
    fn int_urem<'ctx>(builder: &Builder<'ctx>, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        builder.build_int_unsigned_rem(x, y, "rem").unwrap()
    }
    fn int_and<'ctx>(builder: &Builder<'ctx>, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        builder.build_and(x, y, "and").unwrap()
    }
    fn int_or<'ctx>(builder: &Builder<'ctx>, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        builder.build_or(x, y, "or").unwrap()
    }
    fn int_xor<'ctx>(builder: &Builder<'ctx>, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        builder.build_xor(x, y, "xor").unwrap()
    }
    fn int_shl<'ctx>(builder: &Builder<'ctx>, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        builder.build_left_shift(x, y, "shl").unwrap()
    }
    fn int_lshr<'ctx>(builder: &Builder<'ctx>, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        builder.build_right_shift(x, y, false, "lshr").unwrap()
    }
    fn int_ashr<'ctx>(builder: &Builder<'ctx>, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        builder.build_right_shift(x, y, true, "ashr").unwrap()
    }
    let context = Context::create();
    let ops: Vec<(StencilOperation, StencilOperation, for<'a> fn(& Builder<'a>, IntValue<'a>, IntValue<'a>) -> IntValue<'a>)> = vec![
        (StencilOperation::Add, StencilOperation::AddConst, int_add),  
        (StencilOperation::Sub, StencilOperation::SubConst, int_sub),
        (StencilOperation::Mul, StencilOperation::MulConst, int_mul), 
        (StencilOperation::SDiv, StencilOperation::SDivConst, int_sdiv), 
        (StencilOperation::SRem, StencilOperation::SRemConst, int_srem), 
        (StencilOperation::UDiv, StencilOperation::UDivConst, int_udiv), 
        (StencilOperation::URem, StencilOperation::URemConst, int_urem), 
        (StencilOperation::And, StencilOperation::AndConst, int_and), 
        (StencilOperation::Or, StencilOperation::OrConst, int_or), 
        (StencilOperation::Xor, StencilOperation::XorConst, int_xor), 
        (StencilOperation::Shl, StencilOperation::ShlConst, int_shl), 
        (StencilOperation::LShr, StencilOperation::LShrConst, int_lshr), 
        (StencilOperation::AShr, StencilOperation::AShrConst, int_ashr)
    ];
    let types = vec![context.i8_type(), context.i16_type(), context.i32_type(), context.i64_type()];

    let mut stencils = BTreeMap::new();

    for (op_type, const_op_type, op) in ops {
        for ty in types.iter() {
            let codegen = StencilCodeGen::new(&context);
            let codegen_const = StencilCodeGen::new(&context);

            let ir_type = DataType::try_from(*ty).unwrap();
            let stencil_type = StencilType::new(op_type, Some(ir_type.clone()));
            let stencil: Stencil = codegen.compile_int_arith(stencil_type.clone(), *ty, op);
            let const_stencil_type = StencilType::new(const_op_type, Some(ir_type));
            let const_stencil = codegen_const.compile_const_int_arith(const_stencil_type.clone(), *ty, op);
            stencils.insert(stencil_type, stencil);
            stencils.insert(const_stencil_type, const_stencil);
        }
    }

    stencils
}

fn compile_all_cmp() -> BTreeMap<StencilType, Stencil> {
    let context = Context::create();
    fn int_eq<'ctx>(builder: &Builder<'ctx>, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        builder.build_int_compare(inkwell::IntPredicate::EQ, x, y, "eq").unwrap()
    }
    fn int_ne<'ctx>(builder: &Builder<'ctx>, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        builder.build_int_compare(inkwell::IntPredicate::NE, x, y, "ne").unwrap()
    }
    fn int_ugt<'ctx>(builder: &Builder<'ctx>, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        builder.build_int_compare(inkwell::IntPredicate::UGT, x, y, "ugt").unwrap()
    }
    fn int_uge<'ctx>(builder: &Builder<'ctx>, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        builder.build_int_compare(inkwell::IntPredicate::UGE, x, y, "uge").unwrap()
    }
    fn int_ult<'ctx>(builder: &Builder<'ctx>, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        builder.build_int_compare(inkwell::IntPredicate::ULT, x, y, "ult").unwrap()
    }
    fn int_ule<'ctx>(builder: &Builder<'ctx>, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        builder.build_int_compare(inkwell::IntPredicate::ULE, x, y, "ule").unwrap()
    }
    fn int_sgt<'ctx>(builder: &Builder<'ctx>, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        builder.build_int_compare(inkwell::IntPredicate::SGT, x, y, "sgt").unwrap()
    }
    fn int_sge<'ctx>(builder: &Builder<'ctx>, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        builder.build_int_compare(inkwell::IntPredicate::SGE, x, y, "sge").unwrap()
    }
    fn int_slt<'ctx>(builder: &Builder<'ctx>, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        builder.build_int_compare(inkwell::IntPredicate::SLT, x, y, "slt").unwrap()
    }
    fn int_sle<'ctx>(builder: &Builder<'ctx>, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        builder.build_int_compare(inkwell::IntPredicate::SLE, x, y, "sle").unwrap()
    }
    
    let types = vec![context.i8_type(), context.i16_type(), context.i32_type(), context.i64_type()];
    let ops: Vec<(StencilOperation, for<'a> fn(& Builder<'a>, IntValue<'a>, IntValue<'a>) -> IntValue<'a>)> = vec![
        (StencilOperation::Eq, int_eq), 
        (StencilOperation::Not, int_ne), 
        (StencilOperation::UGt, int_ugt), 
        (StencilOperation::UGe, int_uge), 
        (StencilOperation::ULt, int_ult), 
        (StencilOperation::ULe, int_ule), 
        (StencilOperation::SGt, int_sgt), 
        (StencilOperation::SGe, int_sge), 
        (StencilOperation::SLt, int_slt), 
        (StencilOperation::SLe, int_sle)
    ];

    let mut stencils = BTreeMap::new();

    for (sop, op) in ops {
        for ty in types.iter() {
            let codegen = StencilCodeGen::new(&context);
            let ir_type = DataType::try_from(*ty).unwrap();
            let stencil_type = StencilType::new(sop, Some(ir_type.clone()));
            let stencil: Stencil = codegen.compile_stencil(stencil_type.clone(), &[(*ty).into(), (*ty).into()], |args, _| {
                let x = args[0].into_int_value();
                let y = args[1].into_int_value();
                let res = op(&codegen.builder, x, y);
                vec![res.into()]
            });
            stencils.insert(stencil_type, stencil);
        }
    }

    stencils
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
    compile_stencil(&mut stencil_library, |c| c.compile_take_const());
    compile_stencil(&mut stencil_library, |c| c.compile_duplex());
    compile_stencil(&mut stencil_library, |c| c.compile_ret_stencil());
    compile_stencil(&mut stencil_library, |c| c.compile_ghc_wrapper());
    compile_stencil(&mut stencil_library, |c| c.compile_cond());


    let mut int_arith_stencils = compile_all_int_arith();

    stencil_library.append(&mut int_arith_stencils);

    stencil_library
}

