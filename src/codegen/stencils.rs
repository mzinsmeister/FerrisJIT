use inkwell::builder::Builder;

use inkwell::context::Context;

use inkwell::memory_buffer::MemoryBuffer;
use inkwell::module::{Linkage, Module};
use inkwell::passes::PassBuilderOptions;
use inkwell::targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine};

use inkwell::types::{AnyType, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, IntType};
use inkwell::values::{BasicValueEnum, IntValue, PointerValue};
use inkwell::{AddressSpace, OptimizationLevel};

use std::cell::Cell;
use std::collections::BTreeMap;
use std::path::Path;

// For reference (if you look into the Disassembly). 
// Order of Registers in argument passing of the GHC-CC: R13, RBP, R12, RBX, R14, RSI, RDI, R8, R9, R15


pub struct Stencil {
    pub name: String,
    pub code: Vec<u8>,
    pub holes: Vec<usize>,
    pub large: bool
}

fn get_stencil(name: &str, elf: &[u8], cut_jmp: bool, large: bool) -> Stencil {
    let gobj = goblin::elf::Elf::parse(elf).unwrap();

    //println!("---------------------------");

    //println!("{:#?}", gobj);

    //println!("---------------------------");

    // Print relocation record for "PH" symbols

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
        name: name.to_string(),
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

    fn new(context: &'ctx Context, name: &str) -> Self {
        let module = context.create_module(name);
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
                OptimizationLevel::None,
                RelocMode::Static,
                if large {CodeModel::Large } else {CodeModel::Medium},
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

        // For debugging purposes the stencil functions can be dumped to a file and then
        // be inspected for example by llvm-objdump --disassemble sum.o 
        target_machine.write_to_file(&self.module, FileType::Object, Path::new(&format!("{}.o", self.module.get_name().to_str().unwrap()))).unwrap();
        target_machine.write_to_memory_buffer(&self.module, FileType::Object).unwrap()
    }

    fn compile_ghc_wrapper(&self) -> Stencil {
        let i8_type = self.context.i8_type();
        let i8_ptr_type = i8_type.ptr_type(AddressSpace::default());
        let void_type = self.context.void_type();
        let fn_type = void_type.fn_type(&[i8_ptr_type.into()], false);
        let function = self.module.add_function("__GHC_CC-CONVERTER__", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");

        let tailcalltype = void_type.fn_type(&[i8_ptr_type.into()], false);

        let fun = self.module.add_function("PH1", tailcalltype, Some(Linkage::External));

        self.builder.position_at_end(basic_block);

        let stackptr = function.get_nth_param(0).unwrap().into_pointer_value();

        // Add tail call
        let call = self.builder.build_call(fun, &[stackptr.into()], "call").unwrap();

        call.set_call_convention(inkwell::llvm_sys::LLVMCallConv::LLVMGHCCallConv as u32);

        self.builder.build_return(None).unwrap();

        // Print out the LLVM IR
        //println!("{}", self.module.print_to_string().to_string());

        let elf = self.compile(true);
    
        get_stencil("__GHC_CC-CONVERTER__", elf.as_slice(), false, true)
    }

    fn compile_stencil<F: Fn(&[BasicValueEnum<'ctx>], PointerValue<'ctx>) -> Vec<BasicValueEnum<'ctx>>>(&self, name: &str, args: &[BasicMetadataTypeEnum<'ctx>], operation: F) -> Stencil {
        self.module.set_name(name);
        let i8_type = self.context.i8_type();
        let i8_ptr_type = i8_type.ptr_type(AddressSpace::default());


        let void_type = self.context.void_type();

        let mut new_args = Vec::new();
        new_args.push(i8_ptr_type.into());
        for arg in args {
            new_args.push(arg.clone());
        }

        let fn_type = void_type.fn_type(&new_args, false);
        let function = self.module.add_function(name, fn_type, None);
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

        get_stencil(name, elf.as_slice(), true, self.large_ph.get())
    }


    fn compile_take_stack(&self) -> Stencil {
        self.compile_stencil("i64_take-stack", &[], |_, stackptr| {
            let i8_type = self.context.i8_type();
            let offset = self.init_placeholder(self.context.i64_type());
            let valueptr = unsafe { self.builder.build_gep(i8_type, stackptr, &[offset], "valueptr").unwrap() };
            let x = self.builder.build_load(self.context.i64_type(), valueptr, "x").unwrap().into_int_value();
            vec![x.into()]
        })
    }

    fn compile_take2_first_stack(&self) -> Stencil {
        let i64_type = self.context.i64_type();
        self.compile_stencil("i64_take-1-stack", 
                &[i64_type.into(), i64_type.into()], 
                |args, stackptr| {
            let i8_type = self.context.i8_type();
            let offset = self.init_placeholder(self.context.i64_type());
            let valueptr = unsafe { self.builder.build_gep(i8_type, stackptr, &[offset], "valueptr").unwrap() };
            let x = self.builder.build_load(self.context.i64_type(), valueptr, "x").unwrap().into_int_value();
            vec![x.into(), args[1]]
        })
    }

    fn compile_take2_second_stack(&self) -> Stencil {
        let i64_type = self.context.i64_type();
        self.compile_stencil("i64_take-2-stack",  &[i64_type.into()], |args, stackptr| {
            let i8_type = self.context.i8_type();
            let offset = self.init_placeholder(self.context.i64_type());
            let valueptr = unsafe { self.builder.build_gep(i8_type, stackptr, &[offset], "valueptr").unwrap() };
            let x = self.builder.build_load(self.context.i64_type(), valueptr, "x").unwrap().into_int_value();
            vec![args[0], x.into()]
        })
    }

    fn compile_take_const(&self) -> Stencil {
        self.compile_stencil("i64_take-const", &[self.context.i64_type().into()], |_, _| {
            let x = self.init_placeholder(self.context.i64_type());
            vec![x.into()]
        })
    }

    // Compiles a stencil that takes one argument and calls a function with two arguments
    // We could instead also just have one that moves from the first to the second and calls
    // with an undefined first value but this is more flexible and performance should be the same
    fn compile_duplex(&self) -> Stencil {
        self.compile_stencil("i64_duplex", &[self.context.i64_type().into()], |args, _| {
            let x = args[0].into_int_value();
            vec![x.into(), x.into()]
        })
    }

    fn compile_put_stack(&self) -> Stencil {
        self.compile_stencil("i64_put-stack", &[self.context.i64_type().into()], |args, stackptr| {
            let i8_type = self.context.i8_type();
            let x = args[0].into_int_value();
            let offset = self.init_placeholder(self.context.i64_type());
            let valueptr = unsafe { self.builder.build_gep(i8_type, stackptr, &[offset], "valueptr").unwrap() };
            self.builder.build_store(valueptr, x).unwrap();
            vec![x.into()]
        })
    }

    fn compile_int_arith(&self, name: &str, op_type: IntType<'ctx>, perform_op: fn(&Builder<'ctx>, IntValue<'ctx>, IntValue<'ctx>) -> IntValue<'ctx>) -> Stencil {
        self.compile_stencil(name, &[op_type.into(), op_type.into()], |args, _| {
            let x = args[0].into_int_value();
            let y = args[1].into_int_value();
            let res = perform_op(&self.builder, x, y);
            vec![res.into()]
        })
    }

    fn compile_const_int_arith(&self, name: &str, op_type: IntType<'ctx>, perform_op: fn(&Builder<'ctx>, IntValue<'ctx>, IntValue<'ctx>) -> IntValue<'ctx>) -> Stencil {
        self.compile_stencil(name, &[op_type.into()], |args,  _| {
            let x = args[0].into_int_value();
            let y = self.init_placeholder(op_type);
            let res = perform_op(&self.builder, x, y);
            vec![res.into()]
        })
    }


    pub fn compile_ret_stencil(&self) -> Stencil {
        let void_type = self.context.void_type();
        let fn_type = void_type.fn_type(&[], false);
        let function = self.module.add_function("ret", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");

        function.set_call_conventions(inkwell::llvm_sys::LLVMCallConv::LLVMGHCCallConv as u32);

        self.builder.position_at_end(basic_block);

        self.builder.build_return(None).unwrap();
                
        let elf = self.compile(false);
    
        get_stencil("ret", elf.as_slice(), false, false)    
    }
}

fn compile_all_int_arith() -> BTreeMap<String, Stencil> {
    // TODO: We need reverse operations for non commutative operations
    fn int_add<'ctx>(builder: &Builder<'ctx>, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        builder.build_int_add(x, y, "add").unwrap()
    }
    fn int_sub<'ctx>(builder: &Builder<'ctx>, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        builder.build_int_sub(x, y, "sub").unwrap()
    }
    fn int_mul<'ctx>(builder: &Builder<'ctx>, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        builder.build_int_mul(x, y, "mul").unwrap()
    }
    fn int_div<'ctx>(builder: &Builder<'ctx>, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        builder.build_int_signed_div(x, y, "div").unwrap()
    }
    fn int_rem<'ctx>(builder: &Builder<'ctx>, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
        builder.build_int_signed_rem(x, y, "rem").unwrap()
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
    let ops: Vec<(&str, for<'a> fn(& Builder<'a>, IntValue<'a>, IntValue<'a>) -> IntValue<'a>)> = vec![("add", int_add), ("sub", int_sub), ("mul", int_mul), ("div", int_div), ("rem", int_rem), ("and", int_and), ("or", int_or), ("xor", int_xor), ("shl", int_shl), ("lshr", int_lshr), ("ashr", int_ashr)];
    let types = vec![context.i8_type(), context.i16_type(), context.i32_type(), context.i64_type()];

    let mut stencils = BTreeMap::new();

    for (name, op) in ops {
        for ty in types.iter() {
            let codegen = StencilCodeGen::new(&context, "stencil");
            let codegen_const = StencilCodeGen::new(&context, "stencil-const");

            let stencil: Stencil = codegen.compile_int_arith(&format!("{}_{}", ty.print_to_string().to_string(), name), *ty, op);
            let const_stencil = codegen_const.compile_const_int_arith(&format!("{}_{}_const", ty.print_to_string().to_string(), name), *ty, op);
            stencils.insert(stencil.name.clone(), stencil);
            stencils.insert(const_stencil.name.clone(), const_stencil);
        }
    }

    stencils
}

fn compile_stencil(stencil_lib: &mut BTreeMap<String, Stencil>, name: &str, comp_fn: fn(&StencilCodeGen) -> Stencil) {
    let context = Context::create();
    let codegen = StencilCodeGen::new(&context, name);

    let stencil = comp_fn(&codegen);
    stencil_lib.insert(name.to_string(), stencil);
}

pub fn compile_all_stencils() -> BTreeMap<String, Stencil> {
    let mut stencil_library = BTreeMap::new();

    compile_stencil(&mut stencil_library, "i64_put-stack", |c| c.compile_put_stack());
    compile_stencil(&mut stencil_library, "i64_take-stack", |c| c.compile_take_stack());
    compile_stencil(&mut stencil_library, "i64_take-1-stack", |c| c.compile_take2_first_stack());
    compile_stencil(&mut stencil_library, "i64_take-2-stack", |c| c.compile_take2_second_stack());
    compile_stencil(&mut stencil_library, "i64_take-const", |c| c.compile_take_const());
    compile_stencil(&mut stencil_library, "i64_duplex", |c| c.compile_duplex());
    compile_stencil(&mut stencil_library, "ret", |c| c.compile_ret_stencil());
    compile_stencil(&mut stencil_library, "__GHC_CC-CONVERTER__", |c| c.compile_ghc_wrapper());


    let mut int_arith_stencils = compile_all_int_arith();

    stencil_library.append(&mut int_arith_stencils);

    stencil_library
}

