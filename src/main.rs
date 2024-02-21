

use inkwell::builder::Builder;

use inkwell::context::Context;

use inkwell::memory_buffer::MemoryBuffer;
use inkwell::module::{Linkage, Module};
use inkwell::passes::PassBuilderOptions;
use inkwell::targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine};

use inkwell::types::{AnyType, IntType};
use inkwell::values::IntValue;
use inkwell::{AddressSpace, OptimizationLevel};
use libc::uintptr_t;



use std::collections::BTreeMap;
use std::error::Error;



use std::path::Path;

struct Stencil {
    name: String,
    code: Vec<u8>,
    holes: Vec<usize>
}

fn get_stencil(name: &str, elf: &[u8]) -> Stencil {
    let gobj = goblin::elf::Elf::parse(elf).unwrap();

    //println!("---------------------------");

    //println!("{:#?}", gobj);

    println!("---------------------------");

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
                    println!("Relocation: {:?}: {:#?}", name, reloc);
                    relocs.push((name.to_string(), reloc));
                }
            }
        }
    }
    // get the binary code of the "sum" function as a byte array from the ELF file

    let mut offset = 0;
    let mut code = None;
    
    // get .text section offset and size
    for section in gobj.section_headers {
        let name = &strtab.get_at(section.sh_name);
        if let Some(name) = name {
            if name == &".text" {
                offset = section.sh_offset as usize;
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

    Stencil {
        code: code.unwrap().to_vec(),
        name: name.to_string(),
        holes: holes
    }
}

struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {

    fn compile(&self) -> MemoryBuffer {

        // run passes


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
                CodeModel::Large,
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
        //target_machine.write_to_file(&self.module, FileType::Object, Path::new(&format!("{}.o", self.module.get_name().to_str().unwrap()))).unwrap();
        target_machine.write_to_memory_buffer(&self.module, FileType::Object).unwrap()
    }

    fn compile_ghc_wrapper(&self) -> Stencil {
        let i8_type = self.context.i8_type();
        let i8_ptr_type = i8_type.ptr_type(AddressSpace::default());
        let void_type = self.context.void_type();
        let fn_type = void_type.fn_type(&[i8_ptr_type.into()], false);
        let function = self.module.add_function("take_stack", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");

        let tailcalltype = void_type.fn_type(&[i8_ptr_type.into()], false);

        let fun = self.module.add_function("PH1", tailcalltype, Some(Linkage::External));

        self.builder.position_at_end(basic_block);

        let stackptr = function.get_nth_param(0).unwrap().into_pointer_value();

        // Add tail call
        let tc = self.builder.build_call(fun, &[stackptr.into()], "tailcall").unwrap();

        tc.set_call_convention(inkwell::llvm_sys::LLVMCallConv::LLVMGHCCallConv as u32);

        self.builder.build_return(None).unwrap();
                
        // Print out the LLVM IR
        println!("{}", self.module.print_to_string().to_string());

        let elf = self.compile();
    
        get_stencil("GHCCC-Converter", elf.as_slice())
    }

    fn compile_take_stack(&self) -> Stencil {
        let i64_type = self.context.i64_type();
        let i8_type = self.context.i8_type();
        let i8_ptr_type = i8_type.ptr_type(AddressSpace::default());

        let i8_array_type = i8_type.array_type(1048576);
        let void_type = self.context.void_type();
        let fn_type = void_type.fn_type(&[i8_ptr_type.into()], false);
        let function = self.module.add_function("take_stack", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");

        let tailcalltype = void_type.fn_type(&[i8_ptr_type.into(), i64_type.into()], false);

        let tailcallfun = self.module.add_function("tailcall", tailcalltype, Some(Linkage::External));
        tailcallfun.set_call_conventions(inkwell::llvm_sys::LLVMCallConv::LLVMGHCCallConv as u32);

        function.set_call_conventions(inkwell::llvm_sys::LLVMCallConv::LLVMGHCCallConv as u32);

        self.builder.position_at_end(basic_block);

        let global = self.module.add_global(i8_array_type, Some(AddressSpace::default()), "PH1");
        global.set_linkage(Linkage::External);
        global.set_alignment(1);

        let stackptr = function.get_nth_param(0).unwrap().into_pointer_value();

        // Cast the pointer to an integer
        let ptr_as_int = self.builder.build_ptr_to_int(global.as_pointer_value(), i64_type, "ptrtoint").unwrap();

        // Bitcast the integer to a double
        let offset = self.builder.build_bitcast(ptr_as_int, i64_type, "offset").unwrap().into_int_value();

        // Get the value pointer
        let valueptr = unsafe { self.builder.build_gep(i8_ptr_type, stackptr, &[offset], "valueptr").unwrap() };

        // Load argument from stack
        let x = self.builder.build_load(i64_type, valueptr, "x").unwrap().into_int_value();

        // Add tail call
        let tc = self.builder.build_call(tailcallfun, &[stackptr.into(), x.into()], "tailcall").unwrap();

        tc.set_call_convention(inkwell::llvm_sys::LLVMCallConv::LLVMGHCCallConv as u32);

        tc.set_tail_call(true);

        self.builder.build_return(None).unwrap();
                
        // Print out the LLVM IR
        println!("{}", self.module.print_to_string().to_string());

        let elf = self.compile();
    
        let mut stencil = get_stencil("Unpack-Stack-Value", elf.as_slice());

        // remove the last 12 bytes from the code

        stencil.code.truncate(stencil.code.len() - 12);

        stencil
    }

    fn compile_put_stack(&self) -> Stencil {
        let i64_type = self.context.i64_type();
        let i8_type = self.context.i8_type();
        let i8_ptr_type = i8_type.ptr_type(AddressSpace::default());

        let i8_array_type = i8_type.array_type(1048576);
        let void_type = self.context.void_type();
        let fn_type = void_type.fn_type(&[i8_ptr_type.into(), i64_type.into()], false);
        let function = self.module.add_function("put_stack", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");

        function.set_call_conventions(inkwell::llvm_sys::LLVMCallConv::LLVMGHCCallConv as u32);

        Target::initialize_native(&InitializationConfig::default()).unwrap();

        self.builder.position_at_end(basic_block);

        let global = self.module.add_global(i8_array_type, Some(AddressSpace::default()), "PH1");
        global.set_linkage(Linkage::External);
        global.set_alignment(1);

        let stackptr = function.get_nth_param(0).unwrap().into_pointer_value();

        let x = function.get_nth_param(1).unwrap().into_int_value();

        // Cast the pointer to an integer
        let ptr_as_int = self.builder.build_ptr_to_int(global.as_pointer_value(), i64_type, "ptrtoint").unwrap();

        // Bitcast the integer to a double
        let offset = self.builder.build_bitcast(ptr_as_int, i64_type, "offset").unwrap().into_int_value();

        // Get the value pointer
        let valueptr = unsafe { self.builder.build_gep(i8_ptr_type, stackptr, &[offset], "valueptr").unwrap() };

        // Load argument from stack
        self.builder.build_store(valueptr, x).unwrap();

        // Add tail call
        self.builder.build_return(None).unwrap();
                
        // Print out the LLVM IR
        println!("{}", self.module.print_to_string().to_string());

        let elf = self.compile();
    
        get_stencil("Unpack-Stack-Value", elf.as_slice())
    }

    fn compile_all_int_arith() -> BTreeMap<String, Stencil> {
        fn int_add<'ctx>(builder: &'ctx Builder, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
            builder.build_int_add(x, y, "add").unwrap()
        }
        fn int_sub<'ctx>(builder: &'ctx Builder, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
            builder.build_int_sub(x, y, "sub").unwrap()
        }
        fn int_mul<'ctx>(builder: &'ctx Builder, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
            builder.build_int_mul(x, y, "mul").unwrap()
        }
        fn int_div<'ctx>(builder: &'ctx Builder, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
            builder.build_int_signed_div(x, y, "div").unwrap()
        }
        fn int_rem<'ctx>(builder: &'ctx Builder, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
            builder.build_int_signed_rem(x, y, "rem").unwrap()
        }
        fn int_and<'ctx>(builder: &'ctx Builder, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
            builder.build_and(x, y, "and").unwrap()
        }
        fn int_or<'ctx>(builder: &'ctx Builder, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
            builder.build_or(x, y, "or").unwrap()
        }
        fn int_xor<'ctx>(builder: &'ctx Builder, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
            builder.build_xor(x, y, "xor").unwrap()
        }
        fn int_shl<'ctx>(builder: &'ctx Builder, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
            builder.build_left_shift(x, y, "shl").unwrap()
        }
        fn int_lshr<'ctx>(builder: &'ctx Builder, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
            builder.build_right_shift(x, y, false, "lshr").unwrap()
        }
        fn int_ashr<'ctx>(builder: &'ctx Builder, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
            builder.build_right_shift(x, y, true, "ashr").unwrap()
        }
        let context = Context::create();
        let ops: Vec<(&str, for<'a> fn(&'a Builder<'a>, IntValue<'a>, IntValue<'a>) -> IntValue<'a>)> = vec![("add", int_add), ("sub", int_sub), ("mul", int_mul), ("div", int_div), ("rem", int_rem), ("and", int_and), ("or", int_or), ("xor", int_xor), ("shl", int_shl), ("lshr", int_lshr), ("ashr", int_ashr)];
        let types = vec![context.i8_type(), context.i16_type(), context.i32_type(), context.i64_type()];

        let mut stencils = BTreeMap::new();

        for (name, op) in ops {
            for ty in types.iter() {
                let module = context.create_module("stencil");
                let codegen = CodeGen {
                    context: &context,
                    module,
                    builder: context.create_builder()
                };

                let context_const = Context::create();
                let module_const = context.create_module("stencil-const");
                let codegen_const = CodeGen {
                    context: &context_const,
                    module: module_const,
                    builder: context_const.create_builder()
                };


                let stencil: Stencil = codegen.compile_int_arith(name, *ty, op);
                let const_stencil = codegen_const.compile_const_int_arith(name, *ty, op);
                stencils.insert(stencil.name.clone(), stencil);
                stencils.insert(const_stencil.name.clone(), const_stencil);
            }
        }

        stencils
    }

    fn compile_int_arith(&self, name: &str, op_type: IntType<'ctx>, perform_op: for<'a> fn(&'a Builder<'a>, IntValue<'a>, IntValue<'a>) -> IntValue<'a>) -> Stencil {
        let base_type = op_type;
        let i8_type = self.context.i8_type();
        let i8_ptr_type = i8_type.ptr_type(AddressSpace::default());

        let i8_array_type = i8_type.array_type(1048576);
        let fn_type = base_type.fn_type(&[i8_ptr_type.into(), base_type.into(), base_type.into()], false);
        let tail_fn_type = base_type.fn_type(&[i8_ptr_type.into(), base_type.into()], false);
        let function = self.module.add_function("stencil", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");

        let tailcallfun = self.module.add_function("tailcall", tail_fn_type, Some(Linkage::External));
        tailcallfun.set_call_conventions(inkwell::llvm_sys::LLVMCallConv::LLVMGHCCallConv as u32);

        function.set_call_conventions(inkwell::llvm_sys::LLVMCallConv::LLVMGHCCallConv as u32);

        Target::initialize_native(&InitializationConfig::default()).unwrap();

        self.builder.position_at_end(basic_block);

        let global = self.module.add_global(i8_array_type, Some(AddressSpace::default()), "PH1");
        global.set_linkage(Linkage::External);
        global.set_alignment(1);
    
        let stackptr = function.get_nth_param(0).unwrap().into_pointer_value();

        let x = function.get_nth_param(1).unwrap().into_int_value();
        let y = function.get_nth_param(2).unwrap().into_int_value();

        // Bitcast the integer to a double

        let sum = perform_op(&self.builder, x, y);
        
        // Write sum2 onto the stack

        let tc = self.builder.build_call(tailcallfun, &[stackptr.into(), sum.into()], "tailcall").unwrap();

        tc.set_tail_call(true);
        tc.set_call_convention(inkwell::llvm_sys::LLVMCallConv::LLVMGHCCallConv as u32);

        self.builder.build_return(None).unwrap();
        
        // Print out the LLVM IR
        println!("{}", self.module.print_to_string().to_string());

        let elf = self.compile();
    
        let mut stencil = get_stencil(&format!("{}_{}", base_type.print_to_string().to_string(), name), elf.as_slice());

        // remove the last 12 bytes from the code

        stencil.code.truncate(stencil.code.len() - 12);

        stencil
    }

    fn compile_const_int_arith(&self, name: &str, op_type: IntType<'ctx>, perform_op: for<'a> fn(&'a Builder<'a>, IntValue<'a>, IntValue<'a>) -> IntValue<'a>) -> Stencil {
        let base_type = op_type;
        let i8_type = self.context.i8_type();
        let i8_ptr_type = i8_type.ptr_type(AddressSpace::default());

        let i8_array_type = i8_type.array_type(1048576);
        let fn_type = base_type.fn_type(&[i8_ptr_type.into(), base_type.into()], false);
        let function = self.module.add_function("stencil", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");

        let tailcallfun = self.module.add_function("tailcall", fn_type, Some(Linkage::External));
        tailcallfun.set_call_conventions(inkwell::llvm_sys::LLVMCallConv::LLVMGHCCallConv as u32);

        function.set_call_conventions(inkwell::llvm_sys::LLVMCallConv::LLVMGHCCallConv as u32);

        Target::initialize_native(&InitializationConfig::default()).unwrap();

        self.builder.position_at_end(basic_block);

        let global = self.module.add_global(i8_array_type, Some(AddressSpace::default()), "PH1");
        global.set_linkage(Linkage::External);
        global.set_alignment(1);
    
        let stackptr = function.get_nth_param(0).unwrap().into_pointer_value();
        // Cast the pointer to an integer
        let ptr_as_int = self.builder.build_ptr_to_int(global.as_pointer_value(), base_type, "ptrtoint").unwrap();

        // Bitcast the integer to an integer
        let int_as_type = self.builder.build_bitcast(ptr_as_int, base_type, "bitcast").unwrap().into_int_value();

        let x = function.get_nth_param(1).unwrap().into_int_value();

        // Bitcast the integer to a double

        let result = perform_op(&self.builder, int_as_type, x);
        
        let tc = self.builder.build_call(tailcallfun, &[stackptr.into(), result.into()], "tailcall").unwrap();

        tc.set_tail_call(true);
        tc.set_call_convention(inkwell::llvm_sys::LLVMCallConv::LLVMGHCCallConv as u32);

        self.builder.build_return(None).unwrap();
        
        // Print out the LLVM IR
        println!("{}", self.module.print_to_string().to_string());

        let elf = self.compile();
    
        let mut stencil = get_stencil(&format!("{}_{}_const",  base_type.print_to_string().to_string(), name), elf.as_slice());

        // remove the last 12 bytes from the code

        stencil.code.truncate(stencil.code.len() - 12);

        stencil
    }


}

fn main() -> Result<(), Box<dyn Error>> {

    let context = Context::create();
    let module = context.create_module("ghccc-conv");
    let codegen = CodeGen {
        context: &context,
        module,
        builder: context.create_builder()
    };

    let ghc_stencil = codegen.compile_ghc_wrapper();


    let context = Context::create();
    let module = context.create_module("take");
    let codegen = CodeGen {
        context: &context,
        module,
        builder: context.create_builder()
    };

    // since we take the first value on the stack we don't need to patch
    let take_stencil = codegen.compile_take_stack();

    let context = Context::create();
    let module = context.create_module("put");
    let codegen = CodeGen {
        context: &context,
        module,
        builder: context.create_builder()
    };

    let put_stencil = codegen.compile_put_stack();

    let context = Context::create();
    let module = context.create_module("sum");
    let codegen = CodeGen {
        context: &context,
        module,
        builder: context.create_builder()
    };

    let int_arith_stencils = CodeGen::compile_all_int_arith();

    let sum_stencil = int_arith_stencils.get("i64_add_const").unwrap();

    let mult_stencil = int_arith_stencils.get("i64_mul_const").unwrap();

    // Patch the code with a value

    let mut code = take_stencil.code.clone();

    let mut sum1_code = sum_stencil.code.clone();


    let mut mult1_code = mult_stencil.code.clone();

    let holes_values = vec![100000000000u64.to_ne_bytes()];
    for (&ofs, val) in sum_stencil.holes.iter().zip(holes_values.iter()) {
        sum1_code[ofs..ofs + 8].copy_from_slice(val);
    }

    let holes_values = vec![2u64.to_ne_bytes()];
    for (&ofs, val) in sum_stencil.holes.iter().zip(holes_values.iter()) {
        mult1_code[ofs..ofs + 8].copy_from_slice(val);
    }

    // append the sum codes to the code
    code.extend_from_slice(&sum1_code);
    code.extend_from_slice(&mult1_code);

    //Add the put stack code

    code.extend_from_slice(&put_stencil.code);


    // mmap a memory region with read and execute permissions
    let mmap = unsafe {
        libc::mmap(
            std::ptr::null_mut(),
            code.len(),
            libc::PROT_READ | libc::PROT_WRITE | libc::PROT_EXEC,
            libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
            -1,
            0,
        )
    };

    let mut ghcc_code = ghc_stencil.code.clone();

    let holes_values = vec![(mmap as u64).to_ne_bytes()];
    for (&ofs, val) in ghc_stencil.holes.iter().zip(holes_values.iter()) {
        ghcc_code[ofs..ofs + 8].copy_from_slice(val);
    }

    // Copy over ghcc code

    let ghcc_fun = unsafe {
        libc::mmap(
            std::ptr::null_mut(),
            ghcc_code.len(),
            libc::PROT_READ | libc::PROT_WRITE | libc::PROT_EXEC,
            libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
            -1,
            0,
        )
    };

    unsafe {
        std::ptr::copy_nonoverlapping(ghcc_code.as_ptr(), ghcc_fun as *mut u8, ghcc_code.len());
    }

    // Write to the file too

    // Seek to the desired position in the file
    /*let mut file =  OpenOptions::new()
    .read(true)
    .write(true)
    .open("sum.o").unwrap();
    file.seek(SeekFrom::Start(offset as u64)).unwrap();

    // Write the data to the specified range
    file.write_all(code.as_slice()).unwrap();*/


    // copy the code to the memory region
    unsafe {
        std::ptr::copy_nonoverlapping(code.as_ptr(), mmap as *mut u8, code.len());
    }

    // Allocate stack space for our generated code
    let stack_space = unsafe {
        libc::mmap(
            std::ptr::null_mut(),
            0x1000, // 4kb
            libc::PROT_READ | libc::PROT_WRITE,
            libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
            -1,
            0,
        ) as *mut u8
    };

    // cast the memory region to a function pointer
    let f: extern "C" fn(*mut u8) = unsafe { std::mem::transmute(ghcc_fun) };

    // Write argument to the stack
    unsafe {
        std::ptr::write_unaligned(stack_space as *mut uintptr_t, 5);
    }

    // call the function with the stack pointer in RAX
    // This should work, however it requires inline asm and it requires us to use 
    // a "root stencil" that unpacks all of our arguments from our custom stack into
    // the registers that the other stencils expect but that is only done once per 
    // call so it should be fine
    f(stack_space);
    // get the result from the stack;

    let result = unsafe {
        let value = std::ptr::read_unaligned(stack_space as *const uintptr_t);
        value
    };

    println!("Result: {}", result);
    assert_eq!(result, 200000000010);

    // unmap the memory region
    unsafe {
        libc::munmap(mmap, code.len());
        libc::munmap(stack_space as *mut libc::c_void, 0x1000);
    }

    Ok(())
}