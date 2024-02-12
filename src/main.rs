use goblin::elf::Elf;
use inkwell::builder::Builder;
use inkwell::comdat::Comdat;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::{Linkage, Module};
use inkwell::passes::{PassBuilderOptions, PassManager};
use inkwell::targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine};
use inkwell::values::{AsValueRef, UnnamedAddress};
use inkwell::{AddressSpace, OptimizationLevel};
use libc::uintptr_t;

use std::arch::asm;
use std::borrow::Borrow;
use std::error::Error;
use std::fs::{File, OpenOptions};
use std::io::{Seek, SeekFrom, Write};
use std::path::Path;

/*

    We need equivalents for these in the generated code:

    extern "C" void __pochivm_fast_interp_dynamic_specialization_musttail_boilerplate_function_placeholder_ ## ordinal();             \
    extern "C" void __pochivm_fast_interp_dynamic_specialization_notail_boilerplate_function_placeholder_ ## ordinal();               \
    extern char __pochivm_fast_interp_dynamic_specialization_data_placeholder_ ## ordinal [1048576] __attribute__ ((__aligned__(1))); \
    extern "C" void __pochivm_fast_interp_dynamic_specialization_aotc_cpp_function_placeholder_ ## ordinal();

*/

type fn_type = extern "C" fn(*mut u8);

// This is not ideal but maybe it seems to work
// GHC Calling convention seems to use r13 for the first argument 
// (https://github.com/llvm/llvm-project/blob/44d85c5b15bbf6226f442126735b764d81cbf6e3/llvm/lib/Target/X86/X86CallingConv.td#L712)
pub extern "C" fn call_ghc_c_func(f: fn_type, arg: *mut u8) {
    // save all calee saved registers in c calling convention to registers to the stack
    unsafe {
        asm!(
            "push rbx",
            "push rbp",
            "push rsp",
            "push r12",
            "push r13",
            "push r14",
            "push r15",
        );
        asm!(
            "mov r13, {arg}",
            "call {func}",
            func = in(reg) f,
            arg = in(reg) arg,
        );
    // %r estore all %r egisters from the stack
        asm!(
            "pop r15",
            "pop r14",
            "pop r13",
            "pop r12",
            "pop rsp",
            "pop rbp",
            "pop rbx",
        );
    }
}

struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>
}

impl<'ctx> CodeGen<'ctx> {

    fn compile_sum(&self) {
        let i64_type = self.context.i64_type();
        let i8_type = self.context.i8_type();
        let i8_ptr_type = i8_type.ptr_type(AddressSpace::default());

        let i8_array_type = i8_type.array_type(1048576);
        let fn_type = i64_type.fn_type(&[i8_ptr_type.into()], false);
        let function = self.module.add_function("sum", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");

        function.set_call_conventions(inkwell::llvm_sys::LLVMCallConv::LLVMGHCCallConv as u32);

        Target::initialize_native(&InitializationConfig::default()).unwrap();

        self.builder.position_at_end(basic_block);

        let global = self.module.add_global(i8_array_type, Some(AddressSpace::default()), "PH1");
        global.set_linkage(Linkage::External);
        global.set_alignment(1);

        let global2 = self.module.add_global(i8_array_type, Some(AddressSpace::default()), "PH2");
        global2.set_linkage(Linkage::External);
        global2.set_alignment(1);
    

        let stackptr = function.get_nth_param(0).unwrap().into_pointer_value();
        // Cast the pointer to an integer
        let ptr_as_int = self.builder.build_ptr_to_int(global.as_pointer_value(), i64_type, "ptrtoint").unwrap();

        // Bitcast the integer to a double
        let int_as_type = self.builder.build_bitcast(ptr_as_int, i64_type, "bitcast").unwrap().into_int_value();


        let ptr_as_int2 = self.builder.build_ptr_to_int(global2.as_pointer_value(), i64_type, "ptrtoint2").unwrap();

        // Bitcast the integer to a double
        let int_as_type2 = self.builder.build_bitcast(ptr_as_int2, i64_type, "bitcast2").unwrap().into_int_value();

        // Bitcast the integer to a double

        // Load argument from stack
        let x = self.builder.build_load(i64_type, stackptr, "x").unwrap().into_int_value();

        let sum = self.builder.build_int_add(x, int_as_type, "sum").unwrap();
        let sum2 = self.builder.build_int_mul(sum, int_as_type2, "sum2").unwrap();
        
        // Write sum2 onto the stack

        self.builder.build_store(stackptr, sum2).unwrap();

        self.builder.build_return(None);
        
        // Print out the LLVM IR
        println!("{}", self.module.print_to_string().to_string());

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
                CodeModel::JITDefault,
            )
            .unwrap();

        let passes: &[&str] = &[
            "instcombine",
            "reassociate",
            "gvn",
            "simplifycfg",
            // "basic-aa",
            "mem2reg",
        ];



        //target_machine.write_to_file(&self.module, FileType::Object, Path::new("sum.o")).unwrap();
        let res = target_machine.write_to_memory_buffer(&self.module, FileType::Object).unwrap();
        
        let gobj = goblin::elf::Elf::parse(&res.as_slice()).unwrap();


        println!("---------------------------");

        //println!("{:#?}", gobj);

        println!("---------------------------");

        // Print relocation record for "PH1" symbol

        let reltab = gobj.shdr_relocs;
        let strtab = gobj.strtab;
        let symtab = gobj.syms;

        let mut relocs = Vec::new();

        for rel in reltab {
            for reloc in rel.1.into_iter() {
                let sym = symtab.get(reloc.r_sym).unwrap();
                let name = &strtab.get_at(sym.st_name);
                if let Some(name) = name {
                    if name.starts_with("PH") || name == &"sum" {
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
                    for i in 0..size {
                        //print!("{:02x} ", res.as_slice()[offset + i]);
                        code = Some(&res.as_slice()[offset..offset + size]);
                    }
                }
            }
        }

        let mut code = code.unwrap().to_vec();

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

        // Patch the code with a value
    

        for (name, reloc) in relocs {
            if name.starts_with("PH") {
                let offset = reloc.r_offset as usize;
                let value = if name == "PH1" {
                    100000000000u64.to_ne_bytes()
                } else {
                    2u64.to_ne_bytes()
                };
                code[offset..offset + 8].copy_from_slice(&value);
            }
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
        let f: extern "C" fn(*mut u8) = unsafe { std::mem::transmute(mmap) };

        // Write argument to the stack
        unsafe {
            std::ptr::write_unaligned(stack_space as *mut uintptr_t, 10);
        }

        // call the function with the stack pointer in RAX
        // This should work, however it requires inline asm and it requires us to use 
        // 
        call_ghc_c_func(f, stack_space);
        // get the result from the stack;

        let result = unsafe {
            let value = std::ptr::read_unaligned(stack_space as *const uintptr_t);
            value
        };

        println!("Result: {}", result);
        assert_eq!(result, 200000000020);

        // unmap the memory region
        unsafe {
            libc::munmap(mmap, code.len());
            libc::munmap(stack_space as *mut libc::c_void, 0x1000);
        }

    }

}

fn main() -> Result<(), Box<dyn Error>> {
    let context = Context::create();
    let module = context.create_module("sum");
    let codegen = CodeGen {
        context: &context,
        module,
        builder: context.create_builder()
    };

    codegen.compile_sum();
    Ok(())
}