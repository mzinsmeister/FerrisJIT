use goblin::elf::Elf;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::{Linkage, Module};
use inkwell::passes::PassBuilderOptions;
use inkwell::targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine};
use inkwell::values::AsValueRef;
use inkwell::{AddressSpace, OptimizationLevel};

use std::borrow::Borrow;
use std::error::Error;
use std::path::Path;

/*

    We need equivalents for these in the generated code:

    extern "C" void __pochivm_fast_interp_dynamic_specialization_musttail_boilerplate_function_placeholder_ ## ordinal();             \
    extern "C" void __pochivm_fast_interp_dynamic_specialization_notail_boilerplate_function_placeholder_ ## ordinal();               \
    extern char __pochivm_fast_interp_dynamic_specialization_data_placeholder_ ## ordinal [1048576] __attribute__ ((__aligned__(1))); \
    extern "C" void __pochivm_fast_interp_dynamic_specialization_aotc_cpp_function_placeholder_ ## ordinal();

*/

struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>
}

impl<'ctx> CodeGen<'ctx> {

    fn compile_sum(&self) {
        let i64_type = self.context.i64_type();
        let i8_type = self.context.i8_type();
        let i8_array_type = i8_type.array_type(1048576);
        let fn_type = i64_type.fn_type(&[i64_type.into()], false);
        let function = self.module.add_function("sum", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");

        //function.set_call_conventions(inkwell::llvm_sys::LLVMCallConv::LLVMGHCCallConv as u32);

        Target::initialize_native(&InitializationConfig::default()).unwrap();

        self.builder.position_at_end(basic_block);

        let global = self.module.add_global(i8_array_type, Some(AddressSpace::default()), "PH1");
        global.set_linkage(Linkage::External);
        global.set_alignment(1);

        let x = function.get_nth_param(0).unwrap().into_int_value();
        // Cast the pointer to an integer
        let ptr_as_int = self.builder.build_ptr_to_int(global.as_pointer_value(), i64_type, "ptrtoint").unwrap();

        // Bitcast the integer to a double
        let int_as_type = self.builder.build_bitcast(ptr_as_int, i64_type, "bitcast").unwrap().into_int_value();


        // Bitcast the integer to a double

        let sum = self.builder.build_int_add(x, int_as_type, "sum").unwrap();
        self.builder.build_return(Some(&sum)).unwrap();

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
                CodeModel::Large,
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

        let mut ph_relocs = Vec::new();

        for rel in reltab {
            for reloc in rel.1.into_iter() {
                let sym = symtab.get(reloc.r_sym).unwrap();
                let name = &strtab.get_at(sym.st_name);
                if let Some(name) = name {
                    if name.starts_with("PH") {
                        println!("Relocation: {:?}: {:#?}", name, reloc);
                        ph_relocs.push((name.to_string(), reloc));
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
                    for i in 0..size {
                        //print!("{:02x} ", res.as_slice()[offset + i]);
                        code = Some(&res.as_slice()[offset..offset + size]);
                    }
                }
            }
        }

        let mut code = code.unwrap().to_vec();

        // Patch the code with a value

        for (_name, reloc) in ph_relocs {
            let offset = reloc.r_offset as usize;
            let value = 100000000000u64.to_ne_bytes();
            code[offset..offset + 8].copy_from_slice(&value);
        }

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

        // copy the code to the memory region
        unsafe {
            std::ptr::copy_nonoverlapping(code.as_ptr(), mmap as *mut u8, code.len());
        }

        // cast the memory region to a function pointer
        let f: extern "C" fn(u64) -> u64 = unsafe { std::mem::transmute(mmap) };

        // call the function
        let result = f(10);

        println!("Result: {}", result);
        assert_eq!(result, 100000000010);

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