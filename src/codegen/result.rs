// TODO: Once we go beyond basic arithmetic expressions we should have our own IR
//       We should also have a way to represent/address values so that we can insert
//       put/take instructions automatically and so that we can also map the same logic to LLVM IR

use std::os::raw::c_void;

use inkwell::execution_engine::JitFunction;

use super::llvm::stencils::{Stencil, RelocType};

type JITFunctionType = unsafe extern "C" fn(*const u8) -> *mut u8;

pub struct CodeGenResult<'ctx> {
    pub code: GeneratedCode,
    pub llvm_module: inkwell::module::Module<'ctx>,
}

impl<'ctx> CodeGenResult<'ctx> {

    pub fn compile_llvm(&self, opt_level: inkwell::OptimizationLevel) -> LLVMGeneratedCode<'ctx> {
        let exec_engine = self.llvm_module.clone().create_jit_execution_engine(opt_level).unwrap();
        let func = unsafe { exec_engine.get_function::<JITFunctionType>("main").unwrap() };
        LLVMGeneratedCode {
            func
        }
    }
}

pub struct LLVMGeneratedCode<'ctx> {
    func: JitFunction<'ctx, JITFunctionType>
}

impl<'ctx> LLVMGeneratedCode<'ctx> {

    pub fn call(&self, args: &[usize]) -> *mut u8 {
        unsafe{ self.func.call(args.as_ptr() as *const u8) }
    }
}

pub struct GeneratedCode {
    pub stack: *mut u8,
    pub code: *const c_void,
    pub code_len: usize,
    pub ghcc_code: *const c_void
}

impl GeneratedCode {

    pub fn new(stack_size: usize, wrapper_stencil: &Stencil, code: &[u8]) -> Self {

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

        let mut ghcc_code = wrapper_stencil.code.clone();

        let holes_values = vec![(mmap as u64).to_ne_bytes()];
        debug_assert_eq!(holes_values.len(), 1);
        for (&reloc, val) in wrapper_stencil.holes.iter().zip(holes_values.iter()) {
            debug_assert_eq!(reloc.reloc_type, RelocType::Abs64Fun);
            ghcc_code[reloc.offset..reloc.offset + 8].copy_from_slice(val);
        }

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

        // Allocate stack space for our generated code
        // TODO: We could (and maybe should) also use the actual stack for this
        let stack_space = unsafe {
            libc::mmap(
                std::ptr::null_mut(),
                stack_size,
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
                -1,
                0,
            ) as *mut u8
        };
    
        unsafe {
            std::ptr::copy_nonoverlapping(ghcc_code.as_ptr(), ghcc_fun as *mut u8, ghcc_code.len());
        }
    
        // copy the code to the memory region
        unsafe {
            std::ptr::copy_nonoverlapping(code.as_ptr(), mmap as *mut u8, code.len());
        }

        Self {
            stack: stack_space,
            code: mmap,
            code_len: code.len(),
            ghcc_code: ghcc_fun,
        }
    }
    
    pub fn call(&self, args: &[usize]) -> *mut u8 {
            // cast the memory region to a function pointer
        let f: JITFunctionType = unsafe { std::mem::transmute(self.ghcc_code) };

        // Copy args to the stack
        for (i, item) in args.iter().enumerate() {
            unsafe {
                std::ptr::write_unaligned((self.stack as *mut usize).offset(i as isize), *item);
            }
        }

        // call the function with the stack pointer in RAX
        // This should work, however it requires inline asm and it requires us to use 
        // a "root stencil" that unpacks all of our arguments from our custom stack into
        // the registers that the other stencils expect but that is only done once per 
        // call so it should be fine
        unsafe { f(self.stack) }
    }

    // TODO: We have partial support for having 

}
impl Drop for GeneratedCode {
    fn drop(&mut self) {
        unsafe {
            libc::munmap(self.code as *mut libc::c_void, 0x1000);
            libc::munmap(self.ghcc_code as *mut libc::c_void, 0x1000);
            libc::munmap(self.stack as *mut libc::c_void, 0x1000);
        }
    }
}
