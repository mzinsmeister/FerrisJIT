use std::error::Error;

use libc::uintptr_t;

mod expr;
mod codegen;

use clap::Parser;
use csv::Reader;

/// Search for a pattern in a file and display the lines that contain it.
#[derive(Parser)]
struct Cli {
    /// The path to the file to read
    #[arg(short, long)]
    csv: Option<std::path::PathBuf>,
}


fn main() -> Result<(), Box<dyn Error>> {

    let args = Cli::parse();

    // Fill a test vector with 100_000_000 elements
    // If the user specified a csv (no header, comma separated) file we will read the data from there
    // otherwise we will generate a sequential range of numbers 0..10_000_000

    let test_data: Vec<Vec<u64>> = if let Some(csv_path) = args.csv {
        let mut rdr = Reader::from_path(csv_path)?;
        rdr.records()
            .map(|record| {
                let record = record.unwrap();
                let mut vec = Vec::new();
                for field in record.iter() {
                    vec.push(field.parse::<u64>().unwrap());
                }
                vec
            })
            .collect()
    } else {
        //(0..10_000_000u64).map(|n| vec![n]).collect()
        (0..10).map(|n| vec![n]).collect()
    };

    // Compile the stencil library
    let stencils = codegen::compile_all_stencils();

    let ghc_stencil = stencils.get("__GHC_CC-CONVERTER__").unwrap();

    let take_stencil = stencils.get("i64_take-stack").unwrap();
    let take_1_stencil = stencils.get("i64_take-1-stack").unwrap();
    let take_2_stencil = stencils.get("i64_take-2-stack").unwrap();
    let put_stencil = stencils.get("i64_put-stack").unwrap();
    let duplex_stencil = stencils.get("duplex").unwrap();

    let mut code = take_stencil.code.clone();

    code.extend_from_slice(&duplex_stencil.code);

    let sum_stencil = stencils.get("i64_add").unwrap();

    code.extend_from_slice(&sum_stencil.code);
    
    /*let sum_const_stencil = stencils.get("i64_add_const").unwrap();

    let mult_const_stencil = stencils.get("i64_mul_const").unwrap();

    // Patch the code with a value

    let mut sum1_code = sum_const_stencil.code.clone();


    let mut mult1_code = mult_stencil.code.clone();

    let holes_values = vec![100000000000u64.to_ne_bytes()];
    for (&ofs, val) in sum_const_stencil.holes.iter().zip(holes_values.iter()) {
        sum1_code[ofs..ofs + 8].copy_from_slice(val);
    }

    let holes_values = vec![2u64.to_ne_bytes()];
    for (&ofs, val) in mult_const_stencil.holes.iter().zip(holes_values.iter()) {
        mult1_code[ofs..ofs + 8].copy_from_slice(val);
    }

    // append the sum codes to the code
    code.extend_from_slice(&sum1_code);
    code.extend_from_slice(&mult1_code);*/

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

    for line in test_data.iter() {
        // Write argument to the stack
        for (i, item) in line.iter().enumerate() {
            unsafe {
                std::ptr::write_unaligned((stack_space as *mut u64).offset(i as isize), *item);
            }
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
    }

    // unmap the memory region
    unsafe {
        libc::munmap(ghcc_fun, ghcc_code.len());
        libc::munmap(mmap, code.len());
        libc::munmap(stack_space as *mut libc::c_void, 0x1000);
    }

    Ok(())
}