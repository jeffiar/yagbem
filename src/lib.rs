pub mod cpu;
pub mod bus;
pub mod opcodes;

pub use cpu::Cpu;
pub use bus::Mem;

use crate::opcodes::Instruction;
use crate::cpu::Flags;

use std::io::BufReader;
use std::fs::File;
use serde_json::{Value};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug)]
struct CpuStateInit {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    f: u8,
    h: u8,
    l: u8,
    ime: u8,
    pc: u16,
    sp: u16,
    ie: u8,
    ram: Vec<Vec<u16>>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
struct CpuStateFinal {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    f: u8,
    h: u8,
    l: u8,
    ime: u8,
    pc: u16,
    sp: u16,
    ram: Vec<Vec<u16>>,
}

impl CpuStateFinal {
    fn from_cpu(cpu: &Cpu) -> CpuStateFinal {
        let mut ram = Vec::<Vec<u16>>::new();
        for addr in 0x0000..0xffff {
            let val = cpu.mem_read(addr);
            if val != 0 {
                ram.push(vec![addr, val as u16]);
            }
        }
        CpuStateFinal {
            a: cpu.a,
            b: cpu.b,
            c: cpu.c,
            d: cpu.d,
            e: cpu.e,
            f: cpu.flags.bits(),
            h: cpu.h,
            l: cpu.l,
            ime: cpu.interrupt_master_enable as u8,
            pc: cpu.pc,
            sp: cpu.sp,
            ram: ram,
        }
    }

    fn without_ram_zeros(&self) -> CpuStateFinal {
        let mut ret = self.clone();
        ret.ram.retain(|v| { v[1] != 0 } );
        ret
    }
}


#[derive(Serialize, Deserialize, Debug)]
struct Test {
    name: String,
    initial: CpuStateInit,
    r#final: CpuStateFinal,
    cycles: Vec<Value>,
}

fn run_test(test: &Test) {
    // Set up processor
    let mut cpu = Cpu {
        a : test.initial.a,
        b : test.initial.b,
        c : test.initial.c,
        d : test.initial.d,
        e : test.initial.e,
        h : test.initial.h,
        l : test.initial.l,
        flags : Flags::from_bits(test.initial.f).expect("Unexpected flag"),
        pc : test.initial.pc,
        sp : test.initial.sp,
        interrupt_master_enable : test.initial.ime != 0,
        running: true,
        n_cycles: 0,
        n_instrs: 0,
        bus: bus::Bus::new(),
    };

    for pair in test.initial.ram.iter() {
        let addr = pair[0];
        let val = pair[1];
        cpu.mem_write(addr, val as u8);
    }

    let instr = Instruction::decode(cpu.mem_read(cpu.pc),
                                    || {cpu.mem_read(cpu.pc + 1)},
                                    || {cpu.mem_read(cpu.pc + 2)});

    // println!("{}", test.name);
    // println!("{:?}", test.initial);
    // println!("{:?}", test.r#final);

    cpu.execute_instruction(instr);

    let cpu_final = CpuStateFinal::from_cpu(&cpu);
    let test_final = test.r#final.without_ram_zeros();
    let mut result: Result<(),String> = Ok(());

    if cpu.n_cycles as usize != test.cycles.len() * 4 {
        result = Err(format!("cycle count does not match (expected {}, found {})", 
                             test.cycles.len() * 4, 
                             cpu.n_cycles));
    }

    if cpu_final != test_final {
        result = Err(String::from("final state does not match"));
    }

    if let Err(reason) = result {
        println!("Test failed: {} ({})", test.name, instr);
        println!("Reason: {}", reason);
        println!("");
        println!(" {:?}", test.initial);
        println!("{:?}", test_final);
        println!("{:?}", cpu_final);
        std::process::exit(1);
    }
}

fn test_opcode(opcode: &str) {
    let fname = format!("moo-tests/{}.json", opcode);
    let f = File::open(fname).expect("Could not find test file");
    let reader = BufReader::new(f);
    let tests: Vec<Test> = serde_json::from_reader(reader).expect("Json parse failed");

    for test in tests.iter() {
        run_test(test);
    }
    println!("Opcode {}: all tests passed.", opcode);
}

pub fn test_moo(opcode: &Option<String>) {
    match opcode {
        Some(opcode) => { test_opcode(opcode); }
        None => {
            for opcode in 0..256 {
                test_opcode(&format!("{:02x}", opcode))
            }
        }
    }
}
