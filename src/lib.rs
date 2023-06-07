pub mod cpu;
pub mod bus;
pub mod opcodes;

pub use cpu::Cpu;
pub use bus::Mem;

use crate::opcodes::Instruction;
use crate::cpu::Flags;

use std::fmt;
use std::io::BufReader;
use std::fs::File;

use serde_json::{Value};
use serde::{Deserialize, Serialize};
use itertools::Itertools;

#[derive(Serialize, Deserialize, Debug)]
struct CpuStateNoIE {
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
struct CpuState {
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

impl CpuState {
    fn from_cpu(cpu: &Cpu) -> CpuState {
        let mut ram = Vec::<Vec<u16>>::new();
        let mut last_addr: i32 = -1;
        for addr in cpu.bus.dirty_addrs().iter().sorted() {
            let val = cpu.mem_read(*addr);
            if val != 0 && *addr as i32 != last_addr {
                ram.push(vec![*addr, val as u16]);
                last_addr = *addr as i32;
            }
        }
        CpuState {
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

    fn without_ram_zeros(&self) -> CpuState {
        let mut ret = self.clone();
        ret.ram.retain(|v| { v[1] != 0 } );
        ret
    }
}

impl fmt::Display for CpuState {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "BC={:02x}{:02x}   ", self.b, self.c)?;
        write!(f, "DE={:02x}{:02x}   ", self.d, self.e)?;
        write!(f, "HL={:02x}{:02x}   ", self.h, self.l)?;
        write!(f, "A={:02x}={:08b}  ", self.a, self.a)?;
        write!(f, "F ={}{}{}{}   ", 
               if self.f & (1 << 7) != 0 {"Z"} else {"_"},
               if self.f & (1 << 6) != 0 {"N"} else {"_"},
               if self.f & (1 << 5) != 0 {"H"} else {"_"},
               if self.f & (1 << 4) != 0 {"C"} else {"_"})?;
        write!(f, "PC={:04x}   ", self.pc)?;
        write!(f, "SP={:04x}   ", self.sp)?;
        write!(f, "IME={}  ", self.ime)?;
        for v in self.ram.iter() {
            write!(f, "[{:04x}]={:02x} ", v[0], v[1])?;
        }
        Ok(())
    }
}


#[derive(Serialize, Deserialize, Debug)]
struct Test {
    name: String,
    initial: CpuStateNoIE,
    r#final: CpuState,
    cycles: Vec<Value>,
}

fn run_test(test: &Test, debug: u8) {
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
                                    || {cpu.mem_read(cpu.pc.wrapping_add(1))},
                                    || {cpu.mem_read(cpu.pc.wrapping_add(2))});

    let initial = CpuState::from_cpu(&cpu);
    let test_final = test.r#final.without_ram_zeros();

    if debug >= 2 {
        println!("Test {}: ({})", test.name, instr);
        println!("Initial:  {}", initial);
        println!("Expected: {}", test_final);
    }

    cpu.execute_instruction(instr);

    let cpu_final = CpuState::from_cpu(&cpu);

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
        println!("Initial:  {}", initial);
        println!("Expected: {}", test_final);
        println!("Found:    {}", cpu_final);
        std::process::exit(1);
    }
}

fn test_opcode(opcode: &str, debug: u8) {
    let fname = format!("moo-tests/{}.json", opcode);
    match File::open(fname) {
        Ok(f) => {
            let reader = BufReader::new(f);
            let tests: Vec<Test> = serde_json::from_reader(reader).expect("Json parse failed");

            for test in tests.iter() {
                run_test(test, debug);
            }
            println!("Opcode {}: all tests passed.", opcode);
        }
        Err(_) => {
            println!("Opcode {}: no test file found.", opcode);
        }
    }
}

pub fn test_moo(opcode: &Option<String>, debug: u8) {
    match opcode {
        Some(opcode) => { test_opcode(opcode, debug); }
        None => {
            for opcode in 0..256 {
                if opcode == 0x27 { // DAA
                    println!("Skipping opcode 0x27 (DAA)");
                    continue;
                }
                if opcode == 0xcb {
                    for byte2 in 0..256 {
                        test_opcode(&format!("cb {:02x}", byte2), debug);
                    }
                }
                test_opcode(&format!("{:02x}", opcode), debug)
            }
        }
    }
}
