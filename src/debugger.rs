use crate::opcodes::{
    Instruction,
    Opcode,
};
use crate::Cpu;
use crate::bus::Mem;
use std::io::{stdin, stdout, Write};



#[derive(Clone, Copy)]
enum DebugCommand<'a> {
    RepeatLastCommand,
    Next(usize),
    Continue,
    Break(u16),
    Watch(u16),
    WatchReg(&'a str),
    Info,
    Delete(Option<usize>),
}

fn read_debugger_command() -> DebugCommand<'static> {
    print!("(gbdb) ");
    stdout().flush().expect("failed to flush stdout");
    let mut input = String::new();
    stdin().read_line(&mut input).expect("Failed to read input string");

    let mut words = input.trim().split_whitespace();
    match words.next() {
        None => DebugCommand::RepeatLastCommand,
        Some("n") | Some("next") => {
            match words.next() {
                None => DebugCommand::Next(1),
                Some(n) => {
                    match n.parse() {
                        Ok(n) => DebugCommand::Next(n),
                        Err(e) => {
                            println!("Misformatted next command: {}", e);
                            read_debugger_command()
                        }
                    }
                }
            }
        },
        Some("c") | Some("continue") => {
            DebugCommand::Continue
        },
        Some("b") | Some("break") | Some("breakpoint") => {
            match words.next() {
                None => {
                    println!("breakpoint not specified");
                    read_debugger_command()
                }
                Some(n) => {
                    match u16::from_str_radix(n, 16) {
                        Ok(n) => DebugCommand::Break(n),
                        Err(e) => {
                            println!("Misformatted breakpoint: {}", e);
                            read_debugger_command()
                        }
                    }
                }
            }
        }
        Some("d") | Some("del") | Some("delete") => {
            match words.next() {
                None => {
                    DebugCommand::Delete(None)
                }
                Some(n) => {
                    match n.parse() {
                        Ok(n) => DebugCommand::Delete(Some(n)),
                        Err(e) => {
                            println!("Misformatted delete command: {}", e);
                            read_debugger_command()
                        }
                    }
                }
            }
        }
        Some("w") | Some("watch") | Some("watchpoint") => {
            match words.next() {
                None => {
                    println!("watchpoint not specified");
                    read_debugger_command()
                }
                Some("SP") | Some("sp") => { DebugCommand::WatchReg("SP") }
                Some(n) => {
                    match u16::from_str_radix(n, 16) {
                        Ok(n) => DebugCommand::Watch(n),
                        Err(e) => {
                            println!("Misformatted watchpoint: {}", e);
                            read_debugger_command()
                        }
                    }
                }
            }
        }
        Some("i") | Some("info") => {
            match words.next() {
                None => { DebugCommand::Info },
                Some(_) => {
                    println!("Info command takes no arguments (for now)");
                    read_debugger_command()
                }
            }
        }
        Some(unknown) => { 
            println!("Unrecognized command: {}", unknown); 
            read_debugger_command() 
        }
    }

}

enum Breakpoint {
    Instr(u16),
    Mem(u16, u8),
    SP(u16),
    Deleted,
}


pub fn run_debugger(mut cpu: Cpu) {
    // Use the debugger, without graphical display for now
    let mut last_command = DebugCommand::Next(1);
    let mut next_counter: Option<usize> = Some(0);
    let mut breakpoints: Vec<Breakpoint> = Vec::new();
    let mut last_dissembly: Vec<(u16,Instruction)> = cpu.disassemble(cpu.pc, 10);
    let mut stack_start: u16 = 0xfffe;
    let mut old_sp: u16 = 0xfffe;

    cpu.run_with_callback(move |cpu: &mut Cpu| {

        let mut should_break = false;
        let mut break_reason: Vec<String> = vec![];
        match next_counter {
            Some(0) => {
                should_break = true;
                next_counter = None;
            }
            Some(n) => {
                next_counter = Some(n - 1);
            }
            None => { }
        }

        for (i,b) in breakpoints.iter_mut().enumerate() {
            match b {
                Breakpoint::Instr(addr) => {
                    if cpu.pc == *addr {
                        should_break = true;
                        break_reason.push(format!("Breakpoint {} at {:04x}.", i, *addr));
                    }
                },
                Breakpoint::Mem(addr, val) => {
                    if cpu.mem_read(*addr) != *val {
                        should_break = true;
                        break_reason.push(format!("Watchpoint {} at [{:04x}] (old = {:02x}, cur = {:02x})",
                                                  i, addr, *val, cpu.mem_read(*addr)));
                        *val = cpu.mem_read(*addr);
                    }
                }
                Breakpoint::SP(val) => {
                    if cpu.sp != *val {
                        should_break = true;
                        break_reason.push(format!("Watchpoint {} of SP (old = {:04x}, cur = {:04x}",
                                                  i, *val, cpu.sp));
                        *val = cpu.sp;
                    }
                }
                Breakpoint::Deleted => { }
            }
        }
        if !should_break {
            return;
        }

        print!("{esc}c", esc = 27 as char); // clear screen

        // Determine a reasonable range of instructions to display
        let mut dissembly: Vec<(u16,Instruction)> = cpu.disassemble(cpu.pc, 10);
        for i in 0..10 {
            if cpu.pc == last_dissembly[i].0 {
                match i {
                    0..=6  => { dissembly = last_dissembly.clone(); }
                    7..=9 => { dissembly = cpu.disassemble(last_dissembly[i - 6].0, 10); }
                    _ => { unreachable!(); }
                }
                break;
            }
        }
        last_dissembly = dissembly.clone();
        // Determine a good guess for the start of stack
        if cpu.sp != old_sp.wrapping_add(2) && cpu.sp != old_sp.wrapping_sub(2) && cpu.sp != old_sp {
            stack_start = cpu.sp;
        }
        old_sp = cpu.sp;


        let registers: Vec<String> = cpu.display_regs_and_flags(10);
        let mut more_info = vec![String::from(""); 10];
        more_info[0] = format!("n_cycles: {}", cpu.n_cycles);
        more_info[1] = format!("n_instrs: {}", cpu.n_instrs);
        let mut j = 3;
        for b in breakpoints.iter() {
            if let Breakpoint::Mem(addr, _) = b {
                more_info[j] = format!("{:04x}: {:02x}", *addr, cpu.mem_read(*addr));
                j += 1;
            }
        }

        for i in 0..10usize {
            let (pc,instr) = dissembly[i];
            let sp = stack_start - 2 * (10 - 1 - i as u16);
            let pc_arrow = if pc == cpu.pc { "PC->" } else { "" };
            let sp_arrow = if sp == cpu.sp { "SP->" } else { "" };

            let mut instr_str = instr.to_string();
            match instr.opcode {
                Opcode::JumpRelative(e) | Opcode::JumpCondRelative(_,e) => { 
                    instr_str.push_str(&format!("=${:04x}", 2 + pc as i32 + e as i32))
                },
                _ => { }
            }
            print!("{:4}{:04x}: {:20}", 
                     pc_arrow, pc, instr_str);
            print!("{:4}{:04x}: {:04x}    ",
                     sp_arrow, sp, cpu.mem_read16(sp));
            println!("{:15}{}", registers[i], more_info[i]);
        }
        for line in break_reason {
            println!("{}", line);
        }
        
        loop {
            let command =  read_debugger_command();

            let command_to_run = match command {
                DebugCommand::RepeatLastCommand => { last_command },
                _ => { last_command = command; command },
            };

            use DebugCommand::*;
            match command_to_run {
                Next(n_repeats) => { 
                    next_counter = Some(n_repeats - 1);
                    return;
                },
                Continue => { 
                    next_counter = None;
                    return;
                }
                Break(addr) => { 
                    breakpoints.push(Breakpoint::Instr(addr));
                    println!("Breakpoint {}: 0x{:04x}.", breakpoints.len(), addr);
                }
                Watch(addr) => { 
                    breakpoints.push(Breakpoint::Mem(addr, cpu.mem_read(addr)));
                    println!("Watchpoint {}: [{:04x}] (cur = {:02x}).",
                             breakpoints.len(), addr, cpu.mem_read(addr));
                }
                WatchReg("SP") => {
                    breakpoints.push(Breakpoint::SP(cpu.sp));
                    println!("Watchpoint {}: SP (cur = {:02x}).",
                              breakpoints.len(), cpu.sp);
                }
                WatchReg(_) => {
                    println!("Misformatted watchpoint command.")
                }
                Delete(Some(n)) => { 
                    if n >= breakpoints.len() {
                        println!("Breakpoint {} not found.", n);
                        continue;
                    }
                    breakpoints[n] = Breakpoint::Deleted;
                    println!("Deleted breakpoint {}.", n);
                }
                Info => {
                    if breakpoints.len() == 0 {
                        println!("No breakpoints");
                        continue;
                    }
                    for (i,b) in breakpoints.iter().enumerate() {
                        match b {
                            Breakpoint::Instr(addr) => {
                                println!("Breakpoint {}: {:04x}", i, addr);
                            },
                            Breakpoint::Mem(addr, _) => {
                                println!("Watchpoint {}: [{:04x}] (cur = {:02x}).", i, addr, cpu.mem_read(*addr));
                            }
                            Breakpoint::SP(_) => {
                                println!("Watchpoint {}: SP (cur = {:02x}).", i, cpu.sp);
                            }
                            Breakpoint::Deleted => { }
                        }
                    }
                }
                Delete(None) => {
                    breakpoints.clear();
                    println!("All breakpoints deleted.");
                }
                RepeatLastCommand => { unreachable!(); },
            }
        }
    });
}
