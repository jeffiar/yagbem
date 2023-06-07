use crate::bus::{ Mem, Bus, };
use crate::opcodes::{
    Instruction,
    Opcode,
    OpReg8,
    OpReg16,
    Condition,
};
use bitflags::bitflags;

bitflags! {
    // #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
    pub struct Flags: u8 {
        const Z = 1 << 7;
        const N = 1 << 6;
        const H = 1 << 5;
        const C = 1 << 4;
    }
}

pub const PC_START: u16 = 0x0100;
pub const SP_START: u16 = 0xfffe;

pub struct Cpu {
    pub a: u8,
    pub b: u8,
    pub c: u8,
    pub d: u8,
    pub e: u8,
    pub h: u8,
    pub l: u8,
    pub flags: Flags,
    pub sp: u16,
    pub pc: u16,

    pub interrupt_master_enable: bool,
    pub running: bool,

    pub n_cycles: u64,
    pub n_instrs: u64,
    pub bus: Bus,
}

impl Mem for Cpu {
    fn mem_read(&self, addr: u16) -> u8 { 
        self.bus.mem_read(addr) 
    }

    fn mem_write(&mut self, addr: u16, val: u8) { 
        self.bus.mem_write(addr, val) 
    }

    fn mem_read_range(&self, start: u16, end: u16) -> &[u8] {
        self.bus.mem_read_range(start, end)
    }
}

fn add_relative(addr: u16, offset: i8) -> u16 {
    ((addr as i32) + (offset as i32)) as u16
}

impl Condition {
    fn matches(&self, flags: Flags) -> bool {
        match self {
            Condition::NZ => { !flags.contains(Flags::Z) },
            Condition::Z => { flags.contains(Flags::Z) },
            Condition::NC => { !flags.contains(Flags::C) },
            Condition::C => { flags.contains(Flags::C) },
        }
    }
}

impl Cpu {
    pub fn new() -> Self {
        Cpu {
            a: 0,  b: 0,  c: 0,  d: 0,
            e: 0,  h: 0,  l: 0,
            flags: Flags::empty(),
            pc: PC_START, 
            sp: SP_START,
            n_instrs: 0,
            n_cycles: 0,
            running: true,
            interrupt_master_enable: false,
            bus: Bus::new(),
        }
    }

    /// With "flat" memory without any mapping for memory access test purposes
    pub fn new_flat() -> Self {
        let cpu = Cpu::new();
        cpu
    }

    pub fn reset(&mut self) {
        // TODO read the manual for what this is supposed to do!
        self.a = 0;
        self.b = 0;
        self.c = 0;
        self.d = 0;
        self.e = 0;
        self.h = 0;
        self.l = 0;
        self.flags = Flags::empty();
        self.pc = PC_START;
        self.sp = SP_START;
        self.running = true;

        self.interrupt_master_enable = false;
        self.n_instrs = 0;
        self.n_cycles = 0;
        // self.bus.reset();
    }

    pub fn load_rom(&mut self, program: &[u8]) {
        self.bus.load(program, 0x0000);
    }

    pub fn run_instructions_and_halt(&mut self, program: &[u8]) {
        // self.reset();
        self.bus.load(program, self.pc);
        self.bus.mem_write(self.pc + program.len() as u16, 0x76); // add HALT
        self.run();
    }

    fn reg8_read(&self, reg: OpReg8) -> u8 {
        match reg {
            OpReg8::A => self.a,
            OpReg8::B => self.b,
            OpReg8::C => self.c,
            OpReg8::D => self.d,
            OpReg8::E => self.e,
            OpReg8::H => self.h,
            OpReg8::L => self.l,
            OpReg8::HLIndirect => self.mem_read((self.h as u16) << 8 | self.l as u16),
        }
    }

    fn reg8_write(&mut self, reg: OpReg8, val: u8) {
        match reg {
            OpReg8::A => { self.a = val },
            OpReg8::B => { self.b = val },
            OpReg8::C => { self.c = val },
            OpReg8::D => { self.d = val },
            OpReg8::E => { self.e = val },
            OpReg8::H => { self.h = val },
            OpReg8::L => { self.l = val },
            OpReg8::HLIndirect => self.mem_write((self.h as u16) << 8 | self.l as u16, val),
        }
    }

    fn reg16_read(&mut self, reg_pair: OpReg16) -> u16 {
        match reg_pair {
            OpReg16::BC => { (self.b as u16) << 8 | self.c as u16 },
            OpReg16::DE => { (self.d as u16) << 8 | self.e as u16 },
            OpReg16::HL => { (self.h as u16) << 8 | self.l as u16 },
            OpReg16::SP => { self.sp },
            OpReg16::AF => { (self.a as u16) << 8 | self.flags.bits() as u16 },
            OpReg16::HLInc => { 
                let hl = (self.h as u16) << 8 | self.l as u16;
                self.inc_pair(OpReg16::HL);
                hl
            },
            OpReg16::HLDec => {
                let hl = (self.h as u16) << 8 | self.l as u16;
                self.dec_pair(OpReg16::HL);
                hl
            },
        }
    }

    fn reg16_write(&mut self, reg_pair: OpReg16, val: u16) {
        let hi = (val >> 8) as u8;
        let lo = (val & 0xff) as u8;
        match reg_pair {
            OpReg16::BC => { self.b = hi;   self.c = lo; },
            OpReg16::DE => { self.d = hi;   self.e = lo; },
            OpReg16::HL => { self.h = hi;   self.l = lo; },
            OpReg16::SP => { self.sp = val; },
            OpReg16::AF => { self.a = hi;   self.flags.bits = lo; },
            OpReg16::HLInc => { unreachable!() },
            OpReg16::HLDec => { unreachable!() },
        }
    }

    // fn reg8_write_bit(&mut self, reg: OpReg8, bit: u8, val: bool) {
    //     // A little ugly, but I in case I want to keep a separate case for HLIndirect
    //     match reg {
    //         OpReg8::A => { if val {self.a |= (1 << bit)} else {self.a &= !(1 << bit)} }
    //         OpReg8::B => { if val {self.b |= (1 << bit)} else {self.b &= !(1 << bit)} }
    //         OpReg8::C => { if val {self.c |= (1 << bit)} else {self.c &= !(1 << bit)} }
    //         OpReg8::D => { if val {self.d |= (1 << bit)} else {self.d &= !(1 << bit)} }
    //         OpReg8::E => { if val {self.e |= (1 << bit)} else {self.e &= !(1 << bit)} }
    //         OpReg8::H => { if val {self.h |= (1 << bit)} else {self.h &= !(1 << bit)} }
    //         OpReg8::L => { if val {self.l |= (1 << bit)} else {self.l &= !(1 << bit)} }
    //         OpReg8::HLIndirect => {
    //             let hl = (self.h as u16) << 8 | self.l as u16;
    //             self.bus.mem_write_bit(hl, bit, val);
    //         }
    //     }
    // }

    fn add_instr(&mut self, op1: u8, op2: u8, destination: OpReg8, modify_carry: bool) {
        let sum16 = (op1 as u16).wrapping_add(op2 as u16);
        let sum8  = op1.wrapping_add(op2);
        let sum4  = (op1 & 0x0f).wrapping_add(op2 & 0x0f);
        self.reg8_write(destination, sum8);

        self.flags.set(Flags::Z, sum8 == 0);
        self.flags.set(Flags::N, false);
        self.flags.set(Flags::H, (sum4 & 0x10) != 0);
        if modify_carry {
            self.flags.set(Flags::C, (sum16 & 0x100) != 0);
        }
    }

    fn sub_instr(&mut self, op1: u8, op2: u8, destination: OpReg8, modify_carry: bool) {
        let diff = op1.wrapping_sub(op2);
        self.reg8_write(destination, diff);

        self.flags.set(Flags::Z, diff == 0);
        self.flags.set(Flags::N, true);
        self.flags.set(Flags::H, (op1 & 0x0f) < (op2 & 0x0f));
        if modify_carry {
            self.flags.set(Flags::C, op1 < op2);
        }
    }

    fn adc_instr(&mut self, val: u8) {
        if !self.flags.contains(Flags::C) {
            self.add_instr(self.a, val, OpReg8::A, true);
        } else {
            // I think the logic is to set the H and C flags if a carry happens 
            // in either of the two additions (op1 + op2 + 1).
            self.add_instr(self.a, 1, OpReg8::A, true);
            let flags = self.flags.clone();
            self.add_instr(self.a, val, OpReg8::A, true);
            self.flags |= flags;
        }
        self.flags.set(Flags::Z, self.a == 0);
    }

    fn sbc_instr(&mut self, val: u8) {
        if !self.flags.contains(Flags::C) {
            self.sub_instr(self.a, val, OpReg8::A, true);
        } else {
            // I think the logic is to set the H and C flags if a carry happens 
            // in either of the two subitions (op1 + op2 + 1).
            self.sub_instr(self.a, 1, OpReg8::A, true);
            let flags = self.flags.clone();
            self.sub_instr(self.a, val, OpReg8::A, true);
            self.flags |= flags;
        }
        self.flags.set(Flags::Z, self.a == 0);
    }

    fn and_instr(&mut self, val: u8) {
        self.a &= val;
        self.flags.set(Flags::Z, self.a == 0);
        self.flags.set(Flags::N, false);
        self.flags.set(Flags::H, true);
        self.flags.set(Flags::C, false);
    }

    fn xor_instr(&mut self, val: u8) {
        self.a ^= val;
        self.flags.set(Flags::Z, self.a == 0);
        self.flags.set(Flags::N | Flags::H | Flags::C, false);
    }

    fn or_instr(&mut self, val: u8) {
        self.a |= val;
        self.flags.set(Flags::Z, self.a == 0);
        self.flags.set(Flags::N | Flags::H | Flags::C, false);
    }

    fn cp_instr(&mut self, val: u8) {
        // This is identical to SUB except the results are thrown away.
        let op1 = self.a;
        let op2 = val;
        let diff = op1.wrapping_sub(op2);

        self.flags.set(Flags::Z, diff == 0);
        self.flags.set(Flags::N, true);
        self.flags.set(Flags::H, (op1 & 0x0f) < (op2 & 0x0f));
        self.flags.set(Flags::C, op1 < op2);
    }

    fn push_onto_stack(&mut self, val: u16) {
        self.sp = self.sp.wrapping_sub(2);
        self.mem_write16(self.sp, val);
    }

    fn pop_from_stack(&mut self) -> u16 {
        let val = self.mem_read16(self.sp);
        self.sp = self.sp.wrapping_add(2);
        val
    }

    fn inc_pair(&mut self, reg_pair: OpReg16) {
        let val = self.reg16_read(reg_pair);
        self.reg16_write(reg_pair, val.wrapping_add(1));
    }

    fn dec_pair(&mut self, reg_pair: OpReg16) {
        let val = self.reg16_read(reg_pair);
        self.reg16_write(reg_pair, val.wrapping_sub(1));
    }

    fn load_hl_sp_relative(&mut self, rel: i8) {
        // add lower 8 bits as an unsigned int
        let lo = (self.sp & 0xff) as u8;
        let hi = (self.sp >> 8) as u8;
        self.add_instr(lo, rel as u8, OpReg8::L, true);

        // carry or borrow into the upper 8 bits depending on result
        if (rel > 0) & self.flags.contains(Flags::C) {
            self.add_instr(hi, 1, OpReg8::H, true);
        } else if (rel < 0) & !self.flags.contains(Flags::C) {
            self.sub_instr(hi, 1, OpReg8::H, true);
        } else {
            self.h = hi;
            self.flags.remove(Flags::C | Flags::H);
        }

        self.flags.remove(Flags::Z | Flags::N);
    }

    fn set_flags_from_rot_shift(&mut self, val: u8) {
        self.flags.remove(Flags::H | Flags::N);
        self.flags.set(Flags::Z, val == 0);
    }

    pub fn run(&mut self) {
        self.run_with_callback(|_| {}, 2);
    }

    /// Run the CPU and run callback *before* each instruction executed
    pub fn run_with_callback<F>(&mut self, mut callback: F, debug: u8) 
    where F: FnMut(&mut Cpu)
    {
        self.running = true;
        while self.running {
            callback(self);

            let instr = Instruction::decode(self.mem_read(self.pc),
                                            || {self.mem_read(self.pc + 1)},
                                            || {self.mem_read(self.pc + 2)});
            if debug >= 2 {
                eprintln!("{:04x}: {:02x} {} {} {}", 
                          self.pc,
                          self.mem_read(self.pc),
                          if instr.length >= 2 { format!("{:02x}", self.mem_read(self.pc + 1)) } else { "  ".to_string() },
                          if instr.length == 3 { format!("{:02x}", self.mem_read(self.pc + 2)) } else { "  ".to_string() },
                          instr);
            }
            self.execute_instruction(instr);
        }
    }

    pub fn execute_instruction(&mut self, instr: Instruction) {
        self.pc = self.pc.wrapping_add(instr.length);
        self.n_cycles = self.n_cycles.wrapping_add(instr.cycles);
        self.n_instrs = self.n_instrs.wrapping_add(1);

        match instr.opcode {
            Opcode::NoOp => {}
            Opcode::Halt => { self.running = false; }
            Opcode::Stop => { self.running = false; }
            Opcode::Load8(reg, val) => { self.reg8_write(reg, val); }
            Opcode::LoadReg(dst, src) => {
                let val = self.reg8_read(src);
                self.reg8_write(dst, val);
            }
            Opcode::LoadIndirectImm(reg, addr) => { 
                let val = self.mem_read(addr);
                self.reg8_write(reg, val);
            }
            Opcode::LoadIndirect(reg, reg_pair) => { 
                let addr = self.reg16_read(reg_pair);
                let val = self.mem_read(addr);
                self.reg8_write(reg, val);
            }
            Opcode::StoreIndirectImm(addr, reg) => { 
                let val = self.reg8_read(reg);
                self.mem_write(addr, val);
            }
            Opcode::StoreIndirect(reg_pair, reg) => {
                let addr = self.reg16_read(reg_pair);
                let val = self.reg8_read(reg);
                self.mem_write(addr, val);
            }
            Opcode::LoadAFromIndC => {
                let addr = 0xff00 | self.c as u16;
                self.a = self.mem_read(addr);
            }
            Opcode::StoreAToIndC => {
                let addr = 0xff00 | self.c as u16;
                self.mem_write(addr, self.a);
            }
            Opcode::Load16(reg_pair, val) => {
                self.reg16_write(reg_pair, val);
            }
            Opcode::LoadSPHL => { 
                self.sp = self.reg16_read(OpReg16::HL);
            }
            Opcode::LoadHLSPRelative(rel) => {
                self.load_hl_sp_relative(rel);
            }
            Opcode::StoreSP(addr) => {
                self.mem_write16(addr, self.sp);
            }
            Opcode::Push(reg_pair) => {
                let val = self.reg16_read(reg_pair);
                self.push_onto_stack(val);
            }
            Opcode::Pop(reg_pair) => {
                let val = self.pop_from_stack();
                self.reg16_write(reg_pair, val);
            }

            Opcode::Add(reg) => { self.add_instr(self.a, self.reg8_read(reg), OpReg8::A, true); }
            Opcode::Adc(reg) => { self.adc_instr(self.reg8_read(reg)); }
            Opcode::Sub(reg) => { self.sub_instr(self.a, self.reg8_read(reg), OpReg8::A, true); }
            Opcode::Sbc(reg) => { self.sbc_instr(self.reg8_read(reg)); }
            Opcode::And(reg) => { self.and_instr(self.reg8_read(reg)); }
            Opcode::Xor(reg) => { self.xor_instr(self.reg8_read(reg)); }
            Opcode::Or(reg)  => { self.or_instr(self.reg8_read(reg)); }
            Opcode::Cp(reg)  => { self.cp_instr(self.reg8_read(reg)); }

            Opcode::AddImm(val) => { self.add_instr(self.a, val, OpReg8::A, true); }
            Opcode::AdcImm(val) => { self.adc_instr(val); }
            Opcode::SubImm(val) => { self.sub_instr(self.a, val, OpReg8::A, true); }
            Opcode::SbcImm(val) => { self.sbc_instr(val); }
            Opcode::AndImm(val) => { self.and_instr(val); }
            Opcode::XorImm(val) => { self.xor_instr(val); }
            Opcode::OrImm(val) => { self.or_instr(val); }
            Opcode::CpImm(val) => { self.cp_instr(val); }

            Opcode::Inc(reg) => { self.add_instr(self.reg8_read(reg), 1, reg, false); }
            Opcode::Dec(reg) => { self.sub_instr(self.reg8_read(reg), 1, reg, false); }
            Opcode::IncPair(reg_pair) => { self.inc_pair(reg_pair) }
            Opcode::DecPair(reg_pair) => { self.dec_pair(reg_pair) }
            Opcode::AddHL(reg_pair) => {
                let op1 = self.reg16_read(OpReg16::HL);
                let op2 = self.reg16_read(reg_pair);

                let sum32 = (op1 as u32).wrapping_add(op2 as u32);
                let sum16 = op1.wrapping_add(op2);
                let sum12 = (op1 & 0x0fff).wrapping_add(op2 & 0x0fff);

                self.flags.set(Flags::N, false);
                self.flags.set(Flags::H, (sum12 & 0x01000) != 0);
                self.flags.set(Flags::C, (sum32 & 0x10000) != 0);

                self.reg16_write(OpReg16::HL, sum16);
            }
            Opcode::AddSP(rel) => { 
                // A little hacky, but can optimize if it's too slow
                let (h,l) = (self.h, self.l);
                self.load_hl_sp_relative(rel);
                self.sp = (self.h as u16) << 8 | self.l as u16;
                self.h = h;
                self.l = l;
            }

            Opcode::RotateLeftCarry(reg) => {
                let val = self.reg8_read(reg);
                let new_val = val.rotate_left(1);
                self.reg8_write(reg, new_val);
                self.flags.set(Flags::C, (val & 0xf0) != 0);
                self.set_flags_from_rot_shift(new_val);
            }
            Opcode::RotateRightCarry(reg) => {
                let val = self.reg8_read(reg);
                let new_val = val.rotate_right(1);
                self.reg8_write(reg, new_val);
                self.flags.set(Flags::C, (val & 0x01) != 0);
                self.set_flags_from_rot_shift(new_val);
            }
            Opcode::RotateLeft(reg) => {
                let val = self.reg8_read(reg);
                let mut new_val = val.wrapping_shl(1);
                new_val |= self.flags.contains(Flags::C) as u8; // set bit 0 depending on old C flag
                self.reg8_write(reg, new_val);
                self.flags.set(Flags::C, (val & 0x80) != 0);
                self.set_flags_from_rot_shift(new_val);
            }
            Opcode::RotateRight(reg) => {
                let val = self.reg8_read(reg);
                // eprintln!("Before: {val:08b}, {:?}", self.flags);
                let mut new_val = val.wrapping_shr(1);
                new_val |= (self.flags.contains(Flags::C) as u8) << 7; // set bit 0 depending on old C flag
                self.reg8_write(reg, new_val);
                self.flags.set(Flags::C, (val & 0x01) != 0);
                self.set_flags_from_rot_shift(new_val);
                // eprintln!("After:  {new_val:08b}, {:?}", self.flags);
            }
            Opcode::ShiftLeftArithmetic(reg) => {
                let val = self.reg8_read(reg);
                let new_val = val.wrapping_shl(1);
                self.reg8_write(reg, new_val);
                self.flags.set(Flags::C, (val & 0x80) != 0);
                self.set_flags_from_rot_shift(new_val);
            }
            Opcode::ShiftRightArithmetic(reg) => {
                let val = self.reg8_read(reg);
                let new_val = (val as i8).wrapping_shr(1) as u8;
                self.reg8_write(reg, new_val);
                self.flags.set(Flags::C, (val & 0x01) != 0);
                self.set_flags_from_rot_shift(new_val);
            }
            Opcode::SwapNibbles(reg) => {
                let val = self.reg8_read(reg);
                let lo = val & 0x0f;
                let hi = val & 0xf0;
                let new_val = lo << 4 | hi >> 4;
                self.reg8_write(reg, new_val);
                self.flags.remove(Flags::C | Flags::H | Flags::N);
                self.flags.set(Flags::Z, new_val == 0);
            }
            Opcode::ShiftRightLogical(reg) => {
                let val = self.reg8_read(reg);
                let new_val = val.wrapping_shr(1);
                self.reg8_write(reg, new_val);
                self.flags.set(Flags::C, (val & 0x01) != 0);
                self.set_flags_from_rot_shift(new_val);
            }
            Opcode::RLCA => {
                self.a = self.a.rotate_left(1);
                self.flags.set(Flags::C, (self.a & 0x01) != 0);
                self.flags.remove(Flags::H | Flags::N | Flags::Z);
            }
            Opcode::RRCA => {
                self.a = self.a.rotate_right(1);
                self.flags.set(Flags::C, (self.a & 0x80) != 0);
                self.flags.remove(Flags::H | Flags::N | Flags::Z);
            }
            Opcode::RLA => {
                let val = self.a;
                let mut new_val = val.wrapping_shl(1);
                new_val |= self.flags.contains(Flags::C) as u8;
                self.a = new_val;
                self.flags.set(Flags::C, (val & 0x80) != 0);
                self.flags.remove(Flags::H | Flags::N | Flags::Z);
            }
            Opcode::RRA => {
                let val = self.a;
                let mut new_val = val.wrapping_shr(1);
                new_val |= (self.flags.contains(Flags::C) as u8) << 7; 
                self.a = new_val;
                self.flags.set(Flags::C, (val & 0x01) != 0);
                self.flags.remove(Flags::H | Flags::N | Flags::Z);
            }
            Opcode::Bit(bit, reg) => {
                let is_bit_set = (self.reg8_read(reg) & (1 << bit)) != 0;
                self.flags.set(Flags::Z, !is_bit_set);
                self.flags.set(Flags::N, false);
                self.flags.set(Flags::H, true);
            }
            Opcode::Reset(bit, reg) => {
                let mut val = self.reg8_read(reg);
                val &= !(1 << bit); // rust made an interesting choice for the bitwise NOT
                self.reg8_write(reg, val);
            }
            Opcode::Set(bit, reg)   => {
                let mut val = self.reg8_read(reg);
                val |= 1 << bit;
                self.reg8_write(reg, val);
            }

            Opcode::Jump(addr) => {
                self.pc = addr;
            }
            Opcode::JumpCond(condition, addr) => {
                if condition.matches(self.flags) {
                    self.pc = addr;
                    self.n_cycles += 4;
                } 
            }
            Opcode::JumpRelative(rel) => {
                self.pc = add_relative(self.pc, rel);
            }
            Opcode::JumpCondRelative(condition, rel) => { 
                if condition.matches(self.flags) {
                    self.pc = add_relative(self.pc, rel);
                    self.n_cycles += 4;
                }
            }
            Opcode::JumpHL => {
                self.pc = self.reg16_read(OpReg16::HL);
            }

            Opcode::Call(addr) => {
                self.push_onto_stack(self.pc);
                self.pc = addr;
            }
            Opcode::CallCond(condition, addr) => {
                if condition.matches(self.flags) {
                    self.push_onto_stack(self.pc);
                    self.pc = addr;
                    self.n_cycles += 12;
                }
            }
            Opcode::Return => {
                self.pc = self.pop_from_stack(); 
            }
            Opcode::ReturnFromInterrupt => {
                self.pc = self.pop_from_stack();
                self.interrupt_master_enable = true;
            }
            Opcode::ReturnCond(condition) => {
                if condition.matches(self.flags) {
                    self.pc = self.pop_from_stack();
                    self.n_cycles += 12;
                }
            }

            Opcode::DisableInterrupts => { self.interrupt_master_enable = false; }
            Opcode::EnableInterrupts => { self.interrupt_master_enable = true; }
            Opcode::DecimalAdjustAcc => { panic!("Unimplemented instruction DAA"); } // TODO
            Opcode::ComplementAcc => {
                self.a = !self.a;
                self.flags.insert(Flags::H | Flags::N);
            }
            Opcode::SetCarryFlag => {
                self.flags.insert(Flags::C);
                self.flags.remove(Flags::H | Flags::N);
            }
            Opcode::ComplementCarryFlag => {
                self.flags.toggle(Flags::C);
                self.flags.remove(Flags::H | Flags::N);
            }

            Opcode::NotImplemented(opcode) => { panic!("Unimplemented opcode: {opcode:02x}")}
        }
    }
}




#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reset() {
        let mut cpu = Cpu::new_flat();
        cpu.reg8_write(OpReg8::E, 0x40);
        cpu.mem_write(0x0d40, 0x50);
        assert_eq!(cpu.reg8_read(OpReg8::E), 0x40);
        // assert_eq!(cpu.mem_read(0xd40), 0x50);
        cpu.reset();
        assert_eq!(cpu.reg8_read(OpReg8::E), 0);
        // assert_eq!(cpu.mem_read(0xd40), 0);
    }


    #[test]
    fn load8() {
        let mut cpu = Cpu::new_flat();
        assert_eq!(cpu.h, 0);

        // LD   H,$c0
        // LD   L,$ee
        // LD   (HL),$56
        // HALT
        cpu.run_instructions_and_halt(&[0x26, 0xc0, 0x2e, 0xee, 0x36, 0x56]);
        assert_eq!(cpu.h, 0xc0);
        assert_eq!(cpu.l, 0xee);
        assert_eq!(cpu.mem_read(0xc0ee), 0x56);
    }

    #[test]
    fn load_reg_into_hlind() {
        let mut cpu = Cpu::new_flat();
        // LD   C,$23
        // LD   B,C
        // LD   H,$44
        // LD   (HL),C
        // HALT
        cpu.run_instructions_and_halt(&[0x0e, 0x23, 0x41, 0x26, 0x44, 0x71]);
        assert_eq!(cpu.c, 0x23);
        assert_eq!(cpu.b, 0x23);
        assert_eq!(cpu.h, 0x44);
        assert_eq!(cpu.mem_read(0x4400), 0x23);
    }

    #[test]
    fn load_reg() {
        let mut cpu = Cpu::new_flat();
        assert_eq!(cpu.h, 0);

        // LD   A,$33
        // LD   B,A     ; B = $33
        // LD   A,$55
        // LD   E,A     ; E = $55
        // LD   H,E     ; H = $55
        // HALT
        let code = [0x3e, 0x33, 0x47, 0x3e, 0x55, 0x5f, 0x63];
        cpu.run_instructions_and_halt(&code);
        assert_eq!(cpu.b, 0x33);
        assert_eq!(cpu.e, 0x55);
        assert_eq!(cpu.h, 0x55);
        assert_eq!(cpu.c, 0x00);
    }

    #[test]
    fn load_indirect_immediate_0xfa() {
        let mut cpu = Cpu::new_flat();
        assert_ne!(cpu.a, 0x45);
        cpu.mem_write(0x33a4, 0x45);
        cpu.run_instructions_and_halt(&[0xfa, 0xa4, 0x33]); //LD   A,($33a4)
        assert_eq!(cpu.a, 0x45);
        assert_eq!(cpu.n_cycles, 16 + 4);
    }

    #[test]
    fn load_indirect_0x0a() {
        let mut cpu = Cpu::new_flat();
        cpu.b = 0x55;
        cpu.c = 0xa4;
        cpu.mem_write(0x55a4, 0x69);
        cpu.run_instructions_and_halt(&[0x0a]); // LD   A,(BC)
        assert_eq!(cpu.a, 0x69);
        assert_eq!(cpu.n_cycles, 8 + 4);
    }

    #[test]
    fn store_indirect_immediate_0xea() {
        let mut cpu = Cpu::new_flat();
        cpu.a = 0x2f;
        cpu.run_instructions_and_halt(&[0xea, 0x52, 0xcc]); // LD   (DE),A
        assert_eq!(cpu.mem_read(0xcc52), 0x2f);
        assert_eq!(cpu.n_cycles, 16 + 4);
    }

    #[test]
    fn store_indirect_0x12() {
        let mut cpu = Cpu::new_flat();
        cpu.d = 0x11;
        cpu.e = 0x22;
        cpu.a = 0x42;
        cpu.run_instructions_and_halt(&[0x12]); // LD   (DE),A
        assert_eq!(cpu.mem_read(0x1122), 0x42);
        assert_eq!(cpu.mem_read(0x2211), 0x00);
        assert_eq!((cpu.d, cpu.e, cpu.a), (0x11, 0x22, 0x42));
        assert_eq!(cpu.n_cycles, 8 + 4);
    }

    #[test]
    fn load_indirect_zeropage_0xf2() {
        let mut cpu = Cpu::new_flat();
        cpu.c = 0xa4;
        cpu.mem_write(0xffa4, 0x69);
        cpu.run_instructions_and_halt(&[0xf2]); // LD   A,($ff00+C)
        assert_eq!(cpu.a, 0x69);
        assert_eq!(cpu.n_cycles, 8 + 4);
    }

    #[test]
    fn store_indirect_zeropage_0xe2() {
        let mut cpu = Cpu::new_flat();
        cpu.c = 0xcc;
        cpu.a = 0xaa;
        cpu.run_instructions_and_halt(&[0xe2]); // LD   ($ff00+C),A
        assert_eq!(cpu.mem_read(0xffcc), 0xaa);
        assert_eq!(cpu.n_cycles, 8 + 4);
    }

    #[test]
    fn load_indirect_zeropage_immediate_0xf0() {
        let mut cpu = Cpu::new_flat();
        cpu.mem_write(0xff32, 0x69);
        cpu.run_instructions_and_halt(&[0xf0, 0x32]); // LD   A,($ff00+$32)
        assert_eq!(cpu.a, 0x69);
        assert_eq!(cpu.n_cycles, 12 + 4);
    }

    #[test]
    fn store_indirect_zeropage_immediate_0xe0() {
        let mut cpu = Cpu::new_flat();
        cpu.a = 0xab;
        cpu.run_instructions_and_halt(&[0xe0, 0x56]); // LD   ($ff00+$56),A
        assert_eq!(cpu.mem_read(0xff56), 0xab);
        assert_eq!(cpu.n_cycles, 12 + 4);
    }

    enum ROI { Reg(OpReg8), Immediate } // Register Or Immediate

    fn test_math(opcode : u8, reg_or_imm: ROI, operand_1: u8, operand_2: u8, result: u8, flags: Flags) {
        let mut cpu = Cpu::new_flat();
        cpu.h = 0xc0; // avoid any HLindirect overwriting ROM
        match reg_or_imm {
            ROI::Reg(r) => { // Test the version between operands A and B
                cpu.a = operand_1;
                cpu.reg8_write(r, operand_2);
                cpu.run_instructions_and_halt(&[opcode]);
            }
            ROI::Immediate => { // Test the version with an immediate operand
                cpu.a = operand_1;
                cpu.run_instructions_and_halt(&[opcode, operand_2]);
            }
        }
        assert_eq!(cpu.a, result);
        assert_eq!(cpu.flags, flags);
    }

    #[test]
    fn add() {
        let tests = vec![
            (0x01, 0xff, 0x00, Flags::Z | Flags::H | Flags::C),
            (0x80, 0x80, 0x00, Flags::Z | Flags::C),
            (0x89, 0x89, 0x12, Flags::H | Flags::C),
            (0x80, 0x81, 0x01, Flags::C),
            (0x01, 0x02, 0x03, Flags::empty()),
            (0x09, 0x09, 0x12, Flags::H),
            (0x00, 0x00, 0x00, Flags::Z),
            (0x3a, 0xc6, 0x00, Flags::Z | Flags::H | Flags::C), // from nintendo manual
            (0x3c, 0xff, 0x3b, Flags::H | Flags::C),            // from nintendo manual
            (0x3c, 0x12, 0x4e, Flags::empty()),                 // from nintendo manual
        ];
        for (operand_1, operand_2, result, flags) in tests {
            test_math(0x82, ROI::Reg(OpReg8::D), operand_1, operand_2, result, flags);
            test_math(0x83, ROI::Reg(OpReg8::E), operand_1, operand_2, result, flags);
            test_math(0x86, ROI::Reg(OpReg8::HLIndirect), operand_1, operand_2, result, flags);
            test_math(0xc6, ROI::Immediate, operand_1, operand_2, result, flags);
        }
    }

    #[test]
    fn sub() {
        let tests = vec![
            (0x09, 0x03, 0x06, Flags::N),
            (0xfe, 0xfe, 0x00, Flags::N | Flags::Z),
            (0x00, 0x01, 0xff, Flags::N | Flags::H | Flags::C),
            (0x00, 0x70, 0x90, Flags::N | Flags::C),
            (0x10, 0x01, 0x0f, Flags::N | Flags::H),
            (0x3e, 0x3e, 0x00, Flags::N | Flags::Z), // from nintendo manual
            (0x3e, 0x0f, 0x2f, Flags::N | Flags::H), // from nintendo manual
            (0x3e, 0x40, 0xfe, Flags::N | Flags::C), // from nintendo manual

        ];
        for (operand_1, operand_2, result, flags) in tests {
            test_math(0x92, ROI::Reg(OpReg8::D), operand_1, operand_2, result, flags);
            test_math(0x91, ROI::Reg(OpReg8::C), operand_1, operand_2, result, flags);
            test_math(0xd6, ROI::Immediate, operand_1, operand_2, result, flags);
        }
    }

    #[test]
    fn and() {
        let tests = vec![
            (0x00, 0xff, 0x00, Flags::H | Flags::Z),
            (0b00001111, 0b01010101, 0b00000101, Flags::H),
            (0xf0, 0xff, 0xf0, Flags::H),
        ];
        for (operand_1, operand_2, result, flags) in tests {
            test_math(0xa0, ROI::Reg(OpReg8::B), operand_1, operand_2, result, flags);
            test_math(0xa6, ROI::Reg(OpReg8::HLIndirect), operand_1, operand_2, result, flags);
            test_math(0xe6, ROI::Immediate, operand_1, operand_2, result, flags);
        }
    }

    #[test]
    fn xor() {
        let tests = vec![
            (0x00, 0xf0, 0xf0, Flags::empty()),
            (0xff, 0xf0, 0x0f, Flags::empty()),
            (0b01010101, 0b11110000, 0b10100101, Flags::empty()),
            (0xff, 0xff, 0x00, Flags::Z),
        ];
        for (operand_1, operand_2, result, flags) in tests {
            test_math(0xaa, ROI::Reg(OpReg8::D), operand_1, operand_2, result, flags);
            test_math(0xad, ROI::Reg(OpReg8::L), operand_1, operand_2, result, flags);
            test_math(0xee, ROI::Immediate, operand_1, operand_2, result, flags);
        }
    }

    #[test]
    fn or() {
        let tests = vec![
            (0x00, 0xf0, 0xf0, Flags::empty()),
            (0xff, 0xf0, 0xff, Flags::empty()),
            (0b01010101, 0b11110000, 0b11110101, Flags::empty()),
            (0xff, 0xff, 0xff, Flags::empty()),
            (0x00, 0x00, 0x00, Flags::Z),
        ];
        for (operand_1, operand_2, result, flags) in tests {
            test_math(0xb1, ROI::Reg(OpReg8::C), operand_1, operand_2, result, flags);
            test_math(0xb3, ROI::Reg(OpReg8::E), operand_1, operand_2, result, flags);
            test_math(0xf6, ROI::Immediate, operand_1, operand_2, result, flags);
        }
    }


    #[test]
    fn cp() {
        let tests = vec![
            // These are same as the SUB tests except the A register should not be changed.
            (0x09, 0x03, 0x09, Flags::N),
            (0xfe, 0xfe, 0xfe, Flags::N | Flags::Z),
            (0x00, 0x01, 0x00, Flags::N | Flags::H | Flags::C),
            (0x00, 0x70, 0x00, Flags::N | Flags::C),
            (0x10, 0x01, 0x10, Flags::N | Flags::H),
            (0x3e, 0x3e, 0x3e, Flags::N | Flags::Z), // from nintendo manual
            (0x3e, 0x0f, 0x3e, Flags::N | Flags::H), // from nintendo manual
            (0x3e, 0x40, 0x3e, Flags::N | Flags::C), // from nintendo manual
        ];
        for (operand_1, operand_2, result, flags) in tests {
            test_math(0xbd, ROI::Reg(OpReg8::L), operand_1, operand_2, result, flags);
            test_math(0xbe, ROI::Reg(OpReg8::HLIndirect), operand_1, operand_2, result, flags);
            test_math(0xfe, ROI::Immediate, operand_1, operand_2, result, flags);
        }
    }



    #[test]
    fn adc_simple_carry_only() {
        //from the nintendo manual
        let init_cpu = || {
            let mut cpu = Cpu::new_flat();
            cpu.reg16_write(OpReg16::HL, 0xc000);
            cpu.reg8_write(OpReg8::A, 0xe1);
            cpu.reg8_write(OpReg8::E, 0x0f);
            cpu.reg8_write(OpReg8::HLIndirect, 0x1e);
            cpu.flags.set(Flags::C, true);
            cpu
        };

        let mut cpu = init_cpu();
        cpu.run_instructions_and_halt(&[0x8b]);
        assert_eq!(cpu.a, 0xf1);
        assert_eq!(cpu.flags, Flags::H);

        let mut cpu = init_cpu();
        cpu.run_instructions_and_halt(&[0xce, 0x3b]);
        assert_eq!(cpu.a, 0x1d);
        assert_eq!(cpu.flags, Flags::C);

        let mut cpu = init_cpu();
        cpu.run_instructions_and_halt(&[0x8e]);
        assert_eq!(cpu.a, 0x00);
        assert_eq!(cpu.flags, Flags::H | Flags::Z | Flags::C);
    }

    #[test]
    fn sbc_simple_carry_only() {
        //from the nintendo manual
        let init_cpu = || {
            let mut cpu = Cpu::new_flat();
            cpu.reg8_write(OpReg8::A, 0x3b);
            cpu.reg8_write(OpReg8::H, 0x2a);
            cpu.reg8_write(OpReg8::HLIndirect, 0x4f);
            cpu.flags.set(Flags::C, true);
            cpu
        };

        let mut cpu = init_cpu();
        cpu.run_instructions_and_halt(&[0x9c]);
        assert_eq!(cpu.a, 0x10);
        assert_eq!(cpu.flags, Flags::N);

        let mut cpu = init_cpu();
        cpu.run_instructions_and_halt(&[0xde, 0x3a]);
        assert_eq!(cpu.a, 0x00);
        assert_eq!(cpu.flags, Flags::N | Flags::Z);

        let mut cpu = init_cpu();
        cpu.run_instructions_and_halt(&[0x9e]);
        assert_eq!(cpu.a, 0xeb);
        assert_eq!(cpu.flags, Flags::N | Flags::H | Flags::C);
    }


    #[test]
    fn inc() {
        let mut cpu = Cpu::new_flat();
        cpu.b = 3;
        cpu.run_instructions_and_halt(&[0x04]); // INC B
        assert_eq!(cpu.b, 4);
        assert_eq!(cpu.flags, Flags::empty());
        // check overflow
        cpu.d = 0xff;
        cpu.run_instructions_and_halt(&[0x14]); // INC D
        assert_eq!(cpu.d, 0);
        assert_eq!(cpu.flags, Flags::Z | Flags::H);
        // check half-carry flag
        cpu.d = 0x0f;
        cpu.run_instructions_and_halt(&[0x14]); // INC D
        assert_eq!(cpu.d, 0x10);
        assert_eq!(cpu.flags, Flags::H);
    }

    #[test]
    fn dec() {
        let mut cpu = Cpu::new_flat();
        cpu.b = 2;
        cpu.run_instructions_and_halt(&[0x05]); // DEC B
        assert_eq!(cpu.b, 1);
        assert_eq!(cpu.flags, Flags::N);
        // check zero-flag
        cpu.run_instructions_and_halt(&[0x05]); // DEC B
        assert_eq!(cpu.b, 0);
        assert_eq!(cpu.flags, Flags::N | Flags::Z);
        // check underflow
        cpu.run_instructions_and_halt(&[0x05]); // INC D
        assert_eq!(cpu.b, 0xff);
        assert_eq!(cpu.flags, Flags::N | Flags::H);
        // check carry flag is not affected
        cpu.flags.set(Flags::C, true);
        cpu.run_instructions_and_halt(&[0x05]); // INC D
        assert_eq!(cpu.b, 0xfe);
        assert_eq!(cpu.flags, Flags::N | Flags::C);
    }

    #[test]
    fn load_16bit_immediate() {
        let mut cpu = Cpu::new_flat();
        cpu.run_instructions_and_halt(&[0x11, 0x52, 0xaa]); // LD   DE,$76aa
        assert_eq!(cpu.d, 0xaa);
        assert_eq!(cpu.e, 0x52);
        cpu.run_instructions_and_halt(&[0x31, 0x34, 0x12]); // LD   SP,$1234
        assert_eq!(cpu.sp, 0x1234);
        cpu.run_instructions_and_halt(&[0x21, 0x21, 0x43]); // LD   HL,$4321
        assert_eq!(cpu.h, 0x43);
        assert_eq!(cpu.l, 0x21);
        assert_eq!(cpu.reg16_read(OpReg16::HL), 0x4321);
    }

    #[test]
    fn load_sphl() {
        let mut cpu = Cpu::new_flat();
        cpu.h = 0x55;
        cpu.l = 0x23;
        cpu.run_instructions_and_halt(&[0xf9]); // LD   SP,HL
        assert_eq!(cpu.sp, 0x5523);
    }

    #[test]
    fn push() {
        let mut cpu = Cpu::new_flat();
        cpu.b = 0x44; // hi reg
        cpu.c = 0x55; // lo reg
        cpu.sp = 0xfffe;
        cpu.run_instructions_and_halt(&[0xc5]); // PUSH BC
        assert_eq!(cpu.sp, 0xfffc);
        assert_eq!(cpu.mem_read(0xfffc), 0x55); // lo addr, lo reg
        assert_eq!(cpu.mem_read(0xfffd), 0x44); // hi addr, hi reg
    }

    #[test]
    fn push_af() {
        let mut cpu = Cpu::new_flat();
        cpu.sp = 0xfffe;
        cpu.a = 0xff;
        // ADD  A,$02 ; A = 0x01, F = 0b00010000 just carry flag
        cpu.run_instructions_and_halt(&[0xc6, 0x02]);
        assert_eq!(cpu.a, 0x01);
        assert_eq!(cpu.flags, Flags::C | Flags::H);
        assert_eq!(cpu.sp, 0xfffe);
        // PUSH AF
        cpu.run_instructions_and_halt(&[0xf5]);
        assert_eq!(cpu.sp, 0xfffc);
        assert_eq!(cpu.mem_read(0xfffd), 0x01);
        assert_eq!(cpu.mem_read(0xfffc), (Flags::C | Flags::H).bits());
    }

    #[test]
    fn pop() {
        let mut cpu = Cpu::new_flat();
        cpu.sp = 0xfff0;
        cpu.mem_write(0xfff0, 0x5f); //lo address, lo bits
        cpu.mem_write(0xfff1, 0x3c); //hi address, hi bits
        // POP  DE
        cpu.run_instructions_and_halt(&[0xd1]);
        assert_eq!(cpu.d, 0x3c); //hi reg
        assert_eq!(cpu.e, 0x5f); //lo reg
        assert_eq!(cpu.sp, 0xfff2);
    }

    #[test]
    fn pushpop() {
        let mut cpu = Cpu::new_flat();
        cpu.sp = 0xfff0;
        // LD H,$11
        // LD L,$20
        // PUSH HL
        cpu.run_instructions_and_halt(&[0x26, 0x11, 0x2e, 0x20, 0xe5]);
        assert_eq!(cpu.sp, 0xffee);
        assert_eq!(cpu.h, 0x11);
        assert_eq!(cpu.l, 0x20);
        // POP  AF
        cpu.run_instructions_and_halt(&[0xf1]);
        assert_eq!(cpu.sp, 0xfff0);
        assert_eq!(cpu.a, 0x11);
        assert_eq!(cpu.flags.bits(), 0x20);
    }


    #[test]
    fn savesp() {
        let mut cpu = Cpu::new_flat();
        cpu.sp = 0xfff8;
        cpu.run_instructions_and_halt(&[0x08, 0x00, 0xc1]);
        assert_eq!(cpu.sp, 0xfff8);
        assert_eq!(cpu.mem_read16(0xc100), 0xfff8);
    }

    #[test]
    fn loadhlsprelative() {
        let mut cpu = Cpu::new_flat();
        cpu.sp = 0xfff8;
        cpu.run_instructions_and_halt(&[0xf8, 0x02]);
        assert_eq!(cpu.sp, 0xfff8);
        assert_eq!(cpu.reg16_read(OpReg16::HL), 0xfffa);
        assert_eq!(cpu.flags, Flags::empty());

        cpu.flags = Flags::C | Flags::Z; // set some random flags
        cpu.sp = 0xfff8;
        cpu.run_instructions_and_halt(&[0xf8, 0xf0]);
        assert_eq!(cpu.sp, 0xfff8);
        assert_eq!(cpu.reg16_read(OpReg16::HL), 0xffe8);
        assert_eq!(cpu.flags, Flags::empty());
    }

    #[test]
    fn jump_unconditional() {
        let mut cpu = Cpu::new_flat();
        cpu.bus.mem_write(0x1234, 0x76);
        cpu.run_instructions_and_halt(&[0xc3, 0x34, 0x12]); // JP $1234
        assert_eq!(cpu.pc, 0x1235);
    }

    #[test]
    fn jump_hl() {
        let mut cpu = Cpu::new_flat();
        cpu.h = 0x12;
        cpu.l = 0x34;
        cpu.bus.mem_write(0x1234, 0x76);
        cpu.run_instructions_and_halt(&[0xe9]); // JP (HL)
        assert_eq!(cpu.pc, 0x1235);
    }

    #[test]
    fn jump_relative_backwards() {
        let mut cpu = Cpu::new_flat();
        cpu.pc = 0x2000;
        cpu.bus.mem_write(0x1ff0, 0x3c); // INC A
        cpu.bus.mem_write(0x1ff1, 0x76); // HALT
        //                              2000  2001  2002
        cpu.run_instructions_and_halt(&[0x18, -18i8 as u8, 0x04]); // JR -$12 \ INC B
        assert_eq!(cpu.pc, 0x1ff2);
        assert_eq!(cpu.a, 1);
        assert_eq!(cpu.b, 0);
    }

    #[test]
    fn jump_relative_forwards() {
        let mut cpu = Cpu::new_flat();
        cpu.pc = 0x2000;
        cpu.bus.mem_write(0x1ff0, 0x3c); // INC A
        cpu.bus.mem_write(0x1ff1, 0x76); // HALT
        //                              // JR +$02  INC B   INC D  INC H
        cpu.run_instructions_and_halt(&[0x18, 2, 0x04, 0x14,  0x24 ]);
        assert_eq!(cpu.pc, 0x2006);
        assert_eq!(cpu.a, 0);
        assert_eq!(cpu.b, 0);
        assert_eq!(cpu.d, 0);
        assert_eq!(cpu.h, 1);
    }

    fn test_jump_conditional(code: [u8; 3], starting_flags: Flags, expected_branch: bool) {
        let mut cpu = Cpu::new_flat();
        cpu.flags = starting_flags;
        cpu.bus.mem_write(0x1ff0, 0x3c); // INC A
        cpu.bus.mem_write(0x1ff1, 0x76); // HALT
        cpu.bus.mem_write(0x2000, code[0]); //  | Jump instruction here
        cpu.bus.mem_write(0x2001, code[1]); //  | (pad with NOP to make 3 bytes)
        cpu.bus.mem_write(0x2002, code[2]); //  |
        cpu.bus.mem_write(0x2003, 0x04); // INC B
        cpu.bus.mem_write(0x2004, 0x76); // HALT
                                         //
        cpu.pc = 0x2000;
        cpu.run();

        if expected_branch {
            assert_eq!(cpu.a, 1);
            assert_eq!(cpu.b, 0);
            assert_eq!(cpu.pc, 0x1ff2);
        } else {
            assert_eq!(cpu.a, 0);
            assert_eq!(cpu.b, 1);
            assert_eq!(cpu.pc, 0x2005);
        }
    }

    #[test]
    fn jump_conditional_absolute() {
        let tests = [
            // NZ condition
            ([0xc2, 0xf0, 0x1f], Flags::empty()     , true),
            ([0xc2, 0xf0, 0x1f], Flags::Z           , false),
            ([0xc2, 0xf0, 0x1f], Flags::C           , true),
            ([0xc2, 0xf0, 0x1f], Flags::Z | Flags::C, false),
            // Z condition
            ([0xca, 0xf0, 0x1f], Flags::empty()     , false),
            ([0xca, 0xf0, 0x1f], Flags::Z           , true),
            ([0xca, 0xf0, 0x1f], Flags::C           , false),
            ([0xca, 0xf0, 0x1f], Flags::Z | Flags::C, true),
            // NC condition
            ([0xd2, 0xf0, 0x1f], Flags::empty()     , true),
            ([0xd2, 0xf0, 0x1f], Flags::Z           , true),
            ([0xd2, 0xf0, 0x1f], Flags::C           , false),
            ([0xd2, 0xf0, 0x1f], Flags::Z | Flags::C, false),
            // C condition
            ([0xda, 0xf0, 0x1f], Flags::empty()     , false),
            ([0xda, 0xf0, 0x1f], Flags::Z           , false),
            ([0xda, 0xf0, 0x1f], Flags::C           , true),
            ([0xda, 0xf0, 0x1f], Flags::Z | Flags::C, true),
        ];

        for (code, starting_flags, should_branch) in tests {
            test_jump_conditional(code, starting_flags, should_branch);
        }
    }

    #[test]
    fn jump_conditional_relative() {
        let tests = [
            // NZ condition
            ([0x20, -18i8 as u8, 0x00], Flags::empty()     , true),
            ([0x20, -18i8 as u8, 0x00], Flags::Z           , false),
            ([0x20, -18i8 as u8, 0x00], Flags::C           , true),
            ([0x20, -18i8 as u8, 0x00], Flags::Z | Flags::C, false),
            // Z condition
            ([0x28, -18i8 as u8, 0x00], Flags::empty()     , false),
            ([0x28, -18i8 as u8, 0x00], Flags::Z           , true),
            ([0x28, -18i8 as u8, 0x00], Flags::C           , false),
            ([0x28, -18i8 as u8, 0x00], Flags::Z | Flags::C, true),
            // NC condition
            ([0x30, -18i8 as u8, 0x00], Flags::empty()     , true),
            ([0x30, -18i8 as u8, 0x00], Flags::Z           , true),
            ([0x30, -18i8 as u8, 0x00], Flags::C           , false),
            ([0x30, -18i8 as u8, 0x00], Flags::Z | Flags::C, false),
            // C condition
            ([0x38, -18i8 as u8, 0x00], Flags::empty()     , false),
            ([0x38, -18i8 as u8, 0x00], Flags::Z           , false),
            ([0x38, -18i8 as u8, 0x00], Flags::C           , true),
            ([0x38, -18i8 as u8, 0x00], Flags::Z | Flags::C, true),
        ];

        for (code, starting_flags, should_branch) in tests {
            test_jump_conditional(code, starting_flags, should_branch);
        }
    }

    #[test]
    fn call_unconditional() {
        let mut cpu = Cpu::new_flat();
        cpu.pc = 0x8000;
        cpu.sp = 0xfffe;
        cpu.bus.mem_write(0x1234, 0x76);
        cpu.run_instructions_and_halt(&[0xcd, 0x34, 0x12]);
        assert_eq!(cpu.mem_read(0xfffd), 0x80);
        assert_eq!(cpu.mem_read(0xfffc), 0x03);
        assert_eq!(cpu.sp, 0xfffc);
        assert_eq!(cpu.pc, 0x1235); // +1 for the HALT
    }

    fn test_call_conditional(code: [u8; 3], starting_flags: Flags, expected_branch: bool) {
        let mut cpu = Cpu::new_flat();
        cpu.flags = starting_flags;
        cpu.bus.mem_write(0x1ff0, 0x3c); // INC A
        cpu.bus.mem_write(0x1ff1, 0x76); // HALT
        cpu.bus.mem_write(0x2000, code[0]); //  | Call instruction here
        cpu.bus.mem_write(0x2001, code[1]); //  | (pad with NOP to make 3 bytes)
        cpu.bus.mem_write(0x2002, code[2]); //  |
        cpu.bus.mem_write(0x2003, 0x04); // INC B
        cpu.bus.mem_write(0x2004, 0x76); // HALT
                                         //
        cpu.sp = 0xfffe;
        cpu.pc = 0x2000;
        cpu.run();

        if expected_branch {
            assert_eq!(cpu.a, 1);
            assert_eq!(cpu.b, 0);
            assert_eq!(cpu.pc, 0x1ff2);
            assert_eq!(cpu.mem_read(0xfffd), 0x20);
            assert_eq!(cpu.mem_read(0xfffc), 0x03);
            assert_eq!(cpu.sp, 0xfffc);
        } else {
            assert_eq!(cpu.a, 0);
            assert_eq!(cpu.b, 1);
            assert_eq!(cpu.pc, 0x2005);
            assert_eq!(cpu.mem_read(0xfffd), 0x00);
            assert_eq!(cpu.mem_read(0xfffc), 0x00);
            assert_eq!(cpu.sp, 0xfffe);
        }
    }


    #[test]
    fn call_conditional() {
        let tests = [
            // NZ condition
            ([0xc4, 0xf0, 0x1f], Flags::empty()     , true),
            ([0xc4, 0xf0, 0x1f], Flags::Z           , false),
            ([0xc4, 0xf0, 0x1f], Flags::C           , true),
            ([0xc4, 0xf0, 0x1f], Flags::Z | Flags::C, false),
            // Z condition
            ([0xcc, 0xf0, 0x1f], Flags::empty()     , false),
            ([0xcc, 0xf0, 0x1f], Flags::Z           , true),
            ([0xcc, 0xf0, 0x1f], Flags::C           , false),
            ([0xcc, 0xf0, 0x1f], Flags::Z | Flags::C, true),
            // NC condition
            ([0xd4, 0xf0, 0x1f], Flags::empty()     , true),
            ([0xd4, 0xf0, 0x1f], Flags::Z           , true),
            ([0xd4, 0xf0, 0x1f], Flags::C           , false),
            ([0xd4, 0xf0, 0x1f], Flags::Z | Flags::C, false),
            // C condition
            ([0xdc, 0xf0, 0x1f], Flags::empty()     , false),
            ([0xdc, 0xf0, 0x1f], Flags::Z           , false),
            ([0xdc, 0xf0, 0x1f], Flags::C           , true),
            ([0xdc, 0xf0, 0x1f], Flags::Z | Flags::C, true),
        ];

        for (code, starting_flags, should_branch) in tests {
            test_call_conditional(code, starting_flags, should_branch);
        }
    }

    #[test]
    fn call_unconditional_2() {
        let tests = [
            // all of these should take the branch regardless of flags
            ([0xcd, 0xf0, 0x1f], Flags::empty()     , true),
            ([0xcd, 0xf0, 0x1f], Flags::Z           , true),
            ([0xcd, 0xf0, 0x1f], Flags::C           , true),
            ([0xcd, 0xf0, 0x1f], Flags::Z | Flags::C, true),
            // And these should not branch
            ([0x00, 0x00, 0x00], Flags::empty()     , false),
            ([0x00, 0x00, 0x00], Flags::Z           , false),
            ([0x00, 0x00, 0x00], Flags::C           , false),
            ([0x00, 0x00, 0x00], Flags::Z | Flags::C, false),
        ];

        for (code, starting_flags, should_branch) in tests {
            test_call_conditional(code, starting_flags, should_branch);
        }
    }


    #[test]
    fn return_unconditional() {
        let mut cpu = Cpu::new_flat();
        cpu.pc = 0x8000;
        cpu.bus.mem_write(0x9000, 0x3c); //INC A
        cpu.bus.mem_write(0x9001, 0xc9); //RET
        cpu.bus.mem_write(0x9002, 0x14); //INC D
        cpu.bus.mem_write(0x9003, 0x76); //HALT
        cpu.run_instructions_and_halt(&[0xcd, 0x00, 0x90, 0x04]); //CALL $9000 \ INC B
        assert_eq!(cpu.pc, 0x8005);
        assert_eq!(cpu.a, 1);
        assert_eq!(cpu.b, 1);
        assert_eq!(cpu.d, 0);
    }

    #[test]
    fn return_from_interrupt() {
        let mut cpu = Cpu::new_flat();
        // set return address to $8000
        cpu.sp = 0xfffc;
        cpu.mem_write(0xfffd, 0x80);
        cpu.mem_write(0xfffc, 0x00);

        cpu.pc = 0x1000;
        cpu.interrupt_master_enable = false;
        cpu.bus.mem_write(0x1000, 0xd9); // RETI
        cpu.bus.mem_write(0x1001, 0x04);     // INC B
        cpu.bus.mem_write(0x1002, 0x76);     // HALT
                                             // ...
        cpu.bus.mem_write(0x8000, 0x3c);     // INC A
        cpu.bus.mem_write(0x8001, 0x76);     // HALT
                                             //
        cpu.run();
        assert_eq!(cpu.a, 1);
        assert_eq!(cpu.b, 0);
        assert_eq!(cpu.pc, 0x8002);
        assert_eq!(cpu.sp, 0xfffe);
        assert_eq!(cpu.interrupt_master_enable, true);
    }


    fn test_return_conditional(ret_code: [u8; 1], starting_flags: Flags, expected_branch: bool) {
        let mut cpu = Cpu::new_flat();

        // set return address to $8000
        cpu.sp = 0xfffc;
        cpu.mem_write(0xfffd, 0x80);
        cpu.mem_write(0xfffc, 0x00);

        cpu.pc = 0x1000;
        cpu.bus.mem_write(0x1000, ret_code[0]); // RET cc
        cpu.bus.mem_write(0x1001, 0x04);     // INC B
        cpu.bus.mem_write(0x1002, 0x76);     // HALT
                                             // ...
        cpu.bus.mem_write(0x8000, 0x3c);     // INC A
        cpu.bus.mem_write(0x8001, 0x76);     // HALT
                                             //
        cpu.flags = starting_flags;
        cpu.run();
        if expected_branch {
            assert_eq!(cpu.a, 1);
            assert_eq!(cpu.b, 0);
            assert_eq!(cpu.pc, 0x8002);
            assert_eq!(cpu.sp, 0xfffe);
        } else {
            assert_eq!(cpu.a, 0);
            assert_eq!(cpu.b, 1);
            assert_eq!(cpu.pc, 0x1003);
            assert_eq!(cpu.sp, 0xfffc);
        }
    }

    #[test]
    fn return_conditional() {
        let tests = [
            // NZ condition
            ([0xc0], Flags::empty()     , true),
            ([0xc0], Flags::Z           , false),
            ([0xc0], Flags::C           , true),
            ([0xc0], Flags::Z | Flags::C, false),
            // Z condition
            ([0xc8], Flags::empty()     , false),
            ([0xc8], Flags::Z           , true),
            ([0xc8], Flags::C           , false),
            ([0xc8], Flags::Z | Flags::C, true),
            // NC condition
            ([0xd0], Flags::empty()     , true),
            ([0xd0], Flags::Z           , true),
            ([0xd0], Flags::C           , false),
            ([0xd0], Flags::Z | Flags::C, false),
            // C condition
            ([0xd8], Flags::empty()     , false),
            ([0xd8], Flags::Z           , false),
            ([0xd8], Flags::C           , true),
            ([0xd8], Flags::Z | Flags::C, true),
        ];

        for (code, starting_flags, should_branch) in tests {
            test_return_conditional(code, starting_flags, should_branch);
        }
    }

    #[test]
    fn return_unconditional_2() {
        let tests = [
            // all of these should take the branch regardless of flags
            ([0xc9], Flags::empty()     , true),
            ([0xc9], Flags::Z           , true),
            ([0xc9], Flags::C           , true),
            ([0xc9], Flags::Z | Flags::C, true),
            // And these should not branch
            ([0x00], Flags::empty()     , false),
            ([0x00], Flags::Z           , false),
            ([0x00], Flags::C           , false),
            ([0x00], Flags::Z | Flags::C, false),
        ];

        for (code, starting_flags, should_branch) in tests {
            test_return_conditional(code, starting_flags, should_branch);
        }
    }

    #[test]
    fn inc16() {
        let mut cpu = Cpu::new_flat();
        cpu.b = 0x43;
        cpu.c = 0xff;
        cpu.run_instructions_and_halt(&[0x03]); //INC BC
        assert_eq!(cpu.b, 0x44);
        assert_eq!(cpu.c, 0x00);
        assert_eq!(cpu.flags, Flags::empty());

        cpu.run_instructions_and_halt(&[0x03]); //INC BC
        assert_eq!(cpu.b, 0x44);
        assert_eq!(cpu.c, 0x01);
        assert_eq!(cpu.flags, Flags::empty());

        cpu.sp = 0xffff;
        cpu.run_instructions_and_halt(&[0x33]); //INC SP
        assert_eq!(cpu.sp, 0x0000);
        assert_eq!(cpu.flags, Flags::empty());
    }

    #[test]
    fn dec16() {
        let mut cpu = Cpu::new_flat();
        cpu.b = 0x43;
        cpu.c = 0xff;
        cpu.run_instructions_and_halt(&[0x0b]); //DEC BC
        assert_eq!(cpu.b, 0x43);
        assert_eq!(cpu.c, 0xfe);
        assert_eq!(cpu.flags, Flags::empty());

        cpu.d = 0x2a;
        cpu.e = 0x00;
        cpu.run_instructions_and_halt(&[0x1b]); //DEC DE
        assert_eq!(cpu.d, 0x29);
        assert_eq!(cpu.e, 0xff);
        assert_eq!(cpu.flags, Flags::empty());

        cpu.h = 0x00;
        cpu.l = 0x00;
        cpu.run_instructions_and_halt(&[0x2b]); // or not to be... DEC HL
        assert_eq!(cpu.h, 0xff);
        assert_eq!(cpu.l, 0xff);
        assert_eq!(cpu.flags, Flags::empty());
    }

    #[test]
    fn add_hl_dd() {
        let mut cpu = Cpu::new_flat();
        cpu.h = 0x8a;
        cpu.l = 0x23;
        cpu.b = 0x06;
        cpu.c = 0x05;
        cpu.run_instructions_and_halt(&[0x09]); // ADD HL,BC
        assert_eq!(cpu.h, 0x90);
        assert_eq!(cpu.l, 0x28);
        assert_eq!(cpu.flags, Flags::H);
    }

    #[test]
    fn add_hl_dd_2() {
        let mut cpu = Cpu::new_flat();
        cpu.h = 0x8a;
        cpu.l = 0x23;
        cpu.run_instructions_and_halt(&[0x29]); // ADD HL,HL
        assert_eq!(cpu.h, 0x14);
        assert_eq!(cpu.l, 0x46);
        assert_eq!(cpu.flags, Flags::C | Flags::H);
    }

    #[test]
    fn add_sp() {
        let mut cpu = Cpu::new_flat();
        cpu.sp = 0xfff8;
        cpu.run_instructions_and_halt(&[0xe8, 2]); // ADD SP, 2
        assert_eq!(cpu.sp, 0xfffa);
        assert_eq!(cpu.flags, Flags::empty());
        cpu.run_instructions_and_halt(&[0xe8, 6]); // ADD SP, 6
        assert_eq!(cpu.sp, 0x00);
        assert_eq!(cpu.flags, Flags::C | Flags::H);
    }

    #[test]
    fn store_hli_a() {
        let mut cpu = Cpu::new_flat();
        cpu.h = 0xff;
        cpu.l = 0xff;
        cpu.a = 0x56;
        cpu.run_instructions_and_halt(&[0x22]); // LD (HL+),A
        assert_eq!(cpu.mem_read(0xffff), 0x56);
        assert_eq!(cpu.h, 0);
        assert_eq!(cpu.l, 0);
    }

    #[test]
    fn store_hld_a() {
        let mut cpu = Cpu::new_flat();
        cpu.h = 0x40;
        cpu.l = 0x00;
        cpu.a = 0x05;
        cpu.run_instructions_and_halt(&[0x32]); // LD (HL-),A
        assert_eq!(cpu.mem_read(0x4000), 0x05);
        assert_eq!(cpu.h, 0x3f);
        assert_eq!(cpu.l, 0xff);
    }

    #[test]
    fn load_a_hli() {
        let mut cpu = Cpu::new_flat();
        cpu.h = 0x01;
        cpu.l = 0xff;
        cpu.mem_write(0x1ff, 0x56);
        cpu.run_instructions_and_halt(&[0x2a]); // LD A,(HL+)
        assert_eq!(cpu.a, 0x56);
        assert_eq!(cpu.h, 0x02);
        assert_eq!(cpu.l, 0x00);
    }

    #[test]
    fn load_a_hld() {
        let mut cpu = Cpu::new_flat();
        cpu.h = 0x8a;
        cpu.l = 0x5c;
        cpu.mem_write(0x8a5c, 0x3c);
        cpu.run_instructions_and_halt(&[0x3a]); // LD A,(HL-)
        assert_eq!(cpu.a, 0x3c);
        assert_eq!(cpu.h, 0x8a);
        assert_eq!(cpu.l, 0x5b);
    }

    fn test_bit_ops(opcode: [u8; 2], reg: OpReg8, start_val: u8, start_flags: Flags, end_val: u8, end_flags: Flags) {
        let mut cpu = Cpu::new_flat();
        cpu.reg8_write(reg, start_val);
        cpu.flags = start_flags;
        cpu.run_instructions_and_halt(&opcode);
        assert_eq!(cpu.reg8_read(reg), end_val);
        assert_eq!(cpu.flags, end_flags);
    }

    #[test]
    fn rlc() {
        test_bit_ops([0xcb, 0x00], OpReg8::B,          0x85, Flags::empty(), 0x0b, Flags::C);
        test_bit_ops([0xcb, 0x06], OpReg8::HLIndirect, 0x00, Flags::empty(), 0x00, Flags::Z);
    }

    #[test]
    fn rrc() {
        test_bit_ops([0xcb, 0x09], OpReg8::C,          0x01, Flags::empty(), 0x80, Flags::C);
        test_bit_ops([0xcb, 0x09], OpReg8::C,          0x80, Flags::empty(), 0x40, Flags::empty());
        test_bit_ops([0xcb, 0x0e], OpReg8::HLIndirect, 0x00, Flags::empty(), 0x00, Flags::Z);
    }


    #[test]
    fn rl() {
        test_bit_ops([0xcb, 0x15], OpReg8::L,          0x80, Flags::empty(), 0x00, Flags::C | Flags::Z);
        test_bit_ops([0xcb, 0x16], OpReg8::HLIndirect, 0x11, Flags::empty(), 0x22, Flags::empty());
        test_bit_ops([0xcb, 0x16], OpReg8::HLIndirect, 0x00, Flags::C      , 0x01, Flags::empty());
    }

    #[test]
    fn rr() {
        test_bit_ops([0xcb, 0x19], OpReg8::C,          0x01, Flags::empty(), 0x00, Flags::C | Flags::Z);
        test_bit_ops([0xcb, 0x1e], OpReg8::HLIndirect, 0x8a, Flags::empty(), 0x45, Flags::empty());
        test_bit_ops([0xcb, 0x1e], OpReg8::HLIndirect, 0x00, Flags::C,       0x80, Flags::empty());
        test_bit_ops([0xcb, 0x1e], OpReg8::HLIndirect, 0x01, Flags::C,       0x80, Flags::C);
    }

    #[test]
    fn sla() {
        test_bit_ops([0xcb, 0x22], OpReg8::D,          0x80, Flags::empty(), 0x00, Flags::C | Flags::Z);
        test_bit_ops([0xcb, 0x26], OpReg8::HLIndirect, 0xff, Flags::empty(), 0xfe, Flags::C);
        test_bit_ops([0xcb, 0x26], OpReg8::HLIndirect, 0x01, Flags::C      , 0x02, Flags::empty());
    }

    #[test]
    fn sra() {
        test_bit_ops([0xcb, 0x2f], OpReg8::A,          0x8a, Flags::empty(), 0xc5, Flags::empty());
        test_bit_ops([0xcb, 0x2e], OpReg8::HLIndirect, 0x01, Flags::empty(), 0x00, Flags::C | Flags::Z);
        test_bit_ops([0xcb, 0x2e], OpReg8::HLIndirect, 0x02, Flags::C      , 0x01, Flags::empty());
        test_bit_ops([0xcb, 0x2e], OpReg8::HLIndirect, 0xf0, Flags::C      , 0xf8, Flags::empty());
    }

    #[test]
    fn swap() {
        test_bit_ops([0xcb, 0x32], OpReg8::D,          0x80, Flags::empty(), 0x08, Flags::empty());
        test_bit_ops([0xcb, 0x36], OpReg8::HLIndirect, 0x5f, Flags::empty(), 0xf5, Flags::empty());
        test_bit_ops([0xcb, 0x36], OpReg8::HLIndirect, 0x01, Flags::C      , 0x10, Flags::empty());
        test_bit_ops([0xcb, 0x36], OpReg8::HLIndirect, 0x00, Flags::C      , 0x00, Flags::Z);
    }

    #[test]
    fn srl() {
        test_bit_ops([0xcb, 0x3f], OpReg8::A,          0x01, Flags::empty(), 0x00, Flags::C | Flags::Z);
        test_bit_ops([0xcb, 0x3e], OpReg8::HLIndirect, 0xff, Flags::empty(), 0x7f, Flags::C);
        test_bit_ops([0xcb, 0x3e], OpReg8::HLIndirect, 0xf0, Flags::C      , 0x78, Flags::empty());
    }

    #[test]
    fn bitop_bit() {
        // BIT 7, A
        test_bit_ops([0xcb, 0x7f], OpReg8::A,          0x80, Flags::empty(), 0x80, Flags::H);
        // BIT 4, L
        test_bit_ops([0xcb, 0x65], OpReg8::L,          0xef, Flags::empty(), 0xef, Flags::H | Flags::Z);
        // BIT 0, (HL)
        test_bit_ops([0xcb, 0x46], OpReg8::HLIndirect, 0xfe, Flags::C      , 0xfe, Flags::C | Flags::H | Flags::Z);
        // BIT 1, (HL)
        test_bit_ops([0xcb, 0x4e], OpReg8::HLIndirect, 0xfe, Flags::C      , 0xfe, Flags::C | Flags::H);
    }

    #[test]
    fn bitop_reset() {
        // RES 7, A
        test_bit_ops([0xcb, 0xbf], OpReg8::A,          0x80, Flags::empty(), 0x00, Flags::empty());
        // RES 6, A
        test_bit_ops([0xcb, 0xb7], OpReg8::A,          0x80, Flags::empty(), 0x80, Flags::empty());
        // RES 1, L
        test_bit_ops([0xcb, 0x8d], OpReg8::L,          0x3b, Flags::H      , 0x39, Flags::H);
        // RES 3, (HL)
        test_bit_ops([0xcb, 0x9e], OpReg8::HLIndirect, 0xff, Flags::H      , 0xf7, Flags::H);
        // RES 3, (HL)
        test_bit_ops([0xcb, 0x9e], OpReg8::HLIndirect, 0xf0, Flags::H      , 0xf0, Flags::H);
    }

    #[test]
    fn bitop_set() {
        // SET 7, A
        test_bit_ops([0xcb, 0xff], OpReg8::A,          0x00, Flags::empty(), 0x80, Flags::empty());
        // SET 6, A
        test_bit_ops([0xcb, 0xf7], OpReg8::A,          0x00, Flags::empty(), 0x40, Flags::empty());
        // SET 1, L
        test_bit_ops([0xcb, 0xcd], OpReg8::L,          0x39, Flags::H      , 0x3b, Flags::H);
        // SET 3, (HL)
        test_bit_ops([0xcb, 0xde], OpReg8::HLIndirect, 0xf7, Flags::H      , 0xff, Flags::H);
        // SET 3, (HL)
        test_bit_ops([0xcb, 0xde], OpReg8::HLIndirect, 0xff, Flags::H      , 0xff, Flags::H);
    }

    #[test]
    fn rlca() {
        // nintendo manual might be incorrect on the first one
        test_bit_ops([0x07, 0x00], OpReg8::A, 0x85, Flags::empty(), 0x0b, Flags::C);
        test_bit_ops([0x07, 0x00], OpReg8::A, 0x00, Flags::empty(), 0x00, Flags::empty());
        test_bit_ops([0x07, 0x00], OpReg8::A, 0x00, Flags::empty(), 0x00, Flags::empty());
    }

    #[test]
    fn rla() {
        test_bit_ops([0x17, 0x00], OpReg8::A, 0x95, Flags::C, 0x2b, Flags::C);
        test_bit_ops([0x17, 0x00], OpReg8::A, 0x01, Flags::C, 0x03, Flags::empty());
        test_bit_ops([0x17, 0x00], OpReg8::A, 0x00, Flags::H, 0x00, Flags::empty());
    }

    #[test]
    fn rrca() {
        test_bit_ops([0x0f, 0x00], OpReg8::A, 0x3b, Flags::N, 0x9d, Flags::C);
        test_bit_ops([0x0f, 0x00], OpReg8::A, 0x3b, Flags::C, 0x9d, Flags::C);
        test_bit_ops([0x0f, 0x00], OpReg8::A, 0x00, Flags::C, 0x00, Flags::empty());
    }

    #[test]
    fn rra() {
        test_bit_ops([0x1f, 0x00], OpReg8::A, 0x81, Flags::H, 0x40, Flags::C);
        test_bit_ops([0x1f, 0x00], OpReg8::A, 0x00, Flags::C, 0x80, Flags::empty());
    }

    #[test]
    fn cpl() {
        let mut cpu = Cpu::new_flat();
        cpu.a = 0x35;
        cpu.flags = Flags::C;
        cpu.run_instructions_and_halt(&[0x2f]);
        assert_eq!(cpu.a, 0xca);
        assert_eq!(cpu.flags, Flags::C | Flags::H | Flags::N);
    }

    #[test]
    fn ccf() {
        let mut cpu = Cpu::new_flat();
        cpu.a = 0x35;
        cpu.flags = Flags::C | Flags::H | Flags::N | Flags::Z;
        cpu.run_instructions_and_halt(&[0x3f]);
        assert_eq!(cpu.a, 0x35);
        assert_eq!(cpu.flags, Flags::Z);
    }

    #[test]
    fn ccf_2() {
        let mut cpu = Cpu::new_flat();
        cpu.flags = Flags::empty();
        cpu.run_instructions_and_halt(&[0x3f]);
        assert_eq!(cpu.flags, Flags::C);
    }

    #[test]
    fn ccf_3() {
        let mut cpu = Cpu::new_flat();
        cpu.flags = Flags::C;
        cpu.run_instructions_and_halt(&[0x3f]);
        assert_eq!(cpu.flags, Flags::empty());
    }

    #[test]
    fn scf() {
        let mut cpu = Cpu::new_flat();
        cpu.flags = Flags::C | Flags::H | Flags::N | Flags::Z;
        cpu.run_instructions_and_halt(&[0x37]);
        assert_eq!(cpu.flags, Flags::C | Flags::Z);
    }

    #[test]
    fn scf_2() {
        let mut cpu = Cpu::new_flat();
        cpu.flags = Flags::empty();
        cpu.run_instructions_and_halt(&[0x37]);
        assert_eq!(cpu.flags, Flags::C);
    }
}
