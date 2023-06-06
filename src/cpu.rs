use crate::bus::{ Mem, Bus, };
use crate::opcodes::{
    Instruction,
    Opcode,
    OpReg8,
    OpReg16,
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

    n_cycles: u64,
    pub n_instrs: u64,
    bus: Bus,
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

    fn reg16_read(&self, reg_pair: OpReg16) -> u16 {
        match reg_pair {
            OpReg16::BC => { (self.b as u16) << 8 | self.c as u16 },
            OpReg16::DE => { (self.d as u16) << 8 | self.e as u16 },
            OpReg16::HL => { (self.h as u16) << 8 | self.l as u16 },
            OpReg16::SP => { self.sp },
            OpReg16::AF => { (self.a as u16) << 8 | self.flags.bits() as u16 },
            // OpReg16::HLInc => { 0 },
            // OpReg16::HLDec => { 0 },
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
            OpReg16::AF => { },
            // OpReg16::HLInc => { },
            // OpReg16::HLDec => { },
        }
    }

    fn add_and_set_flags(&mut self, op1: u8, op2: u8, destination: OpReg8, modify_carry: bool) {
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
    fn sub_and_set_flags(&mut self, op1: u8, op2: u8, destination: OpReg8, modify_carry: bool) {
        let diff = op1.wrapping_sub(op2);
        self.reg8_write(destination, diff);

        self.flags.set(Flags::Z, diff == 0);
        self.flags.set(Flags::N, true);
        self.flags.set(Flags::H, (op1 & 0x0f) < (op2 & 0x0f));
        if modify_carry {
            self.flags.set(Flags::C, op1 < op2);
        }
    }

    fn adc_and_set_flags(&mut self, val: u8) {
        if !self.flags.contains(Flags::C) {
            self.add_and_set_flags(self.a, val, OpReg8::A, true);
        } else {
            // I think the logic is to set the H and C flags if a carry happens 
            // in either of the two additions (op1 + op2 + 1).
            self.add_and_set_flags(self.a, 1, OpReg8::A, true);
            let flags = self.flags.clone();
            self.add_and_set_flags(self.a, val, OpReg8::A, true);
            self.flags |= flags;
        }
    }

    fn sbc_and_set_flags(&mut self, val: u8) {
        if !self.flags.contains(Flags::C) {
            self.sub_and_set_flags(self.a, val, OpReg8::A, true);
        } else {
            // I think the logic is to set the H and C flags if a carry happens 
            // in either of the two subitions (op1 + op2 + 1).
            self.sub_and_set_flags(self.a, 1, OpReg8::A, true);
            let flags = self.flags.clone();
            self.sub_and_set_flags(self.a, val, OpReg8::A, true);
            self.flags |= flags;
        }
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

    pub fn run(&mut self) {
        self.run_with_callback(|_| {});
    }

    /// Run the CPU and run callback *before* each instruction executed
    pub fn run_with_callback<F>(&mut self, mut callback: F) 
    where F: FnMut(&mut Cpu)
    {
        loop {
            callback(self);

            let instr = Instruction::decode(self.mem_read(self.pc),
                                            || {self.mem_read(self.pc + 1)},
                                            || {self.mem_read(self.pc + 2)});

            println!("{:04x}: {:02x} {} {} {}", 
                      self.pc,
                      self.mem_read(self.pc),
                      if instr.length >= 2 { format!("{:02x}", self.mem_read(self.pc + 1)) } else { "  ".to_string() },
                      if instr.length == 3 { format!("{:02x}", self.mem_read(self.pc + 2)) } else { "  ".to_string() },
                      instr);

            self.pc += instr.length;
            self.n_cycles += instr.cycles;
            self.n_instrs += 1;

            match instr.opcode {
                Opcode::NoOp => {}
                Opcode::Halt => { return; }
                Opcode::Stop => { return; }
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
                    // add lower 8 bits as an unsigned int
                    let lo = (self.sp & 0xff) as u8;
                    let hi = (self.sp >> 8) as u8;
                    self.add_and_set_flags(lo, rel as u8, OpReg8::L, true);

                    // carry or borrow into the upper 8 bits depending on result
                    if (rel > 0) & self.flags.contains(Flags::C) {
                        self.add_and_set_flags(hi, 1, OpReg8::H, true);
                    } else if (rel < 0) & !self.flags.contains(Flags::C) {
                        self.sub_and_set_flags(hi, 1, OpReg8::H, true);
                    } else {
                        self.h = hi;
                        self.flags.remove(Flags::C | Flags::H);
                    }

                    self.flags.remove(Flags::Z | Flags::N);
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

                Opcode::Add(reg) => { self.add_and_set_flags(self.a, self.reg8_read(reg), OpReg8::A, true); }
                Opcode::Adc(reg) => { self.adc_and_set_flags(self.reg8_read(reg)); }
                Opcode::Sub(reg) => { self.sub_and_set_flags(self.a, self.reg8_read(reg), OpReg8::A, true); }
                Opcode::Sbc(reg) => { self.sbc_and_set_flags(self.reg8_read(reg)); }
                Opcode::AddImm(val) => { self.add_and_set_flags(self.a, val, OpReg8::A, true); }
                Opcode::AdcImm(val) => { self.adc_and_set_flags(val); }
                Opcode::SubImm(val) => { self.sub_and_set_flags(self.a, val, OpReg8::A, true); }
                Opcode::SbcImm(val) => { self.sbc_and_set_flags(val); }

                Opcode::Inc(reg) => { self.add_and_set_flags(self.reg8_read(reg), 1, reg, false); }
                Opcode::Dec(reg) => { self.sub_and_set_flags(self.reg8_read(reg), 1, reg, false); }

                Opcode::Jump(addr) => { self.pc = addr; }

                Opcode::DisableInterrupts => { self.interrupt_master_enable = false; }
                Opcode::EnableInterrupts => { self.interrupt_master_enable = true; }

                Opcode::Call(addr) => {
                    self.push_onto_stack(self.pc);
                    self.pc = addr;
                }

                Opcode::NotImplemented(_opcode) => { panic!("Unimplemented opcode")}
            }

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
        // LD L,$22
        // PUSH HL
        cpu.run_instructions_and_halt(&[0x26, 0x11, 0x2e, 0x22, 0xe5]);
        assert_eq!(cpu.sp, 0xffee);
        assert_eq!(cpu.h, 0x11);
        assert_eq!(cpu.l, 0x22);
        // POP  DE
        cpu.run_instructions_and_halt(&[0xd1]);
        assert_eq!(cpu.sp, 0xfff0);
        assert_eq!(cpu.d, 0x11);
        assert_eq!(cpu.e, 0x22);
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
        cpu.run_instructions_and_halt(&[0xc3, 0x34, 0x12]);
        assert_eq!(cpu.pc, 0x1235);
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
}
