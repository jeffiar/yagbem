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

    n_cycles: u64,
    bus: Bus,
}

impl Mem for Cpu {
    fn mem_read(&self, addr: u16) -> u8 { 
        self.bus.mem_read(addr) 
    }

    fn mem_write(&mut self, addr: u16, val: u8) { 
        self.bus.mem_write(addr, val) 
    }
}

impl Cpu {
    pub fn new() -> Self {
        Cpu {
            a: 0,  b: 0,  c: 0,  d: 0,
            e: 0,  
            h: 0,  l: 0,
            flags: Flags::empty(),
            pc: PC_START, 
            sp: SP_START,
            n_cycles: 0,
            bus: Bus::new(),
        }
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
        // self.bus.reset();
    }

    pub fn load(&mut self, program: &[u8]) {
        self.bus.load(program);
    }

    pub fn load_and_run(&mut self, program: &[u8]) {
        // self.reset();
        self.load(program);
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

    pub fn run(&mut self) {
        self.pc = PC_START;

        loop {
            let instr = Instruction::decode(self.mem_read(self.pc),
                                            || {self.mem_read(self.pc + 1)},
                                            || {self.mem_read(self.pc + 2)});
            self.pc += instr.length;
            self.n_cycles += instr.cycles;

            eprintln!("{}", instr);
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
                Opcode::Push(reg_pair) => {
                    let val = self.reg16_read(reg_pair);
                    self.sp = self.sp.wrapping_sub(2);
                    self.mem_write16(self.sp, val);
                }
                Opcode::Pop(reg_pair) => {
                    let val = self.mem_read16(self.sp);
                    self.reg16_write(reg_pair, val);
                    self.sp = self.sp.wrapping_add(2);
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

                Opcode::NotImplemented(_opcode) => { continue; }
            }

        }
    }
}




#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reset() {
        let mut cpu = Cpu::new();
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
        let mut cpu = Cpu::new();
        assert_eq!(cpu.h, 0);

        // LD   H,$c0
        // LD   L,$ee
        // LD   (HL),$56
        // HALT
        cpu.load_and_run(&[0x26, 0xc0, 0x2e, 0xee, 0x36, 0x56, 0x76]);
        assert_eq!(cpu.h, 0xc0);
        assert_eq!(cpu.l, 0xee);
        assert_eq!(cpu.mem_read(0xc0ee), 0x56);
    }

    #[test]
    fn load_reg_into_hlind() {
        let mut cpu = Cpu::new();
        // LD   C,$23
        // LD   B,C
        // LD   H,$44
        // LD   (HL),C
        // HALT
        cpu.load_and_run(&[0x0e, 0x23, 0x41, 0x26, 0x44, 0x71, 0x76]);
        assert_eq!(cpu.c, 0x23);
        assert_eq!(cpu.b, 0x23);
        assert_eq!(cpu.h, 0x44);
        assert_eq!(cpu.mem_read(0x4400), 0x23);
    }

    #[test]
    fn load_reg() {
        let mut cpu = Cpu::new();
        assert_eq!(cpu.h, 0);

        // LD   A,$33
        // LD   B,A     ; B = $33
        // LD   A,$55
        // LD   E,A     ; E = $55
        // LD   H,E     ; H = $55
        // HALT
        let code = [0x3e, 0x33, 0x47, 0x3e, 0x55, 0x5f, 0x63, 0x76];
        cpu.load_and_run(&code);
        assert_eq!(cpu.b, 0x33);
        assert_eq!(cpu.e, 0x55);
        assert_eq!(cpu.h, 0x55);
        assert_eq!(cpu.c, 0x00);
    }

    #[test]
    fn load_indirect_immediate_0xfa() {
        let mut cpu = Cpu::new();
        assert_ne!(cpu.a, 0x45);
        cpu.mem_write(0x33a4, 0x45);
        cpu.load_and_run(&[0xfa, 0xa4, 0x33, 0x76]); //LD   A,($33a4)
        assert_eq!(cpu.a, 0x45);
        assert_eq!(cpu.n_cycles, 16 + 4);
    }

    #[test]
    fn load_indirect_0x0a() {
        let mut cpu = Cpu::new();
        cpu.b = 0x55;
        cpu.c = 0xa4;
        cpu.mem_write(0x55a4, 0x69);
        cpu.load_and_run(&[0x0a, 0x76]); // LD   A,(BC)
        assert_eq!(cpu.a, 0x69);
        assert_eq!(cpu.n_cycles, 8 + 4);
    }

    #[test]
    fn store_indirect_immediate_0xea() {
        let mut cpu = Cpu::new();
        cpu.a = 0x2f;
        cpu.load_and_run(&[0xea, 0x52, 0xcc, 0x76]); // LD   (DE),A
        assert_eq!(cpu.mem_read(0xcc52), 0x2f);
        assert_eq!(cpu.n_cycles, 16 + 4);
    }

    #[test]
    fn store_indirect_0x12() {
        let mut cpu = Cpu::new();
        cpu.d = 0x11;
        cpu.e = 0x22;
        cpu.a = 0x42;
        cpu.load_and_run(&[0x12, 0x76]); // LD   (DE),A
        assert_eq!(cpu.mem_read(0x1122), 0x42);
        assert_eq!(cpu.mem_read(0x2211), 0x00);
        assert_eq!((cpu.d, cpu.e, cpu.a), (0x11, 0x22, 0x42));
        assert_eq!(cpu.n_cycles, 8 + 4);
    }

    #[test]
    fn load_indirect_zeropage_0xf2() {
        let mut cpu = Cpu::new();
        cpu.c = 0xa4;
        cpu.mem_write(0xffa4, 0x69);
        cpu.load_and_run(&[0xf2, 0x76]); // LD   A,($ff00+C)
        assert_eq!(cpu.a, 0x69);
        assert_eq!(cpu.n_cycles, 8 + 4);
    }

    #[test]
    fn store_indirect_zeropage_0xe2() {
        let mut cpu = Cpu::new();
        cpu.c = 0xcc;
        cpu.a = 0xaa;
        cpu.load_and_run(&[0xe2, 0x76]); // LD   ($ff00+C),A
        assert_eq!(cpu.mem_read(0xffcc), 0xaa);
        assert_eq!(cpu.n_cycles, 8 + 4);
    }

    #[test]
    fn load_indirect_zeropage_immediate_0xf0() {
        let mut cpu = Cpu::new();
        cpu.mem_write(0xff32, 0x69);
        cpu.load_and_run(&[0xf0, 0x32, 0x76]); // LD   A,($ff00+$32)
        assert_eq!(cpu.a, 0x69);
        assert_eq!(cpu.n_cycles, 12 + 4);
    }

    #[test]
    fn store_indirect_zeropage_immediate_0xe0() {
        let mut cpu = Cpu::new();
        cpu.a = 0xab;
        cpu.load_and_run(&[0xe0, 0x56, 0x76]); // LD   ($ff00+$56),A
        assert_eq!(cpu.mem_read(0xff56), 0xab);
        assert_eq!(cpu.n_cycles, 12 + 4);
    }

    enum ROI { Reg(OpReg8), Immediate } // Register Or Immediate

    fn test_math(opcode : u8, reg_or_imm: ROI, operand_1: u8, operand_2: u8, result: u8, flags: Flags) {
        match reg_or_imm {
            ROI::Reg(r) => {
                // Test the version between operands A and B
                let mut cpu = Cpu::new();
                cpu.a = operand_1;
                cpu.reg8_write(r, operand_2);
                cpu.load_and_run(&[opcode, 0x76]);
                assert_eq!(cpu.a, result);
                assert_eq!(cpu.flags, flags);
            }
            ROI::Immediate => {
                // Test the version with an immediate operand
                let mut cpu = Cpu::new();
                cpu.a = operand_1;
                cpu.load_and_run(&[opcode, operand_2, 0x76]);
                assert_eq!(cpu.a, result);
                assert_eq!(cpu.flags, flags);
            }
        }
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
            let mut cpu = Cpu::new();
            cpu.reg8_write(OpReg8::A, 0xe1);
            cpu.reg8_write(OpReg8::E, 0x0f);
            cpu.reg8_write(OpReg8::HLIndirect, 0x1e);
            cpu.flags.set(Flags::C, true);
            cpu
        };

        let mut cpu = init_cpu();
        cpu.load_and_run(&[0x8b, 0x76]);
        assert_eq!(cpu.a, 0xf1);
        assert_eq!(cpu.flags, Flags::H);

        let mut cpu = init_cpu();
        cpu.load_and_run(&[0xce, 0x3b, 0x76]);
        assert_eq!(cpu.a, 0x1d);
        assert_eq!(cpu.flags, Flags::C);

        let mut cpu = init_cpu();
        cpu.load_and_run(&[0x8e, 0x76]);
        assert_eq!(cpu.a, 0x00);
        assert_eq!(cpu.flags, Flags::H | Flags::Z | Flags::C);
    }

    #[test]
    fn sbc_simple_carry_only() {
        //from the nintendo manual
        let init_cpu = || {
            let mut cpu = Cpu::new();
            cpu.reg8_write(OpReg8::A, 0x3b);
            cpu.reg8_write(OpReg8::H, 0x2a);
            cpu.reg8_write(OpReg8::HLIndirect, 0x4f);
            cpu.flags.set(Flags::C, true);
            cpu
        };

        let mut cpu = init_cpu();
        cpu.load_and_run(&[0x9c, 0x76]);
        assert_eq!(cpu.a, 0x10);
        assert_eq!(cpu.flags, Flags::N);

        let mut cpu = init_cpu();
        cpu.load_and_run(&[0xde, 0x3a, 0x76]);
        assert_eq!(cpu.a, 0x00);
        assert_eq!(cpu.flags, Flags::N | Flags::Z);

        let mut cpu = init_cpu();
        cpu.load_and_run(&[0x9e, 0x76]);
        assert_eq!(cpu.a, 0xeb);
        assert_eq!(cpu.flags, Flags::N | Flags::H | Flags::C);
    }


    #[test]
    fn inc() {
        let mut cpu = Cpu::new();
        cpu.b = 3;
        cpu.load_and_run(&[0x04, 0x76]); // INC B
        assert_eq!(cpu.b, 4);
        assert_eq!(cpu.flags, Flags::empty());
        // check overflow
        cpu.d = 0xff;
        cpu.load_and_run(&[0x14, 0x76]); // INC D
        assert_eq!(cpu.d, 0);
        assert_eq!(cpu.flags, Flags::Z | Flags::H);
        // check half-carry flag
        cpu.d = 0x0f;
        cpu.load_and_run(&[0x14, 0x76]); // INC D
        assert_eq!(cpu.d, 0x10);
        assert_eq!(cpu.flags, Flags::H);
    }

    #[test]
    fn dec() {
        let mut cpu = Cpu::new();
        cpu.b = 2;
        cpu.load_and_run(&[0x05, 0x76]); // DEC B
        assert_eq!(cpu.b, 1);
        assert_eq!(cpu.flags, Flags::N);
        // check zero-flag
        cpu.load_and_run(&[0x05, 0x76]); // DEC B
        assert_eq!(cpu.b, 0);
        assert_eq!(cpu.flags, Flags::N | Flags::Z);
        // check underflow
        cpu.load_and_run(&[0x05, 0x76]); // INC D
        assert_eq!(cpu.b, 0xff);
        assert_eq!(cpu.flags, Flags::N | Flags::H);
        // check carry flag is not affected
        cpu.flags.set(Flags::C, true);
        cpu.load_and_run(&[0x05, 0x76]); // INC D
        assert_eq!(cpu.b, 0xfe);
        assert_eq!(cpu.flags, Flags::N | Flags::C);
    }

    #[test]
    fn load_16bit_immediate() {
        let mut cpu = Cpu::new();
        cpu.load_and_run(&[0x11, 0x52, 0xaa, 0x76]); // LD   DE,$76aa
        assert_eq!(cpu.d, 0xaa);
        assert_eq!(cpu.e, 0x52);
        cpu.load_and_run(&[0x31, 0x34, 0x12, 0x76]); // LD   SP,$1234
        assert_eq!(cpu.sp, 0x1234);
        cpu.load_and_run(&[0x21, 0x21, 0x43, 0x76]); // LD   HL,$4321
        assert_eq!(cpu.h, 0x43);
        assert_eq!(cpu.l, 0x21);
        assert_eq!(cpu.reg16_read(OpReg16::HL), 0x4321);
    }

    #[test]
    fn load_sphl() {
        let mut cpu = Cpu::new();
        cpu.h = 0x55;
        cpu.l = 0x23;
        cpu.load_and_run(&[0xf9, 0x76]); // LD   SP,HL
        assert_eq!(cpu.sp, 0x5523);
    }

    #[test]
    fn push() {
        let mut cpu = Cpu::new();
        cpu.b = 0x44; // hi reg
        cpu.c = 0x55; // lo reg
        cpu.sp = 0xfffe;
        cpu.load_and_run(&[0xc5, 0x76]); // PUSH BC
        assert_eq!(cpu.sp, 0xfffc);
        assert_eq!(cpu.mem_read(0xfffc), 0x55); // lo addr, lo reg
        assert_eq!(cpu.mem_read(0xfffd), 0x44); // hi addr, hi reg
    }

    #[test]
    fn push_af() {
        let mut cpu = Cpu::new();
        cpu.sp = 0xfffe;
        cpu.a = 0xff;
        // ADD  A,$02 ; A = 0x01, F = 0b00010000 just carry flag
        cpu.load_and_run(&[0xc6, 0x02, 0x76]);
        assert_eq!(cpu.a, 0x01);
        assert_eq!(cpu.flags, Flags::C | Flags::H);
        assert_eq!(cpu.sp, 0xfffe);
        // PUSH AF
        cpu.load_and_run(&[0xf5, 0x76]);
        assert_eq!(cpu.sp, 0xfffc);
        assert_eq!(cpu.mem_read(0xfffd), 0x01);
        assert_eq!(cpu.mem_read(0xfffc), (Flags::C | Flags::H).bits());
    }

    #[test]
    fn pop() {
        let mut cpu = Cpu::new();
        cpu.sp = 0xfff0;
        cpu.mem_write(0xfff0, 0x5f); //lo address, lo bits
        cpu.mem_write(0xfff1, 0x3c); //hi address, hi bits
        // POP  DE
        cpu.load_and_run(&[0xd1, 0x76]);
        assert_eq!(cpu.d, 0x3c); //hi reg
        assert_eq!(cpu.e, 0x5f); //lo reg
        assert_eq!(cpu.sp, 0xfff2);
    }

    #[test]
    fn pushpop() {
        let mut cpu = Cpu::new();
        cpu.sp = 0xfff0;
        // LD H,$11
        // LD L,$22
        // PUSH HL
        cpu.load_and_run(&[0x26, 0x11, 0x2e, 0x22, 0xe5, 0x76]);
        assert_eq!(cpu.sp, 0xffee);
        assert_eq!(cpu.h, 0x11);
        assert_eq!(cpu.l, 0x22);
        // POP  DE
        cpu.load_and_run(&[0xd1, 0x76]);
        assert_eq!(cpu.sp, 0xfff0);
        assert_eq!(cpu.d, 0x11);
        assert_eq!(cpu.e, 0x22);
    }

    // TODO worry about memory mapping during the tests, want a fake memory
}
