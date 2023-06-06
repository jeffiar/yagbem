use std::fmt;
use bitmatch::bitmatch;
use strum_macros;
use strum_macros::FromRepr;
use duplicate::duplicate_item;

#[derive(Debug)]
pub struct Instruction {
    pub opcode: Opcode,
    pub length: u16,
    pub cycles: u64,
}

#[derive(Debug, PartialEq, Clone, Copy, FromRepr, strum_macros::Display)]
pub enum OpReg8 { 
    B,
    C,
    D,
    E,
    H,
    L,
    #[strum(serialize = "(HL)")]
    HLIndirect,
    A,
}

#[derive(Debug, PartialEq, Clone, Copy, FromRepr, strum_macros::Display)]
pub enum OpReg16 { 
    BC,
    DE,
    HL,
    SP,
    AF,
    #[strum(serialize = "HL+")]
    HLInc,
    #[strum(serialize = "HL-")]
    HLDec,
}

#[derive(Debug, PartialEq, Clone, Copy, FromRepr, strum_macros::Display)]
pub enum Condition { 
    NZ,
    Z,
    NC,
    C
}

#[duplicate_item(OpField; [OpReg8]; [OpReg16]; [Condition];)]
impl OpField {
    fn parse(val: u8) -> OpField {
        OpField::from_repr(val.into()).expect("Failed to convert Opfield")
    }
}

#[derive(Debug, PartialEq)]
pub enum Opcode {
    NoOp,
    Halt,
    Stop,
    Load8(OpReg8, u8),
    LoadReg(OpReg8, OpReg8),
    LoadIndirectImm(OpReg8, u16),
    LoadIndirect(OpReg8, OpReg16),
    StoreIndirectImm(u16, OpReg8),
    StoreIndirect(OpReg16, OpReg8),
    LoadAFromIndC,
    StoreAToIndC,

    Load16(OpReg16, u16),
    LoadSPHL,
    LoadHLSPRelative(i8),
    StoreSP(u16),
    Push(OpReg16),
    Pop(OpReg16),

    Add(OpReg8),
    Adc(OpReg8),
    Sub(OpReg8),
    Sbc(OpReg8),
    And(OpReg8),
    Xor(OpReg8),
    Or(OpReg8),
    Cp(OpReg8),
    AddImm(u8),
    AdcImm(u8),
    SubImm(u8),
    SbcImm(u8),
    AndImm(u8),
    XorImm(u8),
    OrImm(u8),
    CpImm(u8),

    Inc(OpReg8),
    Dec(OpReg8),

    IncPair(OpReg16),
    DecPair(OpReg16),

    Jump(u16),
    JumpCond(Condition, u16),
    JumpRelative(i8),
    JumpCondRelative(Condition, i8),

    Call(u16),
    Return,

    DisableInterrupts,
    EnableInterrupts,

    NotImplemented(u8),
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Opcode::*;
        match &self.opcode {
            NoOp                    => write!(f, "NOP"),
            Halt                    => write!(f, "HALT"),
            Stop                    => write!(f, "STOP"),
            Load8(r, n)             => write!(f, "LD   {r},${n:02x}"),
            LoadReg(r, s)           => write!(f, "LD   {r},{s}"),
            LoadIndirectImm(r, mn)  => write!(f, "LD   {r},(${mn:04x})"),
            LoadIndirect(r, hl)     => write!(f, "LD   {r},({hl})"),
            StoreIndirectImm(mn, r) => write!(f, "LD   (${mn:04x}),{r}"), // TODO check if zero pg
            StoreIndirect(hl, r)    => write!(f, "LD   ({hl}),{r}"),
            LoadAFromIndC           => write!(f, "LD   A,($ff00+C)"),
            StoreAToIndC            => write!(f, "LD   ($ff00+C),A"),

            Load16(dd, mn)          => write!(f, "LD   {dd},${mn:04x}"),
            LoadSPHL                => write!(f, "LD   SP,HL"),
            LoadHLSPRelative(e)     => write!(f, "LD   HL,SP+${e:02x}"),
            StoreSP(mn)             => write!(f, "LD   (${mn:04x}),SP"),
            Push(qq)                => write!(f, "PUSH {qq}"),
            Pop(qq)                 => write!(f, "POP  {qq}"),

            Add(r)                  => write!(f, "ADD  A,{r}"),
            Adc(r)                  => write!(f, "ADC  A,{r}"),
            Sub(r)                  => write!(f, "SUB  A,{r}"),
            Sbc(r)                  => write!(f, "SBC  A,{r}"),
            And(r)                  => write!(f, "AND  A,{r}"),
            Xor(r)                  => write!(f, "XOR  A,{r}"),
            Or(r)                   => write!(f, "OR   A,{r}"),
            Cp(r)                   => write!(f, "CP   A,{r}"),

            AddImm(n)               => write!(f, "ADD  A,${n:02x}"),
            AdcImm(n)               => write!(f, "ADC  A,${n:02x}"),
            SubImm(n)               => write!(f, "SUB  A,${n:02x}"),
            SbcImm(n)               => write!(f, "SBC  A,${n:02x}"),
            AndImm(n)               => write!(f, "AND  A,${n:02x}"),
            XorImm(n)               => write!(f, "XOR  A,${n:02x}"),
            OrImm(n)                => write!(f, "OR   A,${n:02x}"),
            CpImm(n)                => write!(f, "CP   A,${n:02x}"),

            Inc(r)                  => write!(f, "INC  {r}"),
            Dec(r)                  => write!(f, "DEC  {r}"),

            IncPair(dd)             => write!(f, "INC  {dd}"),
            DecPair(dd)             => write!(f, "DEC  {dd}"),

            Jump(mn)                => write!(f, "JP   ${mn:04x}"),
            JumpCond(c,mn)          => write!(f, "JP   {c},${mn:04x}"),
            JumpRelative(e)         => write!(f, "JR   ${e:02x}"),
            JumpCondRelative(c,e)   => write!(f, "JR   {c},${e:02x}"),

            Call(mn)                => write!(f, "CALL ${mn:04x}"),
            Return                  => write!(f, "RET"),

            DisableInterrupts       => write!(f, "DI"),
            EnableInterrupts        => write!(f, "EI"),

            NotImplemented(o)       => write!(f, "unimplemented opcode 0x{o:02x}"),
        }
    }
}

impl Instruction {
    fn new(opcode: Opcode, length: u16, cycles: u64) -> Instruction {
        Instruction {
            opcode, length, cycles,
        }
    }

    pub fn decode_from_bytes(code: &[u8]) -> Instruction {
        Self::decode(code[0], || { code [1] }, || { code[2] })
    }

    #[bitmatch]
    pub fn decode(opcode: u8, fetch_operand_1: impl Fn() -> u8, fetch_operand_2: impl Fn() -> u8) -> Instruction {
        let fetch_imm8 = &fetch_operand_1;
        let fetch_imm16 = || { (fetch_operand_2() as u16) << 8 | fetch_operand_1() as u16 };
        let ins = Self::new;
        use Opcode::*;

        #[bitmatch]
        match opcode {
            "00_000_000" => ins(NoOp, 1, 4),
            "01_110_110" => ins(Halt, 1, 4),
            "00_010_000" => ins(Stop, 1, 4),

            "00_110_110" => ins(Load8(OpReg8::HLIndirect, fetch_imm8()), 2, 12),
            "00_rrr_110" => ins(Load8(OpReg8::parse(r), fetch_imm8()), 2, 8),

            "01_110_sss" => ins(LoadReg(OpReg8::HLIndirect, OpReg8::parse(s)), 1, 8),
            "01_rrr_110" => ins(LoadReg(OpReg8::parse(r), OpReg8::HLIndirect), 1, 8),
            "01_rrr_sss" => ins(LoadReg(OpReg8::parse(r), OpReg8::parse(s)), 1, 4),

            // NOTE all the indirect operations involve register A so we could remove that argument
            "11_101_010" => ins(StoreIndirectImm(fetch_imm16(), OpReg8::A), 3, 16),
            "11_100_000" => ins(StoreIndirectImm(0xff00 | fetch_imm8() as u16, OpReg8::A), 2, 12),
            "00_000_010" => ins(StoreIndirect(OpReg16::BC, OpReg8::A), 1, 8),
            "00_010_010" => ins(StoreIndirect(OpReg16::DE, OpReg8::A), 1, 8),
            "00_100_010" => ins(StoreIndirect(OpReg16::HLInc, OpReg8::A), 1, 8),
            "00_110_010" => ins(StoreIndirect(OpReg16::HLDec, OpReg8::A), 1, 8),

            "11_111_010" => ins(LoadIndirectImm(OpReg8::A, fetch_imm16()), 3, 16),
            "11_110_000" => ins(LoadIndirectImm(OpReg8::A, 0xff00 | fetch_imm8() as u16), 2, 12),
            "00_001_010" => ins(LoadIndirect(OpReg8::A, OpReg16::BC), 1, 8),
            "00_011_010" => ins(LoadIndirect(OpReg8::A, OpReg16::DE), 1, 8),
            "00_101_010" => ins(LoadIndirect(OpReg8::A, OpReg16::HLInc), 1, 8),
            "00_111_010" => ins(LoadIndirect(OpReg8::A, OpReg16::HLDec), 1, 8),

            "11_100_010" => ins(StoreAToIndC, 1, 8),
            "11_110_010" => ins(LoadAFromIndC, 1, 8),

            "00_dd0_001" => ins(Load16(OpReg16::parse(d), fetch_imm16()), 3, 12),
            "11_111_001" => ins(LoadSPHL, 1, 8),
            "11_111_000" => ins(LoadHLSPRelative(fetch_imm8() as i8), 2, 12),
            "00_001_000" => ins(StoreSP(fetch_imm16()), 3, 20),
            "11_110_101" => ins(Push(OpReg16::AF), 1, 16),
            "11_qq0_101" => ins(Push(OpReg16::parse(q)), 1, 16),
            "11_110_001" => ins(Pop(OpReg16::AF), 1, 16),
            "11_qq0_001" => ins(Pop(OpReg16::parse(q)), 1, 16),

            "10_000_rrr" => ins(Add(OpReg8::parse(r)), 1, if r == 0b110 { 8 } else { 4 }),
            "10_001_rrr" => ins(Adc(OpReg8::parse(r)), 1, if r == 0b110 { 8 } else { 4 }),
            "10_010_rrr" => ins(Sub(OpReg8::parse(r)), 1, if r == 0b110 { 8 } else { 4 }),
            "10_011_rrr" => ins(Sbc(OpReg8::parse(r)), 1, if r == 0b110 { 8 } else { 4 }),
            "10_100_rrr" => ins(And(OpReg8::parse(r)), 1, if r == 0b110 { 8 } else { 4 }),
            "10_101_rrr" => ins(Xor(OpReg8::parse(r)), 1, if r == 0b110 { 8 } else { 4 }),
            "10_110_rrr" => ins(Or(OpReg8::parse(r)), 1, if r == 0b110 { 8 } else { 4 }),
            "10_111_rrr" => ins(Cp(OpReg8::parse(r)), 1, if r == 0b110 { 8 } else { 4 }),

            "11_000_110" => ins(AddImm(fetch_imm8()), 2, 8),
            "11_001_110" => ins(AdcImm(fetch_imm8()), 2, 8),
            "11_010_110" => ins(SubImm(fetch_imm8()), 2, 8),
            "11_011_110" => ins(SbcImm(fetch_imm8()), 2, 8),
            "11_100_110" => ins(AndImm(fetch_imm8()), 2, 8),
            "11_101_110" => ins(XorImm(fetch_imm8()), 2, 8),
            "11_110_110" => ins(OrImm(fetch_imm8()), 2, 8),
            "11_111_110" => ins(CpImm(fetch_imm8()), 2, 8),

            "00_rrr_100" => ins(Inc(OpReg8::parse(r)), 1, if r == 0b110 { 12 } else { 4 }),
            "00_rrr_101" => ins(Dec(OpReg8::parse(r)), 1, if r == 0b110 { 12 } else { 4 }),

            "00_dd0_011" => ins(IncPair(OpReg16::parse(d)), 1, 8),
            "00_dd1_011" => ins(DecPair(OpReg16::parse(d)), 1, 8),


            "11_000_011" => ins(Jump(fetch_imm16()), 3, 16),
            "11_0cc_010" => ins(JumpCond(Condition::parse(c), fetch_imm16()), 3, 12 /* +4 if branch taken*/),
            "00_011_000" => ins(JumpRelative(fetch_imm8() as i8), 2, 12),
            "00_1cc_000" => ins(JumpCondRelative(Condition::parse(c), fetch_imm8() as i8), 2, 8 /* +4 if branch taken */ ),

            "11_001_101" => ins(Call(fetch_imm16()), 3, 24),
            "11_001_001" => ins(Return, 1, 16),

            "11_110_011" => ins(DisableInterrupts, 1, 4),
            "11_111_011" => ins(EnableInterrupts, 1, 4),



            // _ => panic!("opcode 0x{:02x} not implemented", code[0]),
            "aaaaaaaa" => Self::new(Opcode::NotImplemented(a), 1, 4),
        }
    }
}




#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn display_opcodes() {
        let d = |code| {
            Instruction::decode_from_bytes(code).to_string()
        };

        assert_eq!("NOP", d(&[0x00]));
        assert_eq!("HALT", d(&[0x76]));
        assert_eq!("STOP", d(&[0x10]));
        assert_eq!("LD   D,$45", d(&[0x16, 0x45]));
        assert_eq!("LD   (HL),$22", d(&[0x36, 0x22]));
        assert_eq!("LD   E,D", d(&[0x5a]));
        assert_eq!("LD   C,(HL)", d(&[0x4e]));
        assert_eq!("LD   (HL),A", d(&[0x77]));
        assert_eq!("LD   E,H", d(&[0x5c]));
        assert_eq!("LD   A,(HL)", d(&[0x7e]));
        assert_eq!("LD   A,($abcd)", d(&[0xfa, 0xcd, 0xab]));
        assert_eq!("LD   A,(BC)", d(&[0x0a]));
        assert_eq!("LD   A,(DE)", d(&[0x1a]));
        assert_eq!("LD   ($abcd),A", d(&[0xea, 0xcd, 0xab]));
        assert_eq!("LD   (BC),A", d(&[0x02]));
        assert_eq!("LD   (DE),A", d(&[0x12]));
        assert_eq!("LD   A,($ff00+C)", d(&[0xf2]));
        assert_eq!("LD   ($ff00+C),A", d(&[0xe2]));
        assert_eq!("LD   A,($ff4d)", d(&[0xf0, 0x4d]));
        assert_eq!("LD   ($ffd4),A", d(&[0xe0, 0xd4]));
        assert_eq!("ADD  A,C", d(&[0x81]));
        assert_eq!("INC  (HL)", d(&[0x34]));
        assert_eq!("DEC  C", d(&[0x0d]));
        assert_eq!("SUB  A,D", d(&[0x92]));
        assert_eq!("SBC  A,D", d(&[0x9a]));
        assert_eq!("ADD  A,$69", d(&[0xc6, 0x69]));
        assert_eq!("LD   BC,$ccdd", d(&[0x01, 0xdd, 0xcc]));
        assert_eq!("LD   SP,HL", d(&[0xf9]));
        assert_eq!("PUSH BC", d(&[0xc5]));
        assert_eq!("PUSH DE", d(&[0xd5]));
        assert_eq!("PUSH HL", d(&[0xe5]));
        assert_eq!("PUSH AF", d(&[0xf5]));
        assert_eq!("POP  BC", d(&[0xc1]));
        assert_eq!("POP  DE", d(&[0xd1]));
        assert_eq!("POP  HL", d(&[0xe1]));
        assert_eq!("POP  AF", d(&[0xf1]));
        assert_eq!("LD   ($6655),SP", d(&[0x08, 0x55, 0x66]));
        assert_eq!("LD   HL,SP+$30", d(&[0xf8, 0x30]));
        assert_eq!("LD   HL,SP+$f0", d(&[0xf8, 0xf0]));
        assert_eq!("JP   $1234", d(&[0xc3, 0x34, 0x12]));
        assert_eq!("DI", d(&[0xf3]));
        assert_eq!("EI", d(&[0xfb]));
        assert_eq!("CALL $5544", d(&[0xcd, 0x44, 0x55]));
        assert_eq!("JR   $f0", d(&[0x18, 0xf0]));
        assert_eq!("RET", d(&[0xc9]));
        assert_eq!("INC  BC", d(&[0x03]));
        assert_eq!("INC  DE", d(&[0x13]));
        assert_eq!("INC  HL", d(&[0x23]));
        assert_eq!("INC  SP", d(&[0x33]));
        assert_eq!("DEC  BC", d(&[0x0B]));
        assert_eq!("DEC  DE", d(&[0x1B]));
        assert_eq!("DEC  HL", d(&[0x2B]));
        assert_eq!("DEC  SP", d(&[0x3B]));
        assert_eq!("LD   A,(HL+)", d(&[0x2a]));
        assert_eq!("LD   A,(HL-)", d(&[0x3a]));
        assert_eq!("LD   (HL+),A", d(&[0x22]));
        assert_eq!("LD   (HL-),A", d(&[0x32]));
        assert_eq!("AND  A,D", d(&[0xa2]));
        assert_eq!("XOR  A,H", d(&[0xac]));
        assert_eq!("OR   A,E", d(&[0xb3]));
        assert_eq!("CP   A,C", d(&[0xb9]));
        assert_eq!("AND  A,$05", d(&[0xe6, 0x05]));
        assert_eq!("XOR  A,$05", d(&[0xee, 0x05]));
        assert_eq!("OR   A,$05", d(&[0xf6, 0x05]));
        assert_eq!("CP   A,$05", d(&[0xfe, 0x05]));
        assert_eq!("JP   NZ,$1234", d(&[0xc2, 0x34, 0x12]));
        assert_eq!("JP   Z,$1234", d(&[0xca, 0x34, 0x12]));
        assert_eq!("JP   NC,$1234", d(&[0xd2, 0x34, 0x12]));
        assert_eq!("JP   C,$1234", d(&[0xda, 0x34, 0x12]));
        assert_eq!("JR   NZ,$12", d(&[0x20, 0x12]));
        assert_eq!("JR   Z,$12",  d(&[0x28, 0x12]));
        assert_eq!("JR   NC,$12", d(&[0x30, 0x12]));
        assert_eq!("JR   C,$12",  d(&[0x38, 0x12]));
    }
}
