use std::io::{stderr, Write};

const CYCLES_PER_FRAME: u64 = 114 * 154 * 4;

#[allow(dead_code)]
pub mod register {
    pub const P1   : u16 = 0xFF00; // Joypad input
    pub const SB   : u16 = 0xFF01; // Serial transfer data
    pub const SC   : u16 = 0xFF02; // Serial transfer control
    pub const DIV  : u16 = 0xFF03; // Divider register
    pub const TIMA : u16 = 0xFF04; // Timer counter
    pub const TMA  : u16 = 0xFF05; // Timer modulo
    pub const TAC  : u16 = 0xFF06; // Timer control
    pub const IF   : u16 = 0xFF0F; // Interrupt request flag
    pub const IE   : u16 = 0xFFFF; // Interrupt enable flag
    pub const LCDC : u16 = 0xFF40; // LCD control
    pub const STAT : u16 = 0xFF41; // LCD status information
    pub const SCY  : u16 = 0xFF42; // Scroll Y register
    pub const SCX  : u16 = 0xFF43; // Scroll X register
    pub const LY   : u16 = 0xFF44; // Current LCD scan line in frame
    pub const LCY  : u16 = 0xFF45; // LY compare register
    pub const DMA  : u16 = 0xFF46; // DMA transfer initiation register
    pub const BGP  : u16 = 0xFF47; // Background palette data
    pub const OBP0 : u16 = 0xFF48; // Object palette data 0
    pub const OBP1 : u16 = 0xFF49; // Object palette data 1
    pub const WX   : u16 = 0xFF4A; // Window x-coordinate
    pub const WY   : u16 = 0xFF4B; // Window y-cooridinate
    pub const NR10 : u16 = 0xFF10; // CH1 Freq Sweep
    pub const NR11 : u16 = 0xFF11; // CH1 Sound length / duty cycle
    pub const NR12 : u16 = 0xFF12; // CH1 Volume envelope
    pub const NR13 : u16 = 0xFF13; // CH1 Frequency lo
    pub const NR14 : u16 = 0xFF14; // CH1 Frequency hi / trigger
    pub const NR21 : u16 = 0xFF16; // CH2 Sound length / duty cycle
    pub const NR22 : u16 = 0xFF17; // CH2 Volume envelope
    pub const NR23 : u16 = 0xFF18; // CH2 Frequency lo
    pub const NR24 : u16 = 0xFF19; // CH2 Frequency hi / trigger
    pub const NR30 : u16 = 0xFF1A; // CH3 Sound off
    pub const NR31 : u16 = 0xFF1B; // CH3 Sound length
    pub const NR32 : u16 = 0xFF1C; // CH3 Volume
    pub const NR33 : u16 = 0xFF1D; // CH3 Frequency lo
    pub const NR34 : u16 = 0xFF1E; // CH3 Frequency hi / trigger
    pub const NR41 : u16 = 0xFF20; // CH4 Sound length
    pub const NR42 : u16 = 0xFF21; // CH4 Volume envelope
    pub const NR43 : u16 = 0xFF22; // CH4 Polynomial counter
    pub const NR44 : u16 = 0xFF23; // CH4 length / trigger
    pub const NR50 : u16 = 0xFF24; // Channel control / On-off / Volume
    pub const NR51 : u16 = 0xFF25; // Left / right selection
    pub const NR52 : u16 = 0xFF26; // Sound on-off
}


use bitflags::bitflags;

bitflags! {
    pub struct Interrupt: u8 {
        const VBLANK = 1 << 4; // Vertical Blanking
        const LCDC   = 1 << 3; // LCDC (STAT referenced)
        const TIMER  = 1 << 2; // Timer Overflow
        const SERIAL = 1 << 1; // Serial I/O Transfer Completion
        const INPUT  = 1 << 0; // P10-P13 Terminal Negative Edge
    }
}


pub trait Mem {
    fn mem_read(&self, addr: u16) -> u8;
    fn mem_write(&mut self, addr: u16, val: u8);

    fn mem_write16(&mut self, addr: u16, val: u16) {
        let lo = (val & 0xff) as u8;
        let hi = (val >> 8) as u8;
        self.mem_write(addr, lo);
        self.mem_write(addr.wrapping_add(1), hi);
    }

    fn mem_read16(&self, addr: u16) -> u16 {
        let lo = self.mem_read(addr);
        let hi = self.mem_read(addr.wrapping_add(1));
        (hi as u16) << 8 | lo as u16
    }

    fn mem_read_range(&self, start: u16, end: u16) -> &[u8];
}


#[allow(non_snake_case)]
pub struct Bus {
    pub mem: [u8; 0x10000],
    pub dirty_addrs: Vec<u16>,
    pub IE: Interrupt,
    pub IF: Interrupt,

    n_cycles: u64,
    next_vblank_n_cyc: u64,
}

impl Mem for Bus {
    fn mem_read(&self, addr: u16) -> u8 { 
        match addr {
            register::IE => self.IE.bits(),
            register::IF => self.IF.bits(),
            _ => self.mem[addr as usize] ,
        }
    }
    fn mem_write(&mut self, addr: u16, val: u8) { 
        match addr {
            register::IE => { self.IE = Interrupt::from_bits(val).expect("Bad Interrupt set"); }
            register::IF => { self.IF = Interrupt::from_bits(val).expect("Bad Interrupt set"); }
            register::SC => { 
                // Print the serial transfer byte as ASCII character to stderr
                if (val & 0xf0) != 0 {
                    eprint!("{}", self.mem_read(register::SB) as char);
                    stderr().flush().expect("Failed to write to stderr");
                }
            }
            _ => {}
        }
        self.mem[addr as usize] = val; 
        self.dirty_addrs.push(addr);
    }

    fn mem_read_range(&self, start: u16, end: u16) -> &[u8] {
        &self.mem[start as usize..end as usize]
    }

}

impl Bus {
    pub fn new() -> Bus {
        let mut bus = Bus { 
            mem: [0; 0x10000],
            dirty_addrs: Vec::new(),
            IE: Interrupt::empty(),
            IF: Interrupt::empty(),
            n_cycles: 0,
            next_vblank_n_cyc: CYCLES_PER_FRAME,
        };
        bus.mem_write(register::LCDC, 0x83);
        bus
    }

    pub fn reset(&mut self) {
        *self = Bus::new();
    }

    pub fn load(&mut self, program: &[u8], start: u16) {
        let start = start as usize;
        self.mem[start..(start + program.len())].copy_from_slice(program);
    }

    // pub fn mem_write_bit(&mut self, addr: u16, bit: u8, val: bool) {
        // Can do matching for registers here if desired
    // }
    pub fn clear_dirty_addrs(&mut self) {
        self.dirty_addrs.clear();
    }

    pub fn dirty_addrs(&self) -> &Vec<u16> {
        &self.dirty_addrs
    }

    pub fn sync(&mut self, n_cycles: u64) {
        if n_cycles >= self.next_vblank_n_cyc {
            self.next_vblank_n_cyc += CYCLES_PER_FRAME;
            self.IE.insert(Interrupt::VBLANK);
        };
        self.n_cycles = n_cycles;
    }
}




#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn load_program_to_bus() {
        let mut bus = Bus::new();

        assert_eq!(bus.mem_read(0x200), 0);
        bus.load(&[0x12, 0x34, 0x56], 0x0200);
        assert_eq!(bus.mem_read(0x200), 0x12);
        assert_eq!(bus.mem_read(0x201), 0x34);
        assert_eq!(bus.mem_read(0x202), 0x56);
        assert_eq!(bus.mem_read(0x203), 0x00);
    }

    #[test]
    fn bus_read_write_simple() {
        let mut bus = Bus::new();

        assert_eq!(bus.mem_read(0xc000), 0);
        bus.mem_write(0xc000, 42);
        assert_eq!(bus.mem_read(0xc000), 42);
    }

    #[test]
    fn reset_bus() {
        let mut bus = Bus::new();

        bus.mem_write(0xc010, 42);
        assert_eq!(bus.mem_read(0xc010), 42);
        bus.reset();
        assert_eq!(bus.mem_read(0xc010), 0);
    }

    #[test]
    fn bus_read16_simple() {
        let mut bus = Bus::new();
        bus.mem_write(0xc000, 0x42);
        bus.mem_write(0xc001, 0xaa);
        assert_eq!(bus.mem_read16(0xc000), 0xaa42);
    }

    #[test]
    fn bus_write16_simple() {
        let mut bus = Bus::new();
        bus.mem_write16(0xc100, 0x1122);
        assert_eq!(bus.mem_read(0xc100), 0x22);
        assert_eq!(bus.mem_read(0xc101), 0x11);
    }
}
