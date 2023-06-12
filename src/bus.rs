use std::io::{stderr, Write};
use crate::ppu::Ppu;

#[allow(dead_code)]
pub mod register {
    pub const P1   : u16 = 0xFF00; // Joypad input
    pub const SB   : u16 = 0xFF01; // Serial transfer data
    pub const SC   : u16 = 0xFF02; // Serial transfer control
    pub const DIV  : u16 = 0xFF04; // Divider register
    pub const TIMA : u16 = 0xFF05; // Timer counter
    pub const TMA  : u16 = 0xFF06; // Timer modulo
    pub const TAC  : u16 = 0xFF07; // Timer control
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

impl Interrupt {
    pub fn handler_addr(i: Interrupt) -> u16 {
        match i.bits() {
            0b00001..=0b00001 => 0x60,
            0b00010..=0b00011 => 0x58,
            0b00100..=0b00111 => 0x50,
            0b01000..=0b01111 => 0x48,
            0b10000..=0b10000 => 0x48,
            _ => unreachable!()
        }
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

    fn mem_write_range(&mut self, start: u16, data: &[u8]) {
        for i in 0..data.len() {
            self.mem_write(start + i as u16, data[i]);
        }
    }
}

struct Timer {
    div_counter: u64,
    tima_counter: u64,
    tma: u8,
    period_log_2: u8,
    running: bool,
}

impl Timer {
    const TIMA_MASK: u64 = (0xff << 10);
    fn new() -> Timer {
        Timer {
            div_counter: 0,
            tima_counter: 0,
            tma: 0,
            period_log_2: 10,
            running: false,
        }
    }

    /// Returns whether a timer overflow has ocurred
    fn tick(&mut self, nticks: u64, mem: &mut [u8]) -> bool {
        self.div_counter += nticks;
        mem[register::DIV as usize] = (self.div_counter >> 9) as u8;

        if !self.running {
            return false;
        }

        self.tima_counter += nticks << (10 - self.period_log_2);
        let tima = self.tima_counter >> 10;
        if tima >= 256 {
            mem[register::TIMA as usize] = self.tma;
            self.set_tima(self.tma);
            return true;
        } else {
            mem[register::TIMA as usize] = tima as u8;
            return false;
        }

    }

    fn set_running(&mut self, running: bool) { self.running = running; }

    fn reset_div_counter(&mut self)  { 
        self.div_counter = 0;
        self.tima_counter &= Timer::TIMA_MASK;
    }

    fn set_tma(&mut self, tma: u8)   { self.tma = tma; }

    fn set_tima(&mut self, tima: u8) { 
        self.tima_counter = (tima as u64) << 10; // | (0x3ff & self.tima_counter);
    }

    fn set_tima_period(&mut self, period_bits: u8) {
        self.period_log_2 = match period_bits {
            0b00 => 10,
            0b01 => 4,
            0b10 => 6,
            0b11 => 8,
            _    => unreachable!(),
        };
    }
}

#[allow(non_snake_case)]
pub struct Bus {
    pub mem: [u8; 0x10000],
    pub flat: bool,
    pub IE: Interrupt,
    pub IF: Interrupt,

    n_cycles: u64,
    dirty_addrs: Vec<u16>,

    timer: Timer,
    pub ppu: Ppu,
}

impl Mem for Bus {
    fn mem_read(&self, addr: u16) -> u8 { 
        if self.flat {
            return self.mem[addr as usize];
        }

        match addr {
            register::IE => self.IE.bits(),
            register::IF => self.IF.bits(),
            register::SC => 0x00,
            _ => self.mem[addr as usize] ,
        }
    }

    fn mem_write(&mut self, addr: u16, val: u8) { 
        if self.flat {
            self.mem[addr as usize] = val; 
            self.dirty_addrs.push(addr);
            return;
        }

        match addr {
            register::IE => { self.IE = Interrupt::from_bits(val).expect("Bad Interrupt set"); }
            register::IF => { self.IF = Interrupt::from_bits(val).expect("Bad Interrupt set"); }
            register::LY => { panic!("Register LY (0xff44) is not writeable"); }
            register::DIV => {
                self.timer.reset_div_counter();
            }
            register::TAC => {
                self.timer.set_running((val & 0b100) != 0);
                self.timer.set_tima_period(val & 0b011);
            }
            register::TMA => {
                self.timer.set_tma(val);
            }
            register::TIMA => {
                self.timer.set_tima(val);
            }
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
        let mut bus = Bus::new_flat();
        bus.flat = false;
        bus.mem_write(register::LCDC, 0x83);
        bus
    }

    pub fn new_flat() -> Bus {
        Bus { 
            mem: [0; 0x10000],
            dirty_addrs: Vec::new(),
            flat: true,
            IE: Interrupt::empty(),
            IF: Interrupt::empty(),
            n_cycles: 0,
            timer: Timer::new(),
            ppu: Ppu::new(),
        }
    }

    pub fn load(&mut self, program: &[u8], start: u16) {
        let start = start as usize;
        self.mem[start..(start + program.len())].copy_from_slice(program);
    }

    pub fn clear_dirty_addrs(&mut self) {
        self.dirty_addrs.clear();
    }

    pub fn dirty_addrs(&self) -> &Vec<u16> {
        &self.dirty_addrs
    }

    pub fn sync(&mut self, n_cycles: u64) {
        let nticks = n_cycles - self.n_cycles;

        if self.timer.tick(nticks, &mut self.mem) {
            self.IF.insert(Interrupt::TIMER);
        }

        self.ppu.tick(nticks, &mut self.mem);

        if self.ppu.should_trigger_vblank() {
            self.IF.insert(Interrupt::VBLANK);
        }
        if self.ppu.should_trigger_stat() {
            self.IF.insert(Interrupt::LCDC);
        }

        self.n_cycles = n_cycles;
    }

    pub fn render_frame(&mut self) -> &[u8] {
        self.ppu.render_frame(&self.mem)
    }
}




#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn load_program_to_bus() {
        let mut bus = Bus::new_flat();

        assert_eq!(bus.mem_read(0x200), 0);
        bus.load(&[0x12, 0x34, 0x56], 0x0200);
        assert_eq!(bus.mem_read(0x200), 0x12);
        assert_eq!(bus.mem_read(0x201), 0x34);
        assert_eq!(bus.mem_read(0x202), 0x56);
        assert_eq!(bus.mem_read(0x203), 0x00);
    }

    #[test]
    fn bus_read_write_simple() {
        let mut bus = Bus::new_flat();

        assert_eq!(bus.mem_read(0xc000), 0);
        bus.mem_write(0xc000, 42);
        assert_eq!(bus.mem_read(0xc000), 42);
    }

    #[test]
    fn bus_read16_simple() {
        let mut bus = Bus::new_flat();
        bus.mem_write(0xc000, 0x42);
        bus.mem_write(0xc001, 0xaa);
        assert_eq!(bus.mem_read16(0xc000), 0xaa42);
    }

    #[test]
    fn bus_write16_simple() {
        let mut bus = Bus::new_flat();
        bus.mem_write16(0xc100, 0x1122);
        assert_eq!(bus.mem_read(0xc100), 0x22);
        assert_eq!(bus.mem_read(0xc101), 0x11);
    }

    #[test]
    fn div_register() {
        let mut bus = Bus::new();

        assert_eq!(bus.mem_read(register::DIV), 0);

        bus.sync(420000);
        bus.mem_write(register::DIV, 0x40);
        bus.sync(420000);
        assert_eq!(bus.mem_read(register::DIV), 0);

        bus.sync(420000 + (1 << 9));
        assert_eq!(bus.mem_read(register::DIV), 1);

        bus.sync(420000 + (69 << 9));
        assert_eq!(bus.mem_read(register::DIV), 69);
    }

    #[test]
    fn timer_basic() {
        let mut bus = Bus::new();

        bus.sync(42_000);

        bus.mem_write(register::TAC, 3);
        bus.mem_write(register::TAC, 7);
        bus.sync(42_000 + (5 * (1 << 8)));
        assert_eq!(bus.mem_read(register::TIMA), 5);

         // stop timer
        bus.mem_write(register::TAC, 3);
        bus.sync(100_000 + (5 * (1 << 8)));
        assert_eq!(bus.mem_read(register::TIMA), 5);

         // restart timer
        bus.mem_write(register::TAC, 7);
        bus.sync(100_000 + (42 * (1 << 8)));
        assert_eq!(bus.mem_read(register::TIMA), 42);

        // let timer overflow and check that it triggers interrupt
        bus.sync(100_000 + (256 * (1 << 8)));
        assert_eq!(bus.mem_read(register::TIMA), 0);
        assert!(bus.IF.contains(Interrupt::TIMER));
    }

    #[test]
    fn timer_frequency_10() {
        let mut bus = Bus::new();
        bus.sync(42_000);

        bus.mem_write(register::TAC, 0b010);
        bus.mem_write(register::TAC, 0b110);
        bus.sync(42_000 + (5 * (1 << 6)));
        assert_eq!(bus.mem_read(register::TIMA), 5);

        // let timer overflow and check that it triggers interrupt
        bus.sync(42_000 + (256 * (1 << 6)));
        assert_eq!(bus.mem_read(register::TIMA), 0);
        assert!(bus.IF.contains(Interrupt::TIMER));
    }

    #[test]
    fn timer_frequency_01() {
        let mut bus = Bus::new();
        bus.sync(42_000);

        bus.mem_write(register::TAC, 0b001);
        bus.mem_write(register::TAC, 0b101);
        bus.sync(42_000 + (5 * (1 << 4)));
        assert_eq!(bus.mem_read(register::TIMA), 5);

        // let timer overflow and check that it triggers interrupt
        bus.sync(42_000 + (256 * (1 << 4)));
        assert_eq!(bus.mem_read(register::TIMA), 0);
        assert!(bus.IF.contains(Interrupt::TIMER));
    }

    #[test]
    fn timer_frequency_00() {
        let mut bus = Bus::new();
        bus.sync(42_000);

        bus.mem_write(register::TAC, 0b000);
        bus.mem_write(register::TAC, 0b100);
        bus.sync(42_000 + (5 * (1 << 10)));
        assert_eq!(bus.mem_read(register::TIMA), 5);

        // let timer overflow and check that it triggers interrupt
        bus.sync(42_000 + (256 * (1 << 10)));
        assert_eq!(bus.mem_read(register::TIMA), 0);
        assert!(bus.IF.contains(Interrupt::TIMER));
    }

    #[test]
    fn timer_modulo_load() {
        let mut bus = Bus::new();
        bus.sync(42_000);

        bus.mem_write(register::TMA, 69);
        bus.mem_write(register::TAC, 0b100);
        bus.sync(42_000 + 256 * (1 << 10));

        assert_eq!(bus.mem_read(register::TIMA), 69);
        assert!(bus.IF.contains(Interrupt::TIMER));
    }
}
