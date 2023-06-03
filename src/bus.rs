use crate::cpu;

pub trait Mem {
    fn mem_read(&self, addr: u16) -> u8;
    fn mem_write(&mut self, addr: u16, val: u8);

    fn mem_write16(&mut self, addr: u16, val: u16) {
        let lo = (val & 0xff) as u8;
        let hi = (val >> 8) as u8;
        self.mem_write(addr, lo);
        self.mem_write(addr + 1, hi);
    }

    fn mem_read16(&self, addr: u16) -> u16 {
        let lo = self.mem_read(addr);
        let hi = self.mem_read(addr + 1);
        (hi as u16) << 8 | lo as u16
    }
}

pub struct Bus {
    mem: [u8; 0x10000]
}

impl Mem for Bus {
    fn mem_read(&self, addr: u16) -> u8 { 
        self.mem[addr as usize] 
    }
    fn mem_write(&mut self, addr: u16, val: u8) { 
        self.mem[addr as usize] = val 
    }

}

impl Bus {
    pub fn new() -> Bus {
        Bus { mem: [0; 0x10000] }
    }

    pub fn reset(&mut self) {
        self.mem = [0; 0x10000]
    }

    pub fn load(&mut self, program: &[u8]) {
        let start = cpu::PC_START as usize;
        self.mem[start..(start + program.len())].copy_from_slice(program);
    }
}




#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn load_program_to_bus() {
        let mut bus = Bus::new();

        assert_eq!(bus.mem_read(0x0100), 0);
        bus.load(&[0x12, 0x34, 0x56]);
        assert_eq!(bus.mem_read(0x0100), 0x12);
        assert_eq!(bus.mem_read(0x0101), 0x34);
        assert_eq!(bus.mem_read(0x0102), 0x56);
        assert_eq!(bus.mem_read(0x0103), 0x00);
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
