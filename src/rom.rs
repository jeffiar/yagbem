
pub struct Rom {
    mem: Vec<u8>,
}

impl Rom {
    pub fn new() -> Rom {
        Rom { mem: vec![] }
    }

    pub fn load(&mut self, program: &[u8]) {
        self.mem = program.to_vec();
    }

    pub fn read(&self, addr: u16) -> u8 {
        self.mem[addr as usize]
    }

    pub fn write(&mut self, addr: u16, val: u8) {
        // self.mem[addr as usize] = val;
    }

}
