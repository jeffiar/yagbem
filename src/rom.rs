use std::str;

pub struct Rom {
    program: Vec<u8>,
    ext_ram: Vec<u8>,
    cart_type: CartridgeType,

    ext_ram_enabled: bool,
    rom_bank_num: u8,
    ram_bank_num: u8,

    num_rom_banks: u8,
    num_rom_bits: u8,
    num_ram_banks: u8,
}

#[derive(Debug)]
enum CartridgeType {
    Flat, Mbc1,
}

impl CartridgeType {
    fn parse(byte: u8) -> CartridgeType {
        match byte {
            0 => CartridgeType::Flat,
            1 | 2 | 3 => CartridgeType::Mbc1,
            _ => { panic!("Unsupported cartridge type: {:02x}", byte); }
        }
    }
}

impl Rom {
    pub fn new() -> Rom {
        Rom { 
            program: vec![0; 0x8000],
            ext_ram: vec![0; 0x2000],
            cart_type: CartridgeType::Flat,

            ext_ram_enabled: false,
            rom_bank_num: 1,
            ram_bank_num: 0,

            num_rom_banks: 2,
            num_rom_bits: 1,
            num_ram_banks: 1,
        }
    }

    pub fn load(&mut self, program: &[u8], no_header: bool) {
        if program.len() > self.program.len() {
            self.program = program.to_vec();
        } else {
            self.program[..program.len()].copy_from_slice(program);
        }
        if no_header {
            return;
        }

        let cartridge_type = CartridgeType::parse(program[0x147]);
        self.num_rom_bits = program[0x148] + 1;
        self.num_rom_banks = 2 << program[0x148];
        self.num_ram_banks = match program[0x149] {
            0 => 1, 2 => 1, 3 => 4, 4 => 16, 5 => 8, _ => panic!("Invalid RAM bank number"),
        };
        eprintln!("Number of bytes: {}", program.len());
        eprintln!("Title: {}", str::from_utf8(&program[0x134..=0x143]).unwrap());
        eprintln!("Cartridge type: {:?}", cartridge_type);
        eprintln!("ROM type: {:02x}\t{}x 16KiB banks", program[0x148], self.num_rom_banks);
        eprintln!("RAM type: {:02x}\t{}x 8KiB banks", program[0x149], self.num_ram_banks);

        self.cart_type = cartridge_type;
    }

    pub fn read(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x3fff => {
                // Always mapped to ROM bank 0
                self.program[addr as usize]
            }
            0x4000..=0x7fff => {
                // ROM bank 1
                let offset = match self.cart_type {
                    CartridgeType::Flat => { 0 },
                    CartridgeType::Mbc1 => { (self.rom_bank_num as usize - 1) * 0x4000 }
                };
                // eprintln!("{:06x} -> {:06x}", addr, addr as usize + offset);
                self.program[addr as usize + offset]
            }
            0xa000..=0xbfff => { 
                // TODO implement ext ram switching
                self.ext_ram[addr as usize - 0xa000] 
            }
            _ => { unreachable!() }
        }
    }

    pub fn write(&mut self, addr: u16, val: u8) {
        match addr {
            0x0000..=0x1fff => { 
                self.ext_ram_enabled = (val & 0xf) == 0xa;
            },
            0x2000..=0x3fff => { 
                // only lower 5 bits count
                self.rom_bank_num = val & 0x1f;
                if self.rom_bank_num == 0 {
                    self.rom_bank_num = 1;
                }
                // zero out any bits not needed to specify bank number
                self.rom_bank_num &= (1 << self.num_rom_bits) - 1;
                // eprintln!("Switching ROM bank to {:2} (val = {:08b})", self.rom_bank_num, val);
                assert!(self.rom_bank_num < self.num_rom_banks);
            },
            0x4000..=0x5fff => { 
                self.ram_bank_num = val & 0x3;
            },
            0x6000..=0x7fff => { }, // NOTE unimplemented cuz I don't get it
            0xa000..=0xbfff => {
                // TODO implement ext ram switching
                self.ext_ram[addr as usize - 0xa000] = val;
            }
            _ => { unreachable!() }
        }
        // 
    }

}
