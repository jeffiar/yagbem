pub mod cpu;
pub mod bus;
pub mod opcodes;

pub use cpu::Cpu;
pub use bus::Mem;

pub struct Config {
    pub rom_file_path: String,
}

impl Config {
    pub fn build(
        mut args: impl Iterator<Item = String>
    ) -> Result<Config, &'static str> {
        args.next();

        let rom_file_path = match args.next() {
            Some(arg) => arg,
            None => return Err("No rom file provided"),
        };

        Ok(Config {
            rom_file_path,
        })
    }
}

