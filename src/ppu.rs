use crate::bus::register;

const CYCLES_PER_LINE: u64 = 114 * 4;
const LINES_PER_FRAME: u64 = 154;
const VBLANK_LINE_NUM: u64 = 144;

const SCREEN_FULL_X: usize = 256;
const SCREEN_FULL_Y: usize = 256;
const SCREEN_DISP_X: usize = 160;
const SCREEN_DISP_Y: usize = 144;
// const SCREEN_NUM_DOTS: usize = SCREEN_DISP_X * 3 * SCREEN_DISP_Y;

pub struct Ppu {
    dot_num: u64,  /// T-cycle number in the scanline, between 0 and 114 * 4 - 1.
    line_num: u64, /// scanline number, between 0 and 153. VBlank starts on line 144
    frame_num: u64, /// frame number. Total cycle count is (frame_num * CYCLES_PER_FRAME) +
                    /// (line_num * CYCLES_PER_LINE) + dot_num

    should_trigger_vblank: bool,
    should_trigger_stat: bool,
    stat_intr_line: bool,
    mode: Mode,

    background: Frame,
    screen: Frame,
}

pub struct Frame {
    data: Vec<u8>,
    width: usize,
    height: usize,
}

#[derive(PartialEq, Copy, Clone)]
enum Mode {
    Hblank = 0,
    Vblank = 1,
    OamScan = 2,
    Drawing = 3,
}

impl Frame {
    fn new(width: usize, height: usize) -> Frame {
        Frame {
            data: vec![0; width * height * 3],
            width,
            height,
        }
    }

    fn get_pixel(&self, x: usize, y: usize) -> (u8, u8, u8) {
        let idx = ((y * self.width) + x) * 3;
        (self.data[idx], self.data[idx + 1], self.data[idx + 2])
    }


    fn set_pixel(&mut self, x: usize, y: usize, color: (u8,u8,u8)) {
        if x > self.width || y > self.height {
            panic!("Out of bounds access: ({x},{y}) in ({},{})", self.width, self.height);
        }
        let idx = ((y * self.width) + x) * 3;
        let (r,g,b) = color;
        self.data[idx] = r;
        self.data[idx + 1] = g;
        self.data[idx + 2] = b;
    }

    fn draw_tile(&mut self, x: usize , y: usize, tile_data: &[u8]) {
        for row in 0..8 {
            let mut layer_0 = tile_data[2*row];
            let mut layer_1 = tile_data[2*row+1];

            for col in (0..8).rev() {
                let color_num = (layer_0 & 1) << 1 | (layer_1 & 1);
                layer_0 >>= 1;
                layer_1 >>= 1;

                self.set_pixel(x + col as usize, y + row as usize, PALETTE[color_num as usize]);
            }
        }
    }
}

impl Ppu {
    pub fn new() -> Ppu { 
        Ppu {
            dot_num: 0,
            line_num: 0,
            frame_num: 0,
            stat_intr_line: false,
            mode: Mode::OamScan,

            should_trigger_vblank: false,
            should_trigger_stat: false,

            background: Frame::new(SCREEN_FULL_X, SCREEN_FULL_Y),
            screen: Frame::new(SCREEN_DISP_X, SCREEN_DISP_Y),
        }
    }

    fn determine_mode(dot_num: u64, line_num: u64) -> Mode {
        // Don't worry about variable length drawing time for now
        if line_num >= VBLANK_LINE_NUM {
            Mode::Vblank
        } else {
            match dot_num {
                0..=80 => Mode::OamScan,
                81..=252 => Mode::Drawing,
                _ => Mode::Hblank,
            }
        }
    }

    pub fn tick(&mut self, nticks: u64, mem: &mut [u8]) {
        // update dot, line, and frame numbers
        self.dot_num += nticks;
        if self.dot_num > CYCLES_PER_LINE {
            self.dot_num -= CYCLES_PER_LINE;
            self.line_num += 1;
        }
        if self.line_num > LINES_PER_FRAME {
            self.line_num -= LINES_PER_FRAME;
            self.frame_num += 1;
        }

        let old_stat_intr_line = self.stat_intr_line;
        let old_mode = self.mode;

        let new_match = self.line_num == mem[register::LCY as usize] as u64;
        let new_mode = Ppu::determine_mode(self.dot_num, self.line_num);
        let new_stat_flags = StatFlags::from_bits_truncate(mem[register::STAT as usize]);
        let new_stat_reg = StatReg::new(new_stat_flags, new_match, new_mode);
        let new_stat_intr_line = new_stat_reg.intr_condition_met();

        if (new_mode == Mode::Drawing) && (old_mode != Mode::Drawing) {
            self.draw_line(mem);
        }
        if (new_mode == Mode::Vblank) && (old_mode != Mode::Vblank) {
            self.should_trigger_vblank = true;
        }
        if new_stat_intr_line && !old_stat_intr_line {
            self.should_trigger_stat = true;
        }

        mem[register::STAT as usize] = new_stat_reg.to_u8();
        mem[register::LY as usize] = self.line_num as u8;
        self.stat_intr_line = new_stat_intr_line;
        self.mode = new_mode;
    }

    pub fn line_num(&self) -> u8 { self.line_num as u8 }

    pub fn should_trigger_vblank(&mut self) -> bool {
        if self.should_trigger_vblank {
            self.should_trigger_vblank = false;
            return true;
        } else {
            return false;
        }
    }

    pub fn should_trigger_stat(&mut self) -> bool {
        if self.should_trigger_stat {
            self.should_trigger_stat = false;
            return true;
        } else {
            return false;
        }
    }

    fn update_background(&mut self, mem: &[u8]) {
        let mut addr = 0x9800;
        for y_tile in 0..32 {
            for x_tile in 0..32 {
                let chr = mem[addr];
                let tile_addr = 0x8000 + (chr as u16 * 16) as usize;

                self.background.draw_tile(x_tile * 8, y_tile * 8, &mem[tile_addr..tile_addr + 16]);
                addr += 1;
            }
        }
    }

    fn update_screen(&mut self, mem: &[u8]) {
        let scx = mem[register::SCX as usize];
        let scy = mem[register::SCY as usize];

        for y in 0..SCREEN_DISP_Y {
            for x in 0..SCREEN_DISP_X {
                let tm_x = scx.wrapping_add(x as u8) as usize;
                let tm_y = scy.wrapping_add(y as u8) as usize;
                let rgb = self.background.get_pixel(tm_x, tm_y);
                self.screen.set_pixel(x, y, rgb);
            }
        }
    }

    pub fn render_frame(&mut self, mem: &[u8]) -> &[u8] {
        self.update_background(mem);
        self.update_screen(mem);
        &self.screen.data
    }

    fn draw_line(&mut self, mem: &[u8]) {
        todo!();
    }

}

use bitflags::bitflags;
bitflags! {
    struct StatFlags: u8 {
        const LINEMATCH = 1 << 6;
        const OAM       = 1 << 5;
        const VBLANK    = 1 << 4;
        const HBLANK    = 1 << 3;
    }
}

struct StatReg {
    flags: StatFlags,
    line_match: bool,
    mode: Mode,
}

impl StatReg {
    fn new(flags: StatFlags, line_match: bool, mode: Mode) -> StatReg {
        StatReg { flags, line_match, mode }
    }

    fn intr_condition_met(&self) -> bool {
        (self.mode == Mode::Hblank  &&  self.flags.contains(StatFlags::HBLANK))
            ||(self.mode == Mode::Vblank  &&  self.flags.contains(StatFlags::VBLANK))
            ||(self.mode == Mode::OamScan &&  self.flags.contains(StatFlags::OAM))
            ||(self.line_match && self.flags.contains(StatFlags::LINEMATCH))
    }

    fn to_u8(&self) -> u8 {
        self.flags.bits() | (self.line_match as u8) << 2 | (self.mode as u8)
    }
}

const PALETTE: [(u8,u8,u8); 4] = [ (0xff, 0xff, 0xff), (0xaa, 0xaa, 0xaa), (0x55, 0x55, 0x55), (0, 0, 0)];

