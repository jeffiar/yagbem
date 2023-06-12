use crate::bus::register;
use bitflags::bitflags;

const CYCLES_PER_LINE: u64 = 114 * 4;
const LINES_PER_FRAME: u64 = 154;
const VBLANK_LINE_NUM: u64 = 144;

const SCREEN_DISP_X: usize = 160;
const SCREEN_DISP_Y: usize = 144;
const SCREEN_N_PIX: usize = SCREEN_DISP_X * SCREEN_DISP_Y;
// const SCREEN_NUM_DOTS: usize = SCREEN_DISP_X * 3 * SCREEN_DISP_Y;

pub struct Ppu {
    dot_num: u64,  /// T-cycle number in the scanline, between 0 and 114 * 4 - 1.
    line_num: u64, /// scanline number, between 0 and 153. VBlank starts on line 144
    frame_num: u64, /// frame number. Total cycle count is (frame_num * CYCLES_PER_FRAME) +
                    /// (line_num * CYCLES_PER_LINE) + dot_num

    lcdc: LCDC,

    should_trigger_vblank: bool,
    should_trigger_stat: bool,
    stat_intr_line: bool,
    mode: Mode,

    screen: Frame,
}

bitflags! {
    struct LCDC: u8 {
        const MASTER_ENABLE    = (1 << 7);
        const WDW_TILEMAP      = (1 << 6);
        const WDW_ENABLE       = (1 << 5);
        const TILEDATA         = (1 << 4);
        const BGD_TILEMAP      = (1 << 3);
        const OBJ_SIZE         = (1 << 2);
        const OBJ_ENABLE       = (1 << 1);
        const WDW_BGD_ENABLE   = (1 << 0);
    }
}

pub struct Frame {
    data: [u8; SCREEN_N_PIX * 3],
}

#[derive(PartialEq, Copy, Clone)]
enum Mode {
    Hblank = 0,
    Vblank = 1,
    OamScan = 2,
    Drawing = 3,
}

impl Mode {
    fn determine(dot_num: u64, line_num: u64) -> Mode {
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

}

impl Frame {
    fn new() -> Frame {
        Frame {
            data: [0; SCREEN_N_PIX * 3],
        }
    }

    fn set_pixel(&mut self, x: usize, y: usize, color: (u8,u8,u8)) {
        if x > SCREEN_DISP_X || y > SCREEN_DISP_Y {
            panic!("Out of bounds access: ({x},{y})");
        }
        let idx = ((y * SCREEN_DISP_X) + x) * 3;
        let (r,g,b) = color;
        self.data[idx] = r;
        self.data[idx + 1] = g;
        self.data[idx + 2] = b;
    }

    fn clear(&mut self) {
        self.data = [0; SCREEN_N_PIX * 3];
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

            lcdc: LCDC::from_bits_truncate(0x83),
            should_trigger_vblank: false,
            should_trigger_stat: false,

            screen: Frame::new(),
        }
    }

    pub fn tick(&mut self, nticks: u64, mem: &mut [u8]) {
        if !self.lcdc.contains(LCDC::MASTER_ENABLE) {
            mem[register::STAT as usize] = 0;
            mem[register::LY as usize] = 0;
            return;
        }
        // update dot, line, and frame numbers
        self.dot_num += nticks;
        if self.dot_num >= CYCLES_PER_LINE {
            self.dot_num -= CYCLES_PER_LINE;
            self.line_num += 1;
        }
        if self.line_num >= LINES_PER_FRAME {
            self.line_num -= LINES_PER_FRAME;
            self.frame_num += 1;
        }

        let old_stat_intr_line = self.stat_intr_line;
        let old_mode = self.mode;

        let new_match = self.line_num == mem[register::LCY as usize] as u64;
        let new_mode = Mode::determine(self.dot_num, self.line_num);
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

    fn calc_pixel(&self, x: u8, y: u8, mem: &[u8], tile_map_flag: bool, tile_data_flag: bool) -> (u8,u8,u8) {
        let dx = (x % 8) as usize;
        let dy = (y % 8) as usize;
        let tile_num = ((y as usize / 8) * 32) + (x as usize / 8);
        let tile_id = if tile_map_flag {
            mem[0x9c00 + tile_num]
        } else {
            mem[0x9800 + tile_num]
        };
        let tile_data_addr = if tile_data_flag {
            0x8000 + tile_id as usize * 16
        } else {
            if tile_id >= 128 {
                (0x9000 + (tile_id as isize - 256) * 16) as usize
            } else {
                0x9000 + tile_id as usize * 16
            }
        };

        let bitplane_0 = mem[tile_data_addr + 2*dy];
        let bitplane_1 = mem[tile_data_addr + 2*dy + 1];

        let color_num = (bitplane_0 >> (7 - dx)) & 1 | ((bitplane_1 >> (7 - dx)) & 1) << 1;
        PALETTE[color_num as usize]
    }

    fn draw_line(&mut self, mem: &[u8]) {
        let y = self.line_num as usize;
        let scx = mem[register::SCX as usize];
        let scy = mem[register::SCY as usize];
        let wx = mem[register::WX as usize];
        let wy = mem[register::WY as usize];

        for x in 0..SCREEN_DISP_X {
            let bgd_x = scx.wrapping_add(x as u8);
            let bgd_y = scy.wrapping_add(y as u8);
            let mut bgd_color = self.calc_pixel(bgd_x, bgd_y, mem,
                                                self.lcdc.contains(LCDC::BGD_TILEMAP),
                                                self.lcdc.contains(LCDC::TILEDATA));

            let wdw_x = x as isize - wx as isize + 7;
            let wdw_y = y as isize - wy as isize;
            if self.lcdc.contains(LCDC::WDW_ENABLE) && (wdw_x >= 0) && (wdw_y >= 0)  {
                let wdw_color = self.calc_pixel(wdw_x as u8, wdw_y as u8, mem,
                                                self.lcdc.contains(LCDC::WDW_TILEMAP),
                                                self.lcdc.contains(LCDC::TILEDATA));
                bgd_color = wdw_color;
            }

            if !self.lcdc.contains(LCDC::WDW_BGD_ENABLE) {
                bgd_color = PALETTE[0];
            }

            self.screen.set_pixel(x, y, bgd_color);
        }
    }

    pub fn screen(&mut self) -> &[u8] {
        &self.screen.data
    }

    pub fn set_lcd_control(&mut self, val: u8) {
        let new_lcdc = LCDC::from_bits_truncate(val);
        if !new_lcdc.contains(LCDC::MASTER_ENABLE) && self.lcdc.contains(LCDC::MASTER_ENABLE) {
            // reset the frame clock
            self.dot_num = 0;
            self.line_num = 0;
            self.frame_num = 0;
            self.mode = Mode::OamScan;
            self.screen.clear();
        }
        self.lcdc = new_lcdc;
    }
    pub fn get_lcd_control(&self) -> u8 { self.lcdc.bits() }

}

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

