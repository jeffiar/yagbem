use crate::bus::register;
use bitflags::bitflags;
use strum_macros;

const CYCLES_PER_LINE: usize = 114 * 4;
const LINES_PER_FRAME: usize = 154;
const VBLANK_LINE_NUM: usize = 144;

const SCREEN_DISP_X: usize = 160;
const SCREEN_DISP_Y: usize = 144;
const SCREEN_N_PIX: usize = SCREEN_DISP_X * SCREEN_DISP_Y;

pub struct Ppu {
    dot_num: usize,  /// T-cycle number in the scanline, between 0 and 114 * 4 - 1.
    line_num: usize, /// scanline number, between 0 and 153. VBlank starts on line 144
    frame_num: usize, /// frame number. Total cycle count is (frame_num * CYCLES_PER_FRAME) +
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

#[derive(PartialEq, Copy, Clone)]
enum Mode {
    Hblank = 0,
    Vblank = 1,
    OamScan = 2,
    Drawing = 3,
}

impl Mode {
    fn determine(dot_num: usize, line_num: usize) -> Mode {
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

pub struct Frame {
    data: [u8; SCREEN_N_PIX * 3],
}

impl Frame {
    fn new() -> Frame {
        Frame {
            data: [0; SCREEN_N_PIX * 3],
        }
    }

    fn draw_line(&mut self, y: usize, colors: Vec<Color>) {
        let mut idx = y * SCREEN_DISP_X * 3;
        for color in colors {
            let (r,g,b) = color.to_rgb();
            self.data[idx] = r;
            self.data[idx + 1] = g;
            self.data[idx + 2] = b;
            idx += 3;
        }
    }

    fn clear(&mut self) {
        self.data = [0; SCREEN_N_PIX * 3];
    }
}

struct Sprite {
    y: isize,
    x: isize,
    tile_id: usize,
    priority: bool,
    flipped_x: bool,
    flipped_y: bool,
    palette: PaletteType,
}

impl Sprite {
    fn new(obj_data: &[u8]) -> Sprite {
        Sprite {
            y: obj_data[0] as isize - 16,
            x: obj_data[1] as isize - 8,
            tile_id: obj_data[2] as usize,
            priority: (obj_data[3] & (1 << 7)) != 0,
            flipped_y: (obj_data[3] & (1 << 6)) != 0,
            flipped_x: (obj_data[3] & (1 << 5)) != 0,
            palette: if (obj_data[3] & (1 << 4)) != 0 {
                PaletteType::Object1
            } else {
                PaletteType::Object0
            }
        }
    }
}

#[derive(Copy, Clone)]
enum PaletteType {
    Background, Object0, Object1,
}

use strum_macros::FromRepr;
#[derive(FromRepr, Clone, Copy)]
#[repr(u8)]
enum Color {
    White, Light, Dark, Black,
}

impl Color {
    const fn to_rgb(&self) -> (u8,u8,u8) {
        match self {
            Color::White => (0xff, 0xff, 0xff),
            Color::Light => (0xaa, 0xaa, 0xaa),
            Color::Dark  => (0x55, 0x55, 0x55),
            Color::Black => (0x00, 0x00, 0x00),
        }
    }
}

#[derive(Clone, Copy)]
struct Palette {
    colors: [Color; 4],
}

impl Palette {
    fn from_u8(bits: u8) -> Palette {
        Palette {
            colors: [
                Color::from_repr((bits >> 0) & 3).unwrap(),
                Color::from_repr((bits >> 2) & 3).unwrap(),
                Color::from_repr((bits >> 4) & 3).unwrap(),
                Color::from_repr((bits >> 6) & 3).unwrap(),
            ]
        }
    }

    fn apply(&self, color_num: u8) -> Color {
        self.colors[color_num as usize]
    }
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

    pub fn tick(&mut self, nticks: usize, mem: &mut [u8]) {
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

        let new_match = self.line_num == mem[register::LCY as usize] as usize;
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

    fn draw_line(&mut self, mem: &[u8]) {
        let mut color_data = [0u8; SCREEN_DISP_X];
        let mut palettes = [PaletteType::Background; SCREEN_DISP_X];

        if self.lcdc.contains(LCDC::WDW_BGD_ENABLE) {
            self.draw_background(&mut color_data, mem);
            if self.lcdc.contains(LCDC::WDW_ENABLE) {
                self.draw_window(&mut color_data, mem);
            }
        }

        if self.lcdc.contains(LCDC::OBJ_ENABLE) {
            for obj in self.oam_search(mem) {
                self.draw_obj(obj, &mut color_data, &mut palettes, &mem);
            }
        }

        let line: Vec<Color> = self.apply_palette(&color_data, &palettes, mem);
        self.screen.draw_line(self.line_num, line);
    }

    fn draw_background(&self, line: &mut [u8], mem: &[u8]) {
        let scx = mem[register::SCX as usize];
        let scy = mem[register::SCY as usize];
        let y = scy.wrapping_add(self.line_num as u8);
        let tile_y = y / 8;
        let dy = y % 8;

        let tilemap_addr = if self.lcdc.contains(LCDC::BGD_TILEMAP) { 0x9c00 } else { 0x9800 };

        for disp_x in 0..SCREEN_DISP_X {
            let x = scx.wrapping_add(disp_x as u8);
            let tile_x = x / 8;
            let dx = x % 8;

            let tile_num = tile_y as usize * 32 + tile_x as usize;

            let tile_id = mem[tilemap_addr + tile_num];
            let tiledata_addr = if self.lcdc.contains(LCDC::TILEDATA) {
                0x8000 + tile_id as usize * 16
            } else {
                if tile_id >= 128 {
                    (0x9000 + (tile_id as isize - 256) * 16) as usize
                } else {
                    0x9000 + tile_id as usize * 16
                }
            };

            line[disp_x] = Self::get_color_num_from_tile(tiledata_addr, dx, dy, mem);
        }
    }

    fn draw_window(&self, line: &mut [u8], mem: &[u8]) {
        let wx = mem[register::WX as usize] as isize - 7;
        let wy = mem[register::WY as usize] as usize;
        if self.line_num < wy {
            return;
        }

        let y = self.line_num - wy;
        let tile_y = y / 8;
        let dy = y % 8;
        let tilemap_addr = if self.lcdc.contains(LCDC::WDW_TILEMAP) { 0x9c00 } else { 0x9800 };

        for disp_x in wx..SCREEN_DISP_X as isize {
            let x = disp_x - wx;
            if x < 0 {
                continue;
            }
            let tile_x = x / 8;
            let dx = x % 8;

            // TODO find a way to DRY this logic; it's repeated for background and window
            let tile_num = tile_y as usize * 32 + tile_x as usize;
            let tile_id = mem[tilemap_addr + tile_num];
            let tiledata_addr = if self.lcdc.contains(LCDC::TILEDATA) {
                0x8000 + tile_id as usize * 16
            } else {
                if tile_id >= 128 {
                    (0x9000 + (tile_id as isize - 256) * 16) as usize
                } else {
                    0x9000 + tile_id as usize * 16
                }
            };

            line[disp_x as usize] = Self::get_color_num_from_tile(tiledata_addr, dx as u8, dy as u8, mem);
        }
    }

    fn oam_search(&self, mem: &[u8]) -> Vec<Sprite> {
        let mut sprites = Vec::new();
        let mut sprite_addr = 0xfe00;
        for _ in 0..40 {
            let obj_y = mem[sprite_addr] as isize - 16;
            let y = self.line_num as isize;
            if (obj_y <= y) && (y < obj_y + 8) { // TODO big sprites
                sprites.push(Sprite::new(&mem[sprite_addr..sprite_addr+4]));
            }
            sprite_addr += 4;
        }
        // TODO sort so that priority is obeyed
        sprites
    }

    fn draw_obj(&self, obj: Sprite, line: &mut [u8], palettes: &mut [PaletteType], mem: &[u8]) {
        let dy = (self.line_num as isize - obj.y as isize) as usize;
        for dx in 0..8 {
            let x = obj.x + dx;
            if (x < 0) || (x > line.len() as isize) {
                continue;
            }
            let cnum = Self::get_color_num_from_tile(0x8000 + obj.tile_id * 16,
                                                     (if obj.flipped_x { 7 - dx } else { dx }) as u8,
                                                     (if obj.flipped_y { 7 - dy } else { dy }) as u8,
                                                     mem);
            if (obj.priority && line[x as usize] == 0) || (!obj.priority && cnum != 0) {
                line[x as usize] = cnum;
                palettes[x as usize] = obj.palette;
            }
        }
    }

    fn get_color_num_from_tile(tiledata_addr: usize, dx: u8, dy: u8, mem: &[u8]) -> u8 {
        let bitplane_0 = mem[tiledata_addr + 2*dy as usize];
        let bitplane_1 = mem[tiledata_addr + 2*dy as usize + 1];
        let bp0 = (bitplane_0 & (0x80 >> dx)) != 0;
        let bp1 = (bitplane_1 & (0x80 >> dx)) != 0;
        bp0 as u8 | (bp1 as u8) << 1
    }

    fn apply_palette(&self, color_data: &[u8], palette_types: &[PaletteType], mem: &[u8]) -> Vec<Color> {
        let mut colors = Vec::with_capacity(color_data.len());
        let pal_background = Palette::from_u8(mem[register::BGP as usize]);
        let pal_sprite_0 = Palette::from_u8(mem[register::OBP0 as usize]);
        let pal_sprite_1 = Palette::from_u8(mem[register::OBP1 as usize]);

        for (cnum, palette_type) in color_data.iter().zip(palette_types) {
            let palette = match palette_type {
                PaletteType::Background => pal_background,
                PaletteType::Object0 => pal_sprite_0,
                PaletteType::Object1 => pal_sprite_1,
            };
            let color = palette.apply(*cnum);
            colors.push(color)
        }
        colors
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
