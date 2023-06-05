use std::env;
use std::process;
use std::fs;

use gbem::Config;
use gbem::Cpu;
use gbem::Mem;

use sdl2::event::Event;
use sdl2::EventPump;
use sdl2::keyboard::Keycode;
use sdl2::pixels::Color;
use sdl2::pixels::PixelFormatEnum;

const SCREEN_FULL_X: u32 = 256;
const SCREEN_FULL_Y: u32 = 256;
// const SCREEN_DISP_X = 160;
// const SCREEN_DISP_Y = 144;

const PIXEL_SCALE: f32 = 4.0;

// want functions for:
// [ ] tile map (showing VRAM as pixels)
// - full screen (after drawing tiles)
// - the scrolled portion of full screen

// use sdl2::render::Canvas;
// use sdl2::render::Texture;
// use sdl2::video::Window;
// fn render_tile_map<'a>(cpu: &Cpu, texture: &mut Texture<'a>, canvas: &mut Canvas<Window>) {
    // let mut tile_map_rgb = [0 as u8; (SCREEN_FULL_X * 3 * SCREEN_FULL_Y) as usize];

    // if (cpu.n_instrs / 10) % 2 == 0 {
    //     eprintln!("Updating tile map");
    //     tile_map_rgb = [0xff as u8; (SCREEN_FULL_X * 3 * SCREEN_FULL_Y) as usize];
    // }

    // texture.update(None, &tile_map_rgb, (SCREEN_FULL_X * 3) as usize).unwrap();
    // canvas.copy(&texture, None, None).unwrap();
    // canvas.present();
// }

fn handle_user_input(event_pump: &mut EventPump, mem_disp_start: &mut u16 ) {
    for event in event_pump.poll_iter() {
        match event {
            Event::Quit { .. } | Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
                std::process::exit(0);
            },
            Event::KeyDown { keycode: Some(Keycode::W), .. } => {
                *mem_disp_start = mem_disp_start.saturating_sub(1);
            },
            Event::KeyDown { keycode: Some(Keycode::S), .. } => {
                *mem_disp_start = mem_disp_start.saturating_add(1);
            },
            Event::KeyDown { keycode: Some(Keycode::A), .. } => {
                *mem_disp_start = mem_disp_start.saturating_sub(512 * 16);
            },
            Event::KeyDown { keycode: Some(Keycode::D), .. } => {
                *mem_disp_start = mem_disp_start.saturating_add(512 * 16);
                // dbg!(*mem_disp_start);
            }
            _ => {/* do nothing */}
        };
    }
}

fn color_num_to_rgb(color_num: u8) -> Color {
    match color_num {
        0 => Color::RGB(255, 255, 255),
        1 => Color::RGB(192, 192, 192),
        2 => Color::RGB(128, 128, 128),
        3 => Color::RGB(64, 64, 64),
        _ => unreachable!(),
    }
}

fn paint_tile(tile_data: &[u8], out_rgb: &mut [u8], out_width: u32, out_height: u32, x: u32 , y: u32)  {
    assert_eq!(tile_data.len(), 16);
    // eprintln!("x = {x}, y = {y}");

    let mut out_idx: usize = (((x * out_height) + y) * 3) as usize;
    for row in 0..8 {
        let layer0 = tile_data[2*row];
        let layer1 = tile_data[2*row+1];

        for col in 0..8 {
            // eprintln!("row = {row}, col = {col}, idx = {out_idx}");
            let mut color_num: u8 = 0;
            if layer0 & (1 << (7 - col)) != 0{
                color_num |= 0b01;
            }
            if layer1 & (1 << (7 - col)) != 0{
                color_num |= 0b10;
            }

            let (r,g,b) = color_num_to_rgb(color_num).rgb();
            out_rgb[out_idx] = r;
            out_rgb[out_idx + 1] = g;
            out_rgb[out_idx + 2] = b;
            out_idx += 3;
        }
        out_idx += 3 * (out_width - 8) as usize;
    }
}

fn main() {
    let config = Config::build(env::args()).unwrap_or_else(|err| {
        eprintln!("Problem parsing arguments: {err}");
        process::exit(1);
    });

    let program = fs::read(config.rom_file_path).expect("Could not find rom file");
    eprintln!("Read ROM successfully; {} bytes", program.len());

    // initialize SDL2 (https://bugzmanov.github.io/nes_ebook/chapter_3_4.html)
    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();
    let window = video_subsystem
        .window("Gameboy emulator", 
                (SCREEN_FULL_X as f32 * PIXEL_SCALE) as u32, 
                (SCREEN_FULL_Y as f32 * PIXEL_SCALE) as u32)
        .position_centered()
        .build().unwrap();

   let mut canvas = window.into_canvas().present_vsync().build().unwrap();
   let mut event_pump = sdl_context.event_pump().unwrap();
   canvas.set_scale(PIXEL_SCALE, PIXEL_SCALE).unwrap();

   let creator = canvas.texture_creator();
   let mut texture = creator
       .create_texture_target(PixelFormatEnum::RGB24, SCREEN_FULL_X, SCREEN_FULL_Y).unwrap();


    eprintln!("Initialized SDL2");

    let mut cpu = Cpu::new();
    cpu.load_rom(&program);
    cpu.reset();

    let mut mem_disp_start = 0x0000;

    cpu.run_with_callback(move |cpu| {
        handle_user_input(&mut event_pump, &mut mem_disp_start);
        // render_tile_map(&cpu, &mut texture, &mut canvas);
        let mut tile_map_rgb = [0 as u8; (SCREEN_FULL_X * 3 * SCREEN_FULL_Y) as usize];

        let mut addr = mem_disp_start;
        for x_tile in 0..32 {
            for y_tile in 0..32 {
                paint_tile(&cpu.mem_read_range(addr, addr + 16), 
                           &mut tile_map_rgb, 
                           SCREEN_FULL_X,
                           SCREEN_FULL_Y,
                           x_tile * 8,
                           y_tile * 8);
                addr += 16;
            }
        }

        texture.update(None, &tile_map_rgb, (SCREEN_FULL_X * 3) as usize).unwrap();
        canvas.copy(&texture, None, None).unwrap();
        canvas.present();

        ::std::thread::sleep(std::time::Duration::from_millis(20));
    });
}
