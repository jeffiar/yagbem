use std::fs;
// use std::time::SystemTime;

use gbem::{Cpu, Mem, run_debugger};

use sdl2::event::Event;
use sdl2::EventPump;
use sdl2::keyboard::Keycode;
use sdl2::pixels::Color;
use sdl2::pixels::PixelFormatEnum;

// const CYCLES_PER_SECOND: u64 = 4194304;
const CYCLES_PER_FRAME: u64 = 114 * 154 * 4;

const SCREEN_FULL_X: u32 = 256;
const SCREEN_FULL_Y: u32 = 256;
// const SCREEN_DISP_X = 160;
// const SCREEN_DISP_Y = 144;

const PIXEL_SCALE: f32 = 3.0;

fn handle_user_input(event_pump: &mut EventPump) {
    for event in event_pump.poll_iter() {
        match event {
            Event::Quit { .. } | Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
                std::process::exit(0);
            },
            Event::KeyDown { keycode: Some(Keycode::W), .. } => {
            },
            Event::KeyDown { keycode: Some(Keycode::S), .. } => {
            },
            Event::KeyDown { keycode: Some(Keycode::A), .. } => {
            },
            Event::KeyDown { keycode: Some(Keycode::D), .. } => {
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

fn paint_tile(tile_data: &[u8], out_rgb: &mut [u8], out_width: u32, out_height: u32, x: u32 , y: u32) {
    assert_eq!(tile_data.len(), 16);

    let mut out_idx: usize = (((x * out_height) + y) * 3) as usize;
    for row in 0..8 {
        let layer0 = tile_data[2*row];
        let layer1 = tile_data[2*row+1];

        for col in 0..8 {
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

fn render(cpu: &Cpu, tile_map_rgb: &mut [u8]) {
    let mut addr = 0x9800;
    for x_tile in 0..32 {
        for y_tile in 0..32 {
            let chr = cpu.mem_read(addr);
            let tile_data = 0x8000 + (chr as u16 * 16);
            paint_tile(&cpu.mem_read_range(tile_data, tile_data + 16), 
                       tile_map_rgb, 
                       SCREEN_FULL_X,
                       SCREEN_FULL_Y,
                       x_tile * 8,
                       y_tile * 8);
            addr += 1;
        }
    }
}


use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// Execute a rom file
    Run {
        rom_file: String,
        /// Don't initialize the display
        #[arg(long, short)]
        debug: bool,
        /// Start from 0x0000 and load the Nintendo logo into 0x104
        #[arg(long, short)]
        boot_rom: bool,
    },

    /// Run the jsmoo tests
    TestMoo {
        opcode: Option<String>,
        #[arg(long, short)]
        debug: bool,
    }
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Some(Commands::Run { rom_file, debug, boot_rom }) => {
            run_rom_file(&rom_file, debug, boot_rom);
        }
        Some(Commands::TestMoo {opcode, debug } ) => {
            gbem::test_moo(&opcode, debug);
        }
        None => eprintln!("No command specified.")
    }
}

fn run_rom_file(rom_file: &str, debug: bool, boot_rom: bool) {
    let program = fs::read(rom_file).expect("Could not find rom file");
    eprintln!("Read ROM successfully; {} bytes", program.len());

    let mut cpu = Cpu::new();
    cpu.load_rom(&program);
    cpu.reset();

    if boot_rom {
        cpu.pc = 0;

        // For testing the Nintendo boot ROM: load the Nintendo logo
        // at addr $0104 in ROM cartridge
        cpu.mem_write_range(0x104,
                            &[0xCE,0xED,0x66,0x66,0xCC,0x0D,0x00,0x0B,0x03,0x73,0x00,0x83,0x00,0x0C,0x00,0x0D,
                            0x00,0x08,0x11,0x1F,0x88,0x89,0x00,0x0E,0xDC,0xCC,0x6E,0xE6,0xDD,0xDD,0xD9,0x99,
                            0xBB,0xBB,0x67,0x63,0x6E,0x0E,0xEC,0xCC,0xDD,0xDC,0x99,0x9F,0xBB,0xB9,0x33,0x3E,]);
    }

    if debug {
        run_debugger(cpu);
        return;
    }

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

    let mut frame = [0 as u8; (SCREEN_FULL_X * 3 * SCREEN_FULL_Y) as usize];

    let mut next_frame_n_cycle = 0;
    cpu.run_with_callback(move |cpu: &mut Cpu| {
        if cpu.n_cycles < next_frame_n_cycle {
            return;
        }
        next_frame_n_cycle += CYCLES_PER_FRAME;

        handle_user_input(&mut event_pump);

        render(&cpu, &mut frame); 
        texture.update(None, &frame, (SCREEN_FULL_X * 3) as usize).unwrap();
        canvas.copy(&texture, None, None).unwrap();
        canvas.present();
    });

    eprintln!("Program terminated after {} cycles", cpu.n_cycles);
    std::process::exit(0);
}
