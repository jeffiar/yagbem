use std::fs;
use std::time::SystemTime;

use gbem::{Cpu, Mem, run_debugger};
use gbem::joypad::{JoypadButton, Joypad};

use sdl2::event::Event;
use sdl2::EventPump;
use sdl2::keyboard::Keycode;
use sdl2::pixels::PixelFormatEnum;

// TODO move this somewhere so it's not duplicated
const CYCLES_PER_FRAME: usize = 114 * 154 * 4;
const SCREEN_DISP_X: u32 = 160;
const SCREEN_DISP_Y: u32 = 144;

const PIXEL_SCALE: f32 = 5.0;

use lazy_static::lazy_static;
use std::collections::HashMap;
lazy_static! {
    static ref KEYMAP: HashMap<Keycode, JoypadButton> = {
        let mut key_map = HashMap::new();
        key_map.insert(Keycode::S   , JoypadButton::DOWN);
        key_map.insert(Keycode::W     , JoypadButton::UP);
        key_map.insert(Keycode::D  , JoypadButton::RIGHT);
        key_map.insert(Keycode::A   , JoypadButton::LEFT);
        key_map.insert(Keycode::Comma  , JoypadButton::SELECT);
        key_map.insert(Keycode::Period , JoypadButton::START);
        key_map.insert(Keycode::K      , JoypadButton::BUTTON_A);
        key_map.insert(Keycode::J      , JoypadButton::BUTTON_B);
        key_map
    };
}

fn handle_user_input(event_pump: &mut EventPump, joypad: &mut Joypad) {
    for event in event_pump.poll_iter() {
        match event {
            Event::Quit { .. } | Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
                std::process::exit(0);
            },
            Event::KeyDown { keycode: Some(keycode), .. } => {
                if let Some(key) = KEYMAP.get(&keycode) {
                    joypad.set_button_pressed_status(*key, true);
                }
            },
            Event::KeyUp { keycode: Some(keycode), .. } => {
                if let Some(key) = KEYMAP.get(&keycode) {
                    joypad.set_button_pressed_status(*key, false);
                }
            },
            _ => {/* do nothing */}
        };
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
        /// Print frame timing info to stderr
        #[arg(long, short)]
        time: bool,
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
        Some(Commands::Run { rom_file, debug, boot_rom, time }) => {
            run_rom_file(&rom_file, debug, boot_rom, time);
        }
        Some(Commands::TestMoo {opcode, debug } ) => {
            gbem::test_moo(&opcode, debug);
        }
        None => eprintln!("No command specified.")
    }
}

fn run_rom_file(rom_file: &str, debug: bool, boot_rom: bool, time: bool) {
    let program = fs::read(rom_file).expect("Could not find rom file");
    eprintln!("Read ROM successfully; {} bytes", program.len());

    let mut cpu = Cpu::new();
    cpu.bus.load_rom(&program);

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
                (SCREEN_DISP_X as f32 * PIXEL_SCALE) as u32, 
                (SCREEN_DISP_Y as f32 * PIXEL_SCALE) as u32)
        .position_centered()
        .build().unwrap();

   let mut canvas = window.into_canvas().present_vsync().build().unwrap();
   let mut event_pump = sdl_context.event_pump().unwrap();
   canvas.set_scale(PIXEL_SCALE, PIXEL_SCALE).unwrap();

   let creator = canvas.texture_creator();
   let mut texture = creator
       .create_texture_target(PixelFormatEnum::RGB24, SCREEN_DISP_X, SCREEN_DISP_Y).unwrap();

    eprintln!("Initialized SDL2");


    let mut last_frame_time = SystemTime::now();
    let mut next_frame_n_cycle = 0;
    cpu.run_with_callback(move |cpu: &mut Cpu| {
        if cpu.n_cycles < next_frame_n_cycle {
            return;
        }
        next_frame_n_cycle += CYCLES_PER_FRAME;

        let sim_time = SystemTime::now();

        handle_user_input(&mut event_pump, &mut cpu.bus.joypad);
        texture.update(None, &cpu.bus.screen(), (SCREEN_DISP_X * 3) as usize).unwrap();
        canvas.copy(&texture, None, None).unwrap();
        canvas.present();

        let disp_time = SystemTime::now();

        if time {
            eprintln!("Frame time: {} ms ({} cpu, {} render)",
                      disp_time.duration_since(last_frame_time).unwrap().as_millis(),
                      sim_time.duration_since(last_frame_time).unwrap().as_millis(),
                      disp_time.duration_since(sim_time).unwrap().as_millis(),
            )
        }
        last_frame_time = SystemTime::now();
    });

    println!("Program terminated after {} cycles ({} instructions)", cpu.n_cycles, cpu.n_instrs);
    std::process::exit(0);
}
