use std::fs;
use std::io::{stdin, stdout, Write};
// use std::time::SystemTime;

use gbem::Cpu;
use gbem::Mem;

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

/// Returns true if this tile has changed and needs to be redrawn. // TODO cache this
fn paint_tile(tile_data: &[u8], out_rgb: &mut [u8], out_width: u32, out_height: u32, x: u32 , y: u32) {
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
        Some(Commands::Run { rom_file, debug }) => {
            run_rom_file(&rom_file, debug);
        }
        Some(Commands::TestMoo {opcode, debug } ) => {
            gbem::test_moo(&opcode, debug);
        }
        None => eprintln!("No command specified.")
    }
}

#[derive(Clone, Copy)]
enum DebugCommand {
    RepeatLastCommand,
    Next(usize),
    Continue,
    Breakpoint(u16),
    Delete(Option<usize>),
}

fn read_debugger_command() -> DebugCommand {
    print!("(gbdb) ");
    stdout().flush().expect("failed to flush stdout");
    let mut input = String::new();
    stdin().read_line(&mut input).expect("Failed to read input string");

    let mut words = input.trim().split_whitespace();
    match words.next() {
        None => DebugCommand::RepeatLastCommand,
        Some("n") | Some("next") => {
            match words.next() {
                None => DebugCommand::Next(1),
                Some(n) => {
                    match n.parse() {
                        Ok(n) => DebugCommand::Next(n),
                        Err(e) => {
                            println!("Misformatted next command: {}", e);
                            read_debugger_command()
                        }
                    }
                }
            }
        },
        Some("c") | Some("continue") => {
            DebugCommand::Continue
        },
        Some("b") | Some("break") | Some("breakpoint") => {
            match words.next() {
                None => {
                    println!("breakpoint not specified");
                    read_debugger_command()
                }
                Some(n) => {
                    match u16::from_str_radix(n, 16) {
                        Ok(n) => DebugCommand::Breakpoint(n),
                        Err(e) => {
                            println!("Misformatted breakpoint: {}", e);
                            read_debugger_command()
                        }
                    }
                }
            }
        }
        Some("d") | Some("del") | Some("delete") => {
            match words.next() {
                None => {
                    DebugCommand::Delete(None)
                }
                Some(n) => {
                    match n.parse() {
                        Ok(n) => DebugCommand::Delete(Some(n)),
                        Err(e) => {
                            println!("Misformatted delete command: {}", e);
                            read_debugger_command()
                        }
                    }
                }
            }
        }
        Some(unknown) => { 
            println!("Unrecognized command: {}", unknown); 
            read_debugger_command() 
        }
    }

}

fn run_rom_file(rom_file: &str, debug: bool) {
    let program = fs::read(rom_file).expect("Could not find rom file");
    eprintln!("Read ROM successfully; {} bytes", program.len());

    let mut cpu = Cpu::new();
    cpu.load_rom(&program);
    cpu.reset();


    let mut last_command = DebugCommand::Next(1);
    let mut next_counter: Option<usize> = Some(0);
    let mut breakpoints: Vec<u16> = Vec::new();

    if debug {
        // Don't include the display for now...
        cpu.run_with_callback(move |cpu: &mut Cpu| {
            let mut should_break = false;
            let mut break_reason: Vec<String> = vec![];
            match next_counter {
                Some(0) => {
                    should_break = true;
                    next_counter = None;
                }
                Some(n) => {
                    next_counter = Some(n - 1);
                }
                None => { }
            }
            for (i,b) in breakpoints.iter().enumerate() {
                if cpu.pc == *b {
                    should_break = true;
                    break_reason.push(format!("Breakpoint {} at {:04x}.", i, *b));
                }
            }

            if !should_break {
                return;
            }

            print!("{esc}c", esc = 27 as char); // clear screen
            for line in cpu.display_state() {
                println!("{}", line);
            }
            for line in break_reason {
                println!("{}", line);
            }
            
            loop {
                let command =  read_debugger_command();

                let command_to_run = match command {
                    DebugCommand::RepeatLastCommand => { last_command },
                    _ => { last_command = command; command },
                };

                use DebugCommand::*;
                match command_to_run {
                    Next(n_repeats) => { 
                        next_counter = Some(n_repeats - 1);
                        return;
                    },
                    Continue => { 
                        next_counter = None;
                        return;
                    }
                    Breakpoint(addr) => { 
                        breakpoints.push(addr);
                        println!("Breakpoint {}: 0x{:04x} set.", breakpoints.len(), addr);
                    }
                    Delete(Some(n)) => { 
                        if n >= breakpoints.len() {
                            println!("Breakpoint {} not found.", n);
                            continue;
                        }
                        let addr = breakpoints[n];
                        breakpoints.remove(n);
                        println!("Deleted breakpoint {}: 0x{:04x}.", breakpoints.len(), addr);
                    }
                    Delete(None) => {
                        breakpoints.clear();
                        println!("All breakpoints deleted.");
                    }
                    RepeatLastCommand => { unreachable!(); },
                }
            }
        });
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

    // let mut last_frame_time = SystemTime::now();
    let mut next_frame_n_cycle = 0;
    cpu.run_with_callback(move |cpu: &mut Cpu| {
        if cpu.n_cycles < next_frame_n_cycle {
            return;
        }
        next_frame_n_cycle += CYCLES_PER_FRAME;

        // let now = SystemTime::now();
        // eprintln!("Frame time: {}", now.duration_since(last_frame_time).expect("Time went backwards").as_millis());
        // last_frame_time = now;

        handle_user_input(&mut event_pump);
        let mut tile_map_rgb = [0 as u8; (SCREEN_FULL_X * 3 * SCREEN_FULL_Y) as usize];

        // let t1 = SystemTime::now();

        let mut addr = 0x9800;
        for x_tile in 0..32 {
            for y_tile in 0..32 {
                let chr = cpu.mem_read(addr);
                let tile_data = 0x8000 + (chr as u16 * 16);
                paint_tile(&cpu.mem_read_range(tile_data, tile_data + 16), 
                           &mut tile_map_rgb, 
                           SCREEN_FULL_X,
                           SCREEN_FULL_Y,
                           x_tile * 8,
                           y_tile * 8);
                addr += 1;
            }
        }

        // let t2 = SystemTime::now();
        // eprintln!("Tile paint time: {}", t2.duration_since(t1).expect("Time went backwards").as_millis());

        texture.update(None, &tile_map_rgb, (SCREEN_FULL_X * 3) as usize).unwrap();
        canvas.copy(&texture, None, None).unwrap();
        canvas.present();

        // let t3 = SystemTime::now();
        // eprintln!("Canvas update time: {}", t3.duration_since(t2).expect("Time went backwards").as_millis());

        // ::std::thread::sleep(std::time::Duration::from_millis(20));
    });

    eprintln!("Program terminated after {} cycles", cpu.n_cycles);
    std::process::exit(0);
}
