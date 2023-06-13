# yagbem
yet another gameboy emulator

## Features
* Written in rust (as a way for me to learn the language!)
* Emulation of all SM83 processor instructions
    * passes the blargg `cpu_instrs` test roms
    * passes the [jsmoo](https://github.com/raddad772/jsmoo/tree/main/misc/tests/GeneratedTests/sm83/v1) instruction tests. These were very helpful for validating basic CPU function!
    * *not* cycle accurate -- the base unit of time is one instruction, which can range from 4 - 24 "T-cycles", aka 1 - 4 "M-cycles"
* A basic gdb-like debugger for monitoring processor status
* Line-based PPU (pixel-processing unit) rendering
* 4 of 5 interrupts are generated and handled (LCD Vblank, LCD status change, timer overflow, and joypad button press), but again not at a cycle-accurate level

## Peripherals
* Joypad is hooked up to keyboard: `WASD` for direction, `jk` for B/A, and `,.` for select/start
* LCD screen is displayed using the SDL2 library
* Audio is not yet implemented (but I want to soon!)
* Serial port output gets printed as ASCII to stderr for now
