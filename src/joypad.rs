pub struct Joypad {
    act_buttons_selected: bool,
    dir_buttons_selected: bool,
    status: JoypadButton,
    should_trigger_interrupt: bool,
}

use bitflags::bitflags;
bitflags! {
    pub struct JoypadButton: u8 {
        const START    = (1 << 7);
        const SELECT   = (1 << 6);
        const BUTTON_B = (1 << 5);
        const BUTTON_A = (1 << 4);
        const DOWN     = (1 << 3);
        const UP       = (1 << 2);
        const LEFT     = (1 << 1);
        const RIGHT    = (1 << 0);
    }
}

bitflags! {
    struct ButtonSelection: u8 {
        const ACTION = (1 << 5);
        const DIRECTION = (1 << 4);
    }
}

impl Joypad {
    pub fn new() -> Joypad { 
        Joypad {
            act_buttons_selected: false,
            dir_buttons_selected: false,
            status: JoypadButton::empty(),
            should_trigger_interrupt: false,
        }
    }

    pub fn write(&mut self, val: u8) {
        let sel = ButtonSelection::from_bits_truncate(val);
        self.act_buttons_selected = !sel.contains(ButtonSelection::ACTION);
        self.dir_buttons_selected = !sel.contains(ButtonSelection::DIRECTION);
    }

    pub fn read(&self) -> u8 {
        let mut pressed = 0;
        if self.act_buttons_selected {
            pressed |= (self.status.bits() >> 4) & 0x0f;
        }
        if self.dir_buttons_selected {
            pressed |= self.status.bits() & 0x0f;
        }
        // if pressed != 0 {
        //     eprintln!("Joypad read: {:08b} ({} {})", !pressed, self.act_buttons_selected, self.dir_buttons_selected);
        // }
        !pressed
    }

    pub fn set_button_pressed_status(&mut self, button: JoypadButton, status: bool) {
        // eprintln!("Button status: {:?} {}", button, status);
        let old_status = self.status.contains(button);
        self.status.set(button, status);

        if status && !old_status {
            // don't worry about case where two buttons on same input line are pressed
            // simultaneously
            self.should_trigger_interrupt = true;
        }
    }

    pub fn should_trigger_intr(&mut self) -> bool {
        if self.should_trigger_interrupt {
            self.should_trigger_interrupt = false;
            return true;
        } else {
            return false;
        }
    }
}
