use std::{fs, process};
use std::path::PathBuf;
use rand::Rng;


const FONTSET: [u8; 80] = [
    0xF0, 0x90, 0x90, 0x90, 0xF0, // 0
    0x20, 0x60, 0x20, 0x20, 0x70, // 1
    0xF0, 0x10, 0xF0, 0x80, 0xF0, // 2
    0xF0, 0x10, 0xF0, 0x10, 0xF0, // 3
    0x90, 0x90, 0xF0, 0x10, 0x10, // 4
    0xF0, 0x80, 0xF0, 0x10, 0xF0, // 5
    0xF0, 0x80, 0xF0, 0x90, 0xF0, // 6
    0xF0, 0x10, 0x20, 0x40, 0x40, // 7
    0xF0, 0x90, 0xF0, 0x90, 0xF0, // 8
    0xF0, 0x90, 0xF0, 0x10, 0xF0, // 9
    0xF0, 0x90, 0xF0, 0x90, 0x90, // A
    0xE0, 0x90, 0xE0, 0x90, 0xE0, // B
    0xF0, 0x80, 0x80, 0x80, 0xF0, // C
    0xE0, 0x90, 0x90, 0x90, 0xE0, // D
    0xF0, 0x80, 0xF0, 0x80, 0xF0, // E
    0xF0, 0x80, 0xF0, 0x80, 0x80  // F
];

struct Chip_8 {
    sp:           u8,          // Keeps track of the stack level in use
    delay_timer:  u8,          // When > 0, counts down at 60 Hz to 0
    sound_timer:  u8,          // Buzzer sounds when count hits 0
    rng_byte:     u8,          // Random value in 0..255

    opcode:       u16,         // 35 opcodes, each 2 bytes long
    idx_register: u16,         // Index register I
    pc:           u16,         // Program counter (0x000-0xFFF)
    
    /* 
    * Memory map:
    *   0x000-0x1FF: Chip-8 interpreter (contains font set in emulation)
    *   0x050-0x0A0: Used for the built-in 4x5 pixel font set (0-F)
    *   0x200-0xFFF: Program ROM and RAM
    */
    memory:       [u8; 4096],  // 4KB memory
    gfx:          [u8; 2048],  // 64*32 px screen
    stack:        [u16;  16],  // Stack has 16 levels
    registers:    [u8;   16],  // 15 8-bit general-purpose registers (V0-VE), 16th is used for the carry flag
    key:          [u8;   16],  // HEX-based keypad (0x0-0xF). Stores current state of key
}

impl Chip_8 {
    fn new() -> Chip_8 {
        // Init registers and memory
        let sp:           u8         = 0;      // Reset stack pointer
        let delay_timer:  u8         = 0;
        let sound_timer:  u8         = 0;
        let rng_byte:     u8         = 0;
       
        let opcode:       u16        = 0;      // Reset current opcode
        let idx_register: u16        = 0;      // Reset I
        let pc:           u16        = 0x200;  // Application is loaded at location 0x200, so pc starts here
        
        let mut memory:   [u8; 4096] = [0; 4096];
        let gfx:          [u8; 2048] = [1; 2048];
        let stack:        [u16;  16] = [0; 16];
        let registers:    [u8;   16] = [0; 16];
        let key:          [u8;   16] = [0; 16];

        // Load fontset into memory
        for i in 0..80 {
            memory[0x50 + i] = FONTSET[i];
        }

        Chip_8 {
            pc,
            opcode,
            idx_register,
            sp,
            delay_timer,
            sound_timer,
            rng_byte,
            memory,
            gfx,
            stack,
            registers,
            key
        }
    }

    fn load_rom(&mut self, path: PathBuf) {
        // Read ROM file
        let rom = fs::read(path)
            .unwrap_or_else(|err| {
                eprintln!("Unable to read ROM: {err}");
                process::exit(1);
            }).into_iter();

        // Load ROM data into memory starting at address 0x200
        for (i, byte) in rom.enumerate() {
            self.memory[0x200 + i] = byte;
        }
    }

    /*** Instructions ***/

    // Clear the display (CLS)
    fn op_00E0(&mut self) {
        self.gfx.fill(0);
    }

    // Return from subroutine

}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn construtor() {
        let chp = Chip_8::new();
        assert_eq!(chp.sp,           0);
        assert_eq!(chp.delay_timer,  0);
        assert_eq!(chp.sound_timer,  0);
        assert_eq!(chp.rng_byte,     0);
        assert_eq!(chp.opcode,       0);
        assert_eq!(chp.idx_register, 0);
        assert_eq!(chp.pc,           0x200);
        assert_eq!(chp.gfx,          [1; 2048]);
        assert_eq!(chp.stack,        [0; 16]);
        assert_eq!(chp.registers,    [0; 16]);
        assert_eq!(chp.key,          [0; 16]);

        for i in 0..80 {
            assert_eq!(chp.memory[0x50+i], FONTSET[i]);
        }
    }

    #[test]
    fn cls() {
        let mut chp = Chip_8::new();
        assert_eq!(chp.gfx, [1; 2048]);
        chp.op_00E0();
        assert_eq!(chp.gfx, [0; 2048]);
    }
}