use std::{fs, process};
use std::path::PathBuf;
use rand::Rng;

const W:           usize = 64;
const H:           usize = 32;
const PROGRAM_START: u16 = 0x200;
const ETI_660_START: u16 = 0x600;
const FONTSET: [u8; 80]  = [
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

struct Chip8 {
    sp:           u8,             // Keeps track of the stack level in use
    delay_timer:  u8,             // When > 0, counts down at 60 Hz to 0
    sound_timer:  u8,             // Buzzer sounds when count hits 0
    rng_byte:     u8,             // Random value in 0..255

    opcode:       u16,            // 35 opcodes, each 2 bytes long
    idx_register: u16,            // Index register I
    pc:           u16,            // Program counter (0x000-0xFFF)

    /* 
    * Memory map:
    *   0x000-0x1FF: Chip-8 interpreter (contains font set in emulation)
    *   0x050-0x0A0: Used for the built-in 4x5 pixel font set (0-F)
    *   0x200-0xFFF: Program ROM and RAM
    */
    memory:       [u8;  4096],     // 4KB memory
    gfx:          [u32;  W*H],     // 64*32 px screen
    stack:        [u16;   16],     // Stack has 16 levels
    keypad:       [u8;    16],     // HEX-based keypad (0x0-0xF). Stores current state of key
    registers:    [u8;    16],     // 15 8-bit general-purpose registers (V0-VE; "Vx"), 16th, VF, 
                                   // is used for the carry flag and should not be used by any program
}

impl Chip8 {
    fn new() -> Chip8 {
        // Init registers and memory
        let sp:           u8          = 0;              // Reset stack pointer
        let delay_timer:  u8          = 0;
        let sound_timer:  u8          = 0;
        let rng_byte:     u8          = 0;
        
        let opcode:       u16         = 0;              // Reset current opcode
        let idx_register: u16         = 0;              // Reset I
        let pc:           u16         = PROGRAM_START;  // Application is loaded at location 0x200, so pc starts here
        
        let mut memory:   [u8;  4096] = [0; 4096];
        let gfx:          [u32; 2048] = [1; 2048];
        let stack:        [u16;   16] = [0; 16];
        let keypad:       [u8;    16] = [0; 16];
        let registers:    [u8;    16] = [0; 16];

        // Load fontset into memory
        for i in 0..80 {
            memory[0x50 + i] = FONTSET[i];
        }

        Chip8 {
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
            keypad,
            registers,
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
            self.memory[PROGRAM_START as usize + i] = byte;
        }
    }


    /************************************************/
    /******            Instructions            ******/
    /************************************************/

    // Clear the display (CLS)
    fn op_00E0(&mut self) {
        self.gfx.fill(0);
    }

    // Return from subroutine (RET)
    fn op_00EE(&mut self) {
        self.sp -= 1;
        self.pc = self.stack[self.sp as usize]
    }

    // Jump to location nnn (JP addr)
    fn op_1nnn(&mut self) {
        // "nnn or addr - A 12-bit value, the lowest 12 bits of the instruction"
        let address: u16 = self.opcode & 0x0FFF_u16;
        self.pc = address;
    }

    // Call subroutine at nnn (CALL addr)
    fn op_2nnn(&mut self) {
        // "nnn or addr - A 12-bit value, the lowest 12 bits of the instruction"
        let address: u16 = self.opcode & 0x0FFF_u16;

        self.stack[self.sp as usize] = self.pc;  // Put current PC onto the stack so we can return
        self.sp += 1;                            // Increment SP to next location on the stack
        self.pc = address;
    }

    // Skip to next instruction if Vx == kk (SE Vx, byte)
    fn op_3xkk(&mut self) {
        // "kk or byte - An 8-bit value, the lowest 8 bits of the instruction"
        // "x - A 4-bit value, the lower 4 bits of the high byte of the instruction"
        let vx: usize = ((self.opcode & 0x0F00_u16) >> 8u8) as usize;
        let kk: u8    =   self.opcode as u8;

        if self.registers[vx] == kk {
            self.pc += 2;
        }
    }

    // Skip to next instruction if Vx != kk (SE Vx, byte)
    fn op_4xkk(&mut self) {
        // "kk or byte - An 8-bit value, the lowest 8 bits of the instruction"
        // "x - A 4-bit value, the lower 4 bits of the high byte of the instruction"
        let vx: usize = ((self.opcode & 0x0F00_u16) >> 8u8) as usize;
        let kk: u8    =   self.opcode as u8;

        if self.registers[vx] != kk {
            self.pc += 2;
        }
    }

    // Skip next instruction if Vx == Vy (SE Vx, Vy)
    fn op_5xy0(&mut self) {
        // "x - A 4-bit value, the lower 4 bits of the high byte of the instruction"
        // "y - A 4-bit value, the upper 4 bits of the low byte of the instruction"
        let vx: usize = ((self.opcode & 0x0F00_u16) >> 8u8) as usize;
        let vy: usize = ((self.opcode & 0x00F0_u16) >> 4u8) as usize;

        if self.registers[vx] == self.registers[vy] {
            self.pc += 2;
        }
    }

    // Set Vx = kk (LD Vx, byte)
    // Put value kk into register Vx
    fn op_6xkk(&mut self) {
        // "kk or byte - An 8-bit value, the lowest 8 bits of the instruction"
        let vx: usize = ((self.opcode & 0x0F00_u16) >> 8u8) as usize;
        let kk: u8    =   self.opcode as u8;

        self.registers[vx] = kk;
    }

    // Set Vx = Vx + kk (ADD Vx, byte)
    // Adds the value kk to the value of register Vx, then stores the result in Vx.
    fn op_7xkk(&mut self) {
        // "kk or byte - An 8-bit value, the lowest 8 bits of the instruction"
        // "x - A 4-bit value, the lower 4 bits of the high byte of the instruction"
        let vx: usize = ((self.opcode & 0x0F00_u16) >> 8u8) as usize;
        let kk: u8    =   self.opcode as u8;

        self.registers[vx] += kk;
    }

    // Set Vx = Vy (LD Vx, vy)
    fn op_8xy0(&mut self) {
        // "x - A 4-bit value, the lower 4 bits of the high byte of the instruction"
        // "y - A 4-bit value, the upper 4 bits of the low byte of the instruction"
        let vx: usize = ((self.opcode & 0x0F00_u16) >> 8u8) as usize;
        let vy: usize = ((self.opcode & 0x00F0_u16) >> 4u8) as usize;

        self.registers[vx] = self.registers[vy];
    }

    // Set Vx = Vx OR Vy (OR Vx, Vy)
    fn op_8xy1(&mut self) {
        // "x - A 4-bit value, the lower 4 bits of the high byte of the instruction"
        // "y - A 4-bit value, the upper 4 bits of the low byte of the instruction"
        let vx: usize = ((self.opcode & 0x0F00_u16) >> 8u8) as usize;
        let vy: usize = ((self.opcode & 0x00F0_u16) >> 4u8) as usize;

        self.registers[vx] |= self.registers[vy];
    }

    // Set Vx = Vx AND Vy (AND Vx, Vy)
    fn op_8xy2(&mut self) {
        // "x - A 4-bit value, the lower 4 bits of the high byte of the instruction"
        // "y - A 4-bit value, the upper 4 bits of the low byte of the instruction"
        let vx: usize = ((self.opcode & 0x0F00_u16) >> 8u8) as usize;
        let vy: usize = ((self.opcode & 0x00F0_u16) >> 4u8) as usize;

        self.registers[vx] &= self.registers[vy];
    }

    // Set Vx = Vx XOR Vy (XOR Vx, Vy)
    fn op_8xy3(&mut self) {
        // "x - A 4-bit value, the lower 4 bits of the high byte of the instruction"
        // "y - A 4-bit value, the upper 4 bits of the low byte of the instruction"
        let vx: usize = ((self.opcode & 0x0F00_u16) >> 8u8) as usize;
        let vy: usize = ((self.opcode & 0x00F0_u16) >> 4u8) as usize;

        self.registers[vx] ^= self.registers[vy];
    }

    // Set Vx = Vx + Vy (ADD Vx, Vy)
    fn op_8xy4(&mut self) {
        // "x - A 4-bit value, the lower 4 bits of the high byte of the instruction"
        // "y - A 4-bit value, the upper 4 bits of the low byte of the instruction"
        let vx: usize = ((self.opcode & 0x0F00_u16) >> 8u8) as usize;
        let vy: usize = ((self.opcode & 0x00F0_u16) >> 4u8) as usize;

        let (sum, overflow): (u8, bool) = self.registers[vx].overflowing_add(self.registers[vy]);
        self.registers[vx] = sum;
        self.registers[0xF] = if overflow {1u8} else {0u8};
    }

    // Set Vx = Vx - Vy (SUB Vx, Vy)
    fn op_8xy5(&mut self) {
        // "x - A 4-bit value, the lower 4 bits of the high byte of the instruction"
        // "y - A 4-bit value, the upper 4 bits of the low byte of the instruction"
        let vx: usize = ((self.opcode & 0x0F00_u16) >> 8u8) as usize;
        let vy: usize = ((self.opcode & 0x00F0_u16) >> 4u8) as usize;

        self.registers[0xF] = if self.registers[vx] > self.registers[vy] {1u8} else {0u8};
        self.registers[vx] -= self.registers[vy];
    }

    // Set Vx = Vx shift right 1 (SHR Vx {, Vy})
    // "If the least-significant bit of Vx is 1, then VF is set to 1, otherwise 0. Then Vx is divided by 2."
    fn op_8xy6(&mut self) {
        // "x - A 4-bit value, the lower 4 bits of the high byte of the instruction"
        let vx: usize = ((self.opcode & 0x0F00_u16) >> 8u8) as usize;

        self.registers[0xF] = self.registers[vx] & 0x1;
        self.registers[vx] >>= 1u8;
    }

    // Set Vx = Vy - Vx (SUBN Vx, Vy)
    fn op_8xy7(&mut self) {
        // "x - A 4-bit value, the lower 4 bits of the high byte of the instruction"
        // "y - A 4-bit value, the upper 4 bits of the low byte of the instruction"
        let vx: usize = ((self.opcode & 0x0F00_u16) >> 8u8) as usize;
        let vy: usize = ((self.opcode & 0x00F0_u16) >> 4u8) as usize;

        self.registers[0xF] = if self.registers[vy] > self.registers[vx] {1u8} else {0u8};
        self.registers[vx] = self.registers[vy] - self.registers[vx];
    }

    // Set Vx = Vx shift left 1 (SHL Vx {, Vy})
    // "If the most-significant bit of Vx is 1, then VF is set to 1, otherwise to 0. Then Vx is multiplied by 2."
    fn op_8xyE(&mut self) {
        // "x - A 4-bit value, the lower 4 bits of the high byte of the instruction"
        let vx: usize = ((self.opcode & 0x0F00_u16) >> 8u8) as usize;

        self.registers[0xF] = (self.registers[vx] & 0x80) >> 7u8;
        self.registers[vx] <<= 1u8;
    }

    // Skip next instruction if Vx != Vy (SNE Vx, Vy)
    fn op_9xy0(&mut self) {
        // "x - A 4-bit value, the lower 4 bits of the high byte of the instruction"
        // "y - A 4-bit value, the upper 4 bits of the low byte of the instruction"
        let vx: usize = ((self.opcode & 0x0F00_u16) >> 8u8) as usize;
        let vy: usize = ((self.opcode & 0x00F0_u16) >> 4u8) as usize;

        if self.registers[vx] != self.registers[vy] {
            self.pc += 2;
        }
    }

    // Set I = nnn (LD I, addr)
    fn op_Annn(&mut self) {
        // "nnn or addr - A 12-bit value, the lowest 12 bits of the instruction"
        let address: u16 = self.opcode & 0x0FFF_u16;

        self.idx_register = address;
    }

    // Jump to location nnn + V0 (JP V0, addr)
    fn op_Bnnn(&mut self) {
        // "nnn or addr - A 12-bit value, the lowest 12 bits of the instruction"
        let address: u16 = self.opcode & 0x0FFF_u16;

        self.pc = self.registers[0] as u16 + address;
    }

    // Set Vx = random byte AND kk (RND Vx, byte)
    // "The interpreter generates a random number from 0 to 255,
    //  which is then ANDed with the value kk. The results are stored in Vx"
    fn op_Cxkk(&mut self) {
        // "x - A 4-bit value, the lower 4 bits of the high byte of the instruction"
        // "kk or byte - An 8-bit value, the lowest 8 bits of the instruction"
        let vx: usize = ((self.opcode & 0x0F00_u16) >> 8u8) as usize;
        let kk: u8    = self.opcode as u8;
        let r:  u8    = rand::thread_rng().gen();

        self.registers[vx] = kk & r;
    }

    // Display n-byte sprite starting at location I at (Vx, Vy), set VF = collision (DRW Vx, Vy, nibble)
    // "The interpreter reads n bytes from memory, starting at the address stored in I.
    // These bytes are then displayed as sprites on screen at coordinates (Vx, Vy).
    // Sprites are XORed onto the existing screen. If this causes any pixels to be erased,
    // VF is set to 1, otherwise it is set to 0. If the sprite is positioned so part of
    // it is outside the coordinates of the display, it wraps around to the opposite side of the screen"
    fn op_Dxyn(&mut self) {
        // "x - A 4-bit value, the lower 4 bits of the high byte of the instruction"
        // "y - A 4-bit value, the upper 4 bits of the low byte of the instruction"
        // "n or nibble - A 4-bit value, the lowest 4 bits of the instruction"
        let vx:     usize = ((self.opcode & 0x0F00_u16) >> 8u8) as usize;
        let vy:     usize = ((self.opcode & 0x00F0_u16) >> 4u8) as usize;
        let nibble: usize =  (self.opcode & 0x000F_u16)         as usize;

        // Wrap if going beyond screen boundaries
        let x_pos: u8 = self.registers[vx] % W as u8;
        let y_pos: u8 = self.registers[vy] % H as u8;

        self.registers[0xF] = 0;

        for row in 0..nibble {
            let sprite_byte: u8 = self.memory[self.idx_register as usize + row];
            for col in 0..8 {
                let     sprite_pixel: u8  = sprite_byte & (0x80_u8 >> col);
                let mut screen_pixel: u32 = (self.gfx[(y_pos as usize + row) * W as usize + (x_pos as usize + col)]);

                if sprite_pixel > 0 {
                    if screen_pixel == 0xFFFFFFFF {
                        self.registers[0xF] = 1u8;
                    }
                    screen_pixel ^= 0xFFFFFFFF;
                }
            }
        }
    }

    // Skip next instruction if key with value of Vx is pressed
    fn op_Ex9E(&mut self) {
        // "x - A 4-bit value, the lower 4 bits of the high byte of the instruction"
        let vx:  usize = ((self.opcode & 0x0F00_u16) >> 8u8) as usize;
        let key: usize =   self.registers[vx]                as usize;

        if self.keypad[key] != 0u8 {
            self.pc += 2;
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn construtor() {
        let chp = Chip8::new();
        assert_eq!(chp.sp,           0);
        assert_eq!(chp.delay_timer,  0);
        assert_eq!(chp.sound_timer,  0);
        assert_eq!(chp.rng_byte,     0);
        assert_eq!(chp.opcode,       0);
        assert_eq!(chp.idx_register, 0);
        assert_eq!(chp.pc,           PROGRAM_START);
        assert_eq!(chp.gfx,          [1; 2048]);
        assert_eq!(chp.stack,        [0; 16]);
        assert_eq!(chp.registers,    [0; 16]);
        assert_eq!(chp.keypad,       [0; 16]);

        for i in 0..80 {
            assert_eq!(chp.memory[0x50+i], FONTSET[i]);
        }
    }

    #[test]
    fn op_00E0() {
        let mut chp = Chip8::new();
        assert_eq!(chp.gfx, [1; 2048]);
        chp.op_00E0();
        assert_eq!(chp.gfx, [0; 2048]);
    }

    #[test]
    fn add_test() {
        let f: u8 = 0b11111111;
        let s: u8 = 0b11111111;

        let (sum, carry): (u8, bool) = f.overflowing_add(s);
        let sum_16: u16 = f as u16 + s as u16;

        assert_eq!(sum, sum_16 as u8);
        assert_eq!(sum_16 & 0xFF, sum as u16);
        assert_eq!(carry, true);
    }
}