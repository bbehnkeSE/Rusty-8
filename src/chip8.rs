use std::{fs, process};
use std::path::PathBuf;
use rand::Rng;

const W:             usize    = 64;
const H:             usize    = 32;
const PROGRAM_START: u16      = 0x200;
const ETI_660_START: u16      = 0x600;
const FONTSET:      [u8; 80]  = [
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
    0xF0, 0x80, 0xF0, 0x80, 0x80, // F
];

pub struct Chip8 {
    sp:            u8,            // Keeps track of the stack level in use
    delay_timer:   u8,            // When > 0, counts down at 60 Hz to 0
    sound_timer:   u8,            // Buzzer sounds when count hits 0
    rng_byte:      u8,            // Random value in 0..255

    opcode:        u16,           // 35 opcodes, each 2 bytes long
    idx_register:  u16,           // Index register I
    pc:            u16,           // Program counter (0x000-0xFFF)

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

    // Table of opcode function pointers
    table:  [fn(&mut Chip8); 16],
    table0: [fn(&mut Chip8); 16],
    table8: [fn(&mut Chip8); 16],
    tableE: [fn(&mut Chip8); 16],
    tableF: [fn(&mut Chip8); 256],
}

impl Chip8 {
    pub fn new() -> Self {
        // Init registers and memory
        let sp:            u8         = 0;              // Reset stack pointer
        let delay_timer:   u8         = 0;
        let sound_timer:   u8         = 0;
        let rng_byte:      u8         = 0;
        
        let opcode:        u16        = 0;              // Reset current opcode
        let idx_register:  u16        = 0;              // Reset I
        let pc:            u16        = PROGRAM_START;  // Application is loaded at location 0x200, so pc starts here
        
        let mut memory:   [u8;  4096] = [0; 4096];
        let gfx:          [u32; 2048] = [1; 2048];
        let stack:        [u16;   16] = [0; 16];
        let keypad:       [u8;    16] = [0; 16];
        let registers:    [u8;    16] = [0; 16];

        // Populate function pointer table
        let mut table   = [Chip8::op_null as fn(&mut Chip8); 16];
        let mut table0  = [Chip8::op_null as fn(&mut Chip8); 16];
        let mut table8  = [Chip8::op_null as fn(&mut Chip8); 16];
        let mut tableE  = [Chip8::op_null as fn(&mut Chip8); 16];
        let mut tableF  = [Chip8::op_null as fn(&mut Chip8); 256];
        
        for i in 0..0xE {
            table0[i]   =  Chip8::op_null;
            table8[i]   =  Chip8::op_null;
            tableE[i]   =  Chip8::op_null;
        }

        for i in 0..0x65 {
            tableF[i]   =  Chip8::op_null;
        }

        table[0x0]      =  Chip8::table_0;
        table[0x1]      =  Chip8::op_1nnn;
        table[0x2]      =  Chip8::op_2nnn;
        table[0x3]      =  Chip8::op_3xkk;
        table[0x4]      =  Chip8::op_4xkk;
        table[0x5]      =  Chip8::op_5xy0;
        table[0x6]      =  Chip8::op_6xkk;
        table[0x7]      =  Chip8::op_7xkk;
        table[0x8]      =  Chip8::table_8;
        table[0x9]      =  Chip8::op_9xy0;
        table[0xA]      =  Chip8::op_Annn;
        table[0xB]      =  Chip8::op_Bnnn;
        table[0xC]      =  Chip8::op_Cxkk;
        table[0xD]      =  Chip8::op_Dxyn;
        table[0xE]      =  Chip8::table_e;
        table[0xF]      =  Chip8::table_f;

        table0[0x0]     =  Chip8::op_00E0;
        table0[0xE]     =  Chip8::op_00EE;

        table8[0x0]     =  Chip8::op_8xy0;
        table8[0x1]     =  Chip8::op_8xy1;
        table8[0x2]     =  Chip8::op_8xy2;
        table8[0x3]     =  Chip8::op_8xy3;
        table8[0x4]     =  Chip8::op_8xy4;
        table8[0x5]     =  Chip8::op_8xy5;
        table8[0x6]     =  Chip8::op_8xy6;
        table8[0x7]     =  Chip8::op_8xy7;
        table8[0xE]     =  Chip8::op_8xyE;

        tableE[0x1]     =  Chip8::op_ExA1;
        tableE[0xE]     =  Chip8::op_Ex9E;

        tableF[0x07]    =  Chip8::op_Fx07;
        tableF[0x0A]    =  Chip8::op_Fx0A;
        tableF[0x15]    =  Chip8::op_Fx15;
        tableF[0x18]    =  Chip8::op_Fx18;
        tableF[0x1E]    =  Chip8::op_Fx1E;
        tableF[0x29]    =  Chip8::op_Fx29;
        tableF[0x33]    =  Chip8::op_Fx33;
        tableF[0x55]    =  Chip8::op_Fx55;
        tableF[0x65]    =  Chip8::op_Fx65;
        
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

            table,
            table0,
            table8,
            tableE,
            tableF,
        }
    }

    pub fn load_rom(&mut self, path: PathBuf) {
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

    /* Cycle:
     *   Fetch next instruction
     *   Decode instruction
     *   Execute instruction
     * 
     * Get the first digit of the opcode with a bitmask, shift it over to a single digit from $0 to $F, 
     * then use it as an index into the function pointer array
     */
    pub fn cycle(&mut self) {
        // Fetch next instruction
        self.opcode = (self.memory[(self.pc << 8u8) as usize] | self.memory[(self.pc + 1) as usize]) as u16;
        self.pc    += 2;

        // Decode and execute
        self.table[((self.opcode & 0xF000_u16) >> 12u8) as usize](self);

        // Decrement delay and sound timers if they've been set
        if self.delay_timer > 0 { self.delay_timer -= 1; }
        if self.sound_timer > 0 { self.sound_timer -= 1; }
    }


    /************************************************/
    /******            Instructions            ******/
    /************************************************/

    // Index function pointer tables with opcodes
    fn table_0(&mut self) {
        self.table0[(self.opcode & 0x000F) as usize](self);
    }

    fn table_8(&mut self) {
        self.table8[(self.opcode & 0x000F) as usize](self);
    }

    fn table_e(&mut self) {
        self.tableE[(self.opcode & 0x000F) as usize](self);
    }

    fn table_f(&mut self) {
        self.tableF[(self.opcode & 0x00FF) as usize](self);
    }
    
    
    /*** Opcodes ***/

    // Null
    fn op_null(&mut self) {}
    
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
                let sprite_pixel:      u8  = sprite_byte & (0x80_u8 >> col);
                let screen_pixel: &mut u32 = &mut (self.gfx[(y_pos as usize + row) * W as usize + (x_pos as usize + col)]);

                if sprite_pixel > 0 {
                    if *screen_pixel == 0xFFFFFFFF {
                        self.registers[0xF] = 1u8;
                    }
                    *screen_pixel ^= 0xFFFFFFFF;
                }
            }
        }
    }

    // Skip next instruction if key with value of Vx is pressed (SKP Vx)
    fn op_Ex9E(&mut self) {
        // "x - A 4-bit value, the lower 4 bits of the high byte of the instruction"
        let vx:  usize = ((self.opcode & 0x0F00_u16) >> 8u8) as usize;
        let key: usize =   self.registers[vx]                as usize;

        if self.keypad[key] != 0u8 {
            self.pc += 2;
        }
    }

    // Skip next instruction if key with the value of Vx is not pressed (SKNP Vx)
    fn op_ExA1(&mut self) {
        // "x - A 4-bit value, the lower 4 bits of the high byte of the instruction"
        let vx:  usize = ((self.opcode & 0x0F00_u16) >> 8u8) as usize;
        let key: usize =   self.registers[vx]                as usize;

        if self.keypad[key] == 0u8 {
            self.pc += 2;
        }
    }

    // Set Vx = delay timer (LD Vx, DT)
    fn op_Fx07(&mut self) {
        let vx:  usize = ((self.opcode & 0x0F00_u16) >> 8u8) as usize;

        self.registers[vx] = self.delay_timer;
    }

    // Wait for a key press, store value of the key in Vx (LD Vx, K)
    fn op_Fx0A(&mut self) {
        // "x - A 4-bit value, the lower 4 bits of the high byte of the instruction"
        let vx:  usize = ((self.opcode & 0x0F00_u16) >> 8u8) as usize;

        if      self.keypad[0]  != 0u8 { self.registers[vx] = 0;  }
        else if self.keypad[1]  != 0u8 { self.registers[vx] = 1;  }
        else if self.keypad[2]  != 0u8 { self.registers[vx] = 2;  }
        else if self.keypad[3]  != 0u8 { self.registers[vx] = 3;  }
        else if self.keypad[4]  != 0u8 { self.registers[vx] = 4;  }
        else if self.keypad[5]  != 0u8 { self.registers[vx] = 5;  }
        else if self.keypad[6]  != 0u8 { self.registers[vx] = 6;  }
        else if self.keypad[7]  != 0u8 { self.registers[vx] = 7;  }
        else if self.keypad[8]  != 0u8 { self.registers[vx] = 8;  }
        else if self.keypad[9]  != 0u8 { self.registers[vx] = 9;  }
        else if self.keypad[10] != 0u8 { self.registers[vx] = 10; }
        else if self.keypad[11] != 0u8 { self.registers[vx] = 11; }
        else if self.keypad[12] != 0u8 { self.registers[vx] = 12; }
        else if self.keypad[13] != 0u8 { self.registers[vx] = 13; }
        else if self.keypad[14] != 0u8 { self.registers[vx] = 14; }
        else if self.keypad[15] != 0u8 { self.registers[vx] = 15; }
        else                           { self.pc           -= 2;  }
    }

    // Set delay timer = Vx (LD DT, Vx)
    fn op_Fx15(&mut self) {
        // "x - A 4-bit value, the lower 4 bits of the high byte of the instruction"
        let vx:  usize = ((self.opcode & 0x0F00_u16) >> 8u8) as usize;

        self.delay_timer = self.registers[vx];
    }

    // Set sound timer = Vx (LD ST, Vx)
    fn op_Fx18(&mut self) {
        // "x - A 4-bit value, the lower 4 bits of the high byte of the instruction"
        let vx:  usize = ((self.opcode & 0x0F00_u16) >> 8u8) as usize;

        self.sound_timer = self.registers[vx];
    }

    // Set I = I + Vx (ADD I, Vx)
    fn op_Fx1E(&mut self) {
        // "x - A 4-bit value, the lower 4 bits of the high byte of the instruction"
        let vx:  usize = ((self.opcode & 0x0F00_u16) >> 8u8) as usize;

        self.idx_register += self.registers[vx] as u16;
    }

    // Set I = location of sprite for digit Vx (LD F, Vx)
    // "We know the font characters are located at 0x50, 
    //  and we know they’re five bytes each, so we can get the address of the
    //  first byte of any character by taking an offset from the start address."
    fn op_Fx29(&mut self) {
        // "x - A 4-bit value, the lower 4 bits of the high byte of the instruction"
        let vx:    usize = ((self.opcode & 0x0F00_u16) >> 8u8) as usize;
        let digit: u8    = self.registers[vx];

        self.idx_register = 0x50 + (5 * digit) as u16;
    }

    // Store BCD representation of Vx in memory locations I, I+1, and I+2 (LD B, Vx)
    // "The interpreter takes the decimal value of Vx, and places the hundreds digit
    //  in memory at location in I, the tens digit at location I+1,
    //  and the ones digit at location I+2."
    fn op_Fx33(&mut self) {
        // "x - A 4-bit value, the lower 4 bits of the high byte of the instruction"
        let vx:        usize = ((self.opcode & 0x0F00_u16) >> 8u8) as usize;
        let mut digit: u8    = self.registers[vx];

        // Ones-place
        self.memory[(self.idx_register + 2) as usize] = digit % 10;
        digit /= 10;

        // Tens-place
        self.memory[(self.idx_register + 1) as usize] = digit % 10;
        digit /= 10;

        // Hundreds-place
        self.memory[self.idx_register as usize] = digit % 10;
    }

    // Store registers V0 - Vx in memory starting at location I (LD [I], Vx)
    fn op_Fx55(&mut self) {
        // "x - A 4-bit value, the lower 4 bits of the high byte of the instruction"
        let vx:  usize = ((self.opcode & 0x0F00_u16) >> 8u8) as usize;

        for i in 0..vx {
            self.memory[(self.idx_register + i as u16) as usize] = self.registers[i];
        }
    }

    // Read registers V0 - Vx from memory starting at location I
    fn op_Fx65(&mut self) {
        // "x - A 4-bit value, the lower 4 bits of the high byte of the instruction"
        let vx:  usize = ((self.opcode & 0x0F00_u16) >> 8u8) as usize;

        for i in 0..vx {
            self.registers[i] = self.memory[(self.idx_register + i as u16) as usize];
        }
    }

    // Debug print
    pub fn test_print_memory(&self) {
        println!("{:?}", self.memory)
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
}