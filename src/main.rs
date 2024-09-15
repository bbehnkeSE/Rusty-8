mod chip8;

fn main() {
    let mut chp = chip8::Chip8::new();
    chp.load_rom("roms/test_opcode.ch8".into());

    chp.test_print_memory();
}
