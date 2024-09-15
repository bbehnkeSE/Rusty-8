mod chip8;

fn main() {
    // Try to load test rom and print memory
    let mut chp = chip8::Chip8::new();
    chp.load_rom("roms/test_opcode.ch8".into());

    chp.test_print_memory();
}
