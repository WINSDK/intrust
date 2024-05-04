fn a_fn(s: &str) {
    println!("{s}");
}

fn main() {
    let s = unsafe {
        let ptr = 0x1337 as _;
        let len = 100;

        // First, we build a &[u8]...
        let slice = std::slice::from_raw_parts(ptr, len);

        // ... and then convert that slice into a string slice
        std::str::from_utf8(slice).unwrap()
    };

    a_fn(s);
}
