fn a_fn() {
    dbg!(2100 * 10);
}

fn main() {
    for _ in 0..100 {
        a_fn();
    }
}
