#[derive(Debug)]
struct Custom {
    val: i32,
}

fn main() {
    let a = "some string";
    let b = 0x1377;
    let c = Custom { val: 100 };
    let d = &c.val;
    let e = &(0x1377 as i32) as &dyn std::fmt::Debug;
    dbg!(&a, &b, &c, &d, &e);
}
