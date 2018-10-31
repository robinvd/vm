pub mod bytecode;
pub mod code;

fn is_space(x: char) -> bool {
    [' ', '\t', '\0'].contains(&x)
}

fn is_char(x: char) -> bool {
    (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z')
}

fn is_num(x: char) -> bool {
    x >= '0' && x <= '9'
}
