pub mod bytecode;
pub mod code;

fn is_space(x: char) -> bool {
    [' ', '\t', '\0'].contains(&x)
}

fn is_char(x: char) -> bool {
    if (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z') {
        true
    } else {
        false
    }
}

fn is_num(x: char) -> bool {
    if x >= '0' && x <= '9' {
        true
    } else {
        false
    }
}
