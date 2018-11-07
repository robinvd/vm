pub mod code;

fn is_space(x: char) -> bool {
    [' ', '\t', '\0'].contains(&x)
}
