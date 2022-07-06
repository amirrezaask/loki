#[derive(Debug, PartialEq, Eq)]
pub struct Error {
    msg: String,
    loc: Option<(i32, i32)>,
    severity: i8,
}

impl Error {
    pub fn unknown(msg: String) -> Self {
        Self {
            msg,
            loc: None,
            severity: 1,
        }
    }
    pub fn unexpected(exp: String, found: String, _loc: i32) -> Self {
        Self {
            msg: format!("expected {} found {}", exp, found),
            loc: None,
            severity: 1,
        }
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some((line, col)) = self.loc {
            if self.severity == 0 {
                f.write_fmt(format_args!("Warning: {} at {}:{}", self.msg, line, col))
            } else {
                f.write_fmt(format_args!("Error: {} at {}:{}", self.msg, line, col))
            }
        } else if self.severity == 0 {
            f.write_fmt(format_args!("Warning: {}", self.msg))
        } else {
            f.write_fmt(format_args!("Error: {}", self.msg))
        }
    }
}
impl std::error::Error for Error {}

pub fn panic(msg: String) {
    println!("{}", msg);
    std::process::exit(1);
}