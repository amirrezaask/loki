mod compile;
mod emit;
mod run;
mod core;

pub use compile::compile;
pub use emit::emit;
pub use run::*;
pub use self::core::parse_cli;