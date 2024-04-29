use crate::Error;
use crate::repl::ReplCommandError;
use std::fmt;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Repl(err) => err.fmt(f)?,
        }
        Ok(())
    }
}

impl From<ReplCommandError> for Error {
    fn from(value: ReplCommandError) -> Error {
        Error::Repl(value)
    }
}
