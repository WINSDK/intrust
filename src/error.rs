use crate::Error;
use std::fmt;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::NotACallable(path) => {
                f.write_fmt(format_args!("{path:?} is not a callable."))
            }
            Error::UnknownPath(path) => {
                f.write_fmt(format_args!("path '{path}' is unknown."))
            }
            Error::FileNotFound(path) => {
                f.write_fmt(format_args!("file '{path}' is not a valid file."))
            }
        }
    }
}
