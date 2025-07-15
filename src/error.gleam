import file_streams/file_stream_error
import token.{type Token}

pub type Error {
  Io(file_stream_error.FileStreamError)
  UnclosedBracket
  MismatchedBrackets
  ExpectedTokenAfter(Token)
  ExpectedToken(expected: Token, got: Token)
}

pub fn from_io(err: file_stream_error.FileStreamError) -> Error {
  Io(err)
}
