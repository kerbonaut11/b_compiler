import file_streams/file_stream as fs
import gleam/format.{sprintf}

pub fn write(file: fs.FileStream, str: String) -> Nil {
  let assert Ok(_) = fs.write_chars(file, str)
  Nil
}

pub fn write_ln(file: fs.FileStream, str: String) -> Nil {
  write(file, str <> "\n")
}

pub fn write_ln_indented(file: fs.FileStream, str: String) -> Nil {
  write(file, "  " <> str <> "\n")
}

pub fn write_fmt(file: fs.FileStream, fmt: String, args: a) -> Nil {
  let str = case sprintf(fmt, args) {
    Ok(str) -> str
    Error(err) -> {
      echo fmt
      echo args
      echo err
      panic
    }
  }
  let assert Ok(_) = fs.write_chars(file, str)
  Nil
}

pub fn write_ln_fmt(file: fs.FileStream, fmt: String, args: a) -> Nil {
  write_fmt(file, fmt <> "\n", args)
}

pub fn write_ln_indented_fmt(file: fs.FileStream, fmt: String, args: a) -> Nil {
  write_fmt(file, "  " <> fmt <> "\n", args)
}
