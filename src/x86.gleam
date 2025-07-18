import error.{type Error}
import file_streams/file_stream as fs
import gleam/int
import gleam/list
import gleam/result
import ir_compiler

fn write(file: fs.FileStream, str: String) -> Result(Nil, Error) {
  fs.write_chars(file, str) |> result.map_error(error.Io)
}

fn try_write(
  file: fs.FileStream,
  str: String,
  then: fn() -> Result(Nil, Error),
) -> Result(Nil, Error) {
  case fs.write_chars(file, str) {
    Ok(_) -> then()
    Error(err) -> Error(error.Io(err))
  }
}

pub fn compile(
  file: fs.FileStream,
  program: ir_compiler.IrProgram,
) -> Result(Nil, Error) {
  use <- try_write(file, "format ELF\n")

  use _ <- result.try(
    list.try_fold(program.externs, Nil, fn(_, extrn) {
      use <- try_write(file, "extrn " <> extrn <> "\n")
      Ok(Nil)
    }),
  )

  use <- try_write(file, "globals: dd ")
  use _ <- result.try(
    list.try_fold(program.global_data, Nil, fn(_, global) {
      case global {
        ir_compiler.IntConst(x) -> {
          write(file, int.to_string(x) <> ",")
        }
        ir_compiler.StringConst(offset) -> {
          write(file, "strings+" <> int.to_string(offset) <> ",")
        }
      }
    }),
  )
  use <- try_write(file, "0\n")

  use <- try_write(file, "strings: db ")
  use _ <- result.try(
    list.try_fold(program.str_data, Nil, fn(_, str) {
      write(file, "\"" <> str <> "\",0,")
    }),
  )
  use <- try_write(file, "0\n")

  Ok(Nil)
}
