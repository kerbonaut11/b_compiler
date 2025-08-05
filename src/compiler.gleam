import gleam/set
import gleam/list
import gleam/dict
import lexer.{type Program}
import file_streams/file_stream.{type FileStream}
import file_utils.{write, write_ln,write_fmt, write_ln_fmt}

pub fn write_function_type(out: FileStream, arity: Int) {
  case arity {
    0 -> write(out, " (result i32)")
    _ -> {
      write(out, " (param i32)")
      write_function_type(out, arity-1)
    }
  }
}

pub fn compile(out: FileStream, program: Program) {
  write_ln(out, "(module")

  list.fold(set.to_list(program.extrns), Nil, fn (_,extrn) {
    write_fmt(out, "(import \"~s\" \"~s\" (func", #(extrn.mod, extrn.name))
    write_function_type(out, extrn.arity)
    write_ln(out, "))")
  })

  list.fold(dict.to_list(program.globals), Nil, fn (_,global) {
    let #(name, val) = global
    write_fmt(out, "(global $~s (mut i32) ", name)
    let val = case val {
      lexer.IntConst(x) -> x
      lexer.StringConst(x) -> 0
    }
    write_fmt(out, "(i32.const ~b)", val)
    write_ln(out, ")")
  })

  write_ln(out, ")")
}


