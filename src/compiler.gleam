import gleam/int
import gleam/result
import gleam/string
import gleam/set
import gleam/list
import gleam/dict
import lexer.{type Program}
import file_streams/file_stream.{type FileStream}
import file_utils.{write, write_ln, write_fmt, write_ln_fmt}

fn write_function_type(out: FileStream, arity: Int) {
  case arity {
    0 -> write(out, " (result i32)")
    _ -> {
      write(out, " (param i32)")
      write_function_type(out, arity-1)
    }
  }
}

fn write_function_header(out: FileStream, name: String, func: lexer.Function) {
  write_fmt(out, "(func $~s", name)
  list.fold(func.params, Nil, fn(_,param) {
    write_fmt(out, " (param $~s i32)", param)
  })
  write_ln(out, " (result i32)")
}

fn write_locals(out: FileStream, ast: List(lexer.Statement)) {
  case ast {
    [lexer.AutoDeclaration(name,_), ..next] -> {
      write_ln_fmt(out, "(local $~s i32)", name)
      write_locals(out, next)
    }
    [_, ..next] -> write_locals(out, next)
    [] -> Nil
  }
}

fn hex_digit(x: Int) -> String {
  [case x < 10 {
    True -> x+48
    False -> x-10+97
  }
    |> string.utf_codepoint
    |> result.lazy_unwrap(fn(){panic})]
    |> string.from_utf_codepoints
}

fn hex_byte(x: Int) -> String {
  "\\"
    <> hex_digit(x |> int.bitwise_and(0xf))
    <> hex_digit(x |> int.bitwise_shift_right(4) |> int.bitwise_and(0xf))
}


pub fn compile(out: FileStream, program: Program) {
  write_ln(out, "(module")

  list.fold(set.to_list(program.extrns), Nil, fn (_,extrn) {
    write_fmt(out, "(import \"~s\" \"~s\" (func", #(extrn.mod, extrn.name))
    write_function_type(out, extrn.arity)
    write_ln(out, "))")
  })

  let #(globals, global_data) = list.fold(dict.to_list(program.globals), #(dict.new(), []), fn (acc, global) {
    let #(globals, global_data) = acc
    let #(name, val) = global
    let val = case val {
      lexer.IntConst(x) -> x
      lexer.StringConst(x) -> 0
    }
    #(
      dict.insert(globals, name, dict.size(globals)),
      list.append(global_data, [val])
    )
  })

  list.fold(dict.to_list(program.functions), Nil, fn (_,func) {
    let #(name, func) = func
    write_function_header(out, name, func)
    write_locals(out, func.body)
    write_ln(out, ")")
  })
  write_ln(out, ")")

  write_ln(out, "(data (i32.const 0) \"")
  list.fold(global_data, Nil, fn (_,data) {
    write_ln(out, 
      hex_byte(data)
        <> hex_byte(int.bitwise_shift_right(data, 8))
        <> hex_byte(int.bitwise_shift_right(data, 16))
        <> hex_byte(int.bitwise_shift_right(data, 24))
    )
  })
  write_ln(out, "\")")
}


