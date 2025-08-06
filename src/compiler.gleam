import gleam/int
import gleam/result
import gleam/string
import gleam/set
import gleam/list
import gleam/dict.{type Dict}
import gleam/option.{Some}
import lexer.{type Program}
import expr.{type Expr}
import file_streams/file_stream.{type FileStream}
import file_utils.{write, write_ln, write_fmt, write_ln_fmt}

const page_size = 65536
const max_pages = 65536
const stack_pages = 8

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
    <> hex_digit(x |> int.bitwise_shift_right(4) |> int.bitwise_and(0xf))
    <> hex_digit(x |> int.bitwise_and(0xf))
}

type StringData { StringData(
  map: Dict(String, Int),
  list: List(String),
  offset: Int
)}

fn add_string_data(data: StringData, str: String) -> StringData {
  StringData(
    map: dict.insert(data.map, str, data.offset),
    list: list.append(data.list, [str]),
    offset: data.offset + string.length(str) + 1
  )
}

fn get_offset(data: StringData, str: String) -> Int {
  data.map |> dict.get(str) |> result.lazy_unwrap(fn(){panic})
}

fn add_string_data_block(data: StringData, ast: List(lexer.Statement)) -> StringData {
  case ast {
    [lexer.Assing(_,expr), ..next] |[lexer.Expr(expr), ..next] | [lexer.Return(Some(expr)), ..next]  -> {
      add_string_data_block(add_string_data_expr(data, expr), next)
    }
    [_, ..next] -> add_string_data_block(data, next)
    [] -> data
  }
}


fn add_string_data_expr(data: StringData, expr: expr.Expr) -> StringData {
  case expr {
    expr.Binary(_, a, b) | expr.Index(a, b) -> {
      data |> add_string_data_expr(a)|> add_string_data_expr(b)
    }
    expr.Call(func, args) -> {
      let data = add_string_data_expr(data, func)
      list.fold(args, data, add_string_data_expr)
    }
    expr.Unary(_, expr) -> add_string_data_expr(data, expr)
    expr.StringLiteral(x) -> add_string_data(data, x)
    _ -> data
  }
}


pub fn compile(out: FileStream, program: Program) {
  write_ln(out, "(module")

  list.fold(set.to_list(program.extrns), Nil, fn (_,extrn) {
    write_fmt(out, "(import \"~s\" \"~s\" (func", #(extrn.mod, extrn.name))
    write_function_type(out, extrn.arity)
    write_ln(out, "))")
  })


  let #(globals, global_data, string_data) = list.fold(
    dict.to_list(program.globals),
    #(dict.new(), [], StringData(map: dict.new(), offset: dict.size(program.globals)*4, list: [])),
    fn (acc, global) {
      let #(globals, global_data, string_data) = acc
      let #(name, val) = global
      let #(val, string_data) = case val {
        lexer.IntConst(x) -> #(x, string_data)
        lexer.StringConst(x) -> {
          let data = add_string_data(string_data, x)
          let offset = get_offset(data, x)
          #(offset, data)
        }
      }
      #(
        dict.insert(globals, name, dict.size(globals)),
        list.append(global_data, [val]),
        string_data
      )
  })

  let string_data = list.fold(dict.to_list(program.functions), string_data, fn (data, func) {
    add_string_data_block(data, func.1.body)
  })

  list.fold(dict.to_list(program.functions), Nil, fn (_,func) {
    let #(name, func) = func
    write_function_header(out, name, func)
    write_locals(out, func.body)
    write_ln(out, ")")
  })

  write(out, "(data (i32.const 0) \"")

  list.fold(global_data, Nil, fn (_, data) {
    write(out, 
      hex_byte(data)
        <> hex_byte(int.bitwise_shift_right(data, 8))
        <> hex_byte(int.bitwise_shift_right(data, 16))
        <> hex_byte(int.bitwise_shift_right(data, 24))
    )
  })

  list.fold(string_data.list, Nil, fn (_, str) {
    write(out, str <> "\\00")
    Nil
  })
  write_ln(out, "\")")

  let data_size = string_data.offset + dict.size(program.globals)*4
  let data_pages = data_size / page_size + 1
  write_ln_fmt(out, "(memory ~b ~b)", #(data_pages+stack_pages, max_pages))

  write_ln(out, ")")
}


