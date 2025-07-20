import error.{type Error}
import expr.{type Expr}
import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/result
import gleam/set
import gleam/string
import ir.{type Ir}
import lexer.{type Function, type Global, type Program, type Statement}
import op

type StringData {
  StringData(offset: Int, offsets: Dict(String, Int), ordered: List(String))
}

pub type Constant {
  IntConst(Int)
  StringConst(Int)
}

pub type IrFunction {
  IrFunction(
    name: String,
    arg_count: Int,
    var_count: Int,
    temp_count: Int,
    ir: List(Ir),
  )
}

pub type IrProgram {
  IrProgram(
    str_data: List(String),
    global_data: List(Constant),
    externs: List(String),
    functions: List(IrFunction),
  )
}

pub fn compile(program: Program) -> Result(IrProgram, Error) {
  let str_data = compile_string_data(program)
  let global_data = compile_globals(program.globals, str_data)
  use functions <- result.try(
    program.functions
    |> dict.to_list
    |> list.try_map(fn(x) { compile_function(program, x.0, x.1, str_data) }),
  )
  Ok(IrProgram(
    str_data.ordered,
    global_data,
    set.to_list(program.extrns),
    functions,
  ))
}

fn string_constant_offset(data: StringData, str: String) -> Int {
  case dict.get(data.offsets, str) {
    Ok(offset) -> offset
    Error(_) -> {
      echo data
      panic
    }
  }
}

fn add_string_constant(data: StringData, str: String) -> StringData {
  case dict.has_key(data.offsets, str) {
    True -> data
    False -> {
      StringData(
        data.offset + string.byte_size(str) + 1,
        dict.insert(data.offsets, str, data.offset),
        list.append(data.ordered, [str]),
      )
    }
  }
}

fn compile_string_data(program: Program) -> StringData {
  let data = StringData(offset: 0, offsets: dict.new(), ordered: [])
  let data =
    list.fold(dict.to_list(program.globals), data, fn(data, global) {
      let global = global.1
      case global.init_val {
        lexer.StringConst(str) -> add_string_constant(data, str)
        _ -> data
      }
    })
  list.fold(dict.to_list(program.functions), data, fn(data, function) {
    let function = function.1
    list.fold(function.body, data, fn(data, statement) {
      statement_compile_string_data(data, statement)
    })
  })
}

fn statement_compile_string_data(
  data: StringData,
  statement: Statement,
) -> StringData {
  case statement {
    lexer.Assing(lhs, rhs) -> {
      data |> expr_compile_string_data(lhs) |> expr_compile_string_data(rhs)
    }
    lexer.Expr(expr) | lexer.Return(Some(expr)) -> {
      data |> expr_compile_string_data(expr)
    }
    _ -> data
  }
}

fn expr_compile_string_data(data: StringData, expr: expr.Expr) -> StringData {
  case expr {
    expr.StringLiteral(x) -> {
      add_string_constant(data, x)
    }
    expr.Unary(_, expr) -> {
      data |> expr_compile_string_data(expr)
    }
    expr.Binary(_, lhs, rhs) | expr.Index(lhs, rhs) -> {
      data |> expr_compile_string_data(lhs) |> expr_compile_string_data(rhs)
    }
    expr.Call(function, args) -> {
      let data = expr_compile_string_data(data, function)
      list.fold(args, data, expr_compile_string_data)
    }
    _ -> data
  }
}

fn compile_globals(globals: Dict(String, Global), str_data: StringData) {
  dict.to_list(globals)
  |> list.map(pair.second)
  |> list.sort(fn(a, b) { int.compare(a.id, b.id) })
  |> list.fold([], fn(data, global) {
    case global.init_val {
      lexer.IntConst(x) -> {
        list.append(data, [IntConst(x)])
      }
      lexer.StringConst(x) -> {
        list.append(data, [StringConst(string_constant_offset(str_data, x))])
      }
    }
  })
}

fn compile_function(
  program: Program,
  name: String,
  function: Function,
  str_data: StringData,
) -> Result(IrFunction, error.Error) {
  use #(ir, var_count) <- result.try(compile_function_rec(
    function.body,
    FunctionCompileState(
      program,
      str_data,
      ir: [],
      args: list.index_fold(function.params, dict.new(), dict.insert),
      locals: dict.new(),
      max_var_count: 0,
    ),
  ))

  let temp_count =
    list.fold(ir, 0, fn(max, ir) {
      let dest = case ir {
        ir.LoadString(dest, _)
        | ir.LoadInt(dest, _)
        | ir.BinaryOp(_, dest, _)
        | ir.Not(dest)
        | ir.Neg(dest)
        | ir.Index(dest, _)
        | ir.RefIndex(dest, _)
        | ir.Load(dest)
        | ir.Store(dest, _)
        | ir.GetArg(dest, _)
        | ir.GetArgRef(dest, _)
        | ir.GetAuto(dest, _)
        | ir.GetAutoRef(dest, _)
        | ir.GetGlobal(dest, _)
        | ir.GetGlobalRef(dest, _)
        | ir.LoadName(dest, _)
        | ir.GetTempRef(dest)
        | ir.Call(dest) -> dest
        ir.PushArg(_) | ir.Return | ir.SetAuto(_, _) | ir.SetGlobal(_, _) -> 0
      }

      int.max(dest, max)
    })

  Ok(IrFunction(name, list.length(function.params), var_count, temp_count, ir))
}

type FunctionCompileState {
  FunctionCompileState(
    program: Program,
    str_data: StringData,
    ir: List(Ir),
    args: Dict(String, Int),
    locals: Dict(String, Int),
    max_var_count: Int,
  )
}

fn add_local(state: FunctionCompileState, name: String) -> FunctionCompileState {
  let idx = dict.size(state.locals)
  FunctionCompileState(
    ..state,
    locals: dict.insert(state.locals, name, idx),
    max_var_count: int.max(state.max_var_count, idx + 1),
  )
}

type Storage {
  Auto(Int)
  Arg(Int)
  Global(Int)
  Name(String)
}

fn name_storage(state: FunctionCompileState, name: String) -> Option(Storage) {
  use <- result.lazy_unwrap(
    dict.get(state.locals, name)
    |> result.map(fn(x) { Some(Auto(x)) }),
  )
  use <- result.lazy_unwrap(
    dict.get(state.args, name)
    |> result.map(fn(x) { Some(Arg(x)) }),
  )
  use <- result.lazy_unwrap(
    dict.get(state.program.globals, name)
    |> result.map(fn(x) { Some(Global(x.id)) }),
  )
  case
    dict.has_key(state.program.functions, name)
    || set.contains(state.program.extrns, name)
  {
    True -> Some(Name(name))
    False -> None
  }
}

fn compile_function_rec(
  statements: List(Statement),
  state: FunctionCompileState,
) -> Result(#(List(Ir), Int), error.Error) {
  use <- bool.guard(statements == [], Ok(#(state.ir, state.max_var_count)))
  let assert Ok(statement) = list.first(statements)
  echo state.ir

  case statement {
    lexer.AutoDeclaration(name, size: None) -> {
      compile_function_rec(list.drop(statements, 1), add_local(state, name))
    }

    lexer.Assing(lhs, rhs) -> {
      use rhs_ir <- result.try(compile_expr(0, rhs, state))
      use lhs_ir <- result.try(case lhs {
        expr.Ident(name) -> {
          case name_storage(state, name) {
            Some(Auto(x)) -> Ok([ir.SetAuto(0, x)])
            Some(Global(x)) -> Ok([ir.SetAuto(0, x)])
            Some(Arg(x)) -> todo
            Some(Name(x)) -> todo
            None -> todo
          }
        }

        _ -> todo
      })
      compile_function_rec(
        list.drop(statements, 1),
        FunctionCompileState(
          ..state,
          ir: state.ir |> list.append(rhs_ir) |> list.append(lhs_ir),
        ),
      )
    }

    lexer.Expr(expr) -> {
      use expr_ir <- result.try(compile_expr(0, expr, state))
      compile_function_rec(
        list.drop(statements, 1),
        FunctionCompileState(..state, ir: list.append(state.ir, expr_ir)),
      )
    }

    lexer.Return(expr) -> {
      let expr = option.unwrap(expr, expr.IntLiteral(0))
      use expr_ir <- result.try(compile_expr(0, expr, state))
      Ok(#(
        state.ir |> list.append(expr_ir) |> list.append([ir.Return]),
        state.max_var_count,
      ))
    }
    _ -> todo
  }
}

fn compile_expr(
  dest: Int,
  expr: Expr,
  state: FunctionCompileState,
) -> Result(List(Ir), error.Error) {
  case expr {
    expr.IntLiteral(x) -> Ok([ir.LoadInt(dest, x)])
    expr.StringLiteral(x) -> {
      let offset = string_constant_offset(state.str_data, x)
      Ok([ir.LoadString(dest, offset)])
    }
    expr.Ident(name) ->
      case name_storage(state, name) {
        Some(Auto(x)) -> Ok([ir.GetAuto(dest, x)])
        Some(Arg(x)) -> Ok([ir.GetArg(dest, x)])
        Some(Global(x)) -> Ok([ir.GetGlobal(dest, x)])
        Some(Name(x)) -> Ok([ir.LoadName(dest, x)])
        None -> todo
      }

    expr.Unary(op.Ref, val) -> {
      case val {
        expr.Ident(name) ->
          case name_storage(state, name) {
            Some(Auto(x)) -> Ok([ir.GetAutoRef(dest, x)])
            Some(Arg(x)) -> Ok([ir.GetArgRef(dest, x)])
            Some(Global(x)) -> Ok([ir.GetGlobalRef(dest, x)])
            Some(Name(x)) -> Ok([ir.LoadName(dest, x), ir.GetTempRef(dest)])
            None -> todo
          }

        expr.Index(array, idx) -> {
          use array <- result.try(compile_expr(dest, array, state))
          use idx <- result.try(compile_expr(dest + 1, idx, state))
          Ok(
            array
            |> list.append(idx)
            |> list.append([ir.RefIndex(dest, dest + 1)]),
          )
        }
        _ -> todo
      }
    }

    expr.Unary(op, val) -> {
      use val <- result.try(compile_expr(dest, val, state))
      let op_ir = case op {
        op.Deref -> ir.Load(dest)
        op.Not -> ir.Not(dest)
        op.Neg -> ir.Neg(dest)
        op.Ref -> panic
      }
      Ok(list.append(val, [op_ir]))
    }

    expr.Binary(op, lhs, rhs) -> {
      use lhs <- result.try(compile_expr(dest, lhs, state))
      use rhs <- result.try(compile_expr(dest + 1, rhs, state))
      Ok(
        lhs
        |> list.append(rhs)
        |> list.append([ir.BinaryOp(op, dest, dest + 1)]),
      )
    }

    expr.Index(array, idx) -> {
      use array <- result.try(compile_expr(dest, array, state))
      use idx <- result.try(compile_expr(dest + 1, idx, state))
      Ok(array |> list.append(idx) |> list.append([ir.Index(dest, dest + 1)]))
    }

    expr.Call(function, args) -> {
      use function <- result.try(compile_expr(dest, function, state))
      use ir <- result.try(
        list.try_fold(args, function, fn(ir, arg) {
          use arg_ir <- result.try(compile_expr(dest + 1, arg, state))
          Ok(ir |> list.append(arg_ir) |> list.append([ir.PushArg(dest + 1)]))
        }),
      )
      Ok(list.append(ir, [ir.Call(dest)]))
    }
    _ -> todo
  }
}
