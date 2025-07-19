import file_streams/file_stream as fs
import file_utils.{
  write, write_fmt, write_ln, write_ln_fmt, write_ln_indented,
  write_ln_indented_fmt,
}
import gleam/bool
import gleam/format
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import ir.{type Ir}
import ir_compiler.{type IrFunction, type IrProgram}
import op

const word_size = 4

pub fn compile(file: fs.FileStream, program: IrProgram) -> Nil {
  write_ln(file, "format ELF")
  write_ln(file, "public main")

  list.fold(program.externs, Nil, fn(_, extrn) {
    write_ln_fmt(file, "extrn ~s", extrn)
  })

  write(file, "globals: dd ")
  list.fold(program.global_data, Nil, fn(_, global) {
    case global {
      ir_compiler.IntConst(x) -> {
        write_fmt(file, "~b,", x)
      }
      ir_compiler.StringConst(offset) -> {
        write_fmt(file, "strings+~b,", offset)
      }
    }
  })
  write_ln(file, "0")

  write(file, "strings: db ")
  list.fold(program.str_data, Nil, fn(_, str) {
    write_fmt(file, "\"~s\",0,", str)
  })
  write_ln(file, "0")

  list.fold(program.functions, Nil, fn(_, function) {
    compile_function(file, program, function)
  })

  Nil
}

fn compile_function(
  file: fs.FileStream,
  program: IrProgram,
  function: IrFunction,
) -> Nil {
  write_ln(file, function.name <> ":")
  write_ln_indented(file, "push ebp")
  write_ln_indented(file, "mov ebp,esp")
  let stack_size = { function.var_count + function.temp_count } * word_size
  write_ln_indented_fmt(file, "sub esp, ~b", stack_size)
  compile_function_ir(
    file,
    program,
    function,
    [None, None, None, None, None, None],
    function.ir,
  )
}

fn temp_ptr(idx: Int) -> String {
  let idx = { idx + 1 } * word_size
  let assert Ok(str) = format.sprintf("dword [ebp-~b]", idx)
  str
}

fn auto_ptr(function: IrFunction, idx: Int) -> String {
  temp_ptr(idx + function.temp_count)
}

fn arg_ptr(idx: Int) -> String {
  let idx = { idx + 2 } * word_size
  let assert Ok(str) = format.sprintf("dword [ebp+~b]", idx)
  str
}

type RegisterState =
  List(Option(Int))

type TempState {
  OnStack
  InRegister(Int)
}

fn register_name(register: Int) -> String {
  case register {
    0 -> "eax"
    1 -> "ebx"
    2 -> "ecx"
    3 -> "edx"
    4 -> "edi"
    5 -> "esi"
    _ -> panic
  }
}

fn temp_storage(state: RegisterState, temp_idx: Int) -> TempState {
  list.index_fold(state, OnStack, fn(prev, register, idx) {
    case register {
      Some(reg_idx) if reg_idx == temp_idx -> InRegister(idx)
      _ -> prev
    }
  })
}

fn temp_storage_code(state: RegisterState, temp_idx: Int) -> String {
  case temp_storage(state, temp_idx) {
    InRegister(register) -> register_name(register)
    OnStack -> temp_ptr(temp_idx)
  }
}

fn set_register_state(
  state: RegisterState,
  register: Int,
  val: Option(Int),
) -> RegisterState {
  list.index_map(state, fn(previous, idx) {
    case idx == register {
      True -> val
      False -> previous
    }
  })
}

fn put_in_register(
  file: fs.FileStream,
  state: RegisterState,
  temp_idx: Int,
) -> #(RegisterState, Int) {
  case temp_storage(state, temp_idx) {
    InRegister(register) -> #(state, register)
    OnStack -> {
      let #(state, register) = allocate_register(file, state, temp_idx)
      write_ln_indented_fmt(file, "mov ~s, ~s", #(
        register_name(register),
        temp_ptr(temp_idx),
      ))
      #(state, register)
    }
  }
}

fn move_out_register(
  file: fs.FileStream,
  state: RegisterState,
  register: Int,
) -> RegisterState {
  let assert Ok(previous) = state |> list.drop(register) |> list.first
  case previous {
    Some(idx) ->
      write_ln_indented_fmt(file, "mov ~s, ~s", #(
        temp_ptr(idx),
        register_name(register),
      ))
    None -> Nil
  }
  set_register_state(state, register, None)
}

fn allocate_register(
  file: fs.FileStream,
  state: RegisterState,
  temp_idx: Int,
) -> #(RegisterState, Int) {
  case temp_storage(state, temp_idx) {
    InRegister(register) -> #(state, register)
    OnStack -> {
      let register = int.random(6)
      let state = move_out_register(file, state, register)
      let state = set_register_state(state, register, Some(temp_idx))
      #(state, register)
    }
  }
}

fn compile_function_ir(
  file: fs.FileStream,
  program: IrProgram,
  function: IrFunction,
  register_state: RegisterState,
  ir: List(Ir),
) -> Nil {
  use <- bool.guard(ir == [], Nil)
  let assert Ok(op) = list.first(ir)

  echo register_state

  let register_state = case op {
    ir.LoadInt(dest, x) -> {
      let #(register_state, register) =
        allocate_register(file, register_state, dest)
      write_ln_indented_fmt(file, "mov ~s, ~b", #(register_name(register), x))
      set_register_state(register_state, register, Some(dest))
    }

    ir.LoadString(dest, offset) -> {
      let #(register_state, register) =
        allocate_register(file, register_state, dest)
      write_ln_indented_fmt(file, "mov ~s, strings+~b", #(
        register_name(register),
        offset,
      ))
      set_register_state(register_state, register, Some(dest))
    }

    ir.LoadName(dest, name) -> {
      let #(register_state, register) =
        allocate_register(file, register_state, dest)
      write_ln_indented_fmt(file, "mov ~s, ~s", #(register_name(register), name))
      register_state
    }

    ir.GetArg(dest, arg_idx) -> {
      let #(register_state, register) =
        allocate_register(file, register_state, dest)
      write_ln_indented_fmt(file, "mov ~s, ~s", #(
        register_name(register),
        arg_ptr(arg_idx),
      ))
      register_state
    }

    ir.GetAuto(dest, auto_idx) -> {
      let #(register_state, register) =
        allocate_register(file, register_state, dest)
      write_ln_indented_fmt(file, "mov ~s, ~s", #(
        register_name(register),
        auto_ptr(function, auto_idx),
      ))
      register_state
    }

    ir.SetAuto(src, auto_idx) -> {
      let #(register_state, src) = put_in_register(file, register_state, src)
      write_ln_indented_fmt(file, "mov ~s, ~s", #(
        auto_ptr(function, auto_idx),
        register_name(src),
      ))
      register_state
    }

    ir.GetGlobal(dest, offset) -> {
      let #(register_state, register) =
        allocate_register(file, register_state, dest)
      write_ln_indented_fmt(file, "mov ~s, dword [globals+~b]", #(
        register_name(register),
        offset * word_size,
      ))
      register_state
    }

    ir.SetGlobal(src, offset) -> {
      let #(register_state, src) = put_in_register(file, register_state, src)
      write_ln_indented_fmt(file, "mov dword [globals+~b], ~s", #(
        offset * word_size,
        register_name(src),
      ))
      register_state
    }

    ir.PushArg(src) -> {
      write_ln_indented_fmt(
        file,
        "push ~s",
        temp_storage_code(register_state, src),
      )
      register_state
    }

    ir.Call(dest) -> {
      let register_state = move_out_register(file, register_state, 0)
      write_ln_indented_fmt(
        file,
        "call ~s",
        temp_storage_code(register_state, dest),
      )
      write_ln_indented_fmt(
        file,
        "mov ~s, eax",
        temp_storage_code(register_state, dest),
      )
      register_state
    }

    ir.BinaryOp(op, dest, src) -> {
      let #(register_state, dest) = put_in_register(file, register_state, dest)
      let op = case op {
        op.Add -> "add"
        op.Sub -> "sub"
        op.Mul -> "imul"
        op.Div -> panic
        op.Mod -> panic

        op.And -> "and"
        op.Or -> "or"
        op.Xor -> "xor"
        op.Shl -> "shl"
        op.Shr -> "shr"

        op.Eq -> todo
        op.Ne -> todo
        op.Lt -> todo
        op.Le -> todo
        op.Gt -> todo
        op.Ge -> todo
      }
      write_ln_indented_fmt(file, "~s ~s, ~s", #(
        op,
        register_name(dest),
        temp_storage_code(register_state, src),
      ))
      register_state
    }

    ir.Not(dest) -> {
      write_ln_indented_fmt(
        file,
        "not ~s",
        temp_storage_code(register_state, dest),
      )
      register_state
    }

    ir.Neg(dest) -> {
      write_ln_indented_fmt(
        file,
        "neg ~s",
        temp_storage_code(register_state, dest),
      )
      register_state
    }

    ir.Load(ptr) -> {
      let #(register_state, register) =
        put_in_register(file, register_state, ptr)
      let name = register_name(register)
      write_ln_indented_fmt(file, "mov ~s, [~s]", #(name, name))
      register_state
    }

    ir.Store(ptr, src) -> {
      let #(register_state, ptr) = put_in_register(file, register_state, ptr)
      let #(register_state, src) = put_in_register(file, register_state, src)
      write_ln_indented_fmt(file, "mov [~s], src", #(
        register_name(ptr),
        register_name(src),
      ))
      register_state
    }

    ir.Index(dest, src) -> {
      let #(register_state, dest) = put_in_register(file, register_state, dest)
      let #(register_state, src) = put_in_register(file, register_state, src)
      write_ln_indented_fmt(file, "mov ~s, [~s+~s*4]", #(
        register_name(dest),
        register_name(dest),
        register_name(src),
      ))
      register_state
    }

    ir.RefIndex(dest, src) -> {
      let #(register_state, dest) = put_in_register(file, register_state, dest)
      let #(register_state, src) = put_in_register(file, register_state, src)
      write_ln_indented_fmt(file, "lea ~s, [~s+~s*4]", #(
        register_name(dest),
        register_name(dest),
        register_name(src),
      ))
      register_state
    }

    ir.Return -> {
      write_ln_indented_fmt(
        file,
        "mov eax, ~s",
        temp_storage_code(register_state, 0),
      )
      let stack_size = { function.var_count + function.temp_count } * word_size
      write_ln_indented_fmt(file, "add esp, ~b", stack_size)
      write_ln_indented(file, "leave")
      write_ln_indented(file, "ret")
      register_state
    }
    _ -> todo
  }
  compile_function_ir(file, program, function, register_state, list.drop(ir, 1))
}
