import gleam/list
import gleam/option.{type Option, None, Some}
import token.{type Token}
import token_utils

pub type BinaryOp {
  Add
  Sub
  Mul
  Div
  Mod

  And
  Or
  Xor
  Shl
  Shr

  Eq
  Ne
  Lt
  Le
  Gt
  Ge
}

pub type UnaryOp {
  Ref
  Deref
  Neg
  Not
}

pub fn token_to_binary(token: Token) -> Result(BinaryOp, Nil) {
  case token {
    token.OpAdd -> Ok(Add)
    token.OpSub -> Ok(Sub)
    token.OpMul -> Ok(Mul)
    token.OpDiv -> Ok(Div)
    token.OpMod -> Ok(Mod)
    token.OpAnd -> Ok(And)
    token.OpOr -> Ok(Or)
    token.OpXor -> Ok(Xor)
    token.OpShl -> Ok(Shl)
    token.OpShr -> Ok(Shr)
    _ -> Error(Nil)
  }
}

pub fn token_to_unary(token: Token) -> Result(UnaryOp, Nil) {
  case token {
    token.OpRef -> Ok(Ref)
    token.OpDeref -> Ok(Deref)
    token.OpNeg -> Ok(Neg)
    token.OpNot -> Ok(Not)
    _ -> Error(Nil)
  }
}

pub fn priority(op: BinaryOp) -> Int {
  case op {
    Add | Sub -> 0
    Mul | Div | Mod -> 1
    Shl | Shr -> 2
    And | Or | Xor -> 3
    Eq | Ne | Lt | Le | Gt | Ge -> 4
  }
}

pub fn find_lowest_priority(tokens: List(Token)) -> Option(#(BinaryOp, Int)) {
  let #(_, idx, op, _) =
    list.index_fold(tokens, #(999, None, Add, 0), fn(loop_data, token, index) {
      let #(lowest_priority, idx, op, depth) = loop_data
      let depth = depth + token_utils.bracket_depth(token)
      case token_to_binary(token) {
        Ok(current_op) -> {
          let priority = priority(current_op)
          case priority <= lowest_priority && depth == 0 {
            True -> #(priority, Some(index), current_op, depth)
            False -> #(lowest_priority, idx, op, depth)
          }
        }

        Error(_) -> #(lowest_priority, idx, op, depth)
      }
    })
  case idx {
    Some(idx) -> Some(#(op, idx))
    None -> None
  }
}
