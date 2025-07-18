import error.{type Error}
import gleam/bool
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
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

pub type Expr {
  Ident(String)
  IntLiteral(Int)
  StringLiteral(String)

  Unary(op: UnaryOp, val: Expr)
  Binary(op: BinaryOp, lhs: Expr, rhs: Expr)

  Index(array: Expr, idx: Expr)
  Call(function: Expr, args: List(Expr))
}

pub fn parse(tokens: List(Token)) -> Result(Expr, Error) {
  echo tokens
  case tokens {
    [token.IntLiteral(x)] -> Ok(IntLiteral(x))
    [token.StringLiteral(x)] -> Ok(StringLiteral(x))
    [token.Ident(x)] -> Ok(Ident(x))
    [] -> panic
    _ -> try_parse_binary_op(tokens)
  }
}

fn try_parse_binary_op(tokens: List(Token)) -> Result(Expr, Error) {
  case lowest_priority_op(tokens) {
    Some(#(op, idx)) -> {
      let #(lhs, rhs) = list.split(tokens, idx)
      let rhs = list.drop(rhs, 1)
      use lhs <- result.try(parse(lhs))
      use rhs <- result.try(parse(rhs))
      Ok(Binary(op, lhs, rhs))
    }
    None -> try_parse_unary_op(tokens)
  }
}

fn try_parse_unary_op(tokens: List(Token)) -> Result(Expr, Error) {
  let assert Ok(first) = list.first(tokens)
  case token_to_unary_op(first) {
    Ok(op) -> {
      use val <- result.try(parse(list.drop(tokens, 1)))
      Ok(Unary(op, val))
    }
    Error(_) -> try_parse_misc_expr(tokens)
  }
}

fn try_parse_misc_expr(tokens: List(Token)) -> Result(Expr, Error) {
  let assert Ok(last) = list.last(tokens)
  case last {
    token.RoundClose -> {
      use #(lhs, in_brackets) <- result.try(
        token_utils.split_matching_bracket_reverse(tokens),
      )
      case lhs {
        [] -> parse(in_brackets)
        _ -> {
          use function <- result.try(parse(lhs))
          use args <- result.try(parse_args(in_brackets, []))
          Ok(Call(function, args))
        }
      }
    }

    token.SquareClose -> {
      use #(lhs, in_brackets) <- result.try(
        token_utils.split_matching_bracket_reverse(tokens),
      )
      use array <- result.try(parse(lhs))
      use idx <- result.try(parse(in_brackets))
      Ok(Index(array, idx))
    }
    _ -> {
      echo tokens
      panic
    }
  }
}

fn parse_args(
  tokens: List(Token),
  args: List(Expr),
) -> Result(List(Expr), Error) {
  use <- bool.guard(tokens == [], Ok(args))
  let #(left, right) =
    token_utils.split_at_outside_brackets(tokens, token.Comma)
  use arg <- result.try(parse(left))
  parse_args(right, list.append(args, [arg]))
}

fn token_to_binary_op(token: Token) -> Result(BinaryOp, Nil) {
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

fn token_to_unary_op(token: Token) -> Result(UnaryOp, Nil) {
  case token {
    token.OpRef -> Ok(Ref)
    token.OpDeref -> Ok(Deref)
    token.OpNeg -> Ok(Neg)
    token.OpNot -> Ok(Not)
    _ -> Error(Nil)
  }
}

fn op_priority(op: BinaryOp) -> Int {
  case op {
    Add | Sub -> 0
    Mul | Div | Mod -> 1
    Shl | Shr -> 2
    And | Or | Xor -> 3
    Eq | Ne | Lt | Le | Gt | Ge -> 4
  }
}

fn lowest_priority_op(tokens: List(Token)) -> Option(#(BinaryOp, Int)) {
  let #(_, idx, op, _) =
    list.index_fold(tokens, #(999, None, Add, 0), fn(loop_data, token, index) {
      let #(lowest_priority, idx, op, depth) = loop_data
      let depth = depth + token_utils.bracket_depth(token)
      case token_to_binary_op(token) {
        Ok(current_op) -> {
          let priority = op_priority(current_op)
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
