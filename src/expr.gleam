import error.{type Error}
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
    Error(_) -> try_misc_expr(tokens)
  }
}

fn try_misc_expr(tokens: List(Token)) -> Result(Expr, Error) {
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
          let args = []
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
    _ -> panic
  }
}

fn token_to_binary_op(token: Token) -> Result(BinaryOp, Nil) {
  case token {
    token.OpAdd -> Ok(Add)
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
  }
}

fn lowest_priority_op(tokens: List(Token)) -> Option(#(BinaryOp, Int)) {
  let #(_, idx, op) =
    list.index_fold(tokens, #(999, None, Add), fn(lowest, token, index) {
      let #(lowest_priority, _, _) = lowest
      case token_to_binary_op(token) {
        Ok(op) ->
          case op_priority(op) {
            prio if prio <= lowest_priority -> #(prio, Some(index), op)
            _ -> lowest
          }

        Error(_) -> lowest
      }
    })
  case idx {
    Some(idx) -> Some(#(op, idx))
    None -> None
  }
}
