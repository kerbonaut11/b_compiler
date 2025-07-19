import error.{type Error}
import gleam/bool
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import op.{type BinaryOp, type UnaryOp}
import token.{type Token}
import token_utils

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
  case op.find_lowest_priority(tokens) {
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
  case op.token_to_unary(first) {
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
