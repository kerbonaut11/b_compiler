import error.{type Error}
import gleam/bool
import gleam/list
import gleam/result
import token.{type Token}

pub fn bracket_depth(token: Token) -> Int {
  case token {
    token.RoundOpen | token.CurlyOpen | token.SquareOpen -> 1
    token.RoundClose | token.CurlyClose | token.SquareClose -> -1
    _ -> 0
  }
}

pub fn split_matching_bracket(
  tokens: List(Token),
) -> Result(#(List(Token), List(Token)), Error) {
  let assert Ok(open_bracket) = list.first(tokens)
  use #(in_brackets, end, close_bracket) <- result.try(
    split_matching_bracket_rec(list.drop(tokens, 1), [], 1),
  )
  let brackets_match = case open_bracket, close_bracket {
    token.RoundOpen, token.RoundClose
    | token.CurlyOpen, token.CurlyClose
    | token.SquareOpen, token.SquareClose
    -> True
    _, _ -> False
  }
  use <- bool.guard(!brackets_match, Error(error.MismatchedBrackets))
  Ok(#(in_brackets, end))
}

pub fn split_matching_bracket_reverse(
  tokens: List(Token),
) -> Result(#(List(Token), List(Token)), Error) {
  let invert_brackets = fn(token) {
    case token {
      token.RoundOpen -> token.RoundClose
      token.RoundClose -> token.RoundOpen
      token.CurlyOpen -> token.CurlyClose
      token.CurlyClose -> token.CurlyOpen
      token.SquareOpen -> token.SquareClose
      token.SquareClose -> token.SquareOpen
      _ -> token
    }
  }

  let invert_tokens = fn(tokens) {
    tokens
    |> list.reverse
    |> list.map(invert_brackets)
  }

  tokens
  |> invert_tokens
  |> split_matching_bracket
  |> result.map(fn(tokens) {
    let #(in_brackets, left) = tokens
    #(invert_tokens(left), invert_tokens(in_brackets))
  })
}

fn split_matching_bracket_rec(
  tokens: List(Token),
  in_brackets: List(Token),
  depth: Int,
) -> Result(#(List(Token), List(Token), Token), Error) {
  use first <- result.try(
    list.first(tokens) |> result.map_error(fn(_) { error.UnclosedBracket }),
  )
  let tokens = list.drop(tokens, 1)
  let depth = depth + bracket_depth(first)
  case depth {
    0 -> Ok(#(in_brackets, tokens, first))
    _ ->
      split_matching_bracket_rec(
        tokens,
        list.append(in_brackets, [first]),
        depth,
      )
  }
}

pub fn split_at_outside_brackets(
  tokens: List(Token),
  target: Token,
) -> #(List(Token), List(Token)) {
  split_at_outside_brackets_rec(tokens, target, [], 0)
}

fn split_at_outside_brackets_rec(
  tokens: List(Token),
  target: Token,
  left: List(Token),
  depth: Int,
) -> #(List(Token), List(Token)) {
  use <- bool.guard(tokens == [], #(left, []))
  let assert Ok(first) = list.first(tokens)
  let tokens = list.drop(tokens, 1)
  let depth = depth + bracket_depth(first)
  case depth {
    0 if first == target -> #(left, tokens)
    _ ->
      split_at_outside_brackets_rec(
        tokens,
        target,
        list.append(left, [first]),
        depth,
      )
  }
}

pub fn unwrap_ident(token: Token) -> Result(String, Error) {
  case token {
    token.Ident(x) -> Ok(x)
    _ -> Error(error.ExpectedToken(expected: token.Ident(""), got: token))
  }
}

pub fn assert_eq(
  token: Token,
  expected: Token,
  then: fn() -> Result(a, Error),
) -> Result(a, Error) {
  case token == expected {
    True -> then()
    False -> Error(error.ExpectedToken(expected, got: token))
  }
}
