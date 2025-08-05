import error.{type Error}
import gleam/bool

//import file_streams/file_stream as fs
import gleam/list
import gleam/result
import gleam/string
import token.{type Token}

pub fn parse(src: String) -> Result(List(Token), Error) {
  parse_token(string.append(src, "\n"), []) |> result.map(map_unary_ops)
}

fn parse_token(src: String, tokens: List(Token)) -> Result(List(Token), Error) {
  case src {
    "==" <> next -> parse_token(next, list.append(tokens, [token.OpEq]))
    "!=" <> next -> parse_token(next, list.append(tokens, [token.OpNe]))
    "<=" <> next -> parse_token(next, list.append(tokens, [token.OpLe]))
    ">=" <> next -> parse_token(next, list.append(tokens, [token.OpGe]))
    "<<" <> next -> parse_token(next, list.append(tokens, [token.OpShl]))
    ">>" <> next -> parse_token(next, list.append(tokens, [token.OpShr]))

    ";" <> next -> parse_token(next, list.append(tokens, [token.EndLine]))
    ":" <> next -> parse_token(next, list.append(tokens, [token.Colon]))
    "," <> next -> parse_token(next, list.append(tokens, [token.Comma]))
    "=" <> next -> parse_token(next, list.append(tokens, [token.Assign]))

    "(" <> next -> parse_token(next, list.append(tokens, [token.RoundOpen]))
    ")" <> next -> parse_token(next, list.append(tokens, [token.RoundClose]))
    "{" <> next -> parse_token(next, list.append(tokens, [token.CurlyOpen]))
    "}" <> next -> parse_token(next, list.append(tokens, [token.CurlyClose]))
    "[" <> next -> parse_token(next, list.append(tokens, [token.SquareOpen]))
    "]" <> next -> parse_token(next, list.append(tokens, [token.SquareClose]))

    "+" <> next -> parse_token(next, list.append(tokens, [token.OpAdd]))
    "-" <> next -> parse_token(next, list.append(tokens, [token.OpSub]))
    "*" <> next -> parse_token(next, list.append(tokens, [token.OpMul]))
    "/" <> next -> parse_token(next, list.append(tokens, [token.OpDiv]))
    "%" <> next -> parse_token(next, list.append(tokens, [token.OpMod]))

    "&" <> next -> parse_token(next, list.append(tokens, [token.OpAnd]))
    "|" <> next -> parse_token(next, list.append(tokens, [token.OpOr]))
    "^" <> next -> parse_token(next, list.append(tokens, [token.OpXor]))

    "<" <> next -> parse_token(next, list.append(tokens, [token.OpLt]))
    ">" <> next -> parse_token(next, list.append(tokens, [token.OpGt]))

    "!" <> next -> parse_token(next, list.append(tokens, [token.OpNot]))

    "\"" <> next -> parse_string(next, tokens, "")

    "0x" <> next -> parse_number(next, tokens, 16, 0)
    "0b" <> next -> parse_number(next, tokens, 2, 0)

    "0" <> _
    | "1" <> _
    | "2" <> _
    | "3" <> _
    | "4" <> _
    | "5" <> _
    | "6" <> _
    | "7" <> _
    | "8" <> _
    | "9" <> _ -> parse_number(src, tokens, 10, 0)

    " " <> next | "\n" <> next | "\t" <> next -> parse_token(next, tokens)

    "" -> Ok(tokens)

    _ -> parse_ident(src, tokens, "")
  }
}

const ascii_a_uppercase = 0x41

const ascii_z_uppercase = 0x5a

const ascii_a = 0x61

const ascii_z = 0x7a

const ascii_0 = 0x30

const ascii_9 = 0x39

fn char_encoding(ch: String) -> Int {
  ch
  |> string.to_utf_codepoints
  |> list.first
  |> result.map(string.utf_codepoint_to_int)
  |> result.unwrap(0)
}

fn in_range(x: Int, min: Int, max: Int) {
  x >= min && x <= max
}

fn parse_ident(
  src: String,
  tokens: List(Token),
  str: String,
) -> Result(List(Token), Error) {
  let assert Ok(ch) = string.first(src)
  let ch_encoding = char_encoding(ch)

  let is_alphabetic =
    in_range(ch_encoding, ascii_a, ascii_z)
    || in_range(ch_encoding, ascii_a_uppercase, ascii_z_uppercase)
    || ch_encoding > 0x7f
    || ch == "_"

  case is_alphabetic {
    True -> {
      let str = string.append(str, ch)
      parse_ident(string.drop_start(src, 1), tokens, str)
    }

    False -> {
      let token = case str {
        "auto" -> token.KwAuto
        "extrn" -> token.KwExtern
        "if" -> token.KwIf
        "else" -> token.KwElse
        "return" -> token.KwReturn
        _ -> token.Ident(str)
      }
      parse_token(src, list.append(tokens, [token]))
    }
  }
}

fn parse_string(
  src: String,
  tokens: List(Token),
  str: String,
) -> Result(List(Token), Error) {
  let assert Ok(ch) = string.first(src)
  let next = string.drop_start(src, 1)

  case ch {
    "\"" -> parse_token(next, list.append(tokens, [token.StringLiteral(str)]))
    _ -> parse_string(next, tokens, string.append(str, ch))
  }
}

fn parse_number(
  src: String,
  tokens: List(Token),
  base: Int,
  number: Int,
) -> Result(List(Token), Error) {
  let assert Ok(ch) = string.first(src)
  let ch_encoding = char_encoding(ch)
  let is_lowercase = in_range(ch_encoding, ascii_a, ascii_z)
  let is_uppercase = in_range(ch_encoding, ascii_a_uppercase, ascii_z_uppercase)
  let is_number = in_range(ch_encoding, ascii_0, ascii_9)

  use <- bool.lazy_guard(!{ is_lowercase || is_uppercase || is_number }, fn() {
    parse_token(src, list.append(tokens, [token.IntLiteral(number)]))
  })

  let digit = {
    use <- bool.guard(is_number, ch_encoding - ascii_0)
    use <- bool.guard(is_lowercase, ch_encoding - ascii_a + 10)
    ch_encoding - ascii_a_uppercase + 10
  }

  assert digit < base
  let number = number * base + digit
  parse_number(string.drop_start(src, 1), tokens, base, number)
}

fn map_unary_ops(tokens: List(Token)) -> List(Token) {
  list.map_fold(tokens, token.RoundClose, fn(prev_token, token) {
    let prev_is_end_of_expr = case prev_token {
      token.IntLiteral(_)
      | token.StringLiteral(_)
      | token.Ident(_)
      | token.RoundClose
      | token.SquareClose -> True
      _ -> False
    }
    let token = case token {
      token.OpAnd if !prev_is_end_of_expr -> token.OpRef
      token.OpMul if !prev_is_end_of_expr -> token.OpDeref
      token.OpSub if !prev_is_end_of_expr -> token.OpNeg
      _ -> token
    }
    #(token, token)
  }).1
}
