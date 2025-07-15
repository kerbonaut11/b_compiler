import error.{type Error}
import gleam/dict.{type Dict}
import gleam/list
import gleam/result
import gleam/set.{type Set}
import token.{type Token}
import token_utils

pub type Constant {
  StringConst(String)
  IntConst(Int)
}

pub type Global {
  Global(init_val: Constant, id: Int)
}

pub type Globals =
  Dict(String, Global)

pub type Extrns =
  Set(String)

pub type Function {
  Function(params: List(String), body: List(Token))
}

pub type Functions =
  dict.Dict(String, Function)

pub fn parse(
  tokens: List(Token),
) -> Result(#(Globals, Extrns, Functions), error.Error) {
  parse_item(tokens, dict.new(), set.new(), dict.new())
}

fn parse_item(
  tokens: List(Token),
  globals: Globals,
  extrns: Extrns,
  functions: Functions,
) -> Result(#(Globals, Extrns, Functions), error.Error) {
  case tokens {
    [token.Ident(name), token.RoundOpen, ..end] -> {
      use #(params, end) <- result.try(
        token_utils.split_matching_bracket(list.prepend(end, token.RoundOpen)),
      )
      use params <- result.try(parse_list_of_ident(params, []))
      use #(body, next) <- result.try(case end {
        [token.CurlyOpen, ..] -> token_utils.split_matching_bracket(end)
        _ -> {
          let #(body, next) =
            token_utils.split_at_outside_brackets(end, token.EndLine)
          Ok(#(list.append(body, [token.EndLine]), next))
        }
      })
      let functions = dict.insert(functions, name, Function(params, body))
      parse_item(next, globals, extrns, functions)
    }

    [token.Ident(name), token.EndLine, ..next] -> {
      let id = dict.size(globals)
      let globals = dict.insert(globals, name, Global(IntConst(0), id))
      parse_item(next, globals, extrns, functions)
    }
    [token.Ident(name), token.StringLiteral(x), token.EndLine, ..next] -> {
      let id = dict.size(globals)
      let globals = dict.insert(globals, name, Global(StringConst(x), id))
      parse_item(next, globals, extrns, functions)
    }
    [token.Ident(name), token.IntLiteral(x), token.EndLine, ..next] -> {
      let id = dict.size(globals)
      let globals = dict.insert(globals, name, Global(IntConst(x), id))
      parse_item(next, globals, extrns, functions)
    }

    [token.KwExtern, ..end] -> {
      let #(names, next) =
        token_utils.split_at_outside_brackets(end, token.EndLine)
      use names <- result.try(parse_list_of_ident(names, []))
      let extrns = set.union(extrns, set.from_list(names))
      parse_item(next, globals, extrns, functions)
    }
    [] -> Ok(#(globals, extrns, functions))
    _ -> panic
  }
}

fn parse_list_of_ident(
  tokens: List(Token),
  idents: List(String),
) -> Result(List(String), Error) {
  case tokens {
    [ident, comma, ..next] -> {
      use ident <- result.try(token_utils.unwrap_ident(ident))
      use <- token_utils.assert_eq(comma, token.Comma)
      parse_list_of_ident(next, list.append(idents, [ident]))
    }
    [ident] -> {
      use ident <- result.try(token_utils.unwrap_ident(ident))
      Ok(list.append(idents, [ident]))
    }
    [] -> Ok(idents)
  }
}
