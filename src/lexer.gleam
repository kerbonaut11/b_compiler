import error.{type Error}
import expr.{type Expr}
import gleam/bool
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
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
  Function(params: List(String), body: List(Statement))
}

pub type Functions =
  Dict(String, Function)

pub type Statement {
  AutoDeclaration(name: String, size: Int)
  Assing(lhs: Expr, rhs: Expr)
  Return(Option(Expr))
  Call(Expr)
}

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
    [token.Ident(name), token.RoundOpen, ..] -> {
      use #(params, end) <- result.try(
        token_utils.split_matching_bracket(list.drop(tokens, 1)),
      )
      use params <- result.try(parse_list_of_ident(params, []))
      use #(body, next) <- result.try(parse_statment(end))
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

fn parse_statment(
  tokens: List(Token),
) -> Result(#(List(Statement), List(Token)), Error) {
  echo tokens
  case tokens {
    [token.CurlyOpen, ..] -> {
      use #(body, next) <- result.try(token_utils.split_matching_bracket(tokens))
      use statements <- result.try(parse_compound_statment(body, []))
      Ok(#(statements, next))
    }

    [token.KwAuto, ..end] -> {
      let #(declarations, next) =
        token_utils.split_at_outside_brackets(end, token.EndLine)
      use declarations <- result.try(parse_auto_declarations(declarations, []))
      Ok(#(list.map(declarations, fn(x) { AutoDeclaration(x.0, x.1) }), next))
    }

    [token.KwReturn, token.EndLine, ..next] -> {
      Ok(#([Return(None)], next))
    }

    [token.KwReturn, ..end] -> {
      let #(tokens, next) =
        token_utils.split_at_outside_brackets(end, token.EndLine)
      use expr <- result.try(expr.parse(tokens))
      Ok(#([Return(Some(expr))], next))
    }

    [_, ..] -> {
      let #(tokens, next) =
        token_utils.split_at_outside_brackets(tokens, token.EndLine)
      let #(lhs, rhs) =
        token_utils.split_at_outside_brackets(tokens, token.Assign)

      case rhs == [] {
        True -> {
          use expr <- result.try(expr.parse(tokens))
          Ok(#([Call(expr)], next))
        }

        False -> {
          use lhs <- result.try(expr.parse(lhs))
          use rhs <- result.try(expr.parse(rhs))
          Ok(#([Assing(lhs, rhs)], next))
        }
      }
    }

    [] -> Ok(#([], []))
  }
}

fn parse_auto_declarations(
  tokens: List(Token),
  declarations: List(#(String, Int)),
) -> Result(List(#(String, Int)), Error) {
  case tokens {
    [token.Ident(name), token.Comma, ..next] -> {
      parse_auto_declarations(next, list.append(declarations, [#(name, 1)]))
    }
    [token.Ident(name)] -> {
      Ok(list.append(declarations, [#(name, 1)]))
    }
    [token.Ident(name), token.IntLiteral(size), token.Comma, ..next] -> {
      parse_auto_declarations(next, list.append(declarations, [#(name, size)]))
    }
    [token.Ident(name), token.IntLiteral(size)] -> {
      Ok(list.append(declarations, [#(name, size)]))
    }
    [] -> Ok(declarations)
    _ -> panic
  }
}

fn parse_compound_statment(
  tokens: List(Token),
  statements: List(Statement),
) -> Result(List(Statement), Error) {
  use <- bool.guard(tokens == [], Ok(statements))
  use #(statement, next) <- result.try(parse_statment(tokens))
  parse_compound_statment(next, list.append(statements, statement))
}
