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

pub type Function {
  Function(params: List(String), body: List(Statement))
}

pub type Statement {
  AutoDeclaration(name: String, size: Option(Int))
  Assing(lhs: Expr, rhs: Expr)
  Return(Option(Expr))
  Expr(Expr)
}

pub type Program {
  Program(
    globals: Dict(String, Global),
    functions: Dict(String, Function),
    extrns: Set(String),
  )
}

fn add_function(program: Program, name: String, function: Function) -> Program {
  Program(..program, functions: dict.insert(program.functions, name, function))
}

fn add_global(program: Program, name: String, init_val: Constant) -> Program {
  let global = Global(init_val, id: dict.size(program.globals))
  Program(..program, globals: dict.insert(program.globals, name, global))
}

fn add_extrn(program: Program, name: String) -> Program {
  Program(..program, extrns: set.insert(program.extrns, name))
}

pub fn parse(tokens: List(Token)) -> Result(Program, error.Error) {
  parse_items(
    tokens,
    Program(globals: dict.new(), functions: dict.new(), extrns: set.new()),
  )
}

fn parse_items(
  tokens: List(Token),
  program: Program,
) -> Result(Program, error.Error) {
  case tokens {
    [token.Ident(name), token.RoundOpen, ..] -> {
      use #(params, end) <- result.try(
        token_utils.split_matching_bracket(list.drop(tokens, 1)),
      )
      use params <- result.try(parse_list_of_ident(params, []))
      use #(body, next) <- result.try(parse_statment(end))
      parse_items(next, add_function(program, name, Function(params, body)))
    }

    [token.Ident(name), token.EndLine, ..next] -> {
      parse_items(next, add_global(program, name, IntConst(0)))
    }
    [token.Ident(name), token.StringLiteral(x), token.EndLine, ..next] -> {
      parse_items(next, add_global(program, name, StringConst(x)))
    }
    [token.Ident(name), token.IntLiteral(x), token.EndLine, ..next] -> {
      parse_items(next, add_global(program, name, IntConst(x)))
    }

    [token.KwExtern, ..end] -> {
      let #(names, next) =
        token_utils.split_at_outside_brackets(end, token.EndLine)
      use names <- result.try(parse_list_of_ident(names, []))
      parse_items(next, list.fold(names, program, add_extrn))
    }
    [] -> Ok(program)
    _ -> {
      echo tokens
      panic
    }
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
      Ok(#(list.map(declarations, fn(x) { AutoDeclaration(x.0, None) }), next))
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
          Ok(#([Expr(expr)], next))
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
  declarations: List(#(String, Option(Int))),
) -> Result(List(#(String, Option(Int))), Error) {
  case tokens {
    [token.Ident(name), token.Comma, ..next] -> {
      parse_auto_declarations(next, list.append(declarations, [#(name, None)]))
    }
    [token.Ident(name)] -> {
      Ok(list.append(declarations, [#(name, None)]))
    }
    [token.Ident(name), token.IntLiteral(size), token.Comma, ..next] -> {
      parse_auto_declarations(
        next,
        list.append(declarations, [#(name, Some(size))]),
      )
    }
    [token.Ident(name), token.IntLiteral(size)] -> {
      Ok(list.append(declarations, [#(name, Some(size))]))
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
