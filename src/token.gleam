pub type Token {
  EndLine
  Comma
  Assign
  Colon

  RoundOpen
  RoundClose
  CurlyOpen
  CurlyClose
  SquareOpen
  SquareClose

  OpAdd
  OpSub
  OpMul
  OpDiv
  OpMod

  OpAnd
  OpOr
  OpXor
  OpShl
  OpShr

  OpEq
  OpNe
  OpLt
  OpLe
  OpGt
  OpGe

  OpRef
  OpDeref
  OpNeg
  OpNot

  IntLiteral(Int)
  StringLiteral(String)

  Ident(String)

  KwExtern
  KwAuto
  KwWhile
  KwIf
  KwElse
  KwReturn
}
