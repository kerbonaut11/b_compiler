import gleam/list
import gleeunit
import lexer
import token
import token_utils
import tokenizer

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn tokenizer_test() {
  let src =
    "
  main() {
    extrn printf;
    printf(\"Hello World\");
    auto x,y;
    x = 11 + 0xb - 0xB * 0b1011 / 1 % 100;
  }
"
  let assert Ok(tokens) = tokenizer.parse(src)
  echo tokens
  let assert Ok(#(body, _)) =
    token_utils.split_matching_bracket(list.drop(tokens, 3))
  echo body
  let #(left, _) =
    token_utils.split_at_outside_brackets(list.drop(tokens, 4), token.EndLine)
  echo left
}

pub fn lexer_test() {
  let src =
    "
extrn printf,malloc;

X; Y 10; HELLO \"hello\";

add(x,y) return x+y;
main() {
  printf(HELLO);
}
"
  let assert Ok(tokens) = tokenizer.parse(src)
  echo tokens
  let assert Ok(#(globals, extrns, functions)) = lexer.parse(tokens)
  echo globals
  echo extrns
  echo functions
}
