import file_streams/file_stream
import file_streams/text_encoding
import gleam/list
import gleeunit
import ir_compiler
import lexer
import pprint
import token
import token_utils
import tokenizer
import x86

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

x; y 10; HELLO \"hello\"; LANGUAGE \"b\";

test(x,y) return x+y;
main() {
  auto a, b;
  a = 10;
  a = test(a,1);
  a = &a;
  a = a[1];
  a = &a[1];
  b = \"World\";
  printf(HELLO);
}
"
  let assert Ok(tokens) = tokenizer.parse(src)
  echo tokens
  let assert Ok(program) = lexer.parse(tokens)
  pprint.debug(program)
  let assert Ok(program) = ir_compiler.compile(program)
  pprint.debug(program)

  let assert Ok(file) =
    file_stream.open_write_text("test/build/test.asm", text_encoding.Unicode)
  x86.compile(file, program)
}
