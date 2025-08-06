(module
(import "std" "malloc" (func (param i32) (result i32)))
(import "std" "print" (func (param i32) (result i32)))
(func $main (result i32)
(local $a i32)
(local $b i32)
)
(func $test (param $x i32) (param $y i32) (result i32)
)
)
(data (i32.const 0) "
\00\00\00\00
\00\00\00\00
\00\00\00\00
\a0\00\00\00
")
