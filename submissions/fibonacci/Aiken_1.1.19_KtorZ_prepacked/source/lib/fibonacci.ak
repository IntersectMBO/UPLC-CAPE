use aiken/builtin

const fib_ptr = #"404142434445464748494a4b4c4d8e90929496989a9c9ea0a2e4"

const fib_seq =
  #"000101020305080d1522375990e90179026203db063d0a18105f1a6d2ac2452f6ff1b520012511"

pub fn fibonacci(n: Int) -> Int {
  if n < 0 {
    n
  } else {
    let ptr = builtin.index_bytearray(fib_ptr, n)
    fib_seq
      |> builtin.slice_bytearray(ptr % 64, ptr / 64, _)
      |> builtin.bytearray_to_integer(True, _)
  }
}
