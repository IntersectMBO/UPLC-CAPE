pub fn fibonacci(n: Int) -> Int {
  if n <= 1 {
    n
  } else {
    do_fibonacci(1, 1, n - 2)
  }
}

fn do_fibonacci(last: Int, current: Int, n: Int) -> Int {
  if n == 0 {
    current
  } else {
    do_fibonacci(current, current + last, n - 1)
  }
}
