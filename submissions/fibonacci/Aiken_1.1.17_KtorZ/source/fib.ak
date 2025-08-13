pub fn fibonacci(n: Int) -> Int {
  if n <= 1 {
    n
  } else {
    fibonacci(n - 1) + fibonacci(n - 2)
  }
}
