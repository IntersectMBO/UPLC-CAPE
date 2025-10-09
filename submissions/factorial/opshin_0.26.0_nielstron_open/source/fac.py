def factorial(n: int) -> int:
    a = 1
    while n > 0:
        a *= n
        n -= 1
    return a
