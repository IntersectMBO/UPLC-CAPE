# Factorial (Scalus 0.12.1)

**Scenario:** factorial

## Implementation

Simple recursive factorial via an explicit `pfix` (Y-combinator), matching the surface Scalus in [`FactorialOpen.scala`](https://github.com/Unisay/scalus-cape-submissions/blob/51067e87539321aa77aea6ddd764cef94352202e/src/factorial/FactorialOpen.scala):

```scala
val factorial = pfix: r =>
  λλ("x"): x =>
    if x <= 0 then 1 else x * r(x-1)
```

## Characteristics

- **Algorithm:** Naive recursion via explicit Y-combinator (no accumulator, not tail-recursive — the multiplication wraps the recursive call)
- **Edge case:** factorial(n) = 1 for n ≤ 0

## Source Code

- See [source/README.md](source/README.md) for source code and reproducibility instructions
