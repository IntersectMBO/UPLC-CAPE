(program
  1.1.0
  [
    (lam
      fibonacciLoop-0
      (lam
        n-1
        (case
          [ [ (builtin lessThanEqualsInteger) n-1 ] (con integer 1) ]
          (case
            (constr 0 (con integer 0) (con integer 1) (con integer 2) n-1)
            fibonacciLoop-0
          )
          n-1
        )
      )
    )
    [
      (lam s-2 [ s-2 s-2 ])
      (lam
        s-3
        (lam
          prev-4
          (lam
            curr-5
            (lam
              i-6
              (lam
                n-7
                (case
                  [ [ (builtin lessThanEqualsInteger) i-6 ] n-7 ]
                  curr-5
                  (case
                    (constr
                      0
                      s-3
                      curr-5
                      [ [ (builtin addInteger) prev-4 ] curr-5 ]
                      [ [ (builtin addInteger) (con integer 1) ] i-6 ]
                      n-7
                    )
                    s-3
                  )
                )
              )
            )
          )
        )
      )
    ]
  ]
)