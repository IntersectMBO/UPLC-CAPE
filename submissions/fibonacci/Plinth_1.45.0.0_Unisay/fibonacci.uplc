(program
  1.1.0
  [
    (lam
      fibonacciLoop-0
      (lam
        n-1
        (force
          (case
            (constr
              0
              [ [ (builtin lessThanEqualsInteger) n-1 ] (con integer 1) ]
              (delay n-1)
              (delay
                (case
                  (constr 0 (con integer 0) (con integer 1) (con integer 2) n-1)
                  fibonacciLoop-0
                )
              )
            )
            (force (builtin ifThenElse))
          )
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
                (force
                  (case
                    (constr
                      0
                      (case
                        (constr
                          0
                          [ [ (builtin lessThanEqualsInteger) i-6 ] n-7 ]
                          (con bool False)
                          (con bool True)
                        )
                        (force (builtin ifThenElse))
                      )
                      (delay curr-5)
                      (delay
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
                    (force (builtin ifThenElse))
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