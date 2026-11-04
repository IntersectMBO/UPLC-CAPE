(program
  1.1.0
  (lam
    n-0
    (case
      [ [ (builtin lessThanEqualsInteger) n-0 ] (con integer -1) ]
      (case
        [ [ (builtin lessThanEqualsInteger) n-0 ] (con integer 25) ]
        (case
          (constr
            0
            (lam
              r-1
              (lam
                k-2
                (lam
                  a-3
                  (lam
                    b-4
                    (case
                      [
                        [ (builtin lessThanEqualsInteger) k-2 ] (con integer 0)
                      ]
                      (case
                        (constr
                          0
                          r-1
                          [ [ (builtin subtractInteger) k-2 ] (con integer 1) ]
                          b-4
                          [ [ (builtin addInteger) a-3 ] b-4 ]
                        )
                        r-1
                      )
                      b-4
                    )
                  )
                )
              )
            )
            [ [ (builtin subtractInteger) n-0 ] (con integer 25) ]
            (con integer 46368)
            (con integer 75025)
          )
          (lam r-1 [ r-1 r-1 ])
        )
        (case
          n-0
          (con integer 0)
          (con integer 1)
          (con integer 1)
          (con integer 2)
          (con integer 3)
          (con integer 5)
          (con integer 8)
          (con integer 13)
          (con integer 21)
          (con integer 34)
          (con integer 55)
          (con integer 89)
          (con integer 144)
          (con integer 233)
          (con integer 377)
          (con integer 610)
          (con integer 987)
          (con integer 1597)
          (con integer 2584)
          (con integer 4181)
          (con integer 6765)
          (con integer 10946)
          (con integer 17711)
          (con integer 28657)
          (con integer 46368)
          (con integer 75025)
        )
      )
      n-0
    )
  )
)
