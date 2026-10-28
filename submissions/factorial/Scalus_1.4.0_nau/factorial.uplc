(program
  1.1.0
  [
    (lam r-0 [ r-0 r-0 ])
    (lam
      r-0
      (lam
        x-1
        (case
          [ [ (builtin lessThanEqualsInteger) x-1 ] (con integer -1) ]
          (case
            [ [ (builtin lessThanEqualsInteger) x-1 ] (con integer 12) ]
            [
              [ (builtin multiplyInteger) x-1 ]
              [
                [ r-0 r-0 ]
                [ [ (builtin subtractInteger) x-1 ] (con integer 1) ]
              ]
            ]
            (case
              x-1
              (con integer 1)
              (con integer 1)
              (con integer 2)
              (con integer 6)
              (con integer 24)
              (con integer 120)
              (con integer 720)
              (con integer 5040)
              (con integer 40320)
              (con integer 362880)
              (con integer 3628800)
              (con integer 39916800)
              (con integer 479001600)
            )
          )
          (con integer 1)
        )
      )
    )
  ]
)
