(program
  1.1.0
  [
    (lam a-0 [ a-0 a-0 ])
    (lam
      a-0
      (lam
        b-1
        (case
          [ [ (builtin lessThanEqualsInteger) b-1 ] (con integer 0) ]
          [
            [ (builtin multiplyInteger) b-1 ]
            [
              [ a-0 a-0 ] [ [ (builtin subtractInteger) b-1 ] (con integer 1) ]
            ]
          ]
          (con integer 1)
        )
      )
    )
  ]
)
