(program
  1.1.0
  [
    (lam s-0 [ s-0 s-0 ])
    (lam
      s-1
      (lam
        n-2
        (case
          [ [ (builtin lessThanEqualsInteger) n-2 ] (con integer 0) ]
          [
            [ (builtin multiplyInteger) n-2 ]
            [
              [ s-1 s-1 ] [ [ (builtin subtractInteger) n-2 ] (con integer 1) ]
            ]
          ]
          (con integer 1)
        )
      )
    )
  ]
)