(program
  1.1.0
  [
    (lam s-0 [ s-0 s-0 ])
    (lam
      s-1
      (lam
        n-2
        (force
          (force
            [
              [
                [
                  (force (builtin ifThenElse))
                  [ [ (builtin lessThanEqualsInteger) n-2 ] (con integer 0) ]
                ]
                (delay (delay (con integer 1)))
              ]
              (delay
                (delay
                  [
                    [ (builtin multiplyInteger) n-2 ]
                    [
                      (lam x-3 [ [ s-1 s-1 ] x-3 ])
                      [ [ (builtin subtractInteger) n-2 ] (con integer 1) ]
                    ]
                  ]
                )
              )
            ]
          )
        )
      )
    )
  ]
)