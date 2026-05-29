(program
  1.1.0
  [
    (lam
      forced-0
      [
        (lam s-1 [ s-1 s-1 ])
        (lam
          s-2
          (lam
            n-3
            (force
              (case
                (constr
                  0
                  [ [ (builtin lessThanEqualsInteger) n-3 ] (con integer 0) ]
                  (delay (con integer 1))
                  (delay
                    [
                      [ (builtin multiplyInteger) n-3 ]
                      [
                        [ s-2 s-2 ]
                        [ [ (builtin subtractInteger) n-3 ] (con integer 1) ]
                      ]
                    ]
                  )
                )
                forced-0
              )
            )
          )
        )
      ]
    )
    (force (builtin ifThenElse))
  ]
)
