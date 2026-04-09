(program
  1.1.0
  [
    (lam s-0 [ s-0 s-0 ])
    (lam
      s-1
      (lam
        n-2
        (force
          (case
            (constr
              0
              [ [ (builtin lessThanEqualsInteger) n-2 ] (con integer 0) ]
              (delay (con integer 1))
              (delay
                [
                  [ (builtin multiplyInteger) n-2 ]
                  [
                    [ s-1 s-1 ]
                    [ [ (builtin subtractInteger) n-2 ] (con integer 1) ]
                  ]
                ]
              )
            )
            (force (builtin ifThenElse))
          )
        )
      )
    )
  ]
)