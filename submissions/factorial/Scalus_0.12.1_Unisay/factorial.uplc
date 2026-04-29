(program
  1.1.0
  [
    (lam r-0 [ r-0 r-0 ])
    (lam
      r-0
      (lam
        x-1
        (force
          (case
            (constr
              0
              [ [ (builtin lessThanEqualsInteger) x-1 ] (con integer 0) ]
              (delay (con integer 1))
              (delay
                [
                  [ (builtin multiplyInteger) x-1 ]
                  [
                    [ r-0 r-0 ]
                    [ [ (builtin subtractInteger) x-1 ] (con integer 1) ]
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