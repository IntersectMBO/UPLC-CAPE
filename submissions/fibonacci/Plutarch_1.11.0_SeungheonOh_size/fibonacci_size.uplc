(program
  1.1.0
  [
    (lam i-0 [ i-0 i-0 ])
    (lam
      i-1
      (lam
        i-2
        (force
          (case
            (constr
              0
              [ [ (builtin lessThanEqualsInteger) i-2 ] (con integer 1) ]
              (delay i-2)
              (delay
                [
                  [
                    (builtin addInteger)
                    [
                      [ i-1 i-1 ]
                      [ [ (builtin subtractInteger) i-2 ] (con integer 1) ]
                    ]
                  ]
                  [
                    [ i-1 i-1 ]
                    [ [ (builtin subtractInteger) i-2 ] (con integer 2) ]
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