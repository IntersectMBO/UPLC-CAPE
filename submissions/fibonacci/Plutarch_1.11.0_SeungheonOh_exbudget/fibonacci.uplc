(program
  1.1.0
  [
    (lam
      i-0
      (lam
        i-1
        (force
          (case
            (constr
              0
              [ [ (builtin lessThanEqualsInteger) i-1 ] (con integer 1) ]
              (delay i-1)
              (delay
                [
                  [
                    (builtin addInteger)
                    [
                      [ i-0 i-0 ]
                      [ [ (builtin subtractInteger) i-1 ] (con integer 1) ]
                    ]
                  ]
                  [
                    [ i-0 i-0 ]
                    [ [ (builtin subtractInteger) i-1 ] (con integer 2) ]
                  ]
                ]
              )
            )
            (force (builtin ifThenElse))
          )
        )
      )
    )
    (lam
      i-2
      (lam
        i-3
        (force
          (case
            (constr
              0
              [ [ (builtin lessThanEqualsInteger) i-3 ] (con integer 1) ]
              (delay i-3)
              (delay
                [
                  [
                    (builtin addInteger)
                    [
                      [ i-2 i-2 ]
                      [ [ (builtin subtractInteger) i-3 ] (con integer 1) ]
                    ]
                  ]
                  [
                    [ i-2 i-2 ]
                    [ [ (builtin subtractInteger) i-3 ] (con integer 2) ]
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
