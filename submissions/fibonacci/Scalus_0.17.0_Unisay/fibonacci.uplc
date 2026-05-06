(program
  1.1.0
  [
    (lam a-0 [ a-0 a-0 ])
    (lam
      a-0
      (lam
        b-1
        (force
          (case
            (constr
              0
              [ [ (builtin lessThanEqualsInteger) b-1 ] (con integer 1) ]
              (delay b-1)
              (delay
                [
                  [
                    (builtin addInteger)
                    [
                      [ a-0 a-0 ]
                      [ [ (builtin subtractInteger) b-1 ] (con integer 1) ]
                    ]
                  ]
                  [
                    [ a-0 a-0 ]
                    [ [ (builtin subtractInteger) b-1 ] (con integer 2) ]
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
