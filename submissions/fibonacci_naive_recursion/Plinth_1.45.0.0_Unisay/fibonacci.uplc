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
              [ [ (builtin lessThanEqualsInteger) n-2 ] (con integer 1) ]
              (delay n-2)
              (delay
                [
                  (lam
                    cse-3
                    [
                      [
                        (builtin addInteger)
                        [
                          cse-3
                          [ [ (builtin subtractInteger) n-2 ] (con integer 1) ]
                        ]
                      ]
                      [
                        cse-3
                        [ [ (builtin subtractInteger) n-2 ] (con integer 2) ]
                      ]
                    ]
                  )
                  [ s-1 s-1 ]
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