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
                  [ [ (builtin lessThanEqualsInteger) n-3 ] (con integer 1) ]
                  (delay n-3)
                  (delay
                    [
                      (lam
                        cse-4
                        [
                          [
                            (builtin addInteger)
                            [
                              cse-4
                              [
                                [ (builtin subtractInteger) n-3 ]
                                (con integer 1)
                              ]
                            ]
                          ]
                          [
                            cse-4
                            [
                              [ (builtin subtractInteger) n-3 ] (con integer 2)
                            ]
                          ]
                        ]
                      )
                      [ s-2 s-2 ]
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
