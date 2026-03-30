(program
  1.1.0
  [
    (lam s-0 [ s-0 s-0 ])
    (lam
      s-1
      [
        (lam
          fibonacci-2
          (lam
            n-3
            (force
              (force
                [
                  [
                    [
                      (force (builtin ifThenElse))
                      [
                        [ (builtin lessThanEqualsInteger) n-3 ] (con integer 1)
                      ]
                    ]
                    (delay (delay n-3))
                  ]
                  (delay
                    (delay
                      [
                        [
                          (builtin addInteger)
                          [
                            fibonacci-2
                            [
                              [ (builtin subtractInteger) n-3 ] (con integer 1)
                            ]
                          ]
                        ]
                        [
                          fibonacci-2
                          [ [ (builtin subtractInteger) n-3 ] (con integer 2) ]
                        ]
                      ]
                    )
                  )
                ]
              )
            )
          )
        )
        (lam x-4 [ [ s-1 s-1 ] x-4 ])
      ]
    )
  ]
)