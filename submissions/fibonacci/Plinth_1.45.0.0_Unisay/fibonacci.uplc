(program
  1.1.0
  [
    (lam
      fibonacciLoop-0
      (lam
        n-1
        (force
          (force
            [
              [
                [
                  (force (builtin ifThenElse))
                  [ [ (builtin lessThanEqualsInteger) n-1 ] (con integer 1) ]
                ]
                (delay (delay n-1))
              ]
              (delay
                (delay
                  [
                    [
                      [ [ fibonacciLoop-0 (con integer 0) ] (con integer 1) ]
                      (con integer 2)
                    ]
                    n-1
                  ]
                )
              )
            ]
          )
        )
      )
    )
    [
      (lam s-2 [ s-2 s-2 ])
      (lam
        s-3
        (lam
          prev-4
          (lam
            curr-5
            (lam
              i-6
              (lam
                n-7
                (force
                  (force
                    [
                      [
                        [
                          (force (builtin ifThenElse))
                          [ [ (builtin lessThanEqualsInteger) i-6 ] n-7 ]
                        ]
                        (delay
                          (delay
                            [
                              [
                                [
                                  [ [ s-3 s-3 ] curr-5 ]
                                  [ [ (builtin addInteger) prev-4 ] curr-5 ]
                                ]
                                [ [ (builtin addInteger) (con integer 1) ] i-6 ]
                              ]
                              n-7
                            ]
                          )
                        )
                      ]
                      (delay (delay curr-5))
                    ]
                  )
                )
              )
            )
          )
        )
      )
    ]
  ]
)
