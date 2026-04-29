(program
  1.1.0
  [
    (lam s-0 [ s-0 s-0 ])
    (lam
      s-1
      (lam
        a-2
        (lam
          b-3
          (force
            (force
              [
                [
                  [
                    (force (builtin ifThenElse))
                    [ [ (builtin equalsInteger) (con integer 0) ] b-3 ]
                  ]
                  (delay
                    (delay
                      (force
                        (force
                          [
                            [
                              [
                                (force (builtin ifThenElse))
                                [
                                  [ (builtin lessThanInteger) a-2 ]
                                  (con integer 0)
                                ]
                              ]
                              (delay
                                (delay
                                  [
                                    [
                                      (builtin subtractInteger) (con integer 0)
                                    ]
                                    a-2
                                  ]
                                )
                              )
                            ]
                            (delay (delay a-2))
                          ]
                        )
                      )
                    )
                  )
                ]
                (delay
                  (delay
                    [ [ [ s-1 s-1 ] b-3 ] [ [ (builtin modInteger) a-2 ] b-3 ] ]
                  )
                )
              ]
            )
          )
        )
      )
    )
  ]
)
