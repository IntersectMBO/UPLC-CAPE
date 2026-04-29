(program
  1.1.0
  [
    (lam
      __builtin_IfThenElse-0
      [
        (lam
          __z_combinator__-1
          [
            __z_combinator__-1
            (lam
              fibonacci_FibonacciBase__fibonacci-2
              (lam
                n-3
                (force
                  (case
                    (constr
                      0
                      [
                        [ (builtin lessThanEqualsInteger) n-3 ] (con integer 1)
                      ]
                      (delay n-3)
                      (delay
                        (force
                          (case
                            (constr
                              0
                              [
                                [ (builtin equalsInteger) n-3 ] (con integer 2)
                              ]
                              (delay (con integer 1))
                              (delay
                                [
                                  [
                                    (builtin addInteger)
                                    [
                                      fibonacci_FibonacciBase__fibonacci-2
                                      [
                                        [ (builtin subtractInteger) n-3 ]
                                        (con integer 1)
                                      ]
                                    ]
                                  ]
                                  [
                                    fibonacci_FibonacciBase__fibonacci-2
                                    [
                                      [ (builtin subtractInteger) n-3 ]
                                      (con integer 2)
                                    ]
                                  ]
                                ]
                              )
                            )
                            __builtin_IfThenElse-0
                          )
                        )
                      )
                    )
                    __builtin_IfThenElse-0
                  )
                )
              )
            )
          ]
        )
        (lam
          ff-4
          [
            (lam xx-5 [ ff-4 (lam vv-6 [ [ xx-5 xx-5 ] vv-6 ]) ])
            (lam xx-5 [ ff-4 (lam vv-6 [ [ xx-5 xx-5 ] vv-6 ]) ])
          ]
        )
      ]
    )
    (force (builtin ifThenElse))
  ]
)
