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
              factorial_FactorialBase__factorial-2
              (lam
                n-3
                (force
                  (case
                    (constr
                      0
                      [
                        [ (builtin lessThanEqualsInteger) n-3 ] (con integer 0)
                      ]
                      (delay (con integer 1))
                      (delay
                        [
                          [ (builtin multiplyInteger) n-3 ]
                          [
                            factorial_FactorialBase__factorial-2
                            [
                              [ (builtin subtractInteger) n-3 ] (con integer 1)
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
