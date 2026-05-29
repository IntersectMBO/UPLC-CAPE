(program
  1.1.0
  [
    (lam
      forced-0
      [
        (lam
          fibonacciLoop-1
          (lam
            n-2
            (force
              (case
                (constr
                  0
                  [ [ (builtin lessThanEqualsInteger) n-2 ] (con integer 1) ]
                  (delay n-2)
                  (delay
                    (case
                      (constr
                        0 (con integer 0) (con integer 1) (con integer 2) n-2
                      )
                      fibonacciLoop-1
                    )
                  )
                )
                forced-0
              )
            )
          )
        )
        [
          (lam s-3 [ s-3 s-3 ])
          (lam
            s-4
            (lam
              prev-5
              (lam
                curr-6
                (lam
                  i-7
                  (lam
                    n-8
                    (force
                      (case
                        (constr
                          0
                          [ [ (builtin lessThanInteger) n-8 ] i-7 ]
                          (delay curr-6)
                          (delay
                            (case
                              (constr
                                0
                                s-4
                                curr-6
                                [ [ (builtin addInteger) prev-5 ] curr-6 ]
                                [ [ (builtin addInteger) (con integer 1) ] i-7 ]
                                n-8
                              )
                              s-4
                            )
                          )
                        )
                        forced-0
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
    (force (builtin ifThenElse))
  ]
)
