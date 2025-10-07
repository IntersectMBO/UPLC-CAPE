(
  program
  1.1.0
  [
    (lam __z_combinator__
      [
        (lam fibonacci_FibonacciOpen__fibonacci
          (lam n
            [fibonacci_FibonacciOpen__fibonacci n])) (lam n
          (force [
            (force (builtin ifThenElse)) [
              (builtin lessThanEqualsInteger) n (con integer 1)
            ] (delay n) (delay (force [
              (force (builtin ifThenElse)) [
                (builtin equalsInteger) n (con integer 2)
              ] (delay (con integer 1)) (delay [
                (lam fibIter
                  [fibIter (con integer 1) (con integer 1) n]) [
                  __z_combinator__ (lam fibIter
                    (lam a
                      (lam b
                        (lam count
                          (force [
                            (force (builtin ifThenElse)) [
                              (builtin lessThanEqualsInteger) count
                              (con integer 2)
                            ] (delay b) (delay [
                              fibIter b [(builtin addInteger) a b] [
                                (builtin subtractInteger) count (con integer 1)
                              ]
                            ])
                          ])))))
                ]
              ])
            ]))
          ]))
      ]) (lam ff
      [
        (lam xx
          [ff (lam vv   [xx xx vv])]) (lam xx
          [ff (lam vv   [xx xx vv])])
      ])
  ]
)