(
  program
  1.1.0
  [
    (lam __z_combinator__
      [
        (lam fibonacci_FibonacciBase__fibonacci
          (lam n
            [fibonacci_FibonacciBase__fibonacci n])) [
          __z_combinator__ (lam fibonacci_FibonacciBase__fibonacci
            (lam n
              (force [
                (force (builtin ifThenElse)) [
                  (builtin lessThanEqualsInteger) n (con integer 1)
                ] (delay n) (delay (force [
                  (force (builtin ifThenElse)) [
                    (builtin equalsInteger) n (con integer 2)
                  ] (delay (con integer 1)) (delay [
                    (builtin addInteger) [
                      fibonacci_FibonacciBase__fibonacci [
                        (builtin subtractInteger) n (con integer 1)
                      ]
                    ] [
                      fibonacci_FibonacciBase__fibonacci [
                        (builtin subtractInteger) n (con integer 2)
                      ]
                    ]
                  ])
                ]))
              ])))
        ]
      ]) (lam ff
      [
        (lam xx
          [ff (lam vv   [xx xx vv])]) (lam xx
          [ff (lam vv   [xx xx vv])])
      ])
  ]
)