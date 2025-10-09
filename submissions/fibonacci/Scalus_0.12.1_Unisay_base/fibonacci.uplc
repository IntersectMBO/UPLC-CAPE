(
  program
  1.1.0
  [
    (lam __builtin_IfThenElse
      [
        (lam __z_combinator__
          [
            __z_combinator__ (lam fibonacci_FibonacciBase__fibonacci
              (lam n
                (force (case (constr 0 [
                  (builtin lessThanEqualsInteger) n (con integer 1)
                ] (delay n) (delay (force (case (constr 0 [
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
                ]))
                  __builtin_IfThenElse))))
                  __builtin_IfThenElse))))
          ]) (lam ff
          [
            (lam xx
              [ff (lam vv   [xx xx vv])]) (lam xx
              [ff (lam vv   [xx xx vv])])
          ])
      ]) (force (builtin ifThenElse))
  ]
)