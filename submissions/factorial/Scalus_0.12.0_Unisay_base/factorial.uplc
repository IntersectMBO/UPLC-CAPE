(
  program
  1.1.0
  [
    (lam __z_combinator__
      [
        (lam factorial_FactorialBase__factorial
          (lam n
            [factorial_FactorialBase__factorial n])) [
          __z_combinator__ (lam factorial_FactorialBase__factorial
            (lam n
              (force [
                (force (builtin ifThenElse)) [
                  (builtin lessThanEqualsInteger) n (con integer 0)
                ] (delay (con integer 1)) (delay [
                  (builtin multiplyInteger) n [
                    factorial_FactorialBase__factorial [
                      (builtin subtractInteger) n (con integer 1)
                    ]
                  ]
                ])
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