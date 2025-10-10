(
  program
  1.1.0
  [
    (lam __builtin_IfThenElse
      [
        (lam __z_combinator__
          [
            __z_combinator__ (lam factorial_FactorialBase__factorial
              (lam n
                (force (case (constr 0 [
                  (builtin lessThanEqualsInteger) n (con integer 0)
                ] (delay (con integer 1)) (delay [
                  (builtin multiplyInteger) n [
                    factorial_FactorialBase__factorial [
                      (builtin subtractInteger) n (con integer 1)
                    ]
                  ]
                ]))
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