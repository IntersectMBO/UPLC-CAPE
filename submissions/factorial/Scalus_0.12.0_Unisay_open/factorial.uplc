(
  program
  1.1.0
  [
    (lam __z_combinator__
      [
        (lam factorial_FactorialOpen__factorial
          (lam n
            [factorial_FactorialOpen__factorial n])) (lam n
          (force [
            (force (builtin ifThenElse)) [
              (builtin lessThanEqualsInteger) n (con integer 0)
            ] (delay (con integer 1)) (delay [
              (lam factIter
                [factIter (con integer 1) n]) [
                __z_combinator__ (lam factIter
                  (lam acc
                    (lam count
                      (force [
                        (force (builtin ifThenElse)) [
                          (builtin lessThanEqualsInteger) count (con integer 0)
                        ] (delay acc) (delay [
                          factIter [(builtin multiplyInteger) acc count] [
                            (builtin subtractInteger) count (con integer 1)
                          ]
                        ])
                      ]))))
              ]
            ])
          ]))
      ]) (lam ff
      [
        (lam xx
          [ff (lam vv   [xx xx vv])]) (lam xx
          [ff (lam vv   [xx xx vv])])
      ])
  ]
)