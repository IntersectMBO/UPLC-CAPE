(program
  1.1.0
  [
    (lam
      scalus_examples_cape_factorial_FactorialOpen__factorial'588-0
      [
        scalus_examples_cape_factorial_FactorialOpen__factorial'588-0
        scalus_examples_cape_factorial_FactorialOpen__factorial'588-0
      ]
    )
    (lam
      scalus_examples_cape_factorial_FactorialOpen__factorial'588_cse-1
      (lam
        n_23741'589-2
        (case
          [ [ (builtin lessThanEqualsInteger) n_23741'589-2 ] (con integer 3) ]
          [
            (lam
              __cse_Sub_n_23741'589-3
              [
                [
                  (builtin multiplyInteger)
                  [
                    [
                      (builtin multiplyInteger)
                      [
                        [
                          (builtin multiplyInteger)
                          [
                            [ (builtin multiplyInteger) n_23741'589-2 ]
                            [ __cse_Sub_n_23741'589-3 (con integer 1) ]
                          ]
                        ]
                        [ __cse_Sub_n_23741'589-3 (con integer 2) ]
                      ]
                    ]
                    [ __cse_Sub_n_23741'589-3 (con integer 3) ]
                  ]
                ]
                [
                  [
                    scalus_examples_cape_factorial_FactorialOpen__factorial'588_cse-1
                    scalus_examples_cape_factorial_FactorialOpen__factorial'588_cse-1
                  ]
                  [ __cse_Sub_n_23741'589-3 (con integer 4) ]
                ]
              ]
            )
            [ (builtin subtractInteger) n_23741'589-2 ]
          ]
          (case
            [
              [ (builtin lessThanEqualsInteger) n_23741'589-2 ] (con integer 1)
            ]
            [
              [ (builtin multiplyInteger) n_23741'589-2 ]
              [ [ (builtin subtractInteger) n_23741'589-2 ] (con integer 1) ]
            ]
            (con integer 1)
          )
        )
      )
    )
  ]
)
