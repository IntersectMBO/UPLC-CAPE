(program
  1.1.0
  [
    (lam
      i-0
      [
        (lam i-1 [ i-0 (lam i-2 [ [ i-1 i-1 ] i-2 ]) ])
        (lam i-3 [ i-0 (lam i-4 [ [ i-3 i-3 ] i-4 ]) ])
      ]
    )
    (lam
      i-5
      (lam
        i-6
        (force
          (case
            (constr
              0
              [ [ (builtin lessThanEqualsInteger) i-6 ] (con integer 1) ]
              (delay i-6)
              (delay
                [
                  [
                    (builtin addInteger)
                    [
                      i-5 [ [ (builtin subtractInteger) i-6 ] (con integer 1) ]
                    ]
                  ]
                  [ i-5 [ [ (builtin subtractInteger) i-6 ] (con integer 2) ] ]
                ]
              )
            )
            (force (builtin ifThenElse))
          )
        )
      )
    )
  ]
)
