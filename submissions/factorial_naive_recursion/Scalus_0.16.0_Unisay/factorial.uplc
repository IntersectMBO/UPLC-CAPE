(program
  1.1.0
  [
    (lam
      a-0
      [
        (lam
          b-1
          [
            (lam c-2 [ b-1 (lam d-3 [ [ c-2 c-2 ] d-3 ]) ])
            (lam c-2 [ b-1 (lam d-3 [ [ c-2 c-2 ] d-3 ]) ])
          ]
        )
        (lam
          e-4
          (lam
            f-5
            (force
              (case
                (constr
                  0
                  [ [ (builtin lessThanEqualsInteger) f-5 ] (con integer 0) ]
                  (delay (con integer 1))
                  (delay
                    [
                      [ (builtin multiplyInteger) f-5 ]
                      [
                        e-4
                        [ [ (builtin subtractInteger) f-5 ] (con integer 1) ]
                      ]
                    ]
                  )
                )
                a-0
              )
            )
          )
        )
      ]
    )
    (force (builtin ifThenElse))
  ]
)