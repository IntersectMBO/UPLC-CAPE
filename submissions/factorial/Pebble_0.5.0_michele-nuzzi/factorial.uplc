(program
  1.1.0
  [
    (lam
      a-0
      [
        (lam
          b-1
          (lam
            c-2
            [
              (lam
                d-3 (case (constr 0 d-3 (con integer 1) (con integer 2)) d-3)
              )
              (lam
                d-3
                (lam
                  e-4
                  (lam
                    f-5
                    (case
                      [ [ (builtin lessThanEqualsInteger) f-5 ] c-2 ]
                      e-4
                      (case (constr 0 d-3 [ [ a-0 e-4 ] f-5 ] [ b-1 f-5 ]) d-3)
                    )
                  )
                )
              )
            ]
          )
        )
        [ (builtin addInteger) (con integer 1) ]
      ]
    )
    (builtin multiplyInteger)
  ]
)
