(program
  1.1.0
  [
    (lam
      a-0
      [
        (lam b-1 [ a-0 (lam c-2 [ [ b-1 b-1 ] c-2 ]) ])
        (lam b-1 [ a-0 (lam c-2 [ [ b-1 b-1 ] c-2 ]) ])
      ]
    )
    (lam
      d-3
      (lam
        e-4
        (case
          [ [ (builtin lessThanEqualsInteger) e-4 ] (con integer 0) ]
          [
            [ (builtin multiplyInteger) e-4 ]
            [ d-3 [ [ (builtin subtractInteger) e-4 ] (con integer 1) ] ]
          ]
          (con integer 1)
        )
      )
    )
  ]
)
