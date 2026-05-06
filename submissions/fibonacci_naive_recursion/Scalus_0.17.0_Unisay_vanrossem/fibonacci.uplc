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
          [ [ (builtin lessThanEqualsInteger) e-4 ] (con integer 1) ]
          (case
            [ [ (builtin equalsInteger) e-4 ] (con integer 2) ]
            [
              [
                (builtin addInteger)
                [ d-3 [ [ (builtin subtractInteger) e-4 ] (con integer 1) ] ]
              ]
              [ d-3 [ [ (builtin subtractInteger) e-4 ] (con integer 2) ] ]
            ]
            (con integer 1)
          )
          e-4
        )
      )
    )
  ]
)
