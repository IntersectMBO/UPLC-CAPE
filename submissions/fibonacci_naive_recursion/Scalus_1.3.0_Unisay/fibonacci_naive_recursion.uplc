(program
  1.1.0
  [
    (lam a-0 [ a-0 a-0 ])
    (lam
      b-1
      (lam
        c-2
        (case
          [ [ (builtin lessThanEqualsInteger) c-2 ] (con integer 1) ]
          (case
            [ [ (builtin equalsInteger) c-2 ] (con integer 2) ]
            [
              (lam
                d-3
                [
                  [
                    (builtin addInteger)
                    [
                      d-3 [ [ (builtin subtractInteger) c-2 ] (con integer 1) ]
                    ]
                  ]
                  [ d-3 [ [ (builtin subtractInteger) c-2 ] (con integer 2) ] ]
                ]
              )
              [ b-1 b-1 ]
            ]
            (con integer 1)
          )
          c-2
        )
      )
    )
  ]
)
