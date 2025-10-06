(program
  1.1.0
  [
    (lam
      i-0
      [
        (lam
          i-1
          (lam
            i-2
            (force
              (case
                (constr
                  0
                  [ [ (builtin lessThanEqualsInteger) i-2 ] (con integer 1) ]
                  (delay i-2)
                  (delay
                    [
                      [
                        (builtin addInteger)
                        [
                          [ i-1 i-1 ]
                          [ [ (builtin subtractInteger) i-2 ] (con integer 1) ]
                        ]
                      ]
                      [
                        [ i-1 i-1 ]
                        [ [ (builtin subtractInteger) i-2 ] (con integer 2) ]
                      ]
                    ]
                  )
                )
                i-0
              )
            )
          )
        )
        (lam
          i-3
          (lam
            i-4
            (force
              (case
                (constr
                  0
                  [ [ (builtin lessThanEqualsInteger) i-4 ] (con integer 1) ]
                  (delay i-4)
                  (delay
                    [
                      [
                        (builtin addInteger)
                        [
                          [ i-3 i-3 ]
                          [ [ (builtin subtractInteger) i-4 ] (con integer 1) ]
                        ]
                      ]
                      [
                        [ i-3 i-3 ]
                        [ [ (builtin subtractInteger) i-4 ] (con integer 2) ]
                      ]
                    ]
                  )
                )
                i-0
              )
            )
          )
        )
      ]
    )
    (force (builtin ifThenElse))
  ]
)