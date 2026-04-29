(program
  1.1.0
  (case
    (constr
      0
      [ (builtin addInteger) (con integer -1) ]
      [ (builtin lessThanInteger) (con integer 1) ]
      (force (builtin ifThenElse))
    )
    (lam
      i_0-0
      (lam
        i_1-1
        (lam
          i_2-2
          [
            (lam i_3-3 [ i_3-3 i_3-3 ])
            (lam
              i_4-4
              (lam
                i_5-5
                (force
                  (case
                    (constr
                      0
                      [ i_1-1 i_5-5 ]
                      (delay
                        [
                          [ (builtin multiplyInteger) i_5-5 ]
                          [ [ i_4-4 i_4-4 ] [ i_0-0 i_5-5 ] ]
                        ]
                      )
                      (delay (con integer 1))
                    )
                    i_2-2
                  )
                )
              )
            )
          ]
        )
      )
    )
  )
)