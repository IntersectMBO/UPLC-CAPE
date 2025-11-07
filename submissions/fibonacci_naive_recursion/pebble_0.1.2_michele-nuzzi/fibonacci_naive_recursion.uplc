(program
  1.1.0
  (case
    (constr
      0
    [ (builtin addInteger) (con integer -1) ]
    [ (builtin addInteger) (con integer -2) ]
    [ (builtin lessThanInteger) (con integer 1) ]
    (force (builtin ifThenElse)))
  (lam
    i_0
    (lam
      i_1
      (lam
        i_2
        (lam
          i_3
          [
            (lam i_4 [ i_4 i_4 ])
            (lam
              i_5
              (lam
                i_6
                (force
                  (case
                    (constr
                      0
                    [ i_2 i_6 ]
                    (delay
                      [
                        [ (builtin addInteger) [ [ i_5 i_5 ] [ i_1 i_6 ] ] ]
                        [ [ i_5 i_5 ] [ i_0 i_6 ] ]
                      ]
                    )
                    (delay i_6))
                  i_3)
                )
              )
            )
          ]
        )
      )
    )
  ))
)
