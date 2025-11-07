(program
  1.1.0
  (case
    (constr
      0
    [ (builtin addInteger) (con integer -1) ]
    [ (builtin lessThanInteger) (con integer 1) ]
    (force (builtin ifThenElse)))
  (lam
    i_0
    (lam
      i_1
      (lam
        i_2
        [
          (lam i_3 [ i_3 i_3 ])
          (lam
            i_4
            (lam
              i_5
              (force
                (case
                  (constr
                    0
                  [ i_1 i_5 ]
                  (delay
                    [
                      [ (builtin multiplyInteger) i_5 ]
                      [ [ i_4 i_4 ] [ i_0 i_5 ] ]
                    ]
                  )
                  (delay (con integer 1)))
                i_2)
              )
            )
          )
        ]
      )
    )
  ))
)
