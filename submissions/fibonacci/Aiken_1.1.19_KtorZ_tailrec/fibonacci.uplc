(program
  1.1.0
  (lam
    i_0
    [
      [
        (lam
          i_1
          (lam
            i_2
            (force
              (case
                (constr
                  0
                [ [ (builtin lessThanEqualsInteger) i_2 ] (con integer 1) ]
                (delay i_2)
                (delay
                  [
                    (lam
                      i_3
                      (case
                        (constr
                          0
                        i_3
                        (con integer 1)
                        (con integer 1)
                        [ [ (builtin addInteger) i_2 ] (con integer -2) ])
                      i_3)
                    )
                    (lam
                      i_4
                      (lam
                        i_5
                        (lam
                          i_6
                          (lam
                            i_7
                            (force
                              (case
                                (constr
                                  0
                                [
                                  [ (builtin equalsInteger) i_7 ]
                                  (con integer 0)
                                ]
                                (delay i_6)
                                (delay
                                  (case
                                    (constr
                                      0
                                    i_4
                                    i_6
                                    [ [ (builtin addInteger) i_6 ] i_5 ]
                                    [
                                      [ (builtin addInteger) i_7 ]
                                      (con integer -1)
                                    ])
                                  i_4)
                                ))
                              i_1)
                            )
                          )
                        )
                      )
                    )
                  ]
                ))
              i_1)
            )
          )
        )
        (force (builtin ifThenElse))
      ]
      i_0
    ]
  )
)
