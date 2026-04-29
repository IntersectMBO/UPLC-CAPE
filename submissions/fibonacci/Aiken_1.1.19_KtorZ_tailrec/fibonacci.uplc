(program
  1.1.0
  (lam
    i_0-0
    [
      [
        (lam
          i_1-1
          (lam
            i_2-2
            (force
              (case
                (constr
                  0
                  [ [ (builtin lessThanEqualsInteger) i_2-2 ] (con integer 1) ]
                  (delay i_2-2)
                  (delay
                    [
                      (lam
                        i_3-3
                        (case
                          (constr
                            0
                            i_3-3
                            (con integer 1)
                            (con integer 1)
                            [ [ (builtin addInteger) i_2-2 ] (con integer -2) ]
                          )
                          i_3-3
                        )
                      )
                      (lam
                        i_4-4
                        (lam
                          i_5-5
                          (lam
                            i_6-6
                            (lam
                              i_7-7
                              (force
                                (case
                                  (constr
                                    0
                                    [
                                      [ (builtin equalsInteger) i_7-7 ]
                                      (con integer 0)
                                    ]
                                    (delay i_6-6)
                                    (delay
                                      (case
                                        (constr
                                          0
                                          i_4-4
                                          i_6-6
                                          [
                                            [ (builtin addInteger) i_6-6 ] i_5-5
                                          ]
                                          [
                                            [ (builtin addInteger) i_7-7 ]
                                            (con integer -1)
                                          ]
                                        )
                                        i_4-4
                                      )
                                    )
                                  )
                                  i_1-1
                                )
                              )
                            )
                          )
                        )
                      )
                    ]
                  )
                )
                i_1-1
              )
            )
          )
        )
        (force (builtin ifThenElse))
      ]
      i_0-0
    ]
  )
)