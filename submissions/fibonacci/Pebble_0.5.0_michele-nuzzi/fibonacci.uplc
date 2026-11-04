(program
  1.1.0
  (case
    (constr
      0
      [ (builtin addInteger) (con integer 1) ]
      [ (builtin lessThanInteger) (con integer 1) ]
    )
    (lam
      a-0
      (lam
        b-1
        (lam
          c-2
          (case
            [ b-1 c-2 ]
            c-2
            (case
              [
                (lam
                  d-3
                  [
                    (lam
                      e-4
                      [
                        (lam
                          f-5
                          (case
                            (constr
                              0
                              f-5
                              (con integer 1)
                              (con integer 0)
                              (con integer 2)
                            )
                            f-5
                          )
                        )
                        (lam
                          f-5
                          (lam
                            g-6
                            (lam
                              h-7
                              (lam
                                i-8
                                (case
                                  [ [ e-4 i-8 ] c-2 ]
                                  (constr 0 g-6 h-7)
                                  (case
                                    (constr
                                      0 f-5 [ [ d-3 h-7 ] g-6 ] g-6 [ a-0 i-8 ]
                                    )
                                    f-5
                                  )
                                )
                              )
                            )
                          )
                        )
                      ]
                    )
                    (builtin lessThanEqualsInteger)
                  ]
                )
                (builtin addInteger)
              ]
              (lam d-3 (lam e-4 d-3))
            )
          )
        )
      )
    )
  )
)
