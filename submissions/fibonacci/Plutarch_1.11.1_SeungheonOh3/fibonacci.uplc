(program
  1.1.0
  [
    (lam
      i-0
      (lam
        i-1
        (force
          (case
            (constr
              0
              [ [ (builtin lessThanInteger) i-1 ] (con integer 0) ]
              (delay i-1)
              (delay
                (case
                  (constr
                    0
                    (lam
                      i-2
                      (lam
                        i-3
                        (lam
                          i-4
                          (lam
                            i-5
                            (force
                              (case
                                (constr
                                  0
                                  [
                                    [ (builtin equalsInteger) i-5 ]
                                    (con integer 0)
                                  ]
                                  (delay i-3)
                                  (delay
                                    (case
                                      (constr
                                        0
                                        i-2
                                        i-4
                                        [ [ (builtin addInteger) i-3 ] i-4 ]
                                        [
                                          [ (builtin subtractInteger) i-5 ]
                                          (con integer 1)
                                        ]
                                      )
                                      i-2
                                    )
                                  )
                                )
                                i-0
                              )
                            )
                          )
                        )
                      )
                    )
                    (con integer 0)
                    (con integer 1)
                    i-1
                  )
                  (lam i-6 [ i-6 i-6 ])
                )
              )
            )
            i-0
          )
        )
      )
    )
    (force (builtin ifThenElse))
  ]
)