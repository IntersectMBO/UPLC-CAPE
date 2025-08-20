(program
  1.1.0
  (lam
    input-0
    (case
      [
        [
          [
            [
              [
                [
                  [ (force (builtin chooseData)) input-0 ]
                  (lam
                    ds-1
                    [
                      (lam
                        tup-2
                        [
                          (lam
                            ds-3
                            [
                              (lam ds-4 (constr 1))
                              [ (force (force (builtin sndPair))) tup-2 ]
                            ]
                          )
                          [ (force (force (builtin fstPair))) tup-2 ]
                        ]
                      )
                      [ (builtin unConstrData) input-0 ]
                    ]
                  )
                ]
                (lam
                  ds-5 [ (lam ds-6 (constr 1)) [ (builtin unMapData) input-0 ] ]
                )
              ]
              (lam
                ds-7 [ (lam ds-8 (constr 1)) [ (builtin unListData) input-0 ] ]
              )
            ]
            (lam ds-9 (constr 0 [ (builtin unIData) input-0 ]))
          ]
          (lam ds-10 [ (lam ds-11 (constr 1)) [ (builtin unBData) input-0 ] ])
        ]
        (constr 0)
      ]
      (lam
        ds-12
        (case
          [ [ (builtin equalsInteger) (con integer 42) ] ds-12 ]
          (error)
          (con unit ())
        )
      )
      (error)
    )
  )
)