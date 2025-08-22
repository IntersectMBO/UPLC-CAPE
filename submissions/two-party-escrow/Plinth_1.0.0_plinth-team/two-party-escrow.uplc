(program
  1.1.0
  [
    (lam
      redeemerData-0
      [
        (lam
          redeemerInt-1
          (force
            [
              (lam
                unitval-2
                (case
                  [ [ (builtin equalsInteger) (con integer 0) ] redeemerInt-1 ]
                  (delay
                    (case
                      [
                        [ (builtin equalsInteger) (con integer 1) ]
                        redeemerInt-1
                      ]
                      (case
                        [
                          [ (builtin equalsInteger) (con integer 2) ]
                          redeemerInt-1
                        ]
                        (error)
                        unitval-2
                      )
                      unitval-2
                    )
                  )
                  (delay unitval-2)
                )
              )
              (con unit ())
            ]
          )
        )
        [ (builtin unIData) redeemerData-0 ]
      ]
    )
    (con data (I 1))
  ]
)