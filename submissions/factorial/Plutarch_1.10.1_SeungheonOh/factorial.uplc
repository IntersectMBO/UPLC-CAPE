(program
  1.1.0
  [
    [
      (lam i-0 [ i-0 i-0 ])
      (lam
        i-1
        (lam
          i-2
          (force
            (case
              (constr
                0
                [ [ (builtin equalsInteger) i-2 ] (con integer 0) ]
                (delay (con integer 1))
                (delay
                  [
                    [ (builtin multiplyInteger) i-2 ]
                    [
                      [ i-1 i-1 ]
                      [ [ (builtin subtractInteger) i-2 ] (con integer 1) ]
                    ]
                  ]
                )
              )
              (force (builtin ifThenElse))
            )
          )
        )
      )
    ]
    (con integer 10)
  ]
)