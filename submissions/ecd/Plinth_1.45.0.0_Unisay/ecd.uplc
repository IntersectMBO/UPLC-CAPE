(program
  1.1.0
  [
    (lam s-0 [ s-0 s-0 ])
    (lam
      s-1
      (lam
        a-2
        (lam
          b-3
          (force
            (case
              (constr
                0
                [ [ (builtin equalsInteger) (con integer 0) ] b-3 ]
                (delay
                  (force
                    (case
                      (constr
                        0
                        [ [ (builtin lessThanInteger) a-2 ] (con integer 0) ]
                        (delay
                          [ [ (builtin subtractInteger) (con integer 0) ] a-2 ]
                        )
                        (delay a-2)
                      )
                      (force (builtin ifThenElse))
                    )
                  )
                )
                (delay
                  (case
                    (constr 0 s-1 b-3 [ [ (builtin modInteger) a-2 ] b-3 ]) s-1
                  )
                )
              )
              (force (builtin ifThenElse))
            )
          )
        )
      )
    )
  ]
)