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
          (case
            [ [ (builtin equalsInteger) (con integer 0) ] b-3 ]
            (case (constr 0 s-1 b-3 [ [ (builtin modInteger) a-2 ] b-3 ]) s-1)
            (case
              [ [ (builtin lessThanInteger) a-2 ] (con integer 0) ]
              a-2
              [ [ (builtin subtractInteger) (con integer 0) ] a-2 ]
            )
          )
        )
      )
    )
  ]
)