(program
  1.1.0
  (lam
    n-0
    (case
      [ [ (builtin equalsInteger) (con integer 42) ] n-0 ] (error) (con unit ())
    )
  )
)
