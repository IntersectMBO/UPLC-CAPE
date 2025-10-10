(
  program
  1.1.0
  [
    (lam r
      [r r]) (lam r
      (lam x
        (force (case (constr 0 [
          (builtin lessThanEqualsInteger) x (con integer 1)
        ] (delay x) (delay [
          (builtin addInteger) [
            r r [(builtin subtractInteger) x (con integer 1)]
          ] [r r [(builtin subtractInteger) x (con integer 2)]]
        ]))
          (force (builtin ifThenElse))))))
  ]
)