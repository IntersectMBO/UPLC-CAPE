(
  program
  1.1.0
  [
    (lam r
      [r r]) (lam r
      (lam x
        (force (case (constr 0 [
          (builtin lessThanEqualsInteger) x (con integer 0)
        ] (delay (con integer 1)) (delay [
          (builtin multiplyInteger) x [
            r r [(builtin subtractInteger) x (con integer 1)]
          ]
        ]))
          (force (builtin ifThenElse))))))
  ]
)