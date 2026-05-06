(program 1.1.0 [(lam a [a a]) (lam a
      (lam b
        (force (case (constr 0 [(builtin lessThanEqualsInteger) b
              (con integer 0)] (delay (con integer 1))
            (delay [(builtin multiplyInteger) b [a a [(builtin subtractInteger)
                  b (con integer 1)]]])) (force (builtin ifThenElse))))))])