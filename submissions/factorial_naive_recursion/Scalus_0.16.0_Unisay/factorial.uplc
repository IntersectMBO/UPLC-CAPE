(program 1.1.0 [(lam a
      [(lam b [(lam c [b (lam d [c c d])]) (lam c [b (lam d [c c d])])]) (lam e
          (lam f
            (force (case (constr 0 [(builtin lessThanEqualsInteger) f
                  (con integer 0)] (delay (con integer 1))
                (delay [(builtin multiplyInteger) f [e
                    [(builtin subtractInteger) f (con integer 1)]]])) a))))])
    (force (builtin ifThenElse))])