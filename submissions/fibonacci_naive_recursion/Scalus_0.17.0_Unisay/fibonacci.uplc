(program 1.1.0 [(lam a
      [(lam b [(lam c [b (lam d [c c d])]) (lam c [b (lam d [c c d])])]) (lam e
          (lam f
            (force (case (constr 0 [(builtin lessThanEqualsInteger) f
                  (con integer 1)] (delay f)
                (delay (force (case (constr 0 [(builtin equalsInteger) f
                      (con integer 2)] (delay (con integer 1))
                    (delay [(builtin addInteger) [e [(builtin subtractInteger) f
                          (con integer 1)]] [e [(builtin subtractInteger) f
                          (con integer 2)]]])) a)))) a))))])
    (force (builtin ifThenElse))])