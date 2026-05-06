(program 1.1.0 [(lam a [a a]) (lam a
      (lam b
        (force (case (constr 0 [(builtin lessThanEqualsInteger) b
              (con integer 1)] (delay b) (delay [(builtin addInteger) [a a
                [(builtin subtractInteger) b (con integer 1)]] [a a
                [(builtin subtractInteger) b
                  (con integer 2)]]])) (force (builtin ifThenElse))))))])