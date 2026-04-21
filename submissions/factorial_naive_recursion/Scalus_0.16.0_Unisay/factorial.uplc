(program 1.1.0 [(lam __builtin_IfThenElse
      [(lam ff
          [(lam xx [ff (lam vv [xx xx vv])]) (lam xx [ff (lam vv [xx xx vv])])])
        (lam factorial_naive_recursion_FactorialNaiveRecursion__factorial28
          (lam n-2831429
            (force (case (constr 0 [(builtin lessThanEqualsInteger) n-2831429
                  (con integer 0)] (delay (con integer 1))
                (delay [(builtin multiplyInteger) n-2831429
                  [factorial_naive_recursion_FactorialNaiveRecursion__factorial28
                    [(builtin subtractInteger) n-2831429
                      (con integer 1)]]])) __builtin_IfThenElse))))])
    (force (builtin ifThenElse))])