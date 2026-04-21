(program 1.1.0 [(lam __builtin_IfThenElse
      [(lam ff
          [(lam xx [ff (lam vv [xx xx vv])]) (lam xx [ff (lam vv [xx xx vv])])])
        (lam fibonacci_naive_recursion_FibonacciNaiveRecursion__fibonacci28
          (lam n-2924829
            (force (case (constr 0 [(builtin lessThanEqualsInteger) n-2924829
                  (con integer 1)] (delay n-2924829)
                (delay (force (case (constr 0 [(builtin equalsInteger) n-2924829
                      (con integer 2)] (delay (con integer 1))
                    (delay [(builtin addInteger)
                      [fibonacci_naive_recursion_FibonacciNaiveRecursion__fibonacci28
                        [(builtin subtractInteger) n-2924829 (con integer 1)]]
                      [fibonacci_naive_recursion_FibonacciNaiveRecursion__fibonacci28
                        [(builtin subtractInteger) n-2924829
                          (con integer 2)]]])) __builtin_IfThenElse)))) __builtin_IfThenElse))))])
    (force (builtin ifThenElse))])