(program
  1.1.0
  (lam
    i-0
    (force
      (case
        (constr
          0
          [ [ (builtin lessThanEqualsInteger) i-0 ] (con integer 0) ]
          (delay i-0)
          (delay
            [
              [ (builtin byteStringToInteger) (con bool True) ]
              (case
                (constr
                  0
                  [ [ (builtin multiplyInteger) i-0 ] (con integer 3) ]
                  (con integer 3)
                  (con
                    bytestring
                    #00000000000100000100000200000300000500000800000d0000150000220000370000590000900000e90001790002620003db00063d000a1800105f001a6d002ac200452f006ff100b520012511
                  )
                )
                (builtin sliceByteString)
              )
            ]
          )
        )
        (force (builtin ifThenElse))
      )
    )
  )
)