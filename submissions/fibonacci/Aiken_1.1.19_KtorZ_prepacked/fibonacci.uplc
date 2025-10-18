(program
  1.1.0
  (lam
    i_0
    [
      [
        (lam
          i_1
          (lam
            i_2
            (force
              (case
                (constr
                  0
                [ [ (builtin lessThanInteger) i_2 ] (con integer 0) ]
                (delay i_2)
                (delay
                  [
                    (lam
                      i_3
                      [
                        [ (builtin byteStringToInteger) (con bool True) ]
                        (case
                          (constr
                            0
                          [ [ (builtin modInteger) i_3 ] (con integer 64) ]
                          [ [ (builtin divideInteger) i_3 ] (con integer 64) ]
                          (con
                            bytestring
                            #000101020305080d1522375990e90179026203db063d0a18105f1a6d2ac2452f6ff1b520012511
                          ))
                        (builtin sliceByteString))
                      ]
                    )
                    [
                      [
                        (builtin indexByteString)
                        (con
                          bytestring
                          #404142434445464748494a4b4c4d8e90929496989a9c9ea0a2e4
                        )
                      ]
                      i_2
                    ]
                  ]
                ))
              i_1)
            )
          )
        )
        (force (builtin ifThenElse))
      ]
      i_0
    ]
  )
)
