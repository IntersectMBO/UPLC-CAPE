(program
  1.0.0
  (lam
    v0-0
    [
      (lam
        v1-1
        [
          (lam
            v2-2
            [
              (lam
                v3-3
                [
                  (lam
                    v4-4
                    [
                      (lam v5-5 [ [ [ (force v4-4) v4-4 ] v3-3 ] (delay v5-5) ])
                      v0-0
                    ]
                  )
                  (delay
                    (lam
                      v6-6
                      (lam
                        v7-7
                        (lam
                          v8-8
                          [
                            (lam
                              v9-9
                              (force
                                [
                                  [
                                    [
                                      (force (builtin ifThenElse))
                                      [
                                        (lam
                                          v10-10
                                          [
                                            (lam
                                              v11-11
                                              [
                                                (lam v12-12 v12-12)
                                                (force v11-11)
                                              ]
                                            )
                                            (delay v10-10)
                                          ]
                                        )
                                        [
                                          [
                                            (builtin lessThanEqualsInteger)
                                            (force v8-8)
                                          ]
                                          (con integer 0)
                                        ]
                                      ]
                                    ]
                                    (delay (con integer 1))
                                  ]
                                  (delay
                                    [
                                      [
                                        (lam
                                          v13-13
                                          (lam
                                            v14-14
                                            [
                                              [
                                                (builtin multiplyInteger) v13-13
                                              ]
                                              v14-14
                                            ]
                                          )
                                        )
                                        (force v8-8)
                                      ]
                                      [
                                        (lam
                                          v15-15
                                          [
                                            [ [ (force v6-6) v6-6 ] v7-7 ]
                                            (delay v15-15)
                                          ]
                                        )
                                        [
                                          [
                                            (lam
                                              v16-16
                                              (lam
                                                v17-17
                                                [
                                                  [
                                                    (builtin subtractInteger)
                                                    v16-16
                                                  ]
                                                  v17-17
                                                ]
                                              )
                                            )
                                            (force v8-8)
                                          ]
                                          (con integer 1)
                                        ]
                                      ]
                                    ]
                                  )
                                ]
                              )
                            )
                            (lam v18-18 (con unit ()))
                          ]
                        )
                      )
                    )
                  )
                ]
              )
              (delay [ (lam v19-19 (error)) (con unit ()) ])
            ]
          )
          (delay [ (lam v20-20 (error)) (con unit ()) ])
        ]
      )
      (delay [ (lam v21-21 (error)) (con unit ()) ])
    ]
  )
)