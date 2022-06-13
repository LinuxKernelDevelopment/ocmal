let init_matrix fd sd initfunc = let arr = Array.make_matrix fd sd (initfunc 1 1) in 
                                                                                      (for i = 1 to fd do
                                                                                        for j = 1 to sd do
                                                                                          ignore (arr.(i - 1).(j - 1) <- initfunc (i - 1) (j - 1))
                                                                                        done
                                                                                      done; arr)