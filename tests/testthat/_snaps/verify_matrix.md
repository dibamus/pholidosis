# warns of mismatched column & row names

    Code
      verify_matrix(mat.1)
    Condition
      Warning in `verify_matrix()`:
      mismatched column & row names 
      frontonasal  frontonasag  
    Output
        bad.input.matrix  columnName     rowName
      1                1 frontonasal frontonasag

# warns of non-square matrix

    Code
      verify_matrix(mat.nsq)
    Condition
      Warning in `verify_matrix()`:
      not a square matrix: 40 × 40
    Output
         bad_rows
      1          
      2 extra.row

# warns of uninterpretable inputs

    Code
      verify_matrix(mat.2)
    Condition
      Warning in `verify_matrix()`:
      Uninterpretable entry or entries f in martix @ 7  22  
    Output
                    bad.input.matrix row col
      L_internarial                1   7  22

# warns of non-triangulated network

    Code
      verify_matrix(mat.3)
    Condition
      Warning in `verify_matrix()`:
      graph is not fully triangulated: E != 3V-6 
       > loading graph for visualization only - NOT suitable for comparisons
    Output
                           rostral frontonasal frontal interparietal mental
      rostral                   NA          NA      NA            NA     NA
      frontonasal               NA          NA       1            NA     NA
      frontal                   NA           1      NA             1     NA
      interparietal             NA          NA       1            NA     NA
      mental                    NA          NA      NA            NA     NA
      post-mental               NA          NA      NA            NA      1
      L_internarial              3           1      NA            NA     NA
      L_nasal                   NA           1      NA            NA     NA
      L_ocular                  NA           1       1            NA     NA
      L_supralabial_1            3          NA      NA            NA     NA
      L_supralabial_2           NA          NA      NA            NA     NA
      L_supralabial_3           NA          NA      NA            NA     NA
      L_supralabial_4           NA          NA      NA            NA     NA
      L_postocular_1            NA          NA       1            NA     NA
      L_postocular_2            NA          NA      NA            NA     NA
      L_postfrontal_1           NA          NA       1             1     NA
      L_infralabial             NA          NA      NA            NA      1
      L_post-infralabial_1      NA          NA      NA            NA     NA
      L_post-infralabial_2      NA          NA      NA            NA     NA
      L_post-infralabial_3      NA          NA      NA            NA     NA
      L_post-infralabial_4      NA          NA      NA            NA     NA
      R_internarial              3           1      NA            NA     NA
      R_nasal                   NA           1      NA            NA     NA
      R_ocular                  NA           1       1            NA     NA
      R_supralabial_1            3          NA      NA            NA     NA
      R_supralabial_2           NA          NA      NA            NA     NA
      R_supralabial_3           NA          NA      NA            NA     NA
      R_supralabial_4           NA          NA      NA            NA     NA
      R_postocular_1            NA          NA       1            NA     NA
      R_postocular_2            NA          NA      NA            NA     NA
      R_postfrontal_1           NA          NA       1             1     NA
      R_infralabial             NA          NA      NA            NA      1
      R_post-infralabial_1      NA          NA      NA            NA     NA
      R_post-infralabial_2      NA          NA      NA            NA     NA
      R_post-infralabial_3      NA          NA      NA            NA     NA
      R_post-infralabial_4      NA          NA      NA            NA     NA
                           post-mental L_internarial L_nasal L_ocular L_supralabial_1
      rostral                       NA             3      NA       NA               3
      frontonasal                   NA             1       1        1              NA
      frontal                       NA            NA      NA        1              NA
      interparietal                 NA            NA      NA       NA              NA
      mental                         1            NA      NA       NA              NA
      post-mental                   NA            NA      NA       NA              NA
      L_internarial                 NA            NA       3       NA               3
      L_nasal                       NA             3      NA        1               3
      L_ocular                      NA            NA       1       NA              NA
      L_supralabial_1               NA             3       3       NA              NA
      L_supralabial_2               NA            NA       1        1               2
      L_supralabial_3               NA            NA      NA        1              NA
      L_supralabial_4               NA            NA      NA        1              NA
      L_postocular_1                NA            NA      NA        1              NA
      L_postocular_2                NA            NA      NA        1              NA
      L_postfrontal_1               NA            NA      NA       NA              NA
      L_infralabial                  1            NA      NA       NA              NA
      L_post-infralabial_1           1            NA      NA       NA              NA
      L_post-infralabial_2          NA            NA      NA       NA              NA
      L_post-infralabial_3          NA            NA      NA       NA              NA
      L_post-infralabial_4          NA            NA      NA       NA              NA
      R_internarial                 NA             3      NA       NA              NA
      R_nasal                       NA            NA      NA       NA              NA
      R_ocular                      NA            NA      NA       NA              NA
      R_supralabial_1               NA            NA      NA       NA              NA
      R_supralabial_2               NA            NA      NA       NA              NA
      R_supralabial_3               NA            NA      NA       NA              NA
      R_supralabial_4               NA            NA      NA       NA              NA
      R_postocular_1                NA            NA      NA       NA              NA
      R_postocular_2                NA            NA      NA       NA              NA
      R_postfrontal_1               NA            NA      NA       NA              NA
      R_infralabial                  1            NA      NA       NA              NA
      R_post-infralabial_1           1            NA      NA       NA              NA
      R_post-infralabial_2          NA            NA      NA       NA              NA
      R_post-infralabial_3          NA            NA      NA       NA              NA
      R_post-infralabial_4          NA            NA      NA       NA              NA
                           L_supralabial_2 L_supralabial_3 L_supralabial_4
      rostral                           NA              NA              NA
      frontonasal                       NA              NA              NA
      frontal                           NA              NA              NA
      interparietal                     NA              NA              NA
      mental                            NA              NA              NA
      post-mental                       NA              NA              NA
      L_internarial                     NA              NA              NA
      L_nasal                            1              NA              NA
      L_ocular                           1               1               1
      L_supralabial_1                    2              NA              NA
      L_supralabial_2                   NA               3              NA
      L_supralabial_3                    3              NA               1
      L_supralabial_4                   NA               1              NA
      L_postocular_1                    NA              NA              NA
      L_postocular_2                    NA              NA               1
      L_postfrontal_1                   NA              NA              NA
      L_infralabial                     NA              NA               1
      L_post-infralabial_1              NA              NA              NA
      L_post-infralabial_2              NA              NA              NA
      L_post-infralabial_3              NA              NA              NA
      L_post-infralabial_4              NA              NA               1
      R_internarial                     NA              NA              NA
      R_nasal                           NA              NA              NA
      R_ocular                          NA              NA              NA
      R_supralabial_1                   NA              NA              NA
      R_supralabial_2                   NA              NA              NA
      R_supralabial_3                   NA              NA              NA
      R_supralabial_4                   NA              NA              NA
      R_postocular_1                    NA              NA              NA
      R_postocular_2                    NA              NA              NA
      R_postfrontal_1                   NA              NA              NA
      R_infralabial                     NA              NA              NA
      R_post-infralabial_1              NA              NA              NA
      R_post-infralabial_2              NA              NA              NA
      R_post-infralabial_3              NA              NA              NA
      R_post-infralabial_4              NA              NA              NA
                           L_postocular_1 L_postocular_2 L_postfrontal_1
      rostral                          NA             NA              NA
      frontonasal                      NA             NA              NA
      frontal                           1             NA               1
      interparietal                    NA             NA               1
      mental                           NA             NA              NA
      post-mental                      NA             NA              NA
      L_internarial                    NA             NA              NA
      L_nasal                          NA             NA              NA
      L_ocular                          1              1              NA
      L_supralabial_1                  NA             NA              NA
      L_supralabial_2                  NA             NA              NA
      L_supralabial_3                  NA             NA              NA
      L_supralabial_4                  NA              1              NA
      L_postocular_1                   NA              1               1
      L_postocular_2                    1             NA              NA
      L_postfrontal_1                   1             NA              NA
      L_infralabial                    NA             NA              NA
      L_post-infralabial_1             NA             NA              NA
      L_post-infralabial_2             NA             NA              NA
      L_post-infralabial_3             NA             NA              NA
      L_post-infralabial_4             NA             NA              NA
      R_internarial                    NA             NA              NA
      R_nasal                          NA             NA              NA
      R_ocular                         NA             NA              NA
      R_supralabial_1                  NA             NA              NA
      R_supralabial_2                  NA             NA              NA
      R_supralabial_3                  NA             NA              NA
      R_supralabial_4                  NA             NA              NA
      R_postocular_1                   NA             NA              NA
      R_postocular_2                   NA             NA              NA
      R_postfrontal_1                  NA             NA              NA
      R_infralabial                    NA             NA              NA
      R_post-infralabial_1             NA             NA              NA
      R_post-infralabial_2             NA             NA              NA
      R_post-infralabial_3             NA             NA              NA
      R_post-infralabial_4             NA             NA              NA
                           L_infralabial L_post-infralabial_1 L_post-infralabial_2
      rostral                         NA                   NA                   NA
      frontonasal                     NA                   NA                   NA
      frontal                         NA                   NA                   NA
      interparietal                   NA                   NA                   NA
      mental                           1                   NA                   NA
      post-mental                      1                    1                   NA
      L_internarial                   NA                   NA                   NA
      L_nasal                         NA                   NA                   NA
      L_ocular                        NA                   NA                   NA
      L_supralabial_1                 NA                   NA                   NA
      L_supralabial_2                 NA                   NA                   NA
      L_supralabial_3                 NA                   NA                   NA
      L_supralabial_4                  1                   NA                   NA
      L_postocular_1                  NA                   NA                   NA
      L_postocular_2                  NA                   NA                   NA
      L_postfrontal_1                 NA                   NA                   NA
      L_infralabial                   NA                    1                    1
      L_post-infralabial_1             1                   NA                    1
      L_post-infralabial_2             1                    1                   NA
      L_post-infralabial_3             1                   NA                    1
      L_post-infralabial_4             1                   NA                   NA
      R_internarial                   NA                   NA                   NA
      R_nasal                         NA                   NA                   NA
      R_ocular                        NA                   NA                   NA
      R_supralabial_1                 NA                   NA                   NA
      R_supralabial_2                 NA                   NA                   NA
      R_supralabial_3                 NA                   NA                   NA
      R_supralabial_4                 NA                   NA                   NA
      R_postocular_1                  NA                   NA                   NA
      R_postocular_2                  NA                   NA                   NA
      R_postfrontal_1                 NA                   NA                   NA
      R_infralabial                   NA                   NA                   NA
      R_post-infralabial_1            NA                   NA                   NA
      R_post-infralabial_2            NA                   NA                   NA
      R_post-infralabial_3            NA                   NA                   NA
      R_post-infralabial_4            NA                   NA                   NA
                           L_post-infralabial_3 L_post-infralabial_4 R_internarial
      rostral                                NA                   NA             3
      frontonasal                            NA                   NA             1
      frontal                                NA                   NA            NA
      interparietal                          NA                   NA            NA
      mental                                 NA                   NA            NA
      post-mental                            NA                   NA            NA
      L_internarial                          NA                   NA             3
      L_nasal                                NA                   NA            NA
      L_ocular                               NA                   NA            NA
      L_supralabial_1                        NA                   NA            NA
      L_supralabial_2                        NA                   NA            NA
      L_supralabial_3                        NA                   NA            NA
      L_supralabial_4                        NA                    1            NA
      L_postocular_1                         NA                   NA            NA
      L_postocular_2                         NA                   NA            NA
      L_postfrontal_1                        NA                   NA            NA
      L_infralabial                           1                    1            NA
      L_post-infralabial_1                   NA                   NA            NA
      L_post-infralabial_2                    1                   NA            NA
      L_post-infralabial_3                   NA                    1            NA
      L_post-infralabial_4                    1                   NA            NA
      R_internarial                          NA                   NA            NA
      R_nasal                                NA                   NA             3
      R_ocular                               NA                   NA            NA
      R_supralabial_1                        NA                   NA             3
      R_supralabial_2                        NA                   NA            NA
      R_supralabial_3                        NA                   NA            NA
      R_supralabial_4                        NA                   NA            NA
      R_postocular_1                         NA                   NA            NA
      R_postocular_2                         NA                   NA            NA
      R_postfrontal_1                        NA                   NA            NA
      R_infralabial                          NA                   NA            NA
      R_post-infralabial_1                   NA                   NA            NA
      R_post-infralabial_2                   NA                   NA            NA
      R_post-infralabial_3                   NA                   NA            NA
      R_post-infralabial_4                   NA                   NA            NA
                           R_nasal R_ocular R_supralabial_1 R_supralabial_2
      rostral                   NA       NA               3              NA
      frontonasal                1        1              NA              NA
      frontal                   NA        1              NA              NA
      interparietal             NA       NA              NA              NA
      mental                    NA       NA              NA              NA
      post-mental               NA       NA              NA              NA
      L_internarial             NA       NA              NA              NA
      L_nasal                   NA       NA              NA              NA
      L_ocular                  NA       NA              NA              NA
      L_supralabial_1           NA       NA              NA              NA
      L_supralabial_2           NA       NA              NA              NA
      L_supralabial_3           NA       NA              NA              NA
      L_supralabial_4           NA       NA              NA              NA
      L_postocular_1            NA       NA              NA              NA
      L_postocular_2            NA       NA              NA              NA
      L_postfrontal_1           NA       NA              NA              NA
      L_infralabial             NA       NA              NA              NA
      L_post-infralabial_1      NA       NA              NA              NA
      L_post-infralabial_2      NA       NA              NA              NA
      L_post-infralabial_3      NA       NA              NA              NA
      L_post-infralabial_4      NA       NA              NA              NA
      R_internarial              3       NA               3              NA
      R_nasal                   NA        1               3               1
      R_ocular                   1       NA              NA               1
      R_supralabial_1            3       NA              NA               2
      R_supralabial_2            1        1               2              NA
      R_supralabial_3           NA        1              NA               3
      R_supralabial_4           NA        1              NA              NA
      R_postocular_1            NA        1              NA              NA
      R_postocular_2            NA        1              NA              NA
      R_postfrontal_1           NA       NA              NA              NA
      R_infralabial             NA       NA              NA              NA
      R_post-infralabial_1      NA       NA              NA              NA
      R_post-infralabial_2      NA       NA              NA              NA
      R_post-infralabial_3      NA       NA              NA              NA
      R_post-infralabial_4      NA       NA              NA              NA
                           R_supralabial_3 R_supralabial_4 R_postocular_1
      rostral                           NA              NA             NA
      frontonasal                       NA              NA             NA
      frontal                           NA              NA              1
      interparietal                     NA              NA             NA
      mental                            NA              NA             NA
      post-mental                       NA              NA             NA
      L_internarial                     NA              NA             NA
      L_nasal                           NA              NA             NA
      L_ocular                          NA              NA             NA
      L_supralabial_1                   NA              NA             NA
      L_supralabial_2                   NA              NA             NA
      L_supralabial_3                   NA              NA             NA
      L_supralabial_4                   NA              NA             NA
      L_postocular_1                    NA              NA             NA
      L_postocular_2                    NA              NA             NA
      L_postfrontal_1                   NA              NA             NA
      L_infralabial                     NA              NA             NA
      L_post-infralabial_1              NA              NA             NA
      L_post-infralabial_2              NA              NA             NA
      L_post-infralabial_3              NA              NA             NA
      L_post-infralabial_4              NA              NA             NA
      R_internarial                     NA              NA             NA
      R_nasal                           NA              NA             NA
      R_ocular                           1               1              1
      R_supralabial_1                   NA              NA             NA
      R_supralabial_2                    3              NA             NA
      R_supralabial_3                   NA               1             NA
      R_supralabial_4                    1              NA             NA
      R_postocular_1                    NA              NA             NA
      R_postocular_2                    NA               1              1
      R_postfrontal_1                   NA              NA              1
      R_infralabial                     NA               1             NA
      R_post-infralabial_1              NA              NA             NA
      R_post-infralabial_2              NA              NA             NA
      R_post-infralabial_3              NA              NA             NA
      R_post-infralabial_4              NA               1             NA
                           R_postocular_2 R_postfrontal_1 R_infralabial
      rostral                          NA              NA            NA
      frontonasal                      NA              NA            NA
      frontal                          NA               1            NA
      interparietal                    NA               1            NA
      mental                           NA              NA             1
      post-mental                      NA              NA             1
      L_internarial                    NA              NA            NA
      L_nasal                          NA              NA            NA
      L_ocular                         NA              NA            NA
      L_supralabial_1                  NA              NA            NA
      L_supralabial_2                  NA              NA            NA
      L_supralabial_3                  NA              NA            NA
      L_supralabial_4                  NA              NA            NA
      L_postocular_1                   NA              NA            NA
      L_postocular_2                   NA              NA            NA
      L_postfrontal_1                  NA              NA            NA
      L_infralabial                    NA              NA            NA
      L_post-infralabial_1             NA              NA            NA
      L_post-infralabial_2             NA              NA            NA
      L_post-infralabial_3             NA              NA            NA
      L_post-infralabial_4             NA              NA            NA
      R_internarial                    NA              NA            NA
      R_nasal                          NA              NA            NA
      R_ocular                          1              NA            NA
      R_supralabial_1                  NA              NA            NA
      R_supralabial_2                  NA              NA            NA
      R_supralabial_3                  NA              NA            NA
      R_supralabial_4                   1              NA             1
      R_postocular_1                    1               1            NA
      R_postocular_2                   NA              NA            NA
      R_postfrontal_1                  NA              NA            NA
      R_infralabial                    NA              NA            NA
      R_post-infralabial_1             NA              NA             1
      R_post-infralabial_2             NA              NA             1
      R_post-infralabial_3             NA              NA             1
      R_post-infralabial_4             NA              NA             1
                           R_post-infralabial_1 R_post-infralabial_2
      rostral                                NA                   NA
      frontonasal                            NA                   NA
      frontal                                NA                   NA
      interparietal                          NA                   NA
      mental                                 NA                   NA
      post-mental                             1                   NA
      L_internarial                          NA                   NA
      L_nasal                                NA                   NA
      L_ocular                               NA                   NA
      L_supralabial_1                        NA                   NA
      L_supralabial_2                        NA                   NA
      L_supralabial_3                        NA                   NA
      L_supralabial_4                        NA                   NA
      L_postocular_1                         NA                   NA
      L_postocular_2                         NA                   NA
      L_postfrontal_1                        NA                   NA
      L_infralabial                          NA                   NA
      L_post-infralabial_1                   NA                   NA
      L_post-infralabial_2                   NA                   NA
      L_post-infralabial_3                   NA                   NA
      L_post-infralabial_4                   NA                   NA
      R_internarial                          NA                   NA
      R_nasal                                NA                   NA
      R_ocular                               NA                   NA
      R_supralabial_1                        NA                   NA
      R_supralabial_2                        NA                   NA
      R_supralabial_3                        NA                   NA
      R_supralabial_4                        NA                   NA
      R_postocular_1                         NA                   NA
      R_postocular_2                         NA                   NA
      R_postfrontal_1                        NA                   NA
      R_infralabial                           1                    1
      R_post-infralabial_1                   NA                    1
      R_post-infralabial_2                    1                   NA
      R_post-infralabial_3                   NA                    1
      R_post-infralabial_4                   NA                   NA
                           R_post-infralabial_3 R_post-infralabial_4
      rostral                                NA                   NA
      frontonasal                            NA                   NA
      frontal                                NA                   NA
      interparietal                          NA                   NA
      mental                                 NA                   NA
      post-mental                            NA                   NA
      L_internarial                          NA                   NA
      L_nasal                                NA                   NA
      L_ocular                               NA                   NA
      L_supralabial_1                        NA                   NA
      L_supralabial_2                        NA                   NA
      L_supralabial_3                        NA                   NA
      L_supralabial_4                        NA                   NA
      L_postocular_1                         NA                   NA
      L_postocular_2                         NA                   NA
      L_postfrontal_1                        NA                   NA
      L_infralabial                          NA                   NA
      L_post-infralabial_1                   NA                   NA
      L_post-infralabial_2                   NA                   NA
      L_post-infralabial_3                   NA                   NA
      L_post-infralabial_4                   NA                   NA
      R_internarial                          NA                   NA
      R_nasal                                NA                   NA
      R_ocular                               NA                   NA
      R_supralabial_1                        NA                   NA
      R_supralabial_2                        NA                   NA
      R_supralabial_3                        NA                   NA
      R_supralabial_4                        NA                    1
      R_postocular_1                         NA                   NA
      R_postocular_2                         NA                   NA
      R_postfrontal_1                        NA                   NA
      R_infralabial                           1                    1
      R_post-infralabial_1                   NA                   NA
      R_post-infralabial_2                    1                   NA
      R_post-infralabial_3                   NA                    1
      R_post-infralabial_4                    1                   NA

# correctly loads example

    Code
      verify_matrix(mat.4)
    Output
                           mouth body rostral frontonasal frontal interparietal
      mouth                   NA   NA       1           0       0             0
      body                    NA   NA       0           0       0             1
      rostral                  1   NA      NA          NA      NA            NA
      frontonasal             NA   NA      NA          NA       1            NA
      frontal                 NA   NA      NA           1      NA             1
      interparietal           NA    1      NA          NA       1            NA
      mental                   1   NA      NA          NA      NA            NA
      post-mental             NA    1      NA          NA      NA            NA
      L_internarial           NA   NA       3           1      NA            NA
      L_nasal                 NA   NA      NA           1      NA            NA
      L_ocular                NA   NA      NA           1       1            NA
      L_supralabial_1          1   NA       3          NA      NA            NA
      L_supralabial_2          1   NA      NA          NA      NA            NA
      L_supralabial_3          1   NA      NA          NA      NA            NA
      L_supralabial_4          1    1      NA          NA      NA            NA
      L_postocular_1          NA    1      NA          NA       1            NA
      L_postocular_2          NA    1      NA          NA      NA            NA
      L_postfrontal_1         NA    1      NA          NA       1             1
      L_infralabial            1   NA      NA          NA      NA            NA
      L_post-infralabial_1    NA    1      NA          NA      NA            NA
      L_post-infralabial_2    NA    1      NA          NA      NA            NA
      L_post-infralabial_3    NA    1      NA          NA      NA            NA
      L_post-infralabial_4    NA    1      NA          NA      NA            NA
      R_internarial           NA   NA       3           1      NA            NA
      R_nasal                 NA   NA      NA           1      NA            NA
      R_ocular                NA   NA      NA           1       1            NA
      R_supralabial_1          1   NA       3          NA      NA            NA
      R_supralabial_2          1   NA      NA          NA      NA            NA
      R_supralabial_3          1   NA      NA          NA      NA            NA
      R_supralabial_4          1    1      NA          NA      NA            NA
      R_postocular_1          NA    1      NA          NA       1            NA
      R_postocular_2          NA    1      NA          NA      NA            NA
      R_postfrontal_1         NA    1      NA          NA       1             1
      R_infralabial            1   NA      NA          NA      NA            NA
      R_post-infralabial_1    NA    1      NA          NA      NA            NA
      R_post-infralabial_2    NA    1      NA          NA      NA            NA
      R_post-infralabial_3    NA    1      NA          NA      NA            NA
      R_post-infralabial_4    NA    1      NA          NA      NA            NA
                           mental post-mental L_internarial L_nasal L_ocular
      mouth                     1           0             0       0        0
      body                      0           1             0       0        0
      rostral                  NA          NA             3      NA       NA
      frontonasal              NA          NA             1       1        1
      frontal                  NA          NA            NA      NA        1
      interparietal            NA          NA            NA      NA       NA
      mental                   NA           1            NA      NA       NA
      post-mental               1          NA            NA      NA       NA
      L_internarial            NA          NA            NA       3       NA
      L_nasal                  NA          NA             3      NA        1
      L_ocular                 NA          NA            NA       1       NA
      L_supralabial_1          NA          NA             3       3       NA
      L_supralabial_2          NA          NA            NA       1        1
      L_supralabial_3          NA          NA            NA      NA        1
      L_supralabial_4          NA          NA            NA      NA        1
      L_postocular_1           NA          NA            NA      NA        1
      L_postocular_2           NA          NA            NA      NA        1
      L_postfrontal_1          NA          NA            NA      NA       NA
      L_infralabial             1           1            NA      NA       NA
      L_post-infralabial_1     NA           1            NA      NA       NA
      L_post-infralabial_2     NA          NA            NA      NA       NA
      L_post-infralabial_3     NA          NA            NA      NA       NA
      L_post-infralabial_4     NA          NA            NA      NA       NA
      R_internarial            NA          NA             3      NA       NA
      R_nasal                  NA          NA            NA      NA       NA
      R_ocular                 NA          NA            NA      NA       NA
      R_supralabial_1          NA          NA            NA      NA       NA
      R_supralabial_2          NA          NA            NA      NA       NA
      R_supralabial_3          NA          NA            NA      NA       NA
      R_supralabial_4          NA          NA            NA      NA       NA
      R_postocular_1           NA          NA            NA      NA       NA
      R_postocular_2           NA          NA            NA      NA       NA
      R_postfrontal_1          NA          NA            NA      NA       NA
      R_infralabial             1           1            NA      NA       NA
      R_post-infralabial_1     NA           1            NA      NA       NA
      R_post-infralabial_2     NA          NA            NA      NA       NA
      R_post-infralabial_3     NA          NA            NA      NA       NA
      R_post-infralabial_4     NA          NA            NA      NA       NA
                           L_supralabial_1 L_supralabial_2 L_supralabial_3
      mouth                              1               1               1
      body                               0               0               0
      rostral                            3              NA              NA
      frontonasal                       NA              NA              NA
      frontal                           NA              NA              NA
      interparietal                     NA              NA              NA
      mental                            NA              NA              NA
      post-mental                       NA              NA              NA
      L_internarial                      3              NA              NA
      L_nasal                            3               1              NA
      L_ocular                          NA               1               1
      L_supralabial_1                   NA               2              NA
      L_supralabial_2                    2              NA               3
      L_supralabial_3                   NA               3              NA
      L_supralabial_4                   NA              NA               1
      L_postocular_1                    NA              NA              NA
      L_postocular_2                    NA              NA              NA
      L_postfrontal_1                   NA              NA              NA
      L_infralabial                     NA              NA              NA
      L_post-infralabial_1              NA              NA              NA
      L_post-infralabial_2              NA              NA              NA
      L_post-infralabial_3              NA              NA              NA
      L_post-infralabial_4              NA              NA              NA
      R_internarial                     NA              NA              NA
      R_nasal                           NA              NA              NA
      R_ocular                          NA              NA              NA
      R_supralabial_1                   NA              NA              NA
      R_supralabial_2                   NA              NA              NA
      R_supralabial_3                   NA              NA              NA
      R_supralabial_4                   NA              NA              NA
      R_postocular_1                    NA              NA              NA
      R_postocular_2                    NA              NA              NA
      R_postfrontal_1                   NA              NA              NA
      R_infralabial                     NA              NA              NA
      R_post-infralabial_1              NA              NA              NA
      R_post-infralabial_2              NA              NA              NA
      R_post-infralabial_3              NA              NA              NA
      R_post-infralabial_4              NA              NA              NA
                           L_supralabial_4 L_postocular_1 L_postocular_2
      mouth                              1              0              0
      body                               1              1              1
      rostral                           NA             NA             NA
      frontonasal                       NA             NA             NA
      frontal                           NA              1             NA
      interparietal                     NA             NA             NA
      mental                            NA             NA             NA
      post-mental                       NA             NA             NA
      L_internarial                     NA             NA             NA
      L_nasal                           NA             NA             NA
      L_ocular                           1              1              1
      L_supralabial_1                   NA             NA             NA
      L_supralabial_2                   NA             NA             NA
      L_supralabial_3                    1             NA             NA
      L_supralabial_4                   NA             NA              1
      L_postocular_1                    NA             NA              1
      L_postocular_2                     1              1             NA
      L_postfrontal_1                   NA              1             NA
      L_infralabial                      1             NA             NA
      L_post-infralabial_1              NA             NA             NA
      L_post-infralabial_2              NA             NA             NA
      L_post-infralabial_3              NA             NA             NA
      L_post-infralabial_4               1             NA             NA
      R_internarial                     NA             NA             NA
      R_nasal                           NA             NA             NA
      R_ocular                          NA             NA             NA
      R_supralabial_1                   NA             NA             NA
      R_supralabial_2                   NA             NA             NA
      R_supralabial_3                   NA             NA             NA
      R_supralabial_4                   NA             NA             NA
      R_postocular_1                    NA             NA             NA
      R_postocular_2                    NA             NA             NA
      R_postfrontal_1                   NA             NA             NA
      R_infralabial                     NA             NA             NA
      R_post-infralabial_1              NA             NA             NA
      R_post-infralabial_2              NA             NA             NA
      R_post-infralabial_3              NA             NA             NA
      R_post-infralabial_4              NA             NA             NA
                           L_postfrontal_1 L_infralabial L_post-infralabial_1
      mouth                              0             1                    0
      body                               1             0                    1
      rostral                           NA            NA                   NA
      frontonasal                       NA            NA                   NA
      frontal                            1            NA                   NA
      interparietal                      1            NA                   NA
      mental                            NA             1                   NA
      post-mental                       NA             1                    1
      L_internarial                     NA            NA                   NA
      L_nasal                           NA            NA                   NA
      L_ocular                          NA            NA                   NA
      L_supralabial_1                   NA            NA                   NA
      L_supralabial_2                   NA            NA                   NA
      L_supralabial_3                   NA            NA                   NA
      L_supralabial_4                   NA             1                   NA
      L_postocular_1                     1            NA                   NA
      L_postocular_2                    NA            NA                   NA
      L_postfrontal_1                   NA            NA                   NA
      L_infralabial                     NA            NA                    1
      L_post-infralabial_1              NA             1                   NA
      L_post-infralabial_2              NA             1                    1
      L_post-infralabial_3              NA             1                   NA
      L_post-infralabial_4              NA             1                   NA
      R_internarial                     NA            NA                   NA
      R_nasal                           NA            NA                   NA
      R_ocular                          NA            NA                   NA
      R_supralabial_1                   NA            NA                   NA
      R_supralabial_2                   NA            NA                   NA
      R_supralabial_3                   NA            NA                   NA
      R_supralabial_4                   NA            NA                   NA
      R_postocular_1                    NA            NA                   NA
      R_postocular_2                    NA            NA                   NA
      R_postfrontal_1                   NA            NA                   NA
      R_infralabial                     NA            NA                   NA
      R_post-infralabial_1              NA            NA                   NA
      R_post-infralabial_2              NA            NA                   NA
      R_post-infralabial_3              NA            NA                   NA
      R_post-infralabial_4              NA            NA                   NA
                           L_post-infralabial_2 L_post-infralabial_3
      mouth                                   0                    0
      body                                    1                    1
      rostral                                NA                   NA
      frontonasal                            NA                   NA
      frontal                                NA                   NA
      interparietal                          NA                   NA
      mental                                 NA                   NA
      post-mental                            NA                   NA
      L_internarial                          NA                   NA
      L_nasal                                NA                   NA
      L_ocular                               NA                   NA
      L_supralabial_1                        NA                   NA
      L_supralabial_2                        NA                   NA
      L_supralabial_3                        NA                   NA
      L_supralabial_4                        NA                   NA
      L_postocular_1                         NA                   NA
      L_postocular_2                         NA                   NA
      L_postfrontal_1                        NA                   NA
      L_infralabial                           1                    1
      L_post-infralabial_1                    1                   NA
      L_post-infralabial_2                   NA                    1
      L_post-infralabial_3                    1                   NA
      L_post-infralabial_4                   NA                    1
      R_internarial                          NA                   NA
      R_nasal                                NA                   NA
      R_ocular                               NA                   NA
      R_supralabial_1                        NA                   NA
      R_supralabial_2                        NA                   NA
      R_supralabial_3                        NA                   NA
      R_supralabial_4                        NA                   NA
      R_postocular_1                         NA                   NA
      R_postocular_2                         NA                   NA
      R_postfrontal_1                        NA                   NA
      R_infralabial                          NA                   NA
      R_post-infralabial_1                   NA                   NA
      R_post-infralabial_2                   NA                   NA
      R_post-infralabial_3                   NA                   NA
      R_post-infralabial_4                   NA                   NA
                           L_post-infralabial_4 R_internarial R_nasal R_ocular
      mouth                                   0             0       0        0
      body                                    1             0       0        0
      rostral                                NA             3      NA       NA
      frontonasal                            NA             1       1        1
      frontal                                NA            NA      NA        1
      interparietal                          NA            NA      NA       NA
      mental                                 NA            NA      NA       NA
      post-mental                            NA            NA      NA       NA
      L_internarial                          NA             3      NA       NA
      L_nasal                                NA            NA      NA       NA
      L_ocular                               NA            NA      NA       NA
      L_supralabial_1                        NA            NA      NA       NA
      L_supralabial_2                        NA            NA      NA       NA
      L_supralabial_3                        NA            NA      NA       NA
      L_supralabial_4                         1            NA      NA       NA
      L_postocular_1                         NA            NA      NA       NA
      L_postocular_2                         NA            NA      NA       NA
      L_postfrontal_1                        NA            NA      NA       NA
      L_infralabial                           1            NA      NA       NA
      L_post-infralabial_1                   NA            NA      NA       NA
      L_post-infralabial_2                   NA            NA      NA       NA
      L_post-infralabial_3                    1            NA      NA       NA
      L_post-infralabial_4                   NA            NA      NA       NA
      R_internarial                          NA            NA       3       NA
      R_nasal                                NA             3      NA        1
      R_ocular                               NA            NA       1       NA
      R_supralabial_1                        NA             3       3       NA
      R_supralabial_2                        NA            NA       1        1
      R_supralabial_3                        NA            NA      NA        1
      R_supralabial_4                        NA            NA      NA        1
      R_postocular_1                         NA            NA      NA        1
      R_postocular_2                         NA            NA      NA        1
      R_postfrontal_1                        NA            NA      NA       NA
      R_infralabial                          NA            NA      NA       NA
      R_post-infralabial_1                   NA            NA      NA       NA
      R_post-infralabial_2                   NA            NA      NA       NA
      R_post-infralabial_3                   NA            NA      NA       NA
      R_post-infralabial_4                   NA            NA      NA       NA
                           R_supralabial_1 R_supralabial_2 R_supralabial_3
      mouth                              1               1               1
      body                               0               0               0
      rostral                            3              NA              NA
      frontonasal                       NA              NA              NA
      frontal                           NA              NA              NA
      interparietal                     NA              NA              NA
      mental                            NA              NA              NA
      post-mental                       NA              NA              NA
      L_internarial                     NA              NA              NA
      L_nasal                           NA              NA              NA
      L_ocular                          NA              NA              NA
      L_supralabial_1                   NA              NA              NA
      L_supralabial_2                   NA              NA              NA
      L_supralabial_3                   NA              NA              NA
      L_supralabial_4                   NA              NA              NA
      L_postocular_1                    NA              NA              NA
      L_postocular_2                    NA              NA              NA
      L_postfrontal_1                   NA              NA              NA
      L_infralabial                     NA              NA              NA
      L_post-infralabial_1              NA              NA              NA
      L_post-infralabial_2              NA              NA              NA
      L_post-infralabial_3              NA              NA              NA
      L_post-infralabial_4              NA              NA              NA
      R_internarial                      3              NA              NA
      R_nasal                            3               1              NA
      R_ocular                          NA               1               1
      R_supralabial_1                   NA               2              NA
      R_supralabial_2                    2              NA               3
      R_supralabial_3                   NA               3              NA
      R_supralabial_4                   NA              NA               1
      R_postocular_1                    NA              NA              NA
      R_postocular_2                    NA              NA              NA
      R_postfrontal_1                   NA              NA              NA
      R_infralabial                     NA              NA              NA
      R_post-infralabial_1              NA              NA              NA
      R_post-infralabial_2              NA              NA              NA
      R_post-infralabial_3              NA              NA              NA
      R_post-infralabial_4              NA              NA              NA
                           R_supralabial_4 R_postocular_1 R_postocular_2
      mouth                              1              0              0
      body                               1              1              1
      rostral                           NA             NA             NA
      frontonasal                       NA             NA             NA
      frontal                           NA              1             NA
      interparietal                     NA             NA             NA
      mental                            NA             NA             NA
      post-mental                       NA             NA             NA
      L_internarial                     NA             NA             NA
      L_nasal                           NA             NA             NA
      L_ocular                          NA             NA             NA
      L_supralabial_1                   NA             NA             NA
      L_supralabial_2                   NA             NA             NA
      L_supralabial_3                   NA             NA             NA
      L_supralabial_4                   NA             NA             NA
      L_postocular_1                    NA             NA             NA
      L_postocular_2                    NA             NA             NA
      L_postfrontal_1                   NA             NA             NA
      L_infralabial                     NA             NA             NA
      L_post-infralabial_1              NA             NA             NA
      L_post-infralabial_2              NA             NA             NA
      L_post-infralabial_3              NA             NA             NA
      L_post-infralabial_4              NA             NA             NA
      R_internarial                     NA             NA             NA
      R_nasal                           NA             NA             NA
      R_ocular                           1              1              1
      R_supralabial_1                   NA             NA             NA
      R_supralabial_2                   NA             NA             NA
      R_supralabial_3                    1             NA             NA
      R_supralabial_4                   NA             NA              1
      R_postocular_1                    NA             NA              1
      R_postocular_2                     1              1             NA
      R_postfrontal_1                   NA              1             NA
      R_infralabial                      1             NA             NA
      R_post-infralabial_1              NA             NA             NA
      R_post-infralabial_2              NA             NA             NA
      R_post-infralabial_3              NA             NA             NA
      R_post-infralabial_4               1             NA             NA
                           R_postfrontal_1 R_infralabial R_post-infralabial_1
      mouth                              0             1                    0
      body                               1             0                    1
      rostral                           NA            NA                   NA
      frontonasal                       NA            NA                   NA
      frontal                            1            NA                   NA
      interparietal                      1            NA                   NA
      mental                            NA             1                   NA
      post-mental                       NA             1                    1
      L_internarial                     NA            NA                   NA
      L_nasal                           NA            NA                   NA
      L_ocular                          NA            NA                   NA
      L_supralabial_1                   NA            NA                   NA
      L_supralabial_2                   NA            NA                   NA
      L_supralabial_3                   NA            NA                   NA
      L_supralabial_4                   NA            NA                   NA
      L_postocular_1                    NA            NA                   NA
      L_postocular_2                    NA            NA                   NA
      L_postfrontal_1                   NA            NA                   NA
      L_infralabial                     NA            NA                   NA
      L_post-infralabial_1              NA            NA                   NA
      L_post-infralabial_2              NA            NA                   NA
      L_post-infralabial_3              NA            NA                   NA
      L_post-infralabial_4              NA            NA                   NA
      R_internarial                     NA            NA                   NA
      R_nasal                           NA            NA                   NA
      R_ocular                          NA            NA                   NA
      R_supralabial_1                   NA            NA                   NA
      R_supralabial_2                   NA            NA                   NA
      R_supralabial_3                   NA            NA                   NA
      R_supralabial_4                   NA             1                   NA
      R_postocular_1                     1            NA                   NA
      R_postocular_2                    NA            NA                   NA
      R_postfrontal_1                   NA            NA                   NA
      R_infralabial                     NA            NA                    1
      R_post-infralabial_1              NA             1                   NA
      R_post-infralabial_2              NA             1                    1
      R_post-infralabial_3              NA             1                   NA
      R_post-infralabial_4              NA             1                   NA
                           R_post-infralabial_2 R_post-infralabial_3
      mouth                                   0                    0
      body                                    1                    1
      rostral                                NA                   NA
      frontonasal                            NA                   NA
      frontal                                NA                   NA
      interparietal                          NA                   NA
      mental                                 NA                   NA
      post-mental                            NA                   NA
      L_internarial                          NA                   NA
      L_nasal                                NA                   NA
      L_ocular                               NA                   NA
      L_supralabial_1                        NA                   NA
      L_supralabial_2                        NA                   NA
      L_supralabial_3                        NA                   NA
      L_supralabial_4                        NA                   NA
      L_postocular_1                         NA                   NA
      L_postocular_2                         NA                   NA
      L_postfrontal_1                        NA                   NA
      L_infralabial                          NA                   NA
      L_post-infralabial_1                   NA                   NA
      L_post-infralabial_2                   NA                   NA
      L_post-infralabial_3                   NA                   NA
      L_post-infralabial_4                   NA                   NA
      R_internarial                          NA                   NA
      R_nasal                                NA                   NA
      R_ocular                               NA                   NA
      R_supralabial_1                        NA                   NA
      R_supralabial_2                        NA                   NA
      R_supralabial_3                        NA                   NA
      R_supralabial_4                        NA                   NA
      R_postocular_1                         NA                   NA
      R_postocular_2                         NA                   NA
      R_postfrontal_1                        NA                   NA
      R_infralabial                           1                    1
      R_post-infralabial_1                    1                   NA
      R_post-infralabial_2                   NA                    1
      R_post-infralabial_3                    1                   NA
      R_post-infralabial_4                   NA                    1
                           R_post-infralabial_4
      mouth                                   0
      body                                    1
      rostral                                NA
      frontonasal                            NA
      frontal                                NA
      interparietal                          NA
      mental                                 NA
      post-mental                            NA
      L_internarial                          NA
      L_nasal                                NA
      L_ocular                               NA
      L_supralabial_1                        NA
      L_supralabial_2                        NA
      L_supralabial_3                        NA
      L_supralabial_4                        NA
      L_postocular_1                         NA
      L_postocular_2                         NA
      L_postfrontal_1                        NA
      L_infralabial                          NA
      L_post-infralabial_1                   NA
      L_post-infralabial_2                   NA
      L_post-infralabial_3                   NA
      L_post-infralabial_4                   NA
      R_internarial                          NA
      R_nasal                                NA
      R_ocular                               NA
      R_supralabial_1                        NA
      R_supralabial_2                        NA
      R_supralabial_3                        NA
      R_supralabial_4                         1
      R_postocular_1                         NA
      R_postocular_2                         NA
      R_postfrontal_1                        NA
      R_infralabial                           1
      R_post-infralabial_1                   NA
      R_post-infralabial_2                   NA
      R_post-infralabial_3                    1
      R_post-infralabial_4                   NA

