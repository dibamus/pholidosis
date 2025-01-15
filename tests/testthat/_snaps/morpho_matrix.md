# produces correct character matrix

    Code
      morpho_matrix(simpleGs)
    Condition
      Warning:
      There were 2 warnings in `mutate()`.
      The first warning was:
      i In argument: `character = paste(cur_data()[[1]], "+", cur_data()[[2]])`.
      Caused by warning:
      ! `cur_data()` was deprecated in dplyr 1.1.0.
      i Please use `pick()` instead.
      i Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.
    Output
         A + B B + C C + D D + A A + C B + D A + E D + E E + C E + B
      g1     1     1     1     1     1    NA    NA    NA    NA    NA
      g2     1     1     1     1    NA     1    NA    NA    NA    NA
      g3     1     1     1     1     3    NA    NA    NA    NA    NA
      g4     1     1     1     1     1    NA     1    NA    NA    NA
      g5     1     1     1     1     1    NA     1     1    NA    NA
      g6     1     1     1    NA    NA    NA     1     1     1     1

