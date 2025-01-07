# produces complete g1 data frame

    Code
      graph_edit_distance(simpleGs$g5, simpleGs$g6)$g1.df
    Output
        X1 X2 matched uniqueVs altpath altpath_allmissing topochange unresolved
      1  A  B   FALSE    FALSE      NA              FALSE          1       TRUE
      2  B  C    TRUE    FALSE      NA              FALSE         NA      FALSE
      3  C  D    TRUE    FALSE      NA              FALSE         NA      FALSE
      4  D  A    TRUE    FALSE      NA              FALSE         NA      FALSE
      5  A  C   FALSE    FALSE      NA              FALSE          1       TRUE
      6  A  E    TRUE    FALSE      NA              FALSE         NA      FALSE
      7  B  E    TRUE    FALSE      NA              FALSE         NA      FALSE

# produces complete g2 data frame

    Code
      graph_edit_distance(simpleGs$g5, simpleGs$g6)$g2.df
    Output
        X1 X2 matched uniqueVs altpath altpath_allmissing topochange unresolved
      1  E  C   FALSE    FALSE      NA              FALSE          1       TRUE
      2  B  C    TRUE    FALSE      NA              FALSE         NA      FALSE
      3  C  D    TRUE    FALSE      NA              FALSE         NA      FALSE
      4  D  A    TRUE    FALSE      NA              FALSE         NA      FALSE
      5  B  D   FALSE    FALSE      NA              FALSE          1       TRUE
      6  A  E    TRUE    FALSE      NA              FALSE         NA      FALSE
      7  B  E    TRUE    FALSE      NA              FALSE         NA      FALSE

