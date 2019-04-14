# file id -----------------------------------------------------------------
# @author: josh weinflash
# @created: 2019-04-05
# @purpose: helper functions
# -------------------------------------------------------------------------

my_score_bin = function(score_diff) {
  
  v_brks = c("21+"   = -21,
             "16-20" = -16,
             "11-15" = -11,
             "6-10"  = -6,
             "1-5"   = -1,
             "0"     = 0,
             "1-5"   = 5,
             "6-10"  = 10,
             "11-15" = 15,
             "16-20" = 20,
             "21+"   = Inf)
  
  names(v_brks) = dplyr::case_when(v_brks == 0 ~names(v_brks), 
                                   v_brks < 0 ~ sprintf("Loss\n%s pts", names(v_brks)),
                                   TRUE ~ sprintf("Win\n%s pts", names(v_brks)))
  
  #names(v_brks) = sprintf("%s\n%s pts", ifelse(v_brks < 0, "Loss", "Win"), names(v_brks))
  
  v_out = vector("character", length(score_diff))
  
  for (i in seq_along(score_diff)) {
    
    for (j in seq_along(v_brks)) {
      
      if (score_diff[i] <= v_brks[j]) {
        
        v_out[i] = names(v_brks)[j]
        break
      }
    }
  }
  
  v_out = forcats::fct_expand(v_out, names(v_brks))
  v_out = forcats::fct_relevel(v_out, names(v_brks))
  
  return(v_out)
}


