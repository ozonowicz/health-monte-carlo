
approx_money <- function(count, s2, s3, s4, s44, s5, i){
  
  value_m2 = 0; value_m3 = 0; value_m4 = 0; value_m5 = 0; 
  value_w2 = 0; value_w3 = 0; value_w4 = 0; value_w5 = 0; 
  p_count_m = 0
  p_count_w = 0
  for (f in (1:count)) {
    m_rand <- random_symulation('man', 420)
    w_rand <- random_symulation('woman', 420)  
    m <- vector_help(m_rand, s2, s3, s4, s44, s5, i)
    w <- vector_help(w_rand, s2, s3, s4, s44, s5, i)
    
    value_m2 = value_m2 + m$result2;                  value_w2 = value_w2 + w$result2 
    value_m3 = value_m3 + m$result3;                  value_w3 = value_w3 + w$result3
    value_m4 = value_m4 + m$result4;                  value_w4 = value_w4 + w$result4 
    value_m5 = value_m5 + m$result5;                  value_w5 = value_w5 + w$result5
    p_count_m = p_count_m + annuity_help(m_rand, i);  p_count_w = p_count_w + annuity_help(w_rand, i)
  }
  return(c('Ew_m' = (value_m2+value_m3+value_m4+value_m5)/count, 'Ew_m2' = value_m2/count, 'Ew_m3' = value_m3/count, 'Ew_m4' = value_m4/count, 'Ew_m5' = value_m5/count, 'E_annuity_m' = (value_m2+value_m3+value_m4+value_m5)/p_count_m, 
           'Ew_w' = (value_w2+value_w3+value_w4+value_w5)/count, 'Ew_w2' = value_w2/count, 'Ew_w3' = value_w3/count, 'Ew_w4' = value_w4/count, 'Ew_w5' = value_w5/count, 'E_annuity_W' = (value_w2+value_w3+value_w4+value_w5)/p_count_w))
}
