# nur core möglich. for lr plus unendlich viele regressionen und abwägungen nötig

#' LR Scores by Jahn
#' 
#' Computes scores based on a multidemsional scaling.. 
#' X categories are derived deductively. Scores 
#' Factor scores using the regression method are then considered as party positions on this dominant dimension. 
#'
#' @param a dataframe or matrix
#' @param variable names that should be used for the scaling (usually the variables per101,per102,...)
#' @param invert scores
#' 
#' mds to get stimulus scores and then gl_scaling function
# 



jahn.lrcore.cats <- c(413, 412, 404, 403, 601, 603, 606, 401, 414, 505)
