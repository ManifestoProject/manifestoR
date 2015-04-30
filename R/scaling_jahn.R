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
# mds to get stimulus scores and then gl_scaling function
# 


lrcore <- function(data,
                   vars, 
                   dims=1,
                   minpervote=2) {
  if (!minpervote==is.numeric | minpervote < 0 | minpervote > 99.9999) {
    stop("a problem with the pervotes")
  }
  # calculate distances
  
  # check if vars are in data
  distances <- proxy::simil(data[,vars],)
  distances <- as.matrix(distances, diag = 0)
  
  # mds
  ced <- cmdscale(distances, k = dims, eig = FALSE, add = FALSE, x.ret = FALSE)
  mdsweights <- ced[,1]
  
  scale_gl(data, vars,weights=mdsweights)
  return(lrcorescores)
}
# 
# jahn.lrcore.cats <- c(413, 412, 404, 403, 601, 603, 606, 401, 414, 505)
# 
# mpds <- mp_maindataset()
# germany <- filter(mpds, country==41) %>% 
#   #filter(date > 198000) %>%
#   select(1:76,rile)
# 
# 
# 
# 
# jahn.lrcore.cats <- c("per413", "per412", "per404", "per403", "per601", "per603", "per606", "per401", "per414", "per505")
# 
# 
# a <- germany[,jahn.lrcore.cats]
# 
# b <- simil(a,method="phi-squared", by_rows=FALSE, pairwise=TRUE)
# 
# 
# b2 <- as.matrix(b, diag=0)
# 
# b2
# ced <- cmdscale(b2, k = 2, eig = FALSE, add = FALSE, x.ret = FALSE)
# 
# plot(ced[,1],ced[,2])