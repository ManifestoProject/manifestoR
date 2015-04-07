#' Vanilla Scaling by Gabel & Huber
#' 
#' Computes scores based on the Vanilla method suggested by Gabel & Huber. 
#' A factor analysis identifies the dominant dimension in the data. 
#' Factor scores using the regression method are then considered as party positions on this dominant dimension. 
#' Gabel, M. J., & Huber, J. D. (2000). Putting Parties in Their Place: Inferring Party Left-Right Ideological Positions from Party Manifestos Data. American Journal of Political Science, 44(1), 94–103.
#'
#' @param a dataframe or matrix
#' @param variable names that should be used for the scaling (usually the variables per101,per102,...)
#' @param invert scores (to change the direction of the dimension to facilitate comparison with other indices) (default is FALSE)

vanilla <- function(data,
                     vars = grep("per[0-9]*", names(data), value=TRUE),
                     invert=FALSE) {
   fa.results <- fa(data[,vars],1,scores="regression")
   vanilla.scores <- fa.results$scores[,1] 
   if (invert==TRUE) vanilla.scores <- vanilla.scores*-1
   return(vanilla.scores)
}

