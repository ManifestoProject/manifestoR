#' Vanilla Scaling by Gabel & Huber
#' 
#' Computes scores based on the Vanilla method suggested by Gabel & Huber. 
#' A factor analysis identifies the dominant dimension in the data. 
#' Factor scores using the regression method are then considered as party positions on this dominant dimension. 
#'
#' @references Gabel, M. J., & Huber, J. D. (2000). Putting Parties in Their Place: Inferring Party Left-Right Ideological Positions from Party Manifestos Data. American Journal of Political Science, 44(1), 94-103.
#'
#' @param data a dataframe or matrix
#' @param vars variable names that should be used for the scaling (usually the variables per101,per102,...)
#' @param invert invert scores (to change the direction of the dimension to facilitate comparison with other indices) (default is FALSE)
#' @export
vanilla <- function(data,
                    vars = grep("per\\d{3}$", names(data), value=TRUE),
                    invert=FALSE) {
  fa.results <- psych::fa(data[,vars],1,scores="regression")
  vanilla.scores <- fa.results$scores[,1] 
  if (invert==TRUE) vanilla.scores <- vanilla.scores*-1
  return(vanilla.scores)
}
