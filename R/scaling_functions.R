#' Left-Right Scores based on Franzmann & Kaiser Method
#' 
#' Computes left-right scores based on the Franzmann & Kaiser Method (see
#' reference below). The issue structures are not calculated from scratch but
#' taken as given from Franzmann 2009. Note that they are not available for the
#' entire Manifesto Project Dataset, but only a subset of countries and elections.
#' 
#'
#' @param data A data.frame with cases to be scaled, variables named "per..."
#' @param basevalues flag for transforming data to be relative to the minimum
#' @param smoothing flag for using smoothing
#' @param fkweights alternative set of weights to use Franzmann & Kaiser method
#' @references Franzmann, Simon/ Kaiser, André (2006): Locating Political Parties in Policy Space. A Reanalysis of Party Manifesto Data, Party Politics, 12:2, 163-188
#' @references Franzmann, Simon (2009): The Change of Ideology: How the Left-Right Cleavage transforms into Issue Competition. An Analysis of Party Systems using Party Manifesto Data. PhD Thesis. Cologne.
#' @export
franzmann <- function(data,
                      basevalues = TRUE,
                      smoothing = TRUE,
                      vars = grep("per\\d{3}$", names(data), value = TRUE),
                      fkweights = read.csv(
                        system.file("extdata", "fkweights.csv", package = "manifestoR"),
                        sep=",")) {
   
   

   if(!("country" %in% names(data)) & ("party" %in% names(data))) {
     data <- mutate(data, country = as.integer(substr(party, 1, 2)))
   }

   if (basevalues) {
      ## calculates positional scores = saliency scores - base value // pos scores = (x - min(x)) where x is saliency score
      data <- data %>%
         group_by(country, date) %>%
         #select(one_of(vars)) %>%
         mutate_each_(funs(base=.-min(., na.rm=TRUE)), vars) %>%
         ungroup()
   }

   data %>%
     mutate(year = floor(date/100)) %>%
     select(one_of("country","year")) %>%
     left_join(fkweights) %>%
     select(one_of(vars)) %>%
     { scale_weighted(data, vars = vars, weights = .) /
                  scale_weighted(data, vars = vars, weights = 1) } -> fkscores
   
   if (smoothing) {
      combined <- cbind(data, fkscores)
      fkscores <- fk_smoothing(data = combined, score_name = "fkscores")
   }
   
   return( (fkscores + 1)*5 )

}

#' @export
#' @rdname franzmann
#' @param score_name name of variable with LR Score values to be smoothed
#' @param use_period_length whether to use electoral period length in weighting
fk_smoothing <- function(data, score_name, use_period_length = TRUE) {
  
   # how to deal with "holes" (if party drops out of parliament and then enters later, sorting by party date, does not reflect this...)

   ## check if score is in data
   if (!score_name %in% names(data)) {
      stop("score name not found in data")   
   }
   
   if (!"party" %in% names(data)) {
      stop("no party variable found in data")
   }
   if (!"edate" %in% names(data)) {
      stop("no date variable found in data")
   }
   if (!("Date" %in% class(data$edate))) {
      stop("variable date is not a date")
   }
  
   data[,"the_score"] <- data[,score_name]
   data$n <- c(1:nrow(data))
   smoothed <- data %>% 
      group_by(party) %>% 
      select(one_of("country","edate","the_score","n")) %>% 
      arrange(edate) %>%
      mutate(
         leadedate = lead(edate),
         leglength = as.numeric(difftime(leadedate, edate, units="days")),
         w = leglength/((lag(leglength) + leglength + lead(leglength)/3)),
         p_lag = lag(the_score),
         p = the_score ,
         p_lead = lead(the_score),
         smooth = ifelse(rep(use_period_length, n()),
                         (lag(w)*p_lag + w*p + lead(w)*p_lead)/3,
                         (lead(the_score) + the_score + lag(the_score))/3)) %>%
      ungroup() %>%
      arrange(n)
  
   return(smoothed$smooth)
}

#' Vanilla Scaling by Gabel & Huber
#' 
#' Computes scores based on the Vanilla method suggested by Gabel & Huber. 
#' A factor analysis identifies the dominant dimension in the data. 
#' Factor scores using the regression method are then considered as party positions on this dominant dimension. 
#'
#' @references Gabel, M. J., & Huber, J. D. (2000). Putting Parties in Their Place: Inferring Party Left-Right Ideological Positions from Party Manifestos Data. American Journal of Political Science, 44(1), 94-103.
#'
#' @param data A data.frame with cases to be scaled, variables named "per..."
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
