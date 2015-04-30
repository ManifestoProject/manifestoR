#' Left-Right Scores based on Franzmann & Kaiser Method
#' 
#' Computes scores based on the Franzmann & Kaiser Method (see article..) 
#' issue structures are not calculated from scratch but taken as given from f&k
#'
#' @param a dataframe or matrix
#' @param variable names that should be used for the scaling (this can be used to scale positions on dimensions other than the left-right)
#' @param base values
#' @param smoothing
#' @export
franzmann <- function(data,
                      vars = grep("per\\d{3}$", names(data), value=TRUE),
                      basevalues=TRUE,
                      smoothing=TRUE) {
   
   if (basevalues == TRUE) {
      ## calculates positional scores = saliency scores - base value // pos scores = (x - min(x)) where x is saliency score
      data <- data %>%
         group_by(country,date) %>%
         #select(one_of(vars)) %>%
         mutate_each_(funs(base=.-min(., na.rm=TRUE)), vars) %>%
         ungroup()
   }

   data <- mutate(data,year=floor(date/100))
   fkweights <- read.csv(
     system.file("extdata", "fkweights.csv", package = "manifestoR"),
     sep=",") ## fkweights are in the same structure as the main dataset with var-weights having the same variable names as vars
   
   weights <- select(data,one_of("country","year")) %>% left_join(fkweights) # check again whether left_join is the correct join
   wweights <- weights %>% ungroup %>% select(one_of(vars))

   ## don't know why that works / I do not fully understand how the weighting matrix is used in the scale_gl function, but it outputs something 
   fkscores <- (scale_gl(data,vars=vars,weights=wweights)/scale_gl(data,vars=vars,weights=1))
   
   if (smoothing == TRUE) {
      combined <- cbind(data,fkscores)
      fkscores <- smooth_scores(data=combined,score="fkscores")
   }
   
   return(fkscores)

}

smooth_scores <- function(data,score) {
   
   ## smoothing procedure
   # smoothed score p_smoothed(t) = [ w(t-1)*p(t-1) + w(t)*p(t) + (w+1)*p(t+1) ] / 3
   # w(t) = leglen(t) / [leglen(t-1) + leglen(t) + leglen(t+1)]/3
   # leglen is the length of the electoral period in days

      # how to deal with "holes" (if party drops out of parliament and then enters later, sorting by party date, does not reflect this...)

   ## check if score is in data
   if (!score %in% names(data)) {
      stop("score not found in data")   
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

   data[,"festername"] <- data[,score]
   data$n <- c(1:nrow(data))
   smoothed <- data %>% 
      group_by(party) %>% 
      select(one_of("country","edate","festername","n")) %>% 
      arrange(edate) %>%
      mutate(
         leadedate = lead(edate),
         leglength = as.numeric(difftime(leadedate,edate,units="days")),
         #w_lag = lag(leglength)/((lag(leglength) + leglength + lead(leglength)/3)),
         w = leglength/((lag(leglength) + leglength + lead(leglength)/3)),
         #w_lead = lead(leglength)/(lag(leglength) + leglength + lead(leglength)),
         p_lag = lag(festername), 
         p = festername ,
         p_lead = lead(festername)
      )
      smoothed <- mutate(smoothed, smooth=(lag(w)*p_lag + w*p + lead(w)*p_lead)/3) %>%
      ungroup() %>% arrange(n) %>%
      select(smooth)
   
   return(smoothed)
}

#' Vanilla Scaling by Gabel & Huber
#' 
#' Computes scores based on the Vanilla method suggested by Gabel & Huber. 
#' A factor analysis identifies the dominant dimension in the data. 
#' Factor scores using the regression method are then considered as party positions on this dominant dimension. 
#'
#' @references Gabel, M. J., & Huber, J. D. (2000). Putting Parties in Their Place: Inferring Party Left-Right Ideological Positions from Party Manifestos Data. American Journal of Political Science, 44(1), 94-103.
#'
#' @param a dataframe or matrix
#' @param variable names that should be used for the scaling (usually the variables per101,per102,...)
#' @param invert scores (to change the direction of the dimension to facilitate comparison with other indices) (default is FALSE)
#' @export
vanilla <- function(data,
                    vars = grep("per\\d{3}$", names(data), value=TRUE),
                    invert=FALSE) {
  fa.results <- psych::fa(data[,vars],1,scores="regression")
  vanilla.scores <- fa.results$scores[,1] 
  if (invert==TRUE) vanilla.scores <- vanilla.scores*-1
  return(vanilla.scores)
}
  
#' Simple linear rescaling of values
#' 
#' @param indicates the minimum of the new scale (default is -1)
#' @param indicates the maximum of the new scale (default is +1) 
#' @param indicates the minimum of the existing scale. Can be used to rescale from a known theoretical scale (e.g. -100). If left empty the empirical minimum is used. 
#' @param indicates the maximum of the existing. See above.
#' 
rescale <- function(pos,newmin=-1,newmax=1,oldmin=min(pos),oldmax=max(pos)) {
   
   if(newmin>newmax & oldmin>oldmax) {
      stop("newmin > newmax or oldmin > oldmax")
   }
   
   if(!is.numeric(c(pos,newmin, newmax, oldmin, oldmax))) {
      stop("input variables are not numbers")
   }
   
   oldcenter <- (oldmax + oldmin)/2
   oldrange <- oldmax - oldmin
   newcenter <- (newmax + newmin)/2
   newrange <- newmax - newmin
   
   # shift center to zero
   newpos <- pos - oldcenter
   
   # stretch
   newpos <- newpos*newrange/oldrange
   
   # shift to new mean
   newpos <- newpos + newcenter

   return(newpos)
}



