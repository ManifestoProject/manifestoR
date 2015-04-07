### Franzmann & Kaiser scaling

#' Left-Right Scores based on Franzmann & Kaiser Method
#' 
#' Computes scores based on the Franzmann & Kaiser Method (see article..) 
#' issue structures are not calculated from scratch but taken as given from f&k
#' 
#'
#' @param a dataframe or matrix
#' @param variable names that should be used for the scaling (this can be used to scale positions on dimensions other than the left-right)
#' @param base values
#' @param smoothing
#' 

franzmann <- function(data,
                      vars = grep("per[0-9]+", names(data), value=TRUE),
                      basevalues=TRUE,
                      smoothing=TRUE) {
   
   if (basevalues == TRUE) {
      ## calculates positional scores = saliency scores - base value // pos scores = (x - min(x)) where x is saliency score
      data <- data %>%
         group_by(country,date) %>%
         #select(one_of(vars)) %>%
         mutate_each_(funs(base=.-min(., na.rm=TRUE)), vars)
      
   }

   data <- mutate(data,year=floor(date/100))
   fkweights <- read.csv("R/fkweights.csv", sep=",") ## fkweights are in the same structure as the main dataset with var-weights having the same variable names as vars
   
   weights <- select(data,one_of("country","year")) %>% left_join(fkweights) # check again whether left_join is the correct join
   wweights <- weights %>% ungroup %>% select(one_of(vars))

   ## don't know why that works / I do not fully understand how the weighting matrix is used in the scale_gl function, but it outputs something 
   fkscores <- scale_gl(data,vars=vars,weights=wweights)
   
   if (smoothing == TRUE) {
      
   ## smoothing procedure
   # smoothed score p_smoothed(t) = [ w(t-1)*p(t-1) + w(t)*p(t) + (w+1)*p(t+1) ] / 3
   # w(t) = leglen(t) / [leglen(t-1) + leglen(t) + leglen(t+1)]
   # leglen is the length of the electoral period in days
 
   
   # maybe better to put the smoothing procedure in an extra function
   #fkscores <- smooth_scores(data,fkscores)  
   # does not work properly yet
   # how to deal with "holes" (if party drops out of parliament and then enters later, sorting by party date, does not reflect this...)

   combined <- cbind(data,fkscores)
   
   fkscores <- combined %>% group_by(party) %>% select(one_of("country","edate","date","fkscores")) %>% arrange(date) %>%
      mutate(
         leadedate = lead(edate),
         leglength = as.numeric(difftime(leadedate,edate,units="days")),
         w = leglength/(lag(leglength) + leglength + lead(leglength)),
         p_lag = lag(fkscores), 
         p = fkscores,
         p_lead = lead(fkscores)
         ) %>%
      transmute(fk=((lag(w)*p_lag + w*p + lead(w)*p_lead)/3))
   fkscores <- ungroup(fkscores)
   }
   
   return(fkscores)
}

