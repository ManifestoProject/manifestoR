#' Interpolate values within election periods
#' 
#' As the Manifesto Project's variables are collected election-wise, values
#' for the time/years in between elections are not naturally available.
#' \code{mp_interpolate} allows to approximate them by several methods from
#' the abjacent observations.
#' 
#' @param df a data.frame with observations to be interpolated
#' @param vars a regular expression matching the names of the variables to be interpolated
#' @param by increment of the interpolation sequence, passed to \code{\link{seq.Date}}
#' @param approx Interpolation function, defaults to zoo's \code{\link{na.approx}}
#' @param ... Further arguments, passed on to approx
#'
#' @examples
#' \dontrun{
#' mp_interpolate(mp_maindataset(), method = "constant")
#' mp_interpolate(mp_maindataset(), approx = na.spline, maxgap = 3)
#' }
#' @export
mp_interpolate <- function(df,
                           vars = "(^rile$)|(^per\\d{3,4}$)",
                           by = "year",
                           approx = zoo::na.approx,
                           ...)
  {
  
  the_approx <- functional::Curry(approx, ...)
    
  lapply(unique(df$party), function(the_party) {

    df <- subset(df, party == the_party)
    
    if (nrow(df) > 1) {
      
      df %>%
        right_join(data.frame(edate = seq_Date_multi(df$edate, by = by),
                              party = the_party),
                   by = c("edate", "party")) %>%
        mutate_each_(funs(zoo(., edate) %>% the_approx() %>% as.numeric()),
                     vars = grep(vars, names(df), value = TRUE))
  
    } else {
      return(df)
    }
  }) %>% bind_rows()
  
}

seq_Date_multi <- function(dates, by) {
  
  dates <- mapply(seq.Date,
                  from = as.Date(lag(dates)[-1]),
                  to = as.Date(dates[-1]),
                  MoreArgs = list(by = by)) %>%
    list(last(dates)) %>%
    unlist() %>%
    as.Date(origin = "1970-01-01") %>%
    unique()
  
}

