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

#' Median Voter position
#' 
#' @param x either a vector of values or a data.frame containing a column as
#' named in var (default: rile)
#' @param voteshare either a vector of values or the name of a column in the data.frame
#' x that contains the vote shares
#' @param var variable of which to compute the median voter position (default: rile)
#' @param groups TODO
median_voter <- function(x,
                         voteshares = "pervote",
                         var = "rile",
                         groups = c("country", "edate")) {
  
  if (is.data.frame(x)) {
    x <- ## group_by_(x, .dots = groups)
    x <- unlist(x[,var])
  }
  
  
}

median_voter_single <- function(positions, voteshares) {
    
  df <- data.frame(position = positions[order(positions)],
                   voteshare = voteshares[order(positions)]) %>%
          group_by(position) %>%
          summarize(voteshare = sum(voteshare)) %>%
          ungroup() %>%
          mutate(cumvoteshare = cumsum(voteshare)/sum(voteshare),
                 above50 = cumvoteshare >= 0.5,
                 median_neighbours = (!lag(above50) & above50) |
                                     (lead(above50) & !above50))
  print(df)
  ## TODO implement adjustment
  
  if (all(!is.na(df$median_neighbours)) & sum(df$median_neighbours) == 2) {
#     thresh <- 0.5*sum(df$voteshare)
    df %>%
      subset(median_neighbours) %>%
#       The Lewandowski method
      mutate(weights = abs(cumvoteshare - 0.5)) %>%
      summarise(median_voter = sum(position * weights)/sum(weights))
#       summarise(median_voter = position[1] + (thresh - cumvoteshare[1])/voteshare[2]*(position[2]-position[1]))
  } else {
    df[1,"position"]
  }
}

