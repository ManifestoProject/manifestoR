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
#' @param positions either a vector of values or a data.frame containing a column as
#' named in var (default: rile)
#' @param voteshare either a vector of values or the name of a column in the data.frame
#' x that contains the vote shares
#' @param scale variable of which to compute the median voter position (default: rile)
#' @param groups TODO
median_voter <- function(positions,
                         voteshares = "pervote",
                         scale = "rile",
                         groups = c("country", "edate"),
                         ...) {
  
  if (is.numeric(positions) & is.numeric(voteshares)) {
    median_voter_single(positions, voteshares)
  } else if (is.data.frame(positions)) {
    positions %>%
      group_by_(.dots = groups) %>%
      summarise(median_voter = median_voter_single(select_(., .dots = var),
                                                   select_(., .dots = voteshares)))
  } else {
    stop("Wrong input format for median_voter")
  }
  
}

median_voter_single <- function(positions,
                                voteshares,
                                adjusted = FALSE,
                                scalemin = -100,
                                scalemax = 100) {

  left_bounds <- function(position) {
    if (adjusted) {
      scalemin <- 2*position[1] - position[2]
      ((c(scalemin, position) + c(position, NA))/2)[1:length(position)]
    } else {
      c(scalemin, (position[-1] + position[-length(position)])/2)
    }
  }
  right_bounds <- function(position) {
    if (adjusted) {
      scalemax <- 2*position[length(position)] - position[length(position)-1]
      ((c(NA, position) + c(position, scalemax))/2)[2:(length(position)+1)]
    } else {
      c((position[-1] + position[1:(length(position)-1)])/2, scalemax)
    }
  }
  
  data.frame(position = positions[order(positions)],
             voteshare = voteshares[order(positions)]) %>%
      group_by(position) %>%
      summarize(voteshare = sum(voteshare)) %>%
      ungroup() %>%
      mutate(voteshare = voteshare/sum(voteshare),
             cumvoteshare = cumsum(voteshare)) %>% 
      mutate(leftbound = left_bounds(position),
             rightbound = right_bounds(position),
             above50 = cumvoteshare >= 0.5,
             contains_median = (c(TRUE, !above50[-length(positions)]) & above50)) %>%
      subset(contains_median) %>%
      summarise(median_voter = leftbound + (0.5 - cumvoteshare + voteshare) /
                                    voteshare*(rightbound-leftbound))
}

