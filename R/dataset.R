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
                           vars = "(^rile$)|(^per((\\d{3}(_\\d)?)|\\d{4})$)",
                           by = "year",
                           approx = zoo::na.approx,
                           ...)
  {
  
  curried_approx <- functional::Curry(approx, ...)
  the_approx <- function(x) {
    if (all(is.na(x))) {
      return(NA)
    } else {
      approx(x, ...)
    }
  }
    
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
#' The position of the median voter, calculated after Kim and Fording (1998; 2003),
#' with possible adjustment after McDonald 2002.
#' 
#' \code{median_voter} is able to compute the median voter positions for multiple
#' elections at once, while \code{median_voter_single} treats data as coming from
#' a single election.
#' 
#' @details
#' 
#' calculated according to the formula
#' by Kim and Fording (1998; 2003)
#' 
#' \deqn{m = L + \frac{K-C}{F} W}
#' 
#' Where m is the median voter position, L is lower end of the interval
#' containing the median, K is 0.5*sum(voteshare), C is the cumulative
#' vote share up to but not including the interval containing the median,
#' F is the vote share in the interval containing the median
#' and W is the width of the interval containing the median.
#' 
#' Different parties with the same left-right position 
#' (e.g. alliances) are treated as one party with the cumulative vote share.
#' 
#' In the adjusted formula the midpoint is "mirrored" from the midpoint of
#' the other side: "Rather than assuming the party's voters are so widely
#' dispersed, this variable assumes they are spread in a symmetrical
#' interval around the party's position. For example, for a leftmost
#' party at -15 and a 0 midpoint between it and an adjacent party on the
#' right, we assume the left boundary of that party's voters is -30." (McDonald 2002)
#' 
#' @references Kim, Heemin and Richard C. Fording (1998). "Voter ideology in
#' western democracies, 1946-1989". In: European Journal of Political Research
#' 33.1, 73-97. doi: 10.1111/1475-6765.00376.
#' 
#' Kim, Heemin and Richard C. Fording (2003). "Voter ideology in Western
#' democracies: An update". In: European Journal of Political Research 42.1,
#' 95-105.
#' 
#' McDonald, Michael D. (2002). Median Voters: 1950-1995.
#' url: www2.binghamton.edu/political-science/research/MedianVoter.doc
#' 
#' @param positions either a vector of values or (possible only for \code{median_voter}) a data.frame containing a column as
#' named in argument scale (default: rile) and one as named in argument voteshares (default: pervote);
#' @param voteshares either a vector of values or (possible only for \code{median_voter}) the name of a column in the data.frame
#' \code{positions} that contains the vote shares
#' @param scale variable of which to compute the median voter position (default: rile)
#' @param groups names of grouping variables to use for aggregation, default
#' results in one median voter position per election
#' @param ... further arguments passed to \code{\link{median_voter_single}}
#' 
#' @export
median_voter <- function(positions,
                         voteshares = "pervote",
                         scale = "rile",
                         groups = c("country", "edate"),
                         ...) {

  median_voter_params <- functional::Curry(median_voter_single, ...)

  if (is.numeric(positions) & is.numeric(voteshares)) {

    median_voter_params(positions, voteshares)

  } else if (is.data.frame(positions)) {

    the_positions <- unlist(positions[,scale])  ## unlist to normalize name
    the_voteshares <- unlist(positions[,voteshares])
    data.frame(the_positions, the_voteshares) %>%
       bind_cols(select_(positions, .dots = groups)) %>%  ## standard evaluation because of double name
      group_by_(.dots = groups) %>%
      summarise(median_voter = median_voter_params(the_positions, the_voteshares))

  } else {

    stop("Wrong input format for median_voter")

  }
  
}

lag_fill <- function(vec, val) {
  if (length(vec) == 0) {
    return(c())
  } else if (length(vec) == 1) {
    return(val)
  } else {
    return(c(val, vec[1:(length(vec)-1)]))
  }
  c(val, vec)
}

single_scalar <- function(vec, default = NA) {
  if (length(vec) >= 1) {
    return(vec[1])
  } else {
    return(default)
  }
}

#' @rdname median_voter
#' @param adjusted flag for adjustment after McDonald 2002
#' @param scalemin The minimum of the scale of the positions, used for computing the voter position intervals
#' @param scalemax The maximum of the scale of the positions, used for computing the voter position intervals
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
             contains_median = lag_fill(!above50, TRUE) & above50) %>%
      subset(contains_median) %>%
      summarise(median_voter = single_scalar(leftbound + (0.5 - cumvoteshare + voteshare) /
                                    voteshare*(rightbound-leftbound))) %>%
      unlist() %>%
      as.numeric()
}


#' Compute year from date variable in MPDS
#' 
#' @param mpds a dataframe in format of Manifesto Project Main Dataset
#' @return input data with year variable attached
#' @export
attach_year <- function(mpds) {
  mpds %>%
    mutate(year = substr(date, 1, 4))
}
