#' Compute bootstrap distributions for scaling functions
#' 
#' Bootstrapping of distributions of scaling functions as described by
#' Benoit, Mikhaylov, and Laver (2009). Given a dataset with percentages of CMP
#' categories, for each case the distribution of categories is resampled from
#' a multinomial distribution and the scaling function computed for the resampled
#' values. Arbitrary statistics of the resulting bootstrap distribution can be
#' returned, such as standard deviation, quantiles, etc.
#'
#' @param data A data.frame with cases to be scaled and bootstrapped
#' @param fun function of a data row the bootstraped distribution of which is of interest
#' @param col_filter Regular expression matching the column names that should be
#' permuted for the resampling (usually and by default ther per variables)
#' @param statistics A list (!) of statistics to be computed from the bootstrap
#' distribution; defaults to standard deviation (\code{\link{sd}}). Must be
#' functions or numbers, where numbers are interpreted as quantiles.
#' @param N number of resamples to use for bootstrap distribution
#' @param ... more arguments passed on to \code{fun}
#' @references Benoit, K., Laver, M., & Mikhaylov, S. (2009). Treating Words as Data with Error: Uncertainty in Text Statements of Policy Positions. American Journal of Political Science, 53(2), 495-513. http://doi.org/10.1111/j.1540-5907.2009.00383.x
#' @importFrom stats sd
#' @export
mp_bootstrap <- function(data,
                         fun = rile,
                         col_filter = "per((\\d{3}(_\\d)?)|\\d{4}|(uncod))",
                         statistics = list(sd),
                         N = 1000,
                         ...) {  

  ## deparse arguments for conversion and naming
  stat_names <- sapply(substitute(statistics)[-1], as.character)
  quantiles <- sapply(statistics, is.numeric)
  stat_names[quantiles] <- paste0("q", stat_names[quantiles])
  stat_funs <- statistics
  stat_funs[quantiles] <- lapply(statistics[quantiles],
                                    function(q) {
                                      functional::Curry(stats::quantile, probs = q)
                                    })
  fun_name <- as.character(substitute(fun))

  ## how to bootstrap a single row
  bootstrap_row <- function(row) {

    total <- row$total
    to_permute <- grepl(col_filter, names(row)) & !is.na(row[1,])
    row_permute <- row[,to_permute] %>% mutate(rowid=1)
    row_dontpermute <- row[,!to_permute] %>% mutate(rowid=1)
    bootstrap_distribution <- do.call(
        fun,
        list(right_join(row_dontpermute, by = c("rowid"),
                       as.data.frame(t(stats::rmultinom(N, total, row_permute[1,])))/total * 100),
             ...))

    df <- bind_cols(data.frame(do.call(fun, list(row, ...))),
                    as.data.frame(lapply(stat_funs, do.call, list(bootstrap_distribution))))
    names(df) <- c(fun_name, stat_names)

    return(df)
  }

  ## do the bootstrapping for all rows
  bind_rows(lapply(1:nrow(data),
                   function(idx) {
                      bootstrap_row(data[idx,])
                   } ))

}
