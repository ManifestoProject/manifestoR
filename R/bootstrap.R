#' Compute bootstrap distributions for scaling functions
#' 
#' TODO
#' 
#' TODO reference Benoit
#'
#' @param data A data.frame with cases to be scaled and bootstrap 
#' @param fun name of a function of a data row the bootstraped distribution of which is of interest
#' @param col_filter Regular expression matching the column names that should be
#' permuted for the resampling (usually and by default ther per variables)
#' @param statistics List of statistics to be computed from the bootstrap
#' distribution; defaults to standard deviation (\code{\link{sd}}). Must be characters,
#' calling the functions with the given names or numbers resulting in quantile
#' computations. Note that if both characters and numbers are mixed, they must
#' be combined in a list and not a vector to prevent R's automatic conversion in vectors.
#' @param N number of resamples to use for bootstrap distribution
#' @export
mp_bootstrap <- function(data,
                         fun_name = "rile",
                         col_filter = "per(\\d{3,4}|uncod)",
                         statistics = list("sd"),
                         N = 1000,
                         ...) {
  
  bootstrap_row <- function(row) {
    
    total <- row$total
    row <- select(row, matches(col_filter))
    
    bootstrap_distribution <- do.call(fun_name,
                                      list(resample_row(row, N, total), ...))
    
#     df$sd <- sd(bootstrap_distribution)
    bind_cols(single_col_df(fun_name, do.call(fun_name, list(row), ...)),
              bind_cols(lapply(statistics, function(stat) {
                if (is.numeric(stat)) {
                  stat_fun <- functional::Curry(quantile, probs = stat)
                  stat <- paste0("q", format(stat, digits = 3, scientific = FALSE))
                } else if (is.function(stat)) {
                  stat_fun <- stat
                  stat <- "anonymous_function"
                } else {
                  stat_fun <- stat
                }
                single_col_df(stat, do.call(stat_fun, list(bootstrap_distribution)))
              })))
    
#     return(df)
  }
  
#   bind_cols(data,
            bind_rows(lapply(1:nrow(data),
                             function(idx) {
                               bootstrap_row(data[idx,])
                             } ))
#   )
  
}

resample_row <- function(row, N, total) {
  if(nrow(row) > 1) {
    warning("resample_row called on more than one row, this creates an invalid distribution!")
  }
  df <- as.data.frame(t(rmultinom(N, total, row[1,])))/total * 100 ## pers
  
}

single_col_df <- function(colname, values) {
  
  df <- data.frame(value = values)
  names(df) <- colname
  
  return(df)
  
}

