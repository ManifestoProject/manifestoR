#' Relative measure of party size (RMPS)
#' 
#' Computes the relative measure of party size as suggested by 
#' Giebler/Lacewell/Regel/Werner 2015.
#' 
#' @references Giebler/Lacewell/Regel/Werner (2015). Mass, Catch-all, or 
#' Programmatic? Toward an Empirical Classification of Party Types. Manuscript.
#' 
#' @param data a numerical vector with vote shares
#' @return a vector of rmps values
#' @export
mp_rmps <- function(data) {
  data <- data %>%
    data_frame(data = .) %>% 
    mutate(id = row_number())
  df <- data %>%
    select(-data) %>%
    mutate(id_2 = id) %>%
    expand.grid(.) %>%
    left_join(data %>% rename(data = data), by = "id") %>%
    left_join(data %>% rename(data_2 = data) %>% rename(id_2 = id), 
              by = "id_2") %>%
    group_by(id) %>%
      mutate(score = sum(data/data_2) - 1) %>%
      slice(1L) %>%
    ungroup() %>%
    mutate(score = score/sum(score))
  return(df$score)
}

#' Programmatic clarity measures (PC)
#' 
#' Computes party clarity measures suggested by 
#' Giebler/Lacewell/Regel/Werner 2015. 
#'
#' @references Giebler/Lacewell/Regel/Werner (2015). Mass, Catch-all, or 
#' Programmatic? Toward an Empirical Classification of Party Types. Manuscript.
#' 
#' @param data a dataframe or matrix in format of Manifesto Project Main Dataset
#' @param weighting_kind party or country-specific weighting of the dimensions
#' @param weighting_source name of variable with party importance weighting (can be rmps, pervote)
#' @param auto_rescale_weight rescale weights to 0-1
#' @param auto_rescale_variables rescale dimension variables to 0-1
#' @param dimensions dimensions to be used, must be in the format of the return value of \code{\link{clarity_dimensions}}
#' @return a vector of clarity values
#' @export
mp_clarity <- function(data,
                       weighting_kind = "party",
                       weighting_source = NULL,
                       auto_rescale_weight = TRUE,
                       auto_rescale_variables = TRUE,
                       dimensions = clarity_dimensions()) {
  
  # check validity of weighting value or make weigthing to be true/false dummies
  if (!(weighting_kind %in% c("party", "country"))) {
    stop(paste("Weighting kind", weighting_kind, 
               "not implemented!"))
  }
  if (weighting_kind == "country") {
    if (!(weighting_source %in% c("pervote", "rmps"))) {
      stop(paste("Weighting source", weighting_source, 
                 "not implemented!"))
    }
  } else {
    if (!(is.null(weighting_source))) {
      stop(paste("Weighting source", weighting_source, 
                 "must not be set if weighting kind is party"))
    }
  }
  
  if (is.null(weighting_source)) auto_rescale_weight = FALSE

  data <- data %>%
    { if (auto_rescale_weight) 
        mutate_each_(., funs(./sum(.)), weighting_source) 
      else 
        . } %>%
    { if (auto_rescale_variables) { 
        mutate_(., 
                .dots = setNames(
                  dimensions %>% 
                    unlist() %>% unique() %>% paste0(collapse = "+"),
                  "tmp_mp_clarity_sum")) %>%
        mutate_each_(funs(./tmp_mp_clarity_sum), 
                     dimensions %>% unlist() %>% unique()) %>%
        select(-tmp_mp_clarity_sum)
      }
      else {
        .
      }
    }
  
  dimensions %>%
    lapply(function(dimension) {
      
      score_dim <- abs(scale_bipolar(data,
                                     pos = dimension$pole_1,
                                     neg = dimension$pole_2))
      
      if (weighting_kind == "country") {
        
        sal_dim <- scale_weighted(data, unlist(dimension), weights = 1)
        
        return(score_dim/sal_dim * sum(data[,weighting_source] * sal_dim))
        
      } else {
        
        return(score_dim)
        
      }
    }) %>%
    as.data.frame() %>%
    rowSums(na.rm = TRUE)

}
