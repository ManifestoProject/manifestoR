#' Relative measure of party size (RMPS)
#' 
#' Computes the relative measure of party size as suggested by 
#' Giebler/Lacewell/Regel/Werner 2015.
#' 
#' Hint: In a dataset with multiple elections the usage of the function
#' might require to calculate the measure per election (eg. using group_by)
#' 
#' @references Giebler, Heiko, Onawa Promise Lacewell, Sven Regel and Annika Werner. 2015. 
#' Niedergang oder Wandel? Parteitypen und die Krise der repraesentativen Demokratie. 
#' In Steckt die Demokratie in der Krise?, ed. Wolfgang Merkel, 181-219. Wiesbaden: Springer VS.
#' 
#' @param data a numerical vector with vote shares
#' @return a vector of rmps values
#' @export
mp_rmps <- function(data) {
  data <- data %>%
    data_frame(data = .) %>% 
    mutate(id = row_number())
  data %>%
    select(-data) %>%
    mutate(id_2 = id) %>%
    expand.grid(.) %>%
    left_join(data %>% rename(data = data), by = "id") %>%
    left_join(data %>% rename(data_2 = data) %>% rename(id_2 = id), 
              by = "id_2") %>%
    group_by(id) %>%
      summarise(score = sum(data/data_2) - 1) %>%
    ungroup() %>%
    mutate(score = score/sum(score)) %>%
    .$score
}

#' Programmatic clarity measures (PC)
#' 
#' Computes party clarity measures suggested by 
#' Giebler/Lacewell/Regel/Werner 2015. 
#'
#' @references Giebler, Heiko, Onawa Promise Lacewell, Sven Regel and Annika Werner. 2015. 
#' Niedergang oder Wandel? Parteitypen und die Krise der repraesentativen Demokratie. 
#' In Steckt die Demokratie in der Krise?, ed. Wolfgang Merkel, 181-219. Wiesbaden: Springer VS.
#'
#' @param data a dataframe in format of Manifesto Project Main Dataset
#' @param weighting_kind manifesto or election-specific weighting of the dimensions
#' @param weighting_source name of variable with party importance (likely its importance within an election) weighting (can be rmps, pervote)
#' @param auto_rescale_weight rescale party importance weighting within elections to 0-1
#' @param auto_rescale_variables rescale dimension variables to 0-1
#' @param dimensions dimensions to be used, must be in the format of the return value of \code{\link{clarity_dimensions}}
#' @return a vector of clarity values
#' @export
mp_clarity <- function(data,
                       weighting_kind = "manifesto",
                       weighting_source = NULL,
                       auto_rescale_weight = TRUE,
                       auto_rescale_variables = TRUE,
                       dimensions = clarity_dimensions()) {
  
  # check validity of weighting value or make weigthing to be true/false dummies
  if (weighting_kind == "party") {
    stop(paste("Weighting kind", weighting_kind, 
               "not implemented! It used to refer to the implementation of the \"manifesto\"-based weighting"))
  }
  if (weighting_kind == "country") {
    stop(paste("Weighting kind", weighting_kind, 
               "not implemented! It used to refer to a wrong implementation of the election-based weighting 
               (the correct implementation is now accessible via \"election\")"))
  }
  if (!(weighting_kind %in% c("manifesto", "election"))) {
    stop(paste("Weighting kind", weighting_kind, 
               "not implemented!"))
  }
  if (weighting_kind == "party" && !(is.null(weighting_source))) {
    stop(paste("Weighting source", weighting_source, 
               "must not be set if weighting kind is party"))
  }
  
  if (is.null(weighting_source)) auto_rescale_weight = FALSE
  
  dimension_categories = dimensions %>% unlist() %>% unique()
  
  data <- data %>%
    { if (auto_rescale_weight)
        group_by(., country, edate) %>%
          mutate_at(., weighting_source, funs(./sum(.))) %>%
        ungroup()
      else 
        . } %>%
    { if (auto_rescale_variables) {
        mutate(., tmp_mp_clarity_sum = rowSums(select_(., .dots = dimension_categories))) %>%
        mutate_at(dimension_categories, funs(./tmp_mp_clarity_sum)) %>%
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
      
      sal_dim <- scale_weighted(data, unlist(dimension), weights = 1)
      
      if (weighting_kind == "election") {
        
        data$sal_dim <- sal_dim
        weight <- data %>%
          group_by(country, edate) %>%
            mutate_(.dots = setNames(paste("sum(", weighting_source, " * sal_dim)"), "weight")) %>%
          ungroup() %>%
          .$weight

        return(score_dim / sal_dim * weight)
        
      } else {
        
        return(score_dim)
        
      }
    }) %>%
    as.data.frame() %>%
    rowSums(na.rm = TRUE)

}
