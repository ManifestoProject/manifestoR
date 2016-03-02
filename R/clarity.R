#' Default programmatic clarity dimensions from 
#' Giebler/Lacewell/Regel/Werner 2015.
#'
#' @references Giebler/Lacewell/Regel/Werner (2015). Mass, Catch-all, or 
#' Programmatic? Toward an Empirical Classification of Party Types. Manuscript.
#'
#' @export
clarity_dimensions <- function() {
  list(
    "fsp" = list(pole_1 = c(101), pole_2 = c(102)),
    "mil" = list(pole_1 = c(104), pole_2 = c(105)),
    "int" = list(pole_1 = c(107), pole_2 = c(109)),
    "eui" = list(pole_1 = c(108), pole_2 = c(110)),
    "con" = list(pole_1 = c(203), pole_2 = c(204)),
    "cen" = list(pole_1 = c(301), pole_2 = c(302)),
    "mre" = list(pole_1 = c(401), pole_2 = c(403)),
    "pro" = list(pole_1 = c(406), pole_2 = c(407)),
    "fis" = list(pole_1 = c(409), pole_2 = c(414)),
    "wel" = list(pole_1 = c(504), pole_2 = c(505)),
    "edu" = list(pole_1 = c(506), pole_2 = c(507)),
    "nwl" = list(pole_1 = c(601), pole_2 = c(602)),
    "tmo" = list(pole_1 = c(603), pole_2 = c(604)),
    "mul" = list(pole_1 = c(607), pole_2 = c(608)),
    "lab" = list(pole_1 = c(701), pole_2 = c(702))
  )
}

#' Prefix dimensions
#'
#' @param dimensions a dimensions object (named list with pole_1, pole_2)
#' @param prefix a string prefix to use for all the values in pole_1, pole_2
#'
#' @export
prefix_dimensions <- function(dimensions, prefix) {
  return(dimensions %>%
    lapply(function(x) { 
      list(pole_1 = paste0(prefix, x$pole_1), pole_2 = paste0(prefix, x$pole_2))
    })
  )
}

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
#' @param weighting_source party importance weighting (can be rmps, pervote)
#' @param auto_rescale_weight rescale weights to 0-1
#' @param auto_rescale_variables rescale dimension variables to 0-1
#' @param dimensions dimensions to be used
#' @return a vector of clarity values
#' @export
mp_clarity <- function(data,
                       weighting_kind = "party",
                       weighting_source = NULL,
                       auto_rescale_weight = TRUE,
                       auto_rescale_variables = TRUE,
                       dimensions) {
  
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

  for (i in 1:length(dimensions)) {
    data <- data %>%
      mutate_(.dots = setNames(paste0(c(dimensions[[i]]$pole_1, 
                                        dimensions[[i]]$pole_2) %>% 
                                        paste0(collapse = " + ")), 
                               paste0("sal_dim_", i))) %>%
      mutate_(.dots = setNames(paste0("(",  
                                      dimensions[[i]]$pole_1 %>% 
                                        paste0(collapse = "+"),
                                      "-", 
                                      dimensions[[i]]$pole_2 %>%
                                        paste0(collapse = "+"), 
                                      ")"), 
                               paste0("score_dim_", i))) %>%
     {
        if (weighting_kind == "country") {
          mutate_(., 
                  .dots = setNames(
                    paste0("abs(score_dim_", i, ") / (sal_dim_", i, ")", 
                           "* sum(", weighting_source, " * sal_dim_", i, ")"), 
                    paste0("score_dim_", i)))
        } else {
          mutate_(., 
                  .dots = setNames(
                    paste0("abs(score_dim_", i, ") / (sal_dim_", i, ")", 
                           "* sal_dim_", i), 
                    paste0("score_dim_", i)))
        }
     }
  }
  
  data <- data %>%
    mutate_each(funs(replace(., is.na(.), 0)), matches("^score_")) %>%
    mutate_(.dots = setNames(
              seq(1:length(dimensions)) %>% 
                paste0("score_dim_", .) %>%
                paste0(collapse = " + "), 
              "pc"))

  return(data$pc)
}
