#' Party nicheness measures
#' 
#' Computes party nicheness measures suggested by Bischof 2015 and Meyer and Miller 2013. 
#'
#' @references Bischof, D. (2015). Towards a Renewal of the Niche Party Concept Parties, Market Shares and Condensed Offers. Party Politics.
#' @references Meyer, T.M., & Miller, B. (2013). The Niche Party Concept and Its Measurement. Party Politics 21(2): 259-271.
#' @references Baeck, H., Debus, M., & Dumont, P. (2010). Who gets what in coalition governments? Predictors of portfolio
#' allocation in parliamentary democracies. European Journal of Political Research 50(4): 441-478.
#' 
#' @param data a dataframe or matrix in format of Manifesto Project Main Dataset
#' @param method choose between bischof and meyermiller
#' @param ... parmaeters passed on to specialized functions for differnet methods
#' @export
mp_nicheness <- function(data,
                         method = "bischof",
                         ... ) {
  switch(method,
         "bischof" = nicheness_bischof(data),
         "meyermiller" = nicheness_meyer_miller(data),
         stop(paste("Nicheness method", method, "not implemented!"))
  )
}

meyer_miller_single_election <- function(election_data,
                                         vars,
                                         weights,
                                         party_system_normalization = TRUE,
                                         only_non_zero = TRUE) {
  
  if (is.character(weights)) {
    weights <- unlist(election_data[,weights])
  }
  
  if (only_non_zero) {
    ## kick out variables that are 0 for everyone
    vars <- subset(vars, (election_data %>%
                            select(one_of(vars)) %>%
                            colSums()) > 0.0)
  }
  
  for (name in vars) {
    election_data[,name] <- (election_data[,name] - 
                               rival_mean(election_data[,name], weights = weights))^2
  }
  
  election_data %>%
    select(one_of(vars)) %>%
    rowSums() %>%
    { sqrt( . / length(vars)) } %>%
    iff(party_system_normalization, function(.) { . - rival_mean(., weights = weights) } )

}

rival_mean <- function(x, weights = 1) {
  
  if (length(weights) == 1) {
    weights <- rep(weights, length(x))
  }
  
  (x*weights) %>%
    { sum(.) - . } %>%
    { . / (sum(weights) - weights)}
}

#'
#' @param groups groups of issues to determine niches/policy dimensions; formatted as named lists
#' variable names. For Meyer & Miller: Defaults to adapted version of Baeck et. al 2010 Policy dimensions
#' (without industry, as used in the original paper by Meyer & Miller). For Bischof: defaults
#' to issue groups used in the Bischof 2015 paper
#' @param transform transform to apply to each of the group indicators. Can be a function,
#' character "bischof" to apply log(x + 1), or NULL for no transformation.
#' @param smooth Smoothing of policy dimension values before nicheness computation, as suggested
#' and used by Bischof 2015
#' @param weights vector of the length nrow(data) or the name of a variable in data; is used to
#' weight mean party system position and nicheness; defaults to "pervote" as in Meyer & Miller 2013
#' @param party_system_normalization normalize nicheness result within election (substract weighted mean nicheness)
#' @param only_non_zero When dividing by the number of policy dimensions used for nicheness
#' estimation, ignore dimensions that are zero for all parties (election-wise)
#' @rdname mp_nicheness
#' @export
nicheness_meyer_miller <- function(data,
                                   groups = meyer_miller_2013_policy_dimensions(),
                                   transform = NULL,
                                   smooth = FALSE,
                                   weights = "pervote",
                                   party_system_normalization = TRUE,
                                   only_non_zero = TRUE) {
  
  if (!is.null(transform) && 
      is.character(transform) &&
      transform == "bischof") {
    transform <- function(x) { log(x + 1) }
  } 
  
  nicheness <- data %>%
    aggregate_pers(groups = groups,
                   keep = TRUE) %>%
    iff(!is.null(transform), mutate_each_, funs = funs(transform), vars = names(groups)) %>%
    group_by(party) %>%
    arrange(date) %>%
    iff(smooth, mutate_each_, funs = funs((. + lag(.))/2), vars = names(groups)) %>%  ## TODO think about this
    ungroup() %>%
    { split(., factor(paste0(.$country, .$date, sep = "_"))) } %>%
    lapply(arrange_, "party") %>%
    lapply(as.data.frame) %>%  ## fix necessary due to split
    lapply(meyer_miller_single_election,
           vars = names(groups),
           weights = weights,
           party_system_normalization = party_system_normalization,
           only_non_zero = only_non_zero) %>%
    unlist()
  
  data %>%
    select(one_of(c("country", "party", "date"))) %>%
    arrange(country, date, party) %>%
    mutate(nicheness = nicheness)
    
}

diversification <- function(data, groups) {
  
  data %>%
    select(one_of(groups)) %>%
    { . / rowSums(.) } %>%
    mutate_each_(funs( -. * log_0(.)), vars = groups) %>%
    rowSums()
}

#'
#' @details 
#' List of possible outputs of \code{nicheness_bischof}:
#' 
#' diversification: Shannon's entropy $s_p$ in Bischof 2015
#' 
#' max_divers: used maximum for diversification
#' 
#' min_divers: used minimum for diversification
#' 
#' specialization: inverted diversification
#' 
#' specialization_stand: standardized specialization
#' 
#' nicheness: nicheness according to Meyer & Miller 2013 without vote share weighting
#' 
#' nicheness_stand: standardized nicheness
#' 
#' nicheness_two: sum of nicheness_stand and specialization_stand as proposed by Bischof 2015
#' 
#' 
#' @param out_variables names of variables to return in data.frame. Can be any
#' from the input or that are generated during the computation of Bischof's nicheness
#' measure. See details for a list.
#' @param diversification_bounds Bounds of the range of the diversification measure
#' (Shannon's entropy $s_p$ in Bischof 2015), used for inversion and normalization;
#' default to the theoretical bounds of the entropy of a distribution on 5 discrete
#' elements. If "empirical", the empirical max and min of the diversification measure
#' are used
#' @rdname mp_nicheness
#' @export
nicheness_bischof <- function(data,
                              out_variables = c("party",
                                                "date",
                                                "specialization",
                                                "nicheness",
                                                "nicheness_two"),
                              groups = bischof_issue_groups(),
                              diversification_bounds = c(
                                0.0,
                                rep(1/length(groups), length(groups)) %>%
                                  { -(. * log(.)) } %>%
                                   sum()),
                              smooth = function(x) {
                                (x + lag(x, default = first(first(x))))/2
                              }) {

  data %>%
    aggregate_pers(groups = groups,
                   keep = TRUE) %>%
    mutate_each_(funs(log(. + 1)), vars = names(groups)) %>%
    # smooth with lag
    group_by(party) %>%
    arrange(date) %>%
    iff(is.function(smooth), mutate_each_, funs(smooth), vars = names(groups)) %>%
    ungroup() %>%
    { mutate(., diversification = diversification(., names(groups))) } %>%
    mutate(
      max_divers = ifelse(all(diversification_bounds == "empirical"),
                          max(diversification, na.rm=TRUE),
                          diversification_bounds[2]),
      min_divers = ifelse(all(diversification_bounds == "empirical"),
                          min(diversification, na.rm=TRUE),
                          diversification_bounds[1]),
      specialization = (min_divers + max_divers) - diversification
    ) %>% 
    { split(., factor(paste0(.$country, .$date, sep = "_"))) } %>%
    lapply(arrange_, "party") %>%
    lapply(as.data.frame) %>%  ## fix necessary due to split
    lapply(function(x) {
      mutate(x, nicheness = meyer_miller_single_election(x,
                               vars = names(groups),
                               weights = 1,
                               party_system_normalization = TRUE,
                               only_non_zero = TRUE))}) %>%
    bind_rows() %>%
    mutate(
      max_nic = max(nicheness, na.rm=TRUE),
      min_nic = min(nicheness, na.rm=TRUE),
      nicheness_stand = (nicheness - min_nic)/(max_nic - min_nic),
      
      max_spec = max(specialization, na.rm=TRUE),
      min_spec = min(specialization, na.rm=TRUE),
      
      specialization_stand = (specialization-min_spec)/(max_spec-min_spec),
      specialization_stand_two = ifelse(is.na(specialization_stand),0,specialization_stand), 
      # nicheness two
      nicheness_two = nicheness_stand + specialization_stand_two) %>%
    select(one_of(out_variables))
}

