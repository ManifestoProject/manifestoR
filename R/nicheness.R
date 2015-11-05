#' Party nicheness measures
#' 
#' Computes party nicheness measures suggested by Bischof 2015, Meyer and Miller 2013 and Wagner 2012. 
#'
#' @references Bischof, D. (2015). Towards a Renewal of the Niche Party Concept Parties, Market Shares and Condensed Offers. Party Politics.
#' @references Meyer, T.M., & Miller, B. (2013). The Niche Party Concept and Its Measurement. Party Politics 21(2): 259-271.
#' @references Wagner, M. (2012). Defining and Measuring Niche Parties. Party Politics 18(6): 845-64.
#' @references Baeck, H., Debus, M., & Dumont, P. (2010). Who gets what in coalition governments? Predictors of portfolio
#' allocation in parliamentary democracies. European Journal of Political Research 50(4): 441-478.
#' 
#' @param data a dataframe or matrix in format of Manifesto Project Main Dataset
#' @param method choose between bischof, wagner and meyermiller (currently only bischof is implemented)
#' @param ... parmaeters passed on to specialized functions for differnet methods
#' @export
mp_nicheness <- function(data,
                         method = "bischof",
                         ... ) {
  switch(method,
         "bischof" = nicheness_bischof(data),
         stop(paste("Nicheness method", method, "not implemented!"))
  )
}

#' weights must be a vector of the length nrow(election_data) or the name
#' of a variable in election_data
meyer_miller_single_election <- function(election_data,
                                         vars,
                                         weights) {
  
  if (is.character(weights)) {
    weights <- unlist(election_data[,weights])
  }
  
  for (name in vars) {  ## TODO = Reduce?
    election_data[,name] <- (election_data[,name] - 
                               sapply(election_data$party, function(p) {
                                 sum(election_data[election_data$party != p, name] * weights[election_data$party != p])/
                                   (sum(weights[election_data$party != p]))}))^2
  }
  election_data %>%
    select(one_of(vars)) %>%
    rowSums() %>%
    { sqrt( . / length(vars)) }
}

#'
#' @param groups groups of issues to determine niches/policy dimensions; formatted as named lists
#' variable names. For an example see ... TODO Defaults to Baeck et. al 2010 Policy dimensions
#' withouth industry, as used in the original paper by Meyer & Miller
#' @param transform transform to apply to each of the group indicators. Can be a function,
#' character "bischof" to apply log(x + 1), or NULL for no transformation.
#' @rdname mp_nicheness
#' @export
nicheness_meyer_miller <- function(data,
                                   groups = meyer_miller_2013_policy_dimensions(),
                                   transform = NULL,
                                   smooth = FALSE,
                                   weights = "pervote") {
  
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
    iff(smooth, mutate_each_, funs = funs((. + lag(.))/2), vars = names(groups)) %>%
    ungroup() %>%
    { split(., factor(paste(.$country, .$date))) } %>%
    lapply(arrange_, .dots = "party") %>%
    lapply(meyer_miller_single_election, vars = names(groups), weights = weights) %>%
    unlist()
  
  data %>%
    select(one_of(c("country", "party", "date"))) %>%
    arrange(country, date, party) %>%
    mutate(nicheness = nicheness)
  
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
                              diversification_bounds = c(
                                0.0,
                                rep(1/5, 5) %>%
                                  { -(. * log(.)) } %>%
                                   sum())
                              ) {
  
  data %>%
    mutate(
      n_ecology = per416 + per410 + per501 + per106,
      n_nationalist = per601 + per602 + per605 + per607 + per608,
      n_agrarian = per703,
      n_regional = per301 + per302 + per706,
      n_europe = per406 + per407 + per108 + per110,
      ecology_1 = log(n_ecology + 1),
      nationalist_1 = log(n_nationalist + 1),
      agrarian_1 = log(n_agrarian + 1),
      regional_1 = log(n_regional + 1),
      europe_1 = log(n_europe + 1)
    ) %>%
    # smooth with lag
    group_by(party) %>%
    arrange(date) %>%
    mutate(
      ecology = (ecology_1 + lag(ecology_1))/2,
      ecology = ifelse(row_number(date) == 1,ecology_1,ecology),
      nationalist = (nationalist_1 + lag(nationalist_1))/2, 
      nationalist = ifelse(row_number(date) == 1,nationalist_1,nationalist),
      agrarian = (agrarian_1 + lag(agrarian_1))/2,
      agrarian = ifelse(row_number(date)==1,agrarian_1,agrarian),
      regional = (regional_1 + lag(regional_1))/2,
      regional = ifelse(row_number(date)==1,regional_1,regional),
      europe = (europe_1 + lag(europe_1))/2,
      europe = ifelse(row_number(date)==1,europe_1,europe),
      # sum niche segments
      sum_seg = ecology + nationalist + agrarian + regional + europe,
      # share of niche segments
      seg_eco = ecology/sum_seg,
      seg_nat = nationalist/sum_seg,
      seg_agr = agrarian/sum_seg,
      seg_reg = regional/sum_seg,
      seg_eur = europe/sum_seg,
      # shannons entropy measure of niche segments
      diversification = log(1/(seg_eco^seg_eco * 
                                 seg_nat^seg_nat * 
                                 seg_agr^seg_agr * 
                                 seg_reg^seg_reg * 
                                 seg_eur^seg_eur))
    ) %>%
    ungroup() %>% 
    mutate(
      diversification = ifelse((seg_eco == 1 | 
                                  seg_nat == 1 |
                                  seg_agr == 1 |
                                  seg_reg == 1 |
                                  seg_eur == 1), 
                               0.0,
                               diversification),
      max_divers = ifelse(all(diversification_bounds == "empirical"),
                          max(diversification, na.rm=TRUE),
                          diversification_bounds[2]),
      min_divers = ifelse(all(diversification_bounds == "empirical"),
                          min(diversification, na.rm=TRUE),
                          diversification_bounds[1]),
      specialization = (min_divers + max_divers) - diversification
    ) %>% 
    # mean without party of interest
    group_by(country, date) %>%
    mutate(  ## TODO use meyer_miller here
      N = n(),
      npwq = N-1,
      mean_ecology = (sum(ecology, na.rm=TRUE) - ecology) / npwq,
      mean_nationalist = (sum(nationalist, na.rm=TRUE) - nationalist) / npwq,
      mean_agrarian = (sum(agrarian, na.rm=TRUE) - agrarian) / npwq,
      mean_regional = (sum(regional, na.rm=TRUE) - regional) / npwq,
      mean_europe = (sum(europe, na.rm=TRUE) - europe) / npwq,
      dis_ecology = (ecology - mean_ecology)^2,
      dis_nationalist = (nationalist - mean_nationalist)^2,
      dis_agrarian = (agrarian - mean_agrarian)^2,
      dis_regional = (regional - mean_regional)^2,
      dis_europe = (europe - mean_europe)^2,
      # across countries
      sum_distance =  dis_ecology + dis_nationalist + dis_agrarian + dis_regional + dis_europe,
      divide_distance = sum_distance/5,
      distance = sqrt(divide_distance),
      # per country-election
      nicheness = distance - ((sum(distance, na.rm=TRUE) - distance)/npwq)
    )  %>%
    ungroup() %>%
    mutate(
      max_nic = max(nicheness, na.rm=TRUE),
      min_nic = min(nicheness, na.rm=TRUE),
      nicheness_stand = (nicheness - min_nic)/(max_nic - min_nic),
      
      max_spec = max(specialization, na.rm=TRUE),
      min_spec = min(specialization, na.rm=TRUE),
      
      specialization_stand = (specialization-min_spec)/(max_spec-min_spec),
      specialization_stand_two = specialization_stand,
      specialization_stand_two = ifelse(is.na(specialization_stand_two),0,specialization_stand_two), 
      # nicheness two
      nicheness_two = nicheness_stand + specialization_stand_two) %>%
    select(one_of(out_variables))
}

