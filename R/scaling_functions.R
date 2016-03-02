#' Left-Right Scores based on Franzmann & Kaiser Method
#' 
#' Computes left-right scores based on the Franzmann & Kaiser Method (see
#' reference below). The issue structures are not calculated from scratch but
#' taken as given from Franzmann 2009. Note that they are not available for the
#' entire Manifesto Project Dataset, but only for a subset of countries and elections.
#' 
#'
#' @param data A data.frame with cases to be scaled, variables named "per..."
#' @param basevalues flag for transforming data to be relative to the minimum
#' @param smoothing flag for using smoothing
#' @param issue_structure issue structure to use for Franzmann & Kaiser method, default to original replication values
#' @param party_system_split function to recode the country variable to re-partition
#' party systems. Defaults to splitting Belgium into two halfs as done in Franzmann 2009
#' @param ... passed on to fk_smoothing
#' @references Franzmann, Simon/ Kaiser, André (2006): Locating Political Parties in Policy Space. A Reanalysis of Party Manifesto Data, Party Politics, 12:2, 163-188
#' @references Franzmann, Simon (2009): The Change of Ideology: How the Left-Right Cleavage transforms into Issue Competition. An Analysis of Party Systems using Party Manifesto Data. PhD Thesis. Cologne.
#' @export
franzmann <- function(data,
                      basevalues = TRUE,
                      smoothing = TRUE,
                      vars = grep("per\\d{3}$", names(data), value = TRUE),
                      issue_structure = read_fk_issue_structure(),
                      party_system_split = split_belgium,
                      ...) {
   
   

   if(!("country" %in% names(data)) & ("party" %in% names(data))) {
     data <- mutate(data, country = as.integer(substr(party, 1, 2)))
   }

   if (basevalues) {
      ## calculates positional scores = saliency scores - base value // pos scores = (x - min(x)) where x is saliency score
      data <- data %>%
         group_by(country, edate) %>%
         #select(one_of(vars)) %>%
         mutate_each_(funs(base=.-min(., na.rm=TRUE)), vars) %>%
         ungroup()
   }
  
  if (!is.null(party_system_split) & is.function(party_system_split)) {
    data <- party_system_split(data)
  }

   data %>%
     select(one_of("country", "edate")) %>%
     left_join(issue_structure) %>%
     select(one_of(vars)) %>%
     { scale_weighted(data, vars = vars, weights = .) /
                  scale_weighted(data, vars = vars, weights = 1) } -> fkscores
   
   if (smoothing) {
      combined <- cbind(data, fkscores)
      fkscores <- fk_smoothing(data = combined, score_name = "fkscores", ...)
   }
   
   return( (fkscores + 1)*5 )

}

read_fk_issue_structure <- function(path = system.file("extdata", "fk_issue_structure.sav", package = "manifestoR")) {
  require(haven)
  path %>%
    read_sav() %>%
    mutate(edate = as.Date(edate/(24*60*60), origin = "1582-10-14"),
           country = as.numeric(country)) %>%
    { set_names(., gsub("e(\\d+)_structure", "per\\1", names(.))) } %>%
    select(-countryname)

}

#' Split Belgium party system into two
#' 
#' recodes the country variable of a dataset to 218 (Flanders parties)
#' and 219 (Wallonia parties) from 21 for Belgium
#' 
#' @param data data.frame in format of the Manifesto Project's Main Dataset
#' @param wallonia_parties Party codes for the Wallonia half
#' @param flanders_parties Party codes for the Flanders half
#' @export
split_belgium <- function(data,
                          wallonia_parties = c(21111, 21322, 21422, 21423, 21425, 21426, 21522, 21911, 21912),
                          flanders_parties = c(21112, 21221, 21320, 21321, 21330, 21420, 21421, 21424, 21430, 21520, 21521, 21913, 21914, 21915, 21916, 21917)) {
  data %>%
    mutate(country = ifelse(country == 21,
                            ifelse(party %in% wallonia_parties, 219,
                                   ifelse(party %in% flanders_parties, 218, country)),
                            country))
}

#' @export
#' @rdname franzmann
#' @param score_name name of variable with LR Score values to be smoothed
#' @param use_period_length whether to use electoral period length in weighting
fk_smoothing <- function(data, score_name, use_period_length = TRUE) {
  
   # how to deal with "holes" (if party drops out of parliament and then enters later, sorting by party date, does not reflect this...)

   ## check if score is in data
   if (!score_name %in% names(data)) {
      stop("score name not found in data")   
   }
   
   if (!"party" %in% names(data)) {
      stop("no party variable found in data")
   }
   if (!"edate" %in% names(data)) {
      stop("no date variable found in data")
   }
   if (!("Date" %in% class(data$edate))) {
      stop("variable date is not a date")
   }
  
  lagl <- function(x) { dplyr::lag(x, default = first(x)) }
  leadl <- function(x) { dplyr::lead(x, default = last(x)) }
  
   data[,"the_score"] <- data[,score_name]
   data$n <- c(1:nrow(data))
   smoothed <- data %>% 
      group_by(party) %>% 
      select(one_of("country","edate","the_score","n")) %>% 
      arrange(edate) %>%
      mutate(
         leadedate = lead(edate),
         leglength = as.numeric(difftime(leadedate, edate, units="days")),
         w = leglength/(lagl(leglength) + leglength + leadl(leglength)),
         p_lag = lagl(the_score),
         p = the_score ,
         p_lead = leadl(the_score),
         smooth = ifelse(rep(use_period_length, n()),
                         (lagl(w)*p_lag + w*p + leadl(w)*p_lead),
                         (leadl(the_score) + the_score + lagl(the_score))/3)) %>%
      ungroup() %>%
      arrange(n)
  
   return(smoothed$smooth)
}

#' Vanilla Scaling by Gabel & Huber
#' 
#' Computes scores based on the Vanilla method suggested by Gabel & Huber. 
#' A factor analysis identifies the dominant dimension in the data. 
#' Factor scores using the regression method are then considered as party positions on this dominant dimension. 
#'
#' @references Gabel, M. J., & Huber, J. D. (2000). Putting Parties in Their Place: Inferring Party Left-Right Ideological Positions from Party Manifestos Data. American Journal of Political Science, 44(1), 94-103.
#'
#' @param data A data.frame with cases to be scaled, variables named "per..."
#' @param vars variable names that should be used for the scaling (usually the variables per101,per102,...)
#' @param invert invert scores (to change the direction of the dimension to facilitate comparison with other indices) (default is FALSE)
#' @export
vanilla <- function(data,
                    vars = grep("per\\d{3}$", names(data), value=TRUE),
                    invert=FALSE) {
  fa.results <- psych::fa(data[,vars],1,scores="regression")
  vanilla.scores <- fa.results$scores[,1] 
  if (invert==TRUE) vanilla.scores <- vanilla.scores*-1
  return(vanilla.scores)
}
