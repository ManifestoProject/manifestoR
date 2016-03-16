#' Issue Attention Diversity
#' 
#' Effective number of Manifesto Issues suggested by Zac Greene. When using the measure please cite Greene 2015 (see reference below)
#' @references Greene, Z. (2015). Competing on the Issues How Experience in Government and Economic Conditions Influence the Scope of Parties' Policy Messages. Party Politics.
#'
#' @param data a data.frame in format of Manifesto Project Main Dataset
#' @param method entropy measure used for the effective number of manifesto issues. Possible options are "shannon" for Shannon's H and "herfindahl" for the Herfindahl-Index.
#' @param include_variables names of variables to include
#' @param prefix Prefix of variable names to use (usually "per")
#' @param aggregate_categories list of category groups to aggregate into one issue. Default to selection used in Greene 2015
#' @export
issue_attention_diversity <- function(data,
                                      method="shannon",
                                      prefix = "per",
                                      include_variables = paste0(prefix, setdiff(v4_categories(), "uncod")),
                                      aggregate_categories = list(c(101, 102),
                                                                  c(104, 105),
                                                                  c(107, 109),
                                                                  c(108, 110),
                                                                  c(203, 204),
                                                                  c(301, 302),
                                                                  c(406, 407),
                                                                  c(409, 414),
                                                                  c(504, 505),
                                                                  c(506, 507),
                                                                  c(601, 602),
                                                                  c(603, 604),
                                                                  c(607, 608),
                                                                  c(701, 702))) {
  
  methods <- list(shannon = function(.) { exp(-rowSums(. * log_0(.))) },
                  herfindahl = function(.) { 1/rowSums(.*.) })
  
  prefix_cat <- function(cat) { paste0(prefix, cat)}
  
  groups <- aggregate_categories %>%
    lapply(sapply, prefix_cat)
  names(groups) <- aggregate_categories %>%
    lapply(paste, collapse = "_") %>%
    lapply(prefix_cat) %>%
    unlist()

  include_variables <- c(setdiff(include_variables, unlist(groups)), names(groups))

  data %>%
    aggregate_pers(groups = groups) %>%
    select(one_of(include_variables)) %>%
    { ./rowSums(.) } %>%
    methods[[method]]()

}

log_0 <- function(x) {
   ifelse(x==0,0,log(x))
}


