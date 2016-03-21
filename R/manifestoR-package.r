#' manifestoR R package
#' 
#' Access and process data and documents of the Manifesto Project
#' \tabular{ll}{
#' Package: \tab manifestoR\cr
#' Type: \tab Package\cr
#' License: \tab GPL (>= 3)\cr
#' LazyLoad: \tab yes\cr
#'}
#'
#' @name manifestoR
#' @docType package
#' @title  Access and process data and documents of the Manifesto Project
#' @author Jirka Lewandowski \email{jirka.lewandowski@@wzb.eu}
#' @references \url{https://manifesto-project.wzb.eu/}
#' 
#' @importFrom utils head tail
#' @import zoo
#' @importFrom psych fa
#' @import NLP
#' @import dplyr
#' @import functional
#' @import tm
#' @import jsonlite
NULL

## A fix to let CRAN check NOTEs diasappear for non-standard-evaluation used
## cf. http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
globalVariables(c("one_of", ".", "matches", "starts_with", "contains", # dplyr
                  "manifesto_id", "country", "party", "edate", "per0", # general dataset
                  "md5sum_text.x", "md5sum_text.y", "code", "manifestos", # & metadata & api download
                  "download", "url_original", "is_primary_doc", "originals",
                  "name", "tag", # metadata versions
                  "the_score", "leadedate", "leglength", "countryname", "w", "p", # scaling
                  "p_lead", "p_lag", "lrcorescores", # median_voter_single
                  "position", "voteshare", "cumvoteshare",
                  "above50", "contains_median", "leftbound", "rightbound",
                  "min_divers", "max_divers", "nicheness", "min_nic", ## nicheness
                  "max_nic", "specialization", "min_spec", "max_spec",
                  "specialization_stand", "specialization_stand_two", "nicheness_stand",
                  "tmp_mp_clarity_sum", "data_2", "score" # clarity measure
                ))
