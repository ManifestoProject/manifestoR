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
#' @import NLP
#' @import dplyr
#' @import functional
#' @import tm
#' @import jsonlite
NULL

## A fix to let CRAN check NOTEs diasappear for non-standard-evaluation used
## cf. http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
globalVariables(c("one_of", ".", "matches", # dplyr
                  "manifesto_id", "country", "party", "edate", # general dataset
                  "md5sum_text.x", "md5sum_text.y", # & metadata
                  "download", "url_original",
                  "leadedate", "leglength", "festername", "w", "p", # scaling
                  "p_lead", "p_lag", "lrcorescores"))
