#' Compute Issue Attention Diversity
#' 
#' Effective number of Manifesto Issues suggested by Zac Greene. When using the measure please cite:
#' @references Greene, Z. (2015). Competing on the Issues How Experience in Government and Economic Conditions Influence the Scope of Parties’ Policy Messages. Party Politics.
#'
#' @param data used (has to be a manifesto project dataset)
#' @param entropy measure used for the effective number of manifesto issues. Possible options are "shannon" for Shannon's H and "herfindahl" for the Herfindahl-Index.
#' @export
scale_iad <- function(data, method="shannon") {
   
   datax <- data %>% mutate(
      pertotal = rowSums(.[mp_standard_cats])
      ) %>%
   mutate_each(
      # take only coded part of document into account (not uncoded sentences)
         funs(.=./pertotal),one_of(mp_standard_codes)
      ) %>%
   mutate(
      # merges opposite categories
      per101 = per101 + per102,
      per104 = per104 + per105,
      per107 = per107 + per109,
      per108 = per108 + per110,
      per203 = per203 + per204,
      per301 = per301 + per302,
      per406 = per406 + per407,
      per409 = per409 + per414,
      per504 = per504 + per505,
      per506 = per506 + per507,
      per601 = per601 + per602,
      per603 = per603 + per604,
      per607 = per607 + per608,
      per701 = per701 + per702
      ) 
      if (method=="shannon") {
         datax <- datax %>% mutate_each(
            funs(.=.*log_0(.)), one_of(mp_standard_cats[!mp_standard_cats %in% mp_paired])
         ) %>% 
         mutate(
            sum_of_logs = rowSums(.[mp_standard_cats[!mp_standard_cats %in% mp_paired]]),
            enmi_sh = exp( - sum_of_logs)
         )
         return(datax$enmi_sh)
      } 
      if (method=="herfindahl") {
         datax <- datax %>% mutate_each(
            funs(.=.*.),one_of(mp_standard_cats[!mp_standard_cats %in% mp_paired])
            ) %>%
            mutate(
               sum_of_squares = rowSums(.[mp_standard_cats[!mp_standard_cats %in% mp_paired]]),
               enmi_herf = 1/sum_of_squares
            )
         return(datax$enmi_herf)
      }
}

log_0 <- function(x) {
   ifelse(x==0,0,log(x))
}

mp_standard_cats <- c("per101","per102","per103","per104","per105","per106","per107","per108","per109","per110",
                     "per201","per202","per203","per204",
                     "per301","per302","per303","per304","per305",
                     "per401","per402","per403","per404","per405","per406","per407","per408","per409","per410","per411","per412","per413","per414","per415","per416",
                     "per501","per502","per503","per504","per505","per506","per507",
                     "per601","per602","per603","per604","per605","per606","per607","per608",
                     "per701","per702","per703","per704","per705","per706")

mp_paired <- c("per102","per105","per109","per110","per204","per302","per407","per414","per505","per507","per602","per604","per608","per702")


