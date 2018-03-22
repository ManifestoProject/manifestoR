#' @export
mp_codebook <- function(version = NULL) {
  data_frame(cmp_code = c("101", "102"),
             label = paste("Foreign Special Relations:", c("Positive", "Negative")))
}

#' @export
#' @rdname mp_codebook
mp_describe_code <- function(code, version = NULL) {
  data.frame(code = code) %>%
    left_join(mp_codebook(version), by = "code")
}

#' Codebook of Manifesto Dataset
#' 
#' TODO
#' 
#' \if{html}{ % Only applies to HTML help files
#'   \Sexpr[echo=FALSE, results=rd, stage=build]{
#'   #This doesn't work for pkgdown pages, so detect if the page is being built in pkgdown and skip the widget
#'   in_pkgdown <- any(grepl("as_html.tag_Sexpr", sapply(sys.calls(), function(a) paste(deparse(a), collapse = "\n"))))
#'     if(in_pkgdown) {
#'       mytext <- c('In RStudio, this help file includes a searchable table of values.')
#'     } else {
#'     tmp <- tempfile(fileext=".html")
#'      #Create a DT htmlwidget and save it to a tempfile
#'       htmlwidgets::saveWidget(DT::datatable(manifestoR::mp_codebook(), rownames = FALSE, width=700), tmp)
#'       #Read the widget file in, but remove some html tags
#'       mytext <- paste('Below is a searchable version of the database codes.',
#'       '\\\out{<div style="width:100\%">',
#'          paste(stringi::stri_subset_regex(readLines(tmp), "^</?(!DOCTYPE|meta|body|html)",negate=TRUE), collapse="\n"),
#'       '</div>}',
#'       sep="\n")
#'     }
#'     mytext
#'   }
#' }
#'
#' \if{text,latex}{The HTML version of this help file includes a searchable table of the database codes}
#' 
#' @name codebook
#' @rdname codebook
NULL