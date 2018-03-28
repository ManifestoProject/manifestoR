#' @export
mp_codebook <- function(version = "current") {
  
  if (version == "current") {
    version <- current_dataset_version(south_america = FALSE)
  }
  
  data_frame(cmp_code = c("101", "102"),
             label = paste("Foreign Special Relations:", c("Positive", "Negative")))
}

#' @export
#' @rdname mp_codebook
mp_describe_code <- function(code, version = "current") {
  data.frame(code = code) %>%
    left_join(mp_codebook(version), by = "code")
}

#' \code{mp_view_codebook} displays a searchable table of the codes
#' used in the Manifesto Project Data in the Viewer pane.
#' 
#' @importFrom htmlwidgets prependContent
#' @importFrom DT datatable
#' @importFrom htmltools h1 p a
#' @export
#' @rdname mp_codebook
mp_view_codebook <- function(version = "current") {
  
  DT::datatable(mp_codebook(version)) %>% 
    htmlwidgets::prependContent(htmltools::h1("Manifesto Project Codebook"),
                                htmltools::p(paste0(
        "This table shows the codes and descriptions as given in the Codebook of Dataset Version", version, ".",
        "To see a different version of the codebook, please specify the parameter 'version'. ",
        "Note that the codebook contains only condensed descriptions of the categories. ",
        "For detailed information on coding instructions, you can refer to the different "),
                                             htmltools::a("handbook versions", href = "https://manifesto-project.wzb.eu/information/documents/handbooks"), "."))
  
}