#' Access to the Codebook for the Manifesto Project Main Dataset
#' 
#' These functions provide access to machine- and human-readable versions
#' of the Codebook (variable descriptions) of the Manifesto Project
#' Main Dataset. Currently only the content-analytical variables (categories)
#' are accessible. Note also that the codebook contains only condensed descriptions
#' of the categories. For detailed information on coding instructions, you can refer
#' to the different handbook versions under \URL{https://manifesto-project.wzb.eu/information/documents/handbooks}.
#' 
#' \code{mp_codebook} returns the codebook as a \code{data_frame}, ideal for further automatic processing.
#' 
#' @param version version of the Manifesto Project Main Dataset for which the
#' codebook is requested. Note that only codebooks from version MPDS2017b on
#' are available via the API/manifestoR. Defaults to "currrent", which fetches
#' the most recent codebook version. 
#' 
#' @export
mp_codebook <- function(version = "current", cache = TRUE, chapter = "categories") {
  
  if (version == "current") {
    version <- current_dataset_version(south_america = FALSE)
  }
  
  get_viacache(kmtype.codebook,
               ids = list(key=version, kind=chapter),
               cache = cache,
               apikey = "not_needed",
               versionid = "CONST")
  
}

#' \code{mp_describe_codebook} returns a list with information about the requested code, ideal for quick interactive use.
#' 
#' @param code specific code (as character) to display information about.
#' 
#' @export
#' @rdname mp_codebook
mp_describe_code <- function(code, version = "current", columns = c("title", "description_md")) {
  data_frame(code = code) %>%
    left_join(mp_codebook(version), by = "code") %>%
    select(one_of(columns)) %>%
    as.list()
}

#' \code{mp_view_codebook} displays a searchable table version of the codebook
#' in the Viewer pane.
#' 
#' @param columns Information to display about each variable. Given as a vector of
#' selected column names from: "type", "domain_code", "domain_name", "code", "variable_name",
#' "title", "description_md", "label"
#' 
#' @importFrom htmlwidgets prependContent
#' @importFrom DT datatable
#' @importFrom htmltools h1 p a
#' @export
#' @rdname mp_codebook
mp_view_codebook <- function(version = "current", columns = c("type", "code", "title")) {
  
  mp_codebook(version) %>%
    select(one_of(columns)) %>%
    DT::datatable() %>% 
    htmlwidgets::prependContent(htmltools::h1("Manifesto Project Codebook"),
                                htmltools::p(paste0(
        "This table shows the codes and descriptions as given in the Codebook of Dataset Version", version, ".",
        "To see a different version of the codebook, please specify the parameter 'version'. ",
        "Note that the codebook contains only condensed descriptions of the categories. ",
        "For detailed information on coding instructions, you can refer to the different "),
                                             htmltools::a("handbook versions", href = "https://manifesto-project.wzb.eu/information/documents/handbooks"), "."))
  
}