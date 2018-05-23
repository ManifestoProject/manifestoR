#' Access to the Codebook for the Manifesto Project Main Dataset
#' 
#' @description These functions provide access to machine- and human-readable versions
#' of the Codebook (variable descriptions) of the Manifesto Project
#' Main Dataset, as can be found in PDF form under https://manifesto-project.wzb.eu/datasets .
#' As of this manifestoR release only the content-analytical variables (categories)
#' are accessible. Note also that the codebook contains only condensed descriptions
#' of the categories. For detailed information on coding instructions, you can refer
#' to the different handbook versions under https://manifesto-project.wzb.eu/information/documents/handbooks .
#' Only codebooks from version MPDS2017b on are accessible via the API.
#' 
#' @description \code{mp_codebook} returns the codebook as a \code{data_frame}, ideal for further automatic processing.
#' 
#' @param version version of the Manifesto Project Main Dataset for which the
#' codebook is requested. Note that only codebooks from version MPDS2017b on
#' are available via the API/manifestoR. Defaults to "currrent", which fetches
#' the most recent codebook version. Must be formatted as e.g. "MPDS2017b".
#' @param cache Whether result of API call should be cached locally (defaults to TRUE)
#' @param chapter Which part of the codebook should be returned. As of this manifestoR
#' release, only the content-analytical variables (parameter value "categories") are accessible via the API.
#' 
#' @export
mp_codebook <- function(version = "current", cache = TRUE, chapter = "categories") {
  
  if (chapter != "categories") {
    warning(paste("As of this release of manifestoR the Manifesto Project API does not provide any other chapter",
                  "of the codebook than the categories! You can find PDF versions of the other chapters of the",
                  "codebook under https://manifesto-project.wzb.eu/datasets",
                  "(If you see this warning a no subsequent HTTP Error 404 for your request,",
                  "please update manifestoR!)"),
            immediate. = TRUE)
  }
  
  version_year <- suppressWarnings(as.integer(gsub("(MPDS)(\\d{4})(a|b)", "\\2", version)))
  version_sub <- gsub("(MPDS)(\\d{4})(a|b)", "\\3", version)
  if (!is.na(version_year) && (version_year < 2017 || (version_year == 2017 && version_sub == "a"))) {
    stop("Only codebooks from version MPDS2017b on are accessible via the Manifesto Project API. ",
         "For older versions please refer to the PDFs accessible under https://manifesto-project.wzb.eu/datasets?archived=yes")
  }
  
  if (version == "current") {
    version <- current_dataset_version(south_america = FALSE)
  }
  
  get_viacache(kmtype.codebook,
               ids = list(key=version, kind=chapter),
               cache = cache,
               apikey = "not_needed",
               versionid = "CONST")
  
}

#' \code{mp_describe_code} pretty prints with information about the requested code(s), ideal for quick interactive use.
#' 
#' @param code specific code (as character) to display information about.
#' 
#' @export
#' @importFrom magrittr %T>%
#' @rdname mp_codebook
mp_describe_code <- function(code, version = "current", columns = c("title", "description_md")) {
  data_frame(code = code) %>%
    left_join(mp_codebook(version), by = "code") %>%
    select(one_of("code"), one_of(columns)) %T>%
    apply(1, pretty_print_code_info) %>%
    invisible()
}

pretty_print_code_info <- function(code_info) {
  mapply(function(field_name, value) {
    cat(paste0(field_name, ": ", value), sep = "\n")
  },
  field_name = names(code_info),
  value = code_info)
  cat("\n")
  
  invisible(code_info)
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
  
  if (version == "current") {
    version <- current_dataset_version(south_america = FALSE)
  }
  
  mp_codebook(version) %>%
    select(one_of(columns)) %>%
    DT::datatable(options = list(paging = F)) %>%
    htmlwidgets::prependContent(htmltools::h1("Manifesto Project Codebook"),
                                htmltools::p(paste0(
        "This table shows the codes and descriptions as given in the codebook of dataset version ", version, ".",
        "To see a different version of the codebook, please specify the parameter 'version'. ",
        "Note that the codebook contains only condensed descriptions of the categories. ",
        "For detailed information on coding instructions, you can refer to the different "),
                                             htmltools::a("handbook versions", href = "https://manifesto-project.wzb.eu/information/documents/handbooks"), ".")) %>%
    { .$sizingPolicy = htmlwidgets::sizingPolicy(viewer.defaultWidth = "100%", viewer.defaultHeight = 440, viewer.fill = FALSE); . }

}