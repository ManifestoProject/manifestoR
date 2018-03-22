#' @export
mp_codebook <- function(version = NULL) {
  ## TODO
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
#' \if{html}{\out{
#' 
#' <p>Bla blubb</p>
#' 
#' <link rel="stylesheet" type="text/css" href="https://cdn.datatables.net/1.10.16/css/jquery.dataTables.css">
#'
#' <script type="text/javascript" charset="utf8" src="https://code.jquery.com/jquery-1.12.4.js"></script>
#' <script type="text/javascript" charset="utf8" src="https://cdn.datatables.net/1.10.16/js/jquery.dataTables.js"></script>
#' 
#' <table id="codebook" class="display"></table>
#' 
#' <script type="text/javascript">
#' var data = [
#' [
#'   "Tiger Nixon",
#'   "System Architect",
#'   "Edinburgh",
#'   "5421",
#'   "2011/04/25",
#'   "$3,120"
#'   ],
#' [
#'   "Garrett Winters",
#'   "Director",
#'   "Edinburgh",
#'   "8422",
#'   "2011/07/25",
#'   "$5,300"
#'   ]
#' ];
#' 
#' $(document).ready(function() {
#'  $('#codebook').DataTable( {
#'   data: data,
#'   columns: [
#'     { title: "Name" },
#'     { title: "Position" },
#'     { title: "Office" },
#'     { title: "Extn." },
#'     { title: "Start date" },
#'     { title: "Salary" }
#'   ]
#'  } );
#' } );
#' </script>
#' }}
#' 
#' @name codebook
#' @rdname codebook
NULL