#' Extract patients with signed informed consent
#'
#' Reads patient IDs from an Excel file containing
#' a single column with valied ids.
#'
#' @param path A character string specifying the path
#'   to the Excel file containing the data.
#'
#' @return Integer vector with valid patients IDs
#'
#' @examples
#' \dontrun{
#' p <- here::here("data-raw", "patients_with_informed_consent.xlsx")
#' incl_ids <- extract_ids(p)
#' }
#'
#' @export
extract_ids <- function(path) {
  readxl::read_excel(path) |>
    dplyr::pull() |>
    stringr::str_extract("(\\d)+") |>
    readr::parse_integer()
}
