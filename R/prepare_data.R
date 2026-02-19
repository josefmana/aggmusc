#' Read and format data for further use
#'
#' Reads patient data from an Excel file, using a path
#' (ideally on an external drive to maintain patients'
#' anonymity).
#'
#' @param dp A character string specifying path to
#'   the Excel file containing the data.
#' @param ip A character string specifying path to
#'   the Excel file containing IDs of patients who
#'   have signed informed consent.
#'
#' @return A list of tibbles, each containing the relevant
#'   variables extracted and formatted from the Excel file.
#'
#' @examples
#' \dontrun{
#' p1 <- here::here("data-raw", "some_cool_file_name.xlsx")
#' p2 <- here::here("data-raw", "patients_with_informed_consent.xlsx")
#' data <- prepare_data(p1, p2)
#' }
#'
#' @export
prepare_data <- function(dp, ip) {
  struct <- specify_data_structure(show = FALSE)
  nms <- rlang::set_names(
    seq_along(struct$labels),
    names(struct$labels)
  )
  d0 <- with(struct, {
    lapply(nms, function(i) {
      d <- readxl::read_excel(
        path = dp,
        sheet = i,
        col_types = coltypes[[names(labels)[i]]],
        col_names = names(coltypes[[names(labels)[i]]]),
        skip = 1
      )
      vars <- colnames(d)
      if (!"id" %in% vars) {
        id_col <- vars[stringr::str_detect(vars, "id")]
        if (id_col == "id_imed") {
          d$id <- d$id_imed
        } else {
          d$id <- d[[id_col]] / 19
        }
      }
      d
    })
  })

  incl_ids <- extract_ids(ip)
  for (i in nms) {
    d0[[i]] <- d0[[i]] |>
      dplyr::filter(id %in% incl_ids)
  }
  d0
}
