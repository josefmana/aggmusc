#' Manually recode typos in disease onset
#'
#' Takes patient data and manually corrects
#' known typos in disease onset or duration
#' information. It is intended to be used
#' within the \code{determine_aggressive_phenotype}
#' workflow.
#'
#' Additionally, the function drops invalid data
#' such as EDSS assessments recorded before the
#' documented disease onset.
#'
#' @param data A tibble used for determining the
#'   aggressive phenotype.
#'
#' @return A tibble identical to the input but with
#'   corrected disease duration values and invalid
#'   data removed.
#'
#' @export
recode_typos <- function(data) {
  df <- data |>
    dplyr::mutate(onset = dplyr::case_when(
      id == 604 ~ as.Date("2006-01-02"), # first visit
      id == 1478 ~ as.Date("2011-09-07"), # first visit
      id == 4510 ~ as.Date("2019-05-30"), # one year sooner
      id == 4591 ~ as.Date("2011-11-01"), # first visit
      id == 4596 ~ as.Date("2020-01-14"), # first visit
      id == 4989 ~ as.Date("2017-09-06"), # fixed different date
      id == 5128 ~ as.Date("2024-11-01"), # fixed different date
      .default = onset
    ))

  df <- subset(df, !(id == 571 & edss_time < 0))
  df <- subset(df, !(id == 897 & edss_time < 0))
  df <- subset(df, !(id == 4952 & edss_time < 0))
  df <- subset(df, !(id == 4971 & edss_time < 0))
  df <- subset(df, !(id == 4972 & edss_time < 0))
  df <- subset(df, !(id == 4985 & edss_time < 0))

  df <- subset(df, id != 1147) # drop because we do not have correct visit dates

  df |> dplyr::mutate(edss_time = lubridate::time_length(
    difftime(visit_date, onset),
    unit = "years"
  ))
}
