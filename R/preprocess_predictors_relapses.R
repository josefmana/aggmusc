#' Preprocess relapse counts.
#'
#' @param d A tibble with the original relapse data.
#' @param onset A tibble containing id/disease onset pairs.
#'
#' @return A tibble with data.
#'
#' @export
preprocess_predictors_relapses <- function(d, onset) {
  d |>
    dplyr::left_join(
      onset |>
        dplyr::mutate(plus10 = onset + lubridate::years(10)),
      by = dplyr::join_by(id)
    ) |>
    dplyr::filter(relapse_date < plus10) |>
    tidyr::drop_na() |>
    dplyr::group_by(id) |>
    dplyr::summarise(
      relapse_count = dplyr::n(),
      .groups = "drop"
    )
}
