#' Preprocess treatment variables.
#'
#' @param d A tibble with the original treatment data.
#' @param onset A tibble containing id/disease onset pairs.
#'
#' @return A tibble with data.
#'
#' @export
preprocess_predictors_treatment <- function(d, onset) {
  d1 <- d |>
    dplyr::left_join(
      onset |>
        dplyr::mutate(
          plus5 = onset + lubridate::years(5),
          plus10 = onset + lubridate::years(10)
        ),
      by = dplyr::join_by(id)
    ) |>
    dplyr::mutate(
      treatment = factor(
        dplyr::case_when(
          dmt_effect == "Platform" ~ 1,
          dmt_effect == "HET" ~ 2,
          dmt_effect == "LE_HET" ~ 3
        ),
        levels = 1:3,
        labels = c("Platform", "HET", "LE_HET")
      ),
      end_date = dplyr::if_else(
        is.na(end_date), as.Date("2024-12-31"), end_date
      ),
      duration = lubridate::time_length(
        difftime(end_date, start_date),
        unit = "year"
      )
    ) |>
    dplyr::filter(duration > 0) |>
    tidyr::drop_na()

  tt <- purrr::map_dfr(c(rlang::quo(plus5), rlang::quo(plus10)), function(i) {
    y <- readr::parse_number(rlang::quo_text(i))
    d1 |>
      dplyr::filter(start_date < !!i) |>
      dplyr::mutate(
        end_date = !!i,
        duration = lubridate::time_length(
          difftime(end_date, start_date),
          unit = "year"
        )
      ) |>
      dplyr::group_by(id, treatment) |>
      dplyr::summarise(treatment_time = max(duration) / y, .groups = "drop") |>
      tidyr::pivot_wider(names_from = "treatment", values_from = "treatment_time") |>
      tibble::add_column(years = y)
  }) |>
    tidyr::pivot_wider(
      names_from = "years",
      values_from = c("Platform", "HET", "LE_HET"),
      id_cols = "id"
    ) |>
    dplyr::mutate_all(\(x) dplyr::if_else(is.na(x), 0, x))

  ttt <- d1 |>
    dplyr::group_by(id) |>
    dplyr::summarise(
      time_to_treat = min(lubridate::time_length(
        difftime(start_date, onset),
        unit = "year"
      )),
      .groups = "drop"
    )

  onset |>
    dplyr::left_join(tt, by = "id") |>
    dplyr::left_join(ttt, by = "id") |>
    dplyr::select(-onset)
}
