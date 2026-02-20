#' Preprocess MRI data.
#'
#'
#' @param d A tibble with the original MRI data.
#' @param onset A tibble containing id/disease onset pairs.
#'
#' @return A tibble with data.
#'
#' @export
preprocess_predictors_mri <- function(d, onset) {
  d1 <- d |>
    dplyr::left_join(
      onset |>
        dplyr::mutate(
          plus2 = onset + lubridate::years(2),
          plus5 = onset + lubridate::years(5),
          plus10 = onset + lubridate::years(10)
        ),
      by = dplyr::join_by(id)
    ) |>
    dplyr::mutate(
      close2 = lubridate::time_length(difftime(plus2, mri_date), unit = "years"),
      close5 = lubridate::time_length(difftime(plus5, mri_date), unit = "years"),
      close10 = lubridate::time_length(difftime(plus10, mri_date), unit = "years")
    )

  ints <- c(
    rlang::quo(close2),
    rlang::quo(close5),
    rlang::quo(close10)
  )

  dates <- purrr::map_dfr(ints, function(i) {
    y <- readr::parse_number(rlang::quo_text(i))
    d1 |>
      dplyr::filter(!!i > 0) |>
      dplyr::group_by(id) |>
      dplyr::summarise(
        date = mri_date[which(!!i == min(!!i, na.rm = TRUE))],
        .groups = "drop"
      ) |>
      tibble::add_column(post = y)
  }) |>
    tidyr::pivot_wider(
      names_from = "post",
      values_from = "date",
      id_cols = "id"
    )

  # If the date of all is the same, it must
  # have been sooner rather than later
  for (i in seq_len(nrow(dates))) {
    if (sum(is.na(dates[i, ])) < 2) {
      if (isTRUE(all.equal(dates$`2`[i], dates$`5`[i], dates$`10`[i]))) {
        dates$`5`[i] <- dates$`10`[i] <- NA
      }
    }
  }

  onset |>
    dplyr::select(id) |>
    dplyr::left_join(
      lapply(ints, function(i) {
        y <- readr::parse_number(rlang::quo_text(i))
        onset |>
          dplyr::select(-onset) |>
          dplyr::left_join(
            d1 |>
              dplyr::left_join(dates, by = dplyr::join_by(id)) |>
              dplyr::mutate(keep = mri_date == .data[[as.character(y)]]) |>
              dplyr::filter(keep) |>
              dplyr::select(
                id,
                t2_lesions_volume,
                t1_blackholes_volume,
                normalised_brain_atrophz_bpf_sv,
                t1_tbv_sv,
                corpus_callosum_volume,
                !!i
              ) |>
              dplyr::rename("mri_years_before" = !!i) |>
              dplyr::rename_with(\(x) paste0(x, "_y", y), -c("id")),
            by = dplyr::join_by(id)
          )
      }) |>
        purrr::reduce(
          dplyr::left_join,
          by = dplyr::join_by(id)
        ),
      by = dplyr::join_by(id)
    )
}
