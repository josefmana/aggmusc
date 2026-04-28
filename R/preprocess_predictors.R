#' Preprocess potential predictors
#'
#' Loops through treatment, MRI, and relapse predictors
#' and preprocess them.
#'
#' @param d0 A tibble containing data pre-processed by
#'   \code{determine_aggressive_phenotype}.
#' @param edss A tibble containing EDSS examinations.
#' @param treat A tibble containing treatment variables.
#' @param rel A tibble containing relapse data.
#' @param mri A tibble containing MRI data.
#' @param csf A tibble containing CSF data.
#' @param chol A tibble containing cholesterol data.
#'
#' @return A tibble with data.
#' \describe{
#'   \item{id}{Numeric denoting patient's ID.}
#'   \item{aggressive}{Numeric denoting aggressive MS.}
#'   \item{sex}{Biological sex coded as factor, `0` females,
#'         `1` males.}
#'   \item{age_onset}{Age at symptom onset in years.}
#'   \item{first_visit_dur}{Disease duration in years during
#'         the first EDSS assessment.}
#'   \item{first_edss}{First EDSS score.}
#'   \item{platform_y5}{Proportion of time using Platform
#'         during the first 5 years of disease.}
#'   \item{platform_y10}{Proportion of time using Platform
#'         during the first 10 years of disease.}
#'   \item{het_y5}{Proportion of time using HET
#'         during the first 5 years of disease.}
#'   \item{het_y10}{Proportion of time using HET
#'         during the first 10 years of disease.}
#'   \item{le_het_y5}{Proportion of time using LE HET
#'         during the first 5 years of disease.}
#'   \item{le_het_y10}{Proportion of time using LE HET
#'         during the first 10 years of disease.}
#'   \item{time_to_treat}{Years it took before the first
#'         treatment.}
#'   \item{relapse_count}{Number of relapses during the
#'         first 10 years of disease.}
#'   \item{t2_lesions_volume_first}{Volume in mms of T2 lesions in
#'         MRI at the first available assessment.}
#'   \item{t1_blackholes_volume_first}{Volume in mms of T1 blackholes
#'         in MRI at the first available assessment.}
#'   \item{normalised_brain_atrophy_bpf_sv_first}{Brain atrophy normalised
#'         by TBV at the first available assessment.}
#'   \item{t1_tbv_sv_first}{Total brain volume in T1 at the first available
#'        assessment.}
#'   \item{corpus_callosum_volume_first}{Corpus callosum volume in
#'         mm  at the first available assessment.}
#'   \item{first_mri_dur}{Disease duration in years during
#'         the first MRI assessment.}
#'   \item{t2_lesions_volume_y2}{Volume in mms of T2 lesions in
#'         MRI within the first two years of disease course.}
#'   \item{t1_blackholes_volume_y2}{Volume in mms of T1 blackholes
#'         in MRI within the first two years of disease course.}
#'   \item{normalised_brain_atrophy_bpf_sv_y2}{Brain atrophy normalised
#'         by TBV within the first two years of disease course.}
#'   \item{t1_tbv_sv_y2}{Total brain volume in T1 within
#'         the first two years of disease course.}
#'   \item{corpus_callosum_volume_y2}{Corpus callosum volume in
#'         mm within the first two years of disease course.}
#'   \item{mri_years_before_y2}{How many years before year-two
#'         of disease was MRI measured?}
#'   \item{t2_lesions_volume_y5}{Volume in mms of T2 lesions in
#'         MRI within the first five years of disease course.}
#'   \item{t1_blackholes_volume_y5}{Volume in mms of T1 blackholes
#'         in MRI within the first five years of disease course.}
#'   \item{normalised_brain_atrophy_bpf_sv_y5}{Brain atrophy normalised
#'         by TBV within the first five years of disease course.}
#'   \item{t1_tbv_sv_y5}{Total brain volume in T1 within
#'         the first five years of disease course.}
#'   \item{corpus_callosum_volume_y5}{Corpus callosum volume in
#'         mm within the first five years of disease course.}
#'   \item{mri_years_before_y5}{How many years before year-five
#'         of disease was MRI measured?}
#'   \item{t2_lesions_volume_y10}{Volume in mms of T2 lesions in
#'         MRI within the first ten years of disease course.}
#'   \item{t1_blackholes_volume_y10}{Volume in mms of T1 blackholes
#'         in MRI within the first ten years of disease course.}
#'   \item{normalised_brain_atrophy_bpf_sv_y10}{Brain atrophy normalised
#'         by TBV within the first ten years of disease course.}
#'   \item{t1_tbv_sv_y10}{Total brain volume in T1 within
#'         the first ten years of disease course.}
#'   \item{corpus_callosum_volume_y10}{Corpus callosum volume in
#'         mm within the first ten years of disease course.}
#'   \item{mri_years_before_y10}{How many years before year-ten
#'         of disease was MRI measured?}
#' }
#'
#' @examples
#' \dontrun{
#' raw_data <- here::here("data-raw", "some_cool_file_name.xlsx") |>
#'   prepare_data(here::here("data-raw", "patients_with_informed_consent.xlsx"))
#' clas_data <- determine_aggressive_phenotype(
#'   raw_data$id,
#'   raw_data$relapses,
#'   raw_data$edss,
#'   eye_check = TRUE
#' )
#' df <- preprocess_predictors(
#'   d0    = clas_data$data,
#'   edss  = raw_data$edss,
#'   treat = raw_data$treatment,
#'   rel   = raw_data$relapses,
#'   mri   = raw_data$mri,
#'   csf   = raw_data$csf,
#'   chol  = raw_data$cholesterol
#' )
#' }
#'
#' @export
preprocess_predictors <- function(
    d0,
    edss,
    treat,
    rel,
    mri,
    csf,
    chol
    ) {
  incl <- unique(d0$id)
  t  <- subset(treat, id %in% incl)
  r  <- subset(rel, id %in% incl)
  m  <- subset(mri, id %in% incl)
  c  <- subset(csf, id %in% incl)
  c2 <- subset(chol, id %in% incl)
  e  <- edss |>
    dplyr::filter(id %in% incl) |>
    dplyr::group_by(id) |>
    dplyr::slice(1) |>
    dplyr::ungroup()

  ontab <- d0 |>
    dplyr::select(id, onset)

  d0 |>
    dplyr::left_join(
      e,
      by = dplyr::join_by(id)
    ) |>
    dplyr::mutate(
      age_onset = lubridate::time_length(
        difftime(onset, birth),
        unit = "years"
      ),
      sex = factor(
        dplyr::case_when(gender == "F" ~ 0, gender == "M" ~ 1),
        levels = c(0, 1),
        labels = c("female", "male"),
        ordered = FALSE
      ),
      first_visit_dur = lubridate::time_length(
        difftime(visit_date, onset),
        unit = "years"
      ),
      first_edss = edss
    ) |>
    dplyr::select(
      id,
      aggressive,
      sex,
      age_onset,
      first_visit_dur,
      first_edss
    ) |>
    dplyr::left_join(
      preprocess_predictors_treatment(t, ontab),
      by = dplyr::join_by(id)
    ) |>
    dplyr::left_join(
      preprocess_predictors_relapses(r, ontab),
      by = dplyr::join_by(id)
    ) |>
    dplyr::left_join(
      preprocess_predictors_mri(m, ontab),
      by = dplyr::join_by(id)
    ) |>
    dplyr::mutate(first_visit_dur = dplyr::if_else(
      first_visit_dur < 0, 0, first_visit_dur
    )) |>
    dplyr::rename(
      id = id,
      aggressive = aggressive,
      sex = sex,
      age_onset = age_onset,
      first_visit_dur = first_visit_dur,
      first_edss = first_edss,
      platform_y5 = Platform_5,
      platform_y10 = Platform_10,
      het_y5 = HET_5,
      het_y10 = HET_10,
      le_het_y5 = LE_HET_5,
      le_het_y10 = LE_HET_10,
      time_to_treat = time_to_treat,
      relapse_count = relapse_count,
      t2_lesions_volume_first = t2_lesions_volume_first,
      t1_blackholes_volume_first = t1_blackholes_volume_first,
      normalised_brain_atrophy_bpf_sv_first = normalised_brain_atrophz_bpf_sv_first,
      t1_tbv_sv_first = t1_tbv_sv_first,
      corpus_callosum_volume_first = corpus_callosum_volume_first,
      first_mri_dur = mri_duration_first,
      t2_lesions_volume_y2 = t2_lesions_volume_y2,
      t1_blackholes_volume_y2 = t1_blackholes_volume_y2,
      normalised_brain_atrophy_bpf_sv_y2 = normalised_brain_atrophz_bpf_sv_y2,
      t1_tbv_sv_y2 = t1_tbv_sv_y2,
      corpus_callosum_volume_y2 = corpus_callosum_volume_y2,
      mri_years_before_y2 = mri_years_before_y2,
      t2_lesions_volume_y5 = t2_lesions_volume_y5,
      t1_blackholes_volume_y5 = t1_blackholes_volume_y5,
      normalised_brain_atrophy_bpf_sv_y5 = normalised_brain_atrophz_bpf_sv_y5,
      t1_tbv_sv_y5 = t1_tbv_sv_y5,
      corpus_callosum_volume_y5 = corpus_callosum_volume_y5,
      mri_years_before_y5 = mri_years_before_y5,
      t2_lesions_volume_y10 = t2_lesions_volume_y10,
      t1_blackholes_volume_y10 = t1_blackholes_volume_y10,
      normalised_brain_atrophy_bpf_sv_y10 = normalised_brain_atrophz_bpf_sv_y10,
      t1_tbv_sv_y10 = t1_tbv_sv_y10,
      corpus_callosum_volume_y10 = corpus_callosum_volume_y10,
      mri_years_before_y10 = mri_years_before_y10
    )
}

#' Preprocess MRI data.
#'
#'
#' @param d A tibble with the original MRI data.
#' @param onset A tibble containing id/disease onset pairs.
#'
#' @return A tibble with data.
preprocess_predictors_mri <- function(d, onset) {
  d1 <- d |>
    dplyr::left_join(
      onset |>
        dplyr::mutate(
          plus2  = onset + lubridate::years(2),
          plus5  = onset + lubridate::years(5),
          plus10 = onset + lubridate::years(10)
        ),
      by = dplyr::join_by(id)
    ) |>
    dplyr::mutate(
      mri_duration = lubridate::time_length(difftime(mri_date, onset),  unit = "years"),
      close2  = lubridate::time_length(difftime(plus2, mri_date),  unit = "years"),
      close5  = lubridate::time_length(difftime(plus5, mri_date),  unit = "years"),
      close10 = lubridate::time_length(difftime(plus10, mri_date), unit = "years")
    )

  mri_vars <- c(
    "t2_lesions_volume",
    "t1_blackholes_volume",
    "normalised_brain_atrophz_bpf_sv",
    "t1_tbv_sv",
    "corpus_callosum_volume",
    "mri_duration"
  )

  ints <- c(
    rlang::quo(close2),
    rlang::quo(close5),
    rlang::quo(close10)
  )

  d_first <- d1 |>
    dplyr::filter(mri_duration > 0) |>
    dplyr::arrange(id, mri_duration) |>
    dplyr::group_by(id) |>
    summarise(dplyr::across(
      .cols  = tidyselect::all_of(mri_vars),
      .fns   = dplyr::first,
      .names = "{.col}_first"
    )) |>
    dplyr::ungroup()

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
    ) |>
    dplyr::left_join(
      d_first,
      by = dplyr::join_by(id)
    )
}

#' Preprocess relapse counts.
#'
#' @param d A tibble with the original relapse data.
#' @param onset A tibble containing id/disease onset pairs.
#'
#' @return A tibble with data.
preprocess_predictors_relapses <- function(d, onset) {
  d |>
    dplyr::left_join(
      onset |>
        dplyr::mutate(plus10 = onset + lubridate::years(10)),
      by = dplyr::join_by(id)
    ) |>
    dplyr::filter(relapse_date < plus10) |>
    tidyr::drop_na(relapse_date) |> # Keeps data with no severity rating
    dplyr::group_by(id) |>
    dplyr::summarise(
      relapse_count = dplyr::n(),
      .groups = "drop"
    )
}

#' Preprocess treatment variables.
#'
#' @param d A tibble with the original treatment data.
#' @param onset A tibble containing id/disease onset pairs.
#'
#' @return A tibble with data.
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
