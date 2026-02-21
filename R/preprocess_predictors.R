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
#'   d$id,
#'   d$relapses,
#'   d$edss,
#'   eye_check = TRUE
#' )
#' df <- preprocess_predictors(
#'   d0 = clas_data$data,
#'   edss = raw_data$edss,
#'   treat = raw_data$treatment,
#'   rel = raw_data$relapses,
#'   mri = raw_data$mri,
#'   csf = raw_data$csf,
#'   chol = raw_data$cholesterol
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
  t <- subset(treat, id %in% incl)
  r <- subset(rel, id %in% incl)
  m <- subset(mri, id %in% incl)
  c <- subset(csf, id %in% incl)
  c2 <- subset(chol, id %in% incl)
  e <- edss |>
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
