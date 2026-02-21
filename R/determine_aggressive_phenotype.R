#' Determine aggressive MS
#'
#' Classifies patients as suffering from an aggressive
#' form of Multiple Sclerosis (MS).
#'
#' @param demographics A tibble containing basic demographic
#'   variables.
#' @param relapses A tibble containing relapse dates.
#' @param edss A tibble containing EDSS scores over time.
#' @param eye_check Logical; if `TRUE`, plots of patients
#'   classified as having aggressive disease will be displayed
#'   for manual inspection.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{$data}{A tibble with the original demographic data
#'   plus an additional column indicating whether each patient
#'   was classified as having an aggressive disease course.}
#'   \item{$text}{A summary text describing the classification
#'   process.}
#' }
#'
#' @details
#' Uses the following the following criteria:
#'
#' - (i) The patient does **not** have a Primary Progressive phenotype.
#' - (ii) Disease duration is at least 10 years.
#' - (iii) EDSS at least 6 occurred within the first 10 years of the disease.
#' - (iv) EDSS at least 6 was sustained for at least 6 months after it first appeared.
#' - (v) EDSS never dropped below 6 until the end of the observation period.
#'
#' Additionally, a supplementary criterion must be met:
#' - (suppl.) The EDSS at least 6 threshold did **not** occur within 30 days after a relapse.
#'
#'
#' @examples
#' \dontrun{
#' p1 <- here::here("data-raw", "some_cool_file_name.xlsx")
#' p2 <- here::here("data-raw", "patients_with_informed_consent.xlsx")
#' d <- prepare_data(p1, p2)
#' outcome_data <- determine_aggressive_phenotype(
#'   demographics = d$id,
#'   relapses = d$relapses,
#'   edss = d$edss,
#'   eye_check = TRUE
#' )
#' }
#'
#' @export
determine_aggressive_phenotype <- function(
    demographics,
    relapses,
    edss,
    eye_check = TRUE
) {
  drop_ex <- function(orig, ex) {
    c(na.omit(orig[!orig %in% ex]))
  }
  pull_unqiue <- function(x, y) {
    x |>
      dplyr::distinct(get(y)) |>
      dplyr::pull()
  }

  # EDSS assessments need to have time stamps
  d0 <- dplyr::left_join(
    edss,
    demographics |> dplyr::select(id, onset, phenotype),
    by = dplyr::join_by(id)
  ) |>
    dplyr::mutate(edss_time = lubridate::time_length(
      difftime(visit_date, onset),
      unit = "years"
    ))
  d0 <- recode_typos(d0)
  unique_ids <- unique(d0$id)
  N0 <- length(unique_ids) # Original number of patients

  # Patients must have at least four EDSS measurements
  # and at least one before ten years of disease duration
  lowobs_ids <- d0 |>
    dplyr::group_by(id) |>
    dplyr::summarise(
      obs = dplyr::n(),
      min_time = min(edss_time)
    ) |>
    dplyr::filter(obs < 4 | min_time > 10) |>
    dplyr::pull(id)
  no_lowobs <- length(lowobs_ids)
  d1 <- subset(d0, !id %in% lowobs_ids)
  incl_ids0 <- unique(d1$id)
  cli::cli_alert_warning(
    "Dropped {no_lowobs} out of {N0} patients with less than four EDSS measurements or no EDSS before ten years of disease duration."
  )

  # Primary progressive phenotype disqualifies AMS
  cli::cli_alert_info(
    "Evaluating criterion (i) - The patient does not have a Primary Progressive phenotype."
  )
  N1 <- length(incl_ids0)
  pp_ids <- unique(subset(d1, phenotype == "PP" | is.na(phenotype))$id)
  no_pp <- length(pp_ids)
  d2 <- subset(d1, phenotype != "PP")
  incl_ids1 <- unique(d2$id)
  cli::cli_alert_warning(
    "Dropped {no_pp} out of {N1} patients with the primary progressive or missing phenotype."
  )

  # Disease duration has to be at least 10 years:
  cli::cli_alert_info(
    "Evaluating criterion (ii) - Disease duration is at least 10 years."
  )
  N2 <- length(incl_ids1)
  incl_ids2 <- d2 |>
    dplyr::filter(edss_time >= 10) |>
    pull_unqiue("id")
  excl_ids2 <- drop_ex(incl_ids1, incl_ids2)
  no_dur <- length(excl_ids2)
  d3 <- d2 |>
    dplyr::filter(!id %in% excl_ids2) |>
    dplyr::mutate(days_after_relapse = sapply(seq_along(visit_date), function(i) {
      rels <- relapses[relapses$id == id[i], "relapse_date"]
      sapply(rels, function(j) {
        days <- lubridate::time_length(
          difftime(visit_date[i], j),
          unit = "days"
        )
        Filter(\(x) x >= 0, days)
      }) |>
        unlist() |>
        min() |>
        suppressWarnings()
    }))
  cli::cli_alert_warning(
    "Dropped {no_dur} out of {N2} remaining patients due to short disease duration."
  )

  # High EDSS must have occured soon in the disease
  cli::cli_alert_info(
    "Evaluating criterion (iii) - EDSS at least 6 occurred within the first 10 years of the disease."
  )
  N3 <- length(unique(d3$id))
  incl_ids3 <- d3 |>
    dplyr::filter(edss_time < 10 & edss >= 6 & days_after_relapse > 30) |>
    pull_unqiue("id")
  excl_ids3 <- drop_ex(incl_ids2, incl_ids3)
  no_late <- length(excl_ids3)
  d4 <- subset(d3, !id %in% excl_ids3)
  cli::cli_alert_warning(
    "Dropped {no_late} out of {N3} remaining patients due to no high EDSS soon in the disease progression."
  )

  # High EDSS needs to be sustained
  cli::cli_alert_info(
    "Evaluating criterion (iv) - EDSS at least 6 was sustained for at least 6 months after it first appeared."
  )
  N4 <- length(unique(d4$id))
  incl_ids4 <- d4 |>
    dplyr::filter(edss >= 6 & days_after_relapse > 30) |>
    dplyr::group_by(id) |>
    dplyr::filter(dplyr::row_number() == 1 | dplyr::row_number() == dplyr::n()) |>
    dplyr::summarise(diff = max(edss_time) - min(edss_time), .groups = "drop") |>
    dplyr::filter(diff > 0.5) |>
    pull_unqiue("id")
  excl_ids4 <- drop_ex(incl_ids3, incl_ids4)
  no_sus <- length(excl_ids4)
  d5 <- subset(d4, !id %in% excl_ids4)
  cli::cli_alert_warning(
    "Dropped {no_sus} out of {N4} remaining patients due to not sustaining high EDSS long enough."
  )

  # EDSS did not drop back below six:
  cli::cli_alert_info(
    "Evaluating criterion (v) - EDSS never dropped below 6 until the end of the observation period."
  )
  N5 <- length(unique(d5$id))
  d6 <- d5 |>
    dplyr::left_join(
      d5 |>
        dplyr::group_by(id) |>
        dplyr::filter(dplyr::row_number() == dplyr::n()) |>
        dplyr::summarise(last_edss = edss, last_relapse_time = days_after_relapse),
      by = dplyr::join_by(id)
    ) |>
    dplyr::filter(last_edss >= 6 & last_relapse_time > 30)
  d7 <- dplyr::full_join(
    d6 |>
      dplyr::filter(edss >= 6 & last_relapse_time > 30 & edss_time < 10) |>
      dplyr::group_by(id) |>
      dplyr::filter(dplyr::row_number() == dplyr::n()),
    d6 |>
      dplyr::filter(edss_time >= 10)
  ) |>
    suppressMessages() |>
    dplyr::arrange(id)

  aggressiveMS <- sapply(unique(d7$id), function(i) {
    all(na.omit(subset(d7, id == i)$edss >= 6))
  })
  aggressiveMSids <- unique(d7$id)[aggressiveMS]
  excl_ids5 <- incl_ids4[!incl_ids4 %in% aggressiveMSids]
  no_drop <- length(excl_ids5)
  K <- length(aggressiveMSids)
  perc_aggr <- paste0(sprintf("%.2f", round(100 * K / N3, 2)), "%")
  cli::cli_alert_warning(
    "Dropped {no_drop} out of {N5} remaining paitens due to EDSS returning to lower values."
  )
  cli::cli_alert_success(
    "This has left {K} out of total {N3} eligible patients ({perc_aggr}) being classified
    as suffering the aggressive form of MS.\n\n"
  )

  if (eye_check) {
    cli::cli_alert_info(
      "Plots containing EDSS data of patients classified as suffering the aggressive form
      are shown for manual check."
    )
    bins <- rep(seq_len(ceiling(length(aggressiveMSids)/9)), 9) |>
      sort()
    spl <- split(aggressiveMSids, bins)
    for (i in seq_len(length(spl))) {
      plt <- d5 |>
        dplyr::filter(id %in% spl[[i]]) |>
        dplyr::mutate(`Following relapse` = dplyr::if_else(days_after_relapse < 30, TRUE, FALSE)) |>
        ggplot2::ggplot() +
        ggplot2::aes(x = edss_time, y = edss, group = id) +
        ggplot2::geom_point(size = 1, ggplot2::aes(colour = `Following relapse`)) +
        ggplot2::geom_line() +
        ggplot2::scale_colour_manual(values = c("black", "red3")) +
        ggplot2::geom_vline(
          xintercept = 10,
          colour = "red",
          linetype = "dashed",
          linewidth = 0.4
        ) +
        ggplot2::geom_hline(
          yintercept = 6,
          colour = "red",
          linetype = "dashed",
          linewidth = 0.4
        ) +
        ggplot2::facet_wrap(~ id, ncol = 3) +
        ggplot2::labs(x = "Disease duration at assessment (years)", y = "EDSS") +
        ggplot2::theme(legend.position = "none")
      plot(plt)
    }
  }
  # Prepare a text summarising the process:
  txt <- glue::glue(
    "Data from {N0} patients were extracted from a local database. Out of these,
    {no_lowobs} were excluded due to having less than four EDSS observations or
    no observations before 10 years of disease duration, and {no_pp} were excluded
    due to a diagnosis of primary progressive multiple sclerosis or missing phenotype
    information. Out of the remaining {N2} patients, {N3} patients met the criterion of
    a minimum of 10 years observation time, as defined by recorded EDSS scores. Of these,
    {no_late} patients did not meet the criterion of EDSS at least 6 within the first
    10 years of disease. Further {no_sus} patients did not meet the criterion of sustaining
    EDSS at least 6 until the end of the observation logitudinally. The final sample
    thus comprised {N3} patients, of whom {K} ({perc_aggr}) met criteria for aggressive
    disease."
  )
  cli::cli_alert_success(txt)

  df <- demographics |>
    dplyr::filter(id %in% incl_ids2) |>
    dplyr::mutate(aggressive = dplyr::if_else(id %in% aggressiveMSids, 1, 0))

  list(data = df, text = txt)
}
