targets_data <- list(
  targets::tar_target(
    name = data_file,
    command = "/Volumes/Extreme SSD/aggrescler/imed_export_20250328_data.xlsx",
    format = "file"
  ),

  targets::tar_target(
    name = valid_patients,
    command = "/Volumes/Extreme SSD/aggrescler/patients_with_signed IC_202505_REMUS.xlsx",
    format = "file"
  ),

  targets::tar_target(
    name = raw_data,
    command = prepare_data(
      dp = data_file,
      ip = valid_patients
    )
  ),

  targets::tar_target(
    name = classified_data,
    command = determine_aggressive_phenotype(
      demographics = raw_data$id,
      relapses = raw_data$relapses,
      edss = raw_data$edss,
      eye_check = FALSE
    ) |>
      suppressWarnings()
  ),

  targets::tar_target(
    name = finished_data,
    command = preprocess_predictors(
      d0 = classified_data$data,
      edss = raw_data$edss,
      treat = raw_data$treatment,
      rel = raw_data$relapses,
      mri = raw_data$mri,
      csf = raw_data$csf,
      chol = raw_data$cholesterol
    )
  ),

  targets::tar_target(
    name = pilot_data,
    command = finished_data |>
      dplyr::select()
  )
)
