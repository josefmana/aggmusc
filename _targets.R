# Load packages required to define the pipeline:
library(tidyverse) |> suppressPackageStartupMessages()
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set()

# Run the R scripts in the R/ folder with custom functions:
tar_source()

# Prepare the target list:
list(
  tar_target(
    name = data_file,
    command = "/Volumes/Extreme SSD/aggrescler/imed_export_20250328_data.xlsx",
    format = "file"
  ),
  tar_target(
    name = valid_patients,
    command = "/Volumes/Extreme SSD/aggrescler/patients_with_signed IC_202505_REMUS.xlsx",
    format = "file"
  ),
  tar_target(
    name = raw_data,
    command = prepare_data(data_file)
  ),
  tar_target(
    name = classified_data,
    command = determine_aggressive_phenotype(
      demographics = raw_data$id,
      relapses = raw_data$relapses,
      edss = raw_data$edss,
      eye_check = FALSE
    ) |>
      suppressWarnings()
  ),
  tar_target(
    name = finished_data,
    command = preprocess_predictors(
      d0 = classified_data$data,
      t = raw_data$treatment,
      r = raw_data$relapses,
      m = raw_data$mri,
      c = raw_data$csf,
      o = raw_data$ocb,
      chol = raw_data$cholesterol
    )
  )
)
