# Weighetd data ----
if (!exists("weightedData", inherits = TRUE)) {
  logMessage("Read weighted data")
  weightedPath <- file.path(dataFolder, paste0("weighted_data_", db, ".rds"))
  if (!file.exists(weightedPath)) stop("Missing weighted data: ", weightedPath)
  weightedData <- readRDS(weightedPath)
}

# Define variables and estimates ----
categories <- c(
  "age_category", "infant_sex", "race", "state", "region", "ocs_category", 
  "repeated_preg", "exposure_before_lmp",
  studyVariables$`Substance Use/Abuse`,
  studyVariables$`Comorbidities`,
  studyVariables$`Medications`,
  studyVariables$`Indications ([LMP, LMP+90])`,
  studyVariables$`Indications ([LMP-90, LMP-1])`,
  studyVariables$`Indications ([LMP-90, LMP+90])`,
  "number_ed_visits_category", "number_generics_no_antibiotics_category",
  "number_hospitalisations_category", "number_outpatient_visits_category",
  "lmp_year", "exposure_start_gestational_week", "exposure_start_year"
)
continuous <- c(
  "age", "ocs", "num_ed_visits", "num_dx3", "num_hosp", "num_out_visits_dx",
  "num_generics_no_antibiotic", "exposure_days", "pregnant_exposure_days"
)

estimates <- list(
  c("mean", "sd", "median", "q25", "q75", "min", "max"),
  c("count", "percentage", "count_missing", "percentage_missing")
)
variables <- list(continuous, categories)

# Get table one ----
tableOneUnweighted <- data |>
  rename("subject_id" = "motherid") |>
  mutate(comparison_weights = "none") |>
  PatientProfiles::summariseResult(
    group = list("exposure"),
    includeOverallGroup = FALSE,
    strata = list("comparison_weights"),
    includeOverallStrata = FALSE,
    variables = variables,
    estimates = estimates
  )

tableOneWeighted <- weightedData |>
  rename("subject_id" = "motherid") |>
  PatientProfiles::summariseResult(
    group = list("exposure"),
    includeOverallGroup = FALSE,
    strata = list("comparison_weights"),
    includeOverallStrata = FALSE,
    variables = variables,
    estimates = estimates,
    counts = TRUE,
    weights = "ps_weight"
  )

tableOneClean <- bind(tableOneUnweighted, tableOneWeighted) |>
  mutate(
    cdm_name = db,
    variable_level = if_else(
      is.na(variable_level) & variable_name %in% categories,
      gsub("w1_|w2_", "", variable_name),
      variable_level
    ),
    variable_name = case_when(
      variable_name %in% studyVariables$`Indications ([LMP-90, LMP-1])` ~ "Indications [LMP-90, LMP-1]",
      variable_name %in% studyVariables$`Indications ([LMP, LMP+90])` ~ "Indications [LMP, LMP+90]",
      variable_name %in% studyVariables$`Indications ([LMP-90, LMP+90])` ~ "Indications [LMP-90, LMP+90]",
      variable_name %in% studyVariables$`Comorbidities` ~ "Comorbidities [LMP-90, Delivery]",
      variable_name %in% studyVariables$`Medications` ~ "Medications [LMP-90, LMP+90]",
      variable_name %in% studyVariables$`Substance Use/Abuse` ~ "Substance Use/Abuse [LMP-90, LMP+90]",
      .default = variable_name
    )
  ) 

tableOneClean |>
  exportSummarisedResult( 
    fileName = paste0("table_one_", db, ".csv"),
    minCellCount = minCellCount[db],
    path = dbResultsFolder
  )
