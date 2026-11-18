# Read weighted data if not available ----
if (!exists("weightedData", inherits = TRUE)) {
  weightedPath <- file.path(dataFolder, paste0("weighted_data_", db, ".rds"))
  if (!file.exists(weightedPath)) stop("Missing weighted data: ", weightedPath)
  weightedData <- readRDS(weightedPath)
}

required <- c("motherid", "exposure", ".treated", "ps_weight", "comparison_weights", studyVariables$Outcomes)
if (!all(required %in% names(weightedData))) {
  stop("Missing weighted data columns: ", paste(setdiff(required, names(weightedData)), collapse = ", "))
}

# Run outcome models ----
results <- purrr::map_dfr(comparators, function(comparator) {
  comparison <- paste0(exposure, " vs. ", comparator)
  crude <- subsetComparison(data, exposure, comparator)
  adjusted <- weightedData |>
    filter(.data$comparison_weights == comparison)

  if (!nrow(crude) || !nrow(adjusted)) stop("Missing comparison data: ", comparison)
  if (!all(0:1 %in% crude$.treated) || !all(0:1 %in% adjusted$.treated)) {
    stop("Both treatment groups are required: ", comparison)
  }

  purrr::map_dfr(studyVariables$Outcomes, function(outcome) {
    dplyr::bind_rows(
      summariseModel(crude, outcome, exposure, comparator, weighting = "none", minCellCount = minCellCount[db]),
      summariseModel(adjusted, outcome, exposure, comparator, weighting = "ps_weight", minCellCount = minCellCount[db])
    )
  })
})

# Create and export summarised result ----
newModelResult(results, db) |>
  exportSummarisedResult(
    fileName = paste0("outcome_model_", db, ".csv"),
    path = dbResultsFolder,
    minCellCount = 0L # already suppressed in creation
  )
