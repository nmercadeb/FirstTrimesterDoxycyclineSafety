# Variables ----
psCovariates <- c(
  # Demographics
  "age", "age_group", "race"[db == "Medicaid"], "lmp_art", "exposure_start", "region", 
  
  # Obtetrics / Lifestyle
  "ocs", "repeated_preg", "smoking", "alcohol_abuse", "substance_abuse",
  
  # General markers of disease burden (continuous variables)
  "num_dx3", "num_ed_visits", "num_hosp", "num_generics_no_antibiotic", "num_out_visits_dx",
  
  # Comorbidities
  studyVariables$`Comorbidities`,
  
  # Medications
  studyVariables$`Medications`,
  
  # Indications
  paste0("w1_", names(indicationGroups)), paste0("w2_", names(indicationGroups))
)

# Initialize results
psCoefficients <- list()
psDistribution <- list()
weightDistribution <- list()
weightStrata <- list()
weightedData <- list()

# Start PS estimation by comparison ----
for (comparator in comparators) {
  
  ## Prepare data ----
  x <- subsetComparison(data, exposure, comparator)
  nInitial <- nrow(x)
  prep <- prepareModelData(x, psCovariates)
  x <- prep$data
  vars <- prep$vars
  
  ## Check for missing values
  if (anyNA(x[, vars, drop = FALSE])) {
    stop("Missing values detected in PS covariates for ", comparator)
  }
  
  ## Drop variables with no variation
  constantVars <- vars[
    vapply(
      x[, vars, drop = FALSE],
      function(v) dplyr::n_distinct(v) <= 1L,
      logical(1)
    )
  ]
  if (length(constantVars) > 0) {
    message(
      "Removing constant PS covariates for ", comparator, ": ",
      paste(constantVars, collapse = ", ")
    )
  }
  vars <- setdiff(vars, constantVars)
  
  ## Check treatment groups
  if (dplyr::n_distinct(x$.treated) != 2L) {
    stop("Both treatment groups are required for ", comparator)
  }
  
  ## Estimate PS ----
  formula <- reformulate(vars, response = ".treated")
  fit <- glm(formula, data = x, family = binomial())
  x <- x |>
    bind_cols(
      predict.glm(fit, newdata = x, type = "response") |>
        as_tibble() |>
        rename("ps" = "value")
    ) |>
    filter(!is.na(ps) & !is.infinite(ps))
  
  if (nrow(x) != nInitial) stop("Non-finite PS for ", comparator)
  
  ## Save coefficients
  psCoefficients[[comparator]] <- tibble(
    cohort_name_reference = comparator,
    cohort_name_comparator = exposure,
    variable_name = names(fit$coefficients),
    estimate_name = "coefficient",
    estimate_value = as.character(fit$coefficients)
  )
  
  ## Save PS distribution
  psDistribution[[comparator]] <- getDistributionPS(x, exposure, comparator)
  
  ## Add weights ----
  x <- x |> getFineStratificationWeights()
  
  ## Save weighted data
  weightedData[[comparator]] <- x |>
    mutate(
      comparison_weights = paste0(.env$exposure, " vs. ", .env$comparator)
    )
  
  ## Save weight distribution
  weightDistribution[[comparator]] <- x |>
    group_by(.treated) |>
    summarise(
      count = n(),
      sum_weights = sum(ps_weight),
      mean_weight = mean(ps_weight),
      sd_weight = sd(ps_weight),
      median_weight = median(ps_weight),
      q25_weight = quantile(ps_weight, 0.25),
      q75_weight = quantile(ps_weight, 0.75),
      min_weight = min(ps_weight),
      max_weight = max(ps_weight),
      effective_sample_size =
        sum(ps_weight)^2 / sum(ps_weight^2),
      .groups = "drop"
    ) |>
    mutate(
      cohort_name_reference = comparator,
      cohort_name_comparator = exposure,
      variable_name = if_else(.treated == 1, "exposed", "comparator"),
      strata_name = "overall",
      strata_level = "overall"
    ) |>
    select(-.treated)
  
  ## Save weight distribution by PS stratum
  weightStrata[[comparator]] <- x |>
    group_by(ps_stratum, .treated) |>
    summarise(
      count = n(),
      sum_weights = sum(ps_weight),
      mean_weight = mean(ps_weight),
      sd_weight = sd(ps_weight),
      median_weight = median(ps_weight),
      q25_weight = quantile(ps_weight, 0.25),
      q75_weight = quantile(ps_weight, 0.75),
      min_weight = min(ps_weight),
      max_weight = max(ps_weight),
      .groups = "drop"
    ) |>
    mutate(
      cohort_name_reference = comparator,
      cohort_name_comparator = exposure,
      variable_name = if_else(.treated == 1, "exposed", "comparator"),
      strata_name = "ps_stratum",
      strata_level = as.character(ps_stratum)
    ) |>
    select(-.treated, -ps_stratum)
}

# Save results ----
## Weighted data ----
weightedData <- weightedData |>
  list_rbind() 

saveRDS(weightedData, here(dataFolder, paste0("weighted_data_", db, ".rds")))

## Coefficients ----
psCoefficients |>
  purrr::list_rbind() |>
  mutate(variable_level = NA_character_) |>
  exportPSResult(
    resultType = "propensity_score_coefficients",
    db = db,
    dataFolder = dbResultsFolder,
    minCellCount = minCellCount[db]
  )

## PS distribution ----
psDistribution |>
  purrr::list_rbind() |>
  pivot_longer(
    cols = c(ps_x, ps_y),
    names_to = "estimate_name",
    values_to = "estimate_value"
  ) |>
  exportPSResult(
    resultType = "propensity_score_distribution",
    db = db,
    dataFolder = dbResultsFolder,
    minCellCount = minCellCount[db]
  )

## Weights distribution ----
weightResults <- bind_rows(
  purrr::list_rbind(weightDistribution),
  purrr::list_rbind(weightStrata)
) |>
  pivot_longer(
    cols = c(
      count, sum_weights, mean_weight, sd_weight,
      median_weight, q25_weight, q75_weight,
      min_weight, max_weight, effective_sample_size
    ),
    names_to = "estimate_name",
    values_to = "estimate_value"
  ) |>
  uniteGroup(c(
    "cohort_name_reference",
    "cohort_name_comparator"
  )) |>
  uniteAdditional() |>
  mutate(
    result_id = 1L,
    cdm_name = db,
    variable_level = NA_character_,
    estimate_value = as.character(estimate_value),
    estimate_type = "numeric"
  ) |>
  select(all_of(resultColumns())) |>
  newSummarisedResult(
    settings = tibble(
      result_id = 1L,
      package_name = "study_code",
      package_version = "v0.0.1",
      result_type = "propensity_score_weight_distribution"
    )
  ) |>
  exportSummarisedResult(
    fileName = paste0("propensity_score_weight_distribution_", db, ".csv"),
    minCellCount = minCellCount[db],
    path = dbResultsFolder
  )
