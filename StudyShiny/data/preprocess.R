# Offline preparation. Import the exact exported OMOP CSV structure.
# The export includes settings as rows with variable_name == "settings".
source("functions.R", local = TRUE)
files <- list.files("data/exports", pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
if (!length(files)) stop("No CSV exports found under data/exports")
resultList <- c(table_one = "summarise_table", ps_distribution = "propensity_score_distribution",
                ps_coefficients = "propensity_score_coefficients", weights = "propensity_score_weight_distribution",
                outcomes = "outcome_model")
resultTypes <- c(table_one = "summarise_table", ps_distribution = "propensity_score_distribution",
                 ps_coefficients = "propensity_score_coefficients", weights = "propensity_score_weight_distribution",
                 outcomes = "outcome_model")
result <- lapply(files, readExport)
data <- stats::setNames(lapply(names(resultTypes), function(kind) {
  matches <- result[vapply(result, function(z) identical(z$type, resultTypes[[kind]]), logical(1))]
  if (!length(matches)) return(tibble::tibble())
  dplyr::bind_rows(lapply(matches, function(z) z$rows))
}), names(resultTypes))
values <- lapply(data, getValues)
choices <- lapply(values, identity)
selected <- lapply(choices, function(z) lapply(z, function(v) if(length(v)) v[[1]] else character()))
databases <- sort(unique(unlist(lapply(data, function(x) x$cdm_name))))
comparisons <- unique(c(data$outcomes$group_level, data$table_one$strata_level))
comparisons <- sort(comparisons[grepl(" vs. ", comparisons, fixed = TRUE)])
save(data, values, choices, selected, databases, comparisons, file = "data/shinyData.RData")
message("Prepared ", length(files), " exported files; ", paste(names(data), lengths(data), sep = "=", collapse = ", "))
