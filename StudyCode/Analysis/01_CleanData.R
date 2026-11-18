# Read data
inputPath <- file.path(dataFolder, dataFiles[[db]])
if (!file.exists(inputPath)) stop("Missing SAS file: ", inputPath)
logMessage("Read ", inputPath)
data <- haven::read_sas(inputPath) |>
  dplyr::mutate(database_name = db) |>
  # TEMPORARY
  rename("motherid" = "randomID", "pregnancyid" = "randomid_date")


# Convert indicator flags into a single category and drop individual columns
logMessage("Collapse categorical variables")
data <- collapseIndicators(data, categoryPrefixes)

# Add combined w1/w2 indication flags and clinically grouped flags
# Existing individual w1_*/w2_* columns remain unchanged
logMessage("Add indication flags")
data <- addIndicationFlags(data, indicationGroups)

# Add variables
data <- data |>
  mutate(
    lmp_art = as.Date(lmp_art, format = "%d%b%Y"),
    exposure_start = as.Date(exposure_start, origin = "1960-01-01"),
    exposure_end = as.Date(exposure_end, origin = "1960-01-01"),
    exposure_start_gestational_day = as.numeric(exposure_start - lmp_art),
    exposure_days = as.numeric(exposure_end - exposure_start),
    pregnant_exposure_days = if_else(
      exposure_start < lmp_art,
      as.numeric(exposure_end - lmp_art),
      exposure_days
    ),
    exposure_start_gestational_week = cut(
      x = exposure_start_gestational_day,
      breaks = seq.int(-91, 91, 7),
      labels = paste0("Week ", -13:12),
      right = FALSE
    ),
    exposure_before_lmp = if_else(
      exposure_start < lmp_art, 1L, 0L
    ),
    lmp_year = year(lmp_art),
    exposure_start_year = year(exposure_start)
  )

# Standardise names after prefix-based processing (case sensitive prefixes).
data <- dplyr::rename_with(data, tolower)
if (anyDuplicated(names(data))) stop("Duplicate column names after lowercasing")
if (anyNA(data$exposure)) warning("Missing exposure labels detected")

logMessage("Save clean data")
saveRDS(data, cleanedPath)
