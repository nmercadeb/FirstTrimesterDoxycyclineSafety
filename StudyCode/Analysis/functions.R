collapseIndicators <- function(data, prefixes) {
  for (nm in names(prefixes)) {
    prefix <- unname(prefixes[[nm]])
    cols <- grep(paste0("^", prefix), names(data), value = TRUE)
    if (!length(cols)) {
      data[[nm]] <- rep(NA_character_, nrow(data))
      next
    }
    # Do not silently classify missing, zero or multiple positive indicators.
    mat <- as.matrix(data[, cols, drop = FALSE])
    suppressWarnings(storage.mode(mat) <- "numeric")
    if (any(!is.na(mat) & !(mat %in% c(0, 1)))) {
      stop("Non-binary indicator(s) in ", nm)
    }
    positive <- !is.na(mat) & mat == 1
    n_positive <- rowSums(positive)
    if (any(n_positive > 1)) {
      warning(sum(n_positive > 1), " rows have multiple positive indicators for ", nm, "; set to NA")
    }
    idx <- max.col(positive * 1L, ties.method = "first")
    out <- sub(paste0("^", prefix), "", cols[idx])
    out[n_positive != 1L] <- NA_character_
    data[[nm]] <- out
    data[cols] <- NULL
  }
  data
}

addIndicationFlags <- function(data, groups) {
  base_names <- unique(sub("^w[12]_", "", grep("^w[12]_", names(data), value = TRUE)))
  for (nm in base_names) {
    cols <- intersect(paste0(c("w1_", "w2_"), nm), names(data))
    if (nm %in% names(data)) stop("Overall indication would overwrite existing column: ", nm)
    data[[nm]] <- as.integer(rowSums(as.data.frame(lapply(data[cols], function(x) !is.na(x) & x == 1))) > 0)
    # NOTE: absent window and NA are treated as 0. Review this assumption.
  }
  for (group in names(groups)) {
    for (window in c("w1", "w2", "overall")) {
      vars <- if (window == "overall") groups[[group]] else paste0(window, "_", groups[[group]])
      cols <- intersect(vars, names(data))
      new_name <- if (window == "overall") group else paste0(window, "_", group)
      if (!length(cols)) {
        warning("No input columns for ", new_name, "; setting NA")
        data[[new_name]] <- rep(NA_integer_, nrow(data))
      } else if (length(cols) == 1 && identical(cols, new_name)) {
        next # already an individual flag; do not overwrite it
      } else {
        data[[new_name]] <- as.integer(rowSums(as.data.frame(lapply(data[cols], function(x) !is.na(x) & x == 1))) > 0)
      }
    }
  }
  data
}

logMessage <- function(...) {
  msg <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", paste0(..., collapse = ""))
  message(msg)
  cat(msg, "\n", file = logFile, append = TRUE)
}

makeResult <- function(rows, db, resultType, extraSettings = list()) {
  if (!nrow(rows)) stop("No rows to create summarised_result: ", resultType)
  required <- c("group_level", "variable_name", "variable_level", "estimate_name", "estimate_type", "estimate_value")
  stopifnot(all(required %in% names(rows)))
  if (!"strata_name" %in% names(rows)) rows$strata_name <- "overall"
  if (!"strata_level" %in% names(rows)) rows$strata_level <- "overall"
  out <- tibble::tibble(
    result_id = 1L, cdm_name = db,
    group_name = "exposure", group_level = as.character(rows$group_level),
    strata_name = as.character(rows$strata_name), strata_level = as.character(rows$strata_level),
    variable_name = as.character(rows$variable_name), variable_level = as.character(rows$variable_level),
    estimate_name = as.character(rows$estimate_name), estimate_type = as.character(rows$estimate_type),
    estimate_value = as.character(rows$estimate_value),
    additional_name = "overall", additional_level = "overall"
  )
  settings <- tibble::as_tibble(c(list(
    result_id = 1L, result_type = resultType,
    package_name = "doxycyclineSafety", package_version = "0.1.0"
  ), extraSettings))
  omopgenerics::newSummarisedResult(out, settings = settings)
}

exportResult <- function(result, dbFolder, name, minCellCount) {
  omopgenerics::exportSummarisedResult(
    result, minCellCount = minCellCount,
    path = dbFolder, fileName = paste0(name, ".csv")
  )
  saveRDS(result, file.path(dbFolder, paste0(name, ".rds")))
}

# For ordinary in-memory pregnancy-level SAS data, not CDM cohort tables.
# This is a study helper named summariseResult, NOT CohortCharacteristics::summariseResult.
summariseResult <- function(data, db, numericVars, categoricalVars, binaryVars) {
  if (!"exposure" %in% names(data)) stop("Missing exposure column")
  exposures <- unique(as.character(stats::na.omit(data$exposure)))
  out <- list()
  k <- 0L
  add <- function(group, variable, level, estimate, type, value) {
    tibble::tibble(group_level = group, variable_name = variable,
                   variable_level = level, estimate_name = estimate,
                   estimate_type = type, estimate_value = as.character(value))
  }
  for (exp in exposures) {
    sub <- data[!is.na(data$exposure) & as.character(data$exposure) == exp, , drop = FALSE]
    k <- k + 1L
    out[[k]] <- add(exp, "number_records", NA_character_, "count", "integer", nrow(sub))
    if ("motherid" %in% names(sub)) {
      k <- k + 1L
      out[[k]] <- add(exp, "number_subjects", NA_character_, "count", "integer", dplyr::n_distinct(sub$motherid, na.rm = TRUE))
    }
    for (nm in intersect(numericVars, names(sub))) {
      x <- suppressWarnings(as.numeric(sub[[nm]]))
      for (est in c("mean", "sd", "median", "q25", "q75")) {
        val <- if (all(is.na(x))) NA_real_ else switch(est,
          mean = mean(x, na.rm = TRUE), sd = stats::sd(x, na.rm = TRUE),
          median = stats::median(x, na.rm = TRUE),
          q25 = as.numeric(stats::quantile(x, 0.25, na.rm = TRUE)),
          q75 = as.numeric(stats::quantile(x, 0.75, na.rm = TRUE)))
        k <- k + 1L
        out[[k]] <- add(exp, nm, NA_character_, est, "numeric", val)
      }
    }
    for (nm in intersect(categoricalVars, names(sub))) {
      tab <- table(as.character(sub[[nm]]), useNA = "no")
      for (lev in names(tab)) {
        k <- k + 1L
        out[[k]] <- add(exp, nm, lev, "count", "integer", as.integer(tab[[lev]]))
        k <- k + 1L
        out[[k]] <- add(exp, nm, lev, "percentage", "percentage", 100 * as.integer(tab[[lev]]) / nrow(sub))
      }
    }
    for (nm in intersect(binaryVars, names(sub))) {
      x <- suppressWarnings(as.numeric(sub[[nm]]))
      if (any(!is.na(x) & !x %in% c(0, 1))) stop("Expected 0/1 variable: ", nm)
      n <- sum(x == 1, na.rm = TRUE)
      k <- k + 1L
      out[[k]] <- add(exp, nm, "1", "count", "integer", n)
      k <- k + 1L
      out[[k]] <- add(exp, nm, "1", "percentage", "percentage", 100 * n / nrow(sub))
    }
  }
  makeResult(dplyr::bind_rows(out), db, "characterisation")
}

# Match exposure labels case-insensitively, without changing source data.
subsetComparison <- function(data, exposure, comparator) {
  exp <- toupper(trimws(as.character(data$exposure)))
  keep <- !is.na(exp) & exp %in% c(exposure, comparator)
  out <- data[keep, , drop = FALSE]
  out$.treated <- as.integer(exp[keep] == exposure)
  if (length(unique(out$.treated)) != 2L) stop("Both exposure groups are required: ", exposure, " / ", comparator)
  out
}

prepareModelData <- function(data, vars) {
  vars <- intersect(vars, names(data))
  if (!length(vars)) stop("No available model covariates")
  for (nm in vars) {
    if (is.character(data[[nm]]) || is.logical(data[[nm]])) data[[nm]] <- factor(data[[nm]])
  }
  list(data = data, vars = vars)
}

getDistributionPS <- function(x, exposure, comparator) {
  res <- list()
  for (cohort in c(exposure, comparator)) {
    d <- density(x |> filter(.data$exposure == cohort) |> pull(ps))
    res[[cohort]] <- tibble(
      cohort_name_reference = comparator,
      cohort_name_comparator = exposure,
      variable_name = cohort,
      ps_x = d$x, 
      ps_y = d$y,
      variable_level = as.character(1:length(d$x))
    ) 
  }
  return(bind_rows(res))
}

getFineStratificationWeights <- function(x, nStrata = 50) {
  
  # PS quantile cutpoints among exposed only
  cuts <- x |>
    filter(.treated == 1) |>
    pull(ps) |>
    quantile(
      probs = seq(0, 1, length.out = nStrata + 1),
      na.rm = TRUE
    ) |>
    unique()
  
  if (length(cuts) < nStrata + 1) {
    warning("Fewer than ", nStrata, " distinct PS strata possible")
  }
  
  # Assign strata to both groups
  x <- x |>
    mutate(ps_stratum = findInterval(ps, vec = cuts, all.inside = TRUE))
  
  # Calculate stratum-specific counts
  strataCounts <- x |>
    count(ps_stratum, .treated, name = "n") |>
    tidyr::pivot_wider(
      names_from = .treated,
      values_from = n,
      values_fill = 0,
      names_prefix = "n_"
    ) |>
    rename(
      n_comparator = n_0,
      n_exposed = n_1
    )
  
  # Check positivity
  if (any(strataCounts$n_comparator == 0 & strataCounts$n_exposed > 0)) {
    stop("Some PS strata contain exposed but no comparators")
  }
  
  # Calculate ATT fine-stratification weights
  strataCounts <- strataCounts |>
    mutate(weight_comparator = n_exposed / n_comparator)
  
  # Assign weights
  x <- x |>
    left_join(
      strataCounts |>
        select(ps_stratum, weight_comparator),
      by = "ps_stratum"
    ) |>
    mutate(ps_weight = if_else(.treated == 1, 1, weight_comparator)) |>
    select(-weight_comparator)
  
  return(x)
}

exportPSResult <- function(x, resultType, db, dataFolder, minCellCount) {
  
  x |>
    uniteGroup(c("cohort_name_reference", "cohort_name_comparator")) |>
    uniteStrata() |>
    uniteAdditional() |>
    mutate(
      result_id = 1L,
      cdm_name = db,
      estimate_value = as.character(estimate_value),
      estimate_type = "numeric"
    ) |>
    select(all_of(resultColumns())) |>
    newSummarisedResult(
      settings = tibble(
        result_id = 1L,
        package_name = "study_code",
        package_version = "v0.0.1",
        result_type = resultType
      )
    ) |>
    exportSummarisedResult(
      fileName = paste0(resultType, "_", db, ".csv"),
      path = dataFolder,
      minCellCount = minCellCount
    )
}

getModelWeights <- function(x, weighting = "none") {
  if (identical(weighting, "none")) return(rep(1, nrow(x)))
  if (!weighting %in% names(x)) stop("Weight column not found: ", weighting)
  w <- x[[weighting]]
  if (!is.numeric(w) || anyNA(w) || any(!is.finite(w)) || any(w <= 0)) {
    stop("Invalid weights in ", weighting)
  }
  w
}

getEventCounts <- function(x, outcome, weighting = "none", treatment = ".treated") {
  if (!all(c(outcome, treatment) %in% names(x))) stop("Missing outcome or treatment column")
  y <- x[[outcome]]
  trt <- x[[treatment]]
  if (anyNA(y) || anyNA(trt) || any(!y %in% 0:1) || any(!trt %in% 0:1)) {
    stop("Outcome and treatment must be complete binary indicators: ", outcome)
  }
  w <- getModelWeights(x, weighting)
  c(
    events_exposed = sum(y == 1 & trt == 1),
    events_comparator = sum(y == 1 & trt == 0),
    weighted_events_exposed = sum(w[y == 1 & trt == 1]),
    weighted_events_comparator = sum(w[y == 1 & trt == 0])
  )
}

formatCount <- function(value, rawCount, minCellCount) {
  if (rawCount == 0) return("0")
  if (rawCount < minCellCount) return("-")
  as.character(value)
}

fitModel <- function(x, outcome, weighting = "none", cluster = "motherid",
                     family = stats::binomial(), confLevel = 0.95) {
  empty <- c(coef = NA_real_, se = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_)
  if (!cluster %in% names(x) || anyNA(x[[cluster]])) {
    return(list(values = empty, status = "invalid_cluster"))
  }
  y <- x[[outcome]]
  trt <- x$.treated
  w <- getModelWeights(x, weighting)
  if (anyNA(y) || anyNA(trt) || any(!y %in% 0:1) || any(!trt %in% 0:1)) {
    return(list(values = empty, status = "invalid_input"))
  }
  if (length(unique(trt)) != 2L ||
      any(vapply(0:1, function(g) length(unique(y[trt == g])) < 2L, logical(1)))) {
    return(list(values = empty, status = "separation_or_no_events"))
  }
  
  modelData <- data.frame(.outcome = y, .treated = trt, .weight = w)
  fit <- tryCatch(
    suppressWarnings(stats::glm(.outcome ~ .treated, data = modelData,
                                weights = .weight, family = family)),
    error = function(e) NULL
  )
  if (is.null(fit) || !isTRUE(fit$converged)) {
    return(list(values = empty, status = "model_failed"))
  }
  variance <- tryCatch(
    sandwich::vcovCL(fit, cluster = x[[cluster]], type = "HC1"),
    error = function(e) NULL
  )
  beta <- stats::coef(fit)[[".treated"]]
  if (is.null(variance) || !is.finite(beta) ||
      !is.finite(variance[".treated", ".treated"]) ||
      variance[".treated", ".treated"] <= 0) {
    return(list(values = empty, status = "robust_se_failed"))
  }
  se <- sqrt(variance[".treated", ".treated"])
  z <- stats::qnorm(1 - (1 - confLevel) / 2)
  # 'coef' is the exponentiated treatment coefficient (OR for logistic models).
  values <- c(coef = exp(beta), se = se,
              ci_lower = exp(beta - z * se), ci_upper = exp(beta + z * se))
  if (any(!is.finite(values))) return(list(values = empty, status = "nonfinite_estimate"))
  list(values = values, status = "ok")
}

summariseModel <- function(x, outcome, exposure, comparator, weighting = "none",
                           minCellCount = 11L, strataName = "overall",
                           strataLevel = "overall", cluster = "motherid") {
  counts <- getEventCounts(x, outcome, weighting)
  nE <- counts[["events_exposed"]]
  nC <- counts[["events_comparator"]]
  eligible <- nE >= minCellCount && nC >= minCellCount
  fit <- if (eligible) {
    fitModel(x, outcome, weighting = weighting, cluster = cluster)
  } else {
    list(values = c(coef = NA_real_, se = NA_real_,
                    ci_lower = NA_real_, ci_upper = NA_real_),
         status = "small_event_cell")
  }
  
  estimates <- c(
    events_exposed = formatCount(nE, nE, minCellCount),
    events_comparator = formatCount(nC, nC, minCellCount),
    weighted_events_exposed = formatCount(counts[["weighted_events_exposed"]], nE, minCellCount),
    weighted_events_comparator = formatCount(counts[["weighted_events_comparator"]], nC, minCellCount),
    vapply(fit$values, as.character, character(1)),
    status = fit$status
  )
  
  tibble::tibble(
    group_name = "comparison",
    group_level = paste0(exposure, " vs. ", comparator),
    strata_name = strataName,
    strata_level = strataLevel,
    variable_name = "outcome",
    variable_level = outcome,
    estimate_name = names(estimates),
    estimate_type = c(rep("character", 4), rep("numeric", 4), "character"),
    estimate_value = unname(estimates),
    additional_name = "overall",
    additional_level = "overall",
    weighting = weighting
  )
}

newModelResult <- function(x, db, resultType = "outcome_model") {
  weightings <- unique(x$weighting)
  settings <- tibble::tibble(
    result_id = seq_along(weightings),
    package_name = "doxycyclineSafety",
    package_version = "0.1.0",
    result_type = resultType,
    weighting = weightings
  )
  x <- x |>
    dplyr::mutate(
      result_id = match(.data$weighting, weightings),
      cdm_name = db
    ) |>
    dplyr::select(dplyr::all_of(omopgenerics::resultColumns()))
  omopgenerics::newSummarisedResult(x, settings = settings)
}
