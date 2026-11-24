# Packages ----
library(OmopViewer)
library(omopgenerics)

# Functions ----
downloadPlot <- function(filename, type = "ggplot2") {
  if (type == "ggplot2") {
    fun <- "ggplot2::ggsave"
  } else if (type == "plotly") {
    fun <- "plotly::save_image"
  }
  list(
    label = "Download plot",
    filters = list(
      width = list(
        button_type = "numericInput",
        label = "Width",
        value = 15
      ),
      height = list(
        button_type = "numericInput",
        label = "Height",
        value = 15
      ),
      units = list(
        button_type = "pickerInput",
        label = "Units",
        selected = "cm",
        choices = c("px", "cm", "inch"),
        multiple = FALSE
      ),
      dpi = list(
        button_type = "numericInput",
        label = "DPI",
        value = 300
      )
    ),
    render = paste0(fun, "(
      filename = file,
      plot = <reactive_data>,
      width = as.numeric(input$width),
      height = as.numeric(input$height),
      units = input$units,
      dpi = as.numeric(input$dpi)
    )"),
    filename = filename
  )
}

# Data ----
res <- omopgenerics::importSummarisedResult(here::here(""))

# Panels ----
panelDetails <- panelDetailsFromResult(result = res)

## Study population ----
### Table One
allVars <- res |> filterSettings(result_type == "summarise_table") |> dplyr::pull(variable_name) |> unique()
timeVars <- c("exposure_start_gestational_week", "exposure_start_year")
panelDetails$summarise_table$title <- "Cohort Characteristics"
panelDetails$summarise_table$icon <- "users-gear"
panelDetails$summarise_table$automatic_filters <- c("group", "strata", "additional", "variable_name", "estimate_name")
panelDetails$summarise_table$content$tidy <- NULL
panelDetails$summarise_table$data <- list(variable_name = setdiff(allVars, timeVars), result_type = "summarise_table")

### Time distributions
panelDetails$time_distributions <- panelDetails$summarise_table
panelDetails$time_distributions$title <- "Time Zero"
panelDetails$time_distributions$icon <- "calendar-days"
panelDetails$time_distributions$automatic_filters <- c("group", "strata", "additional", "variable_name", "estimate_name")
panelDetails$time_distributions$data <- list(variable_name = timeVars, result_type = "summarise_table")
panelDetails$time_distributions$content <- list(
  plot = list(
    title       = "Plot",
    output_type = "ui",
    reactive    = "<filtered_data> |> visOmopResults::barPlot(x = input$time_distributions_scale, y = 'count', facet = input$time_distributions_facet, facet = input$time_distributions_color)",
    render      = "renderInteractivePlot(<reactive_data>, input$time_distributions_interactive)",
    filters = list(
      time_distributions_interactive = list(
        button_type = "materialSwitch",
        label = "Interactive",
        value = TRUE
      ),
      time_distributions_scale = list(
        button_type = "pickerInput",
        label = "Scale",
        choices = c("gestational_time", "calendar_time"),
        selected = 'gestational_time',
        multiple = TRUE
      ),
      time_distributions_color = list(
        button_type = "pickerInput",
        label = "Color",
        choices = c(
          'cdm_name', '<group>', '<strata>', '<additional>', '<settings>'
        ),
        selected = 'exposure',
        multiple = TRUE
      ),
      time_distributions_facet = list(
        button_type = "pickerInput",
        label = "Facet",
        choices = c(
          'cdm_name', '<group>', '<strata>', '<additional>', '<settings>'
        ),
        selected = 'cdm_name',
        multiple = TRUE
      )
    ),
    download = downloadPlot("plot_time_zero.png")
  )
)

## Standardised Mean Differences 
panelDetails$standardised_mean_difference$icon <- "scale-balanced"
panelDetails$standardised_mean_difference$content$table <- NULL
panelDetails$standardised_mean_difference$content$plot <- list(
  title       = "Plot",
  output_type = "ui",
  reactive    = "<filtered_data> |> visOmopResults::scatterPlot(x = 'smd_none', y = 'smd_ps_weight', line = FALSE, point = TRUE, ribbon = FALSE, facet = input$smd_facet, facet = input$smd_color)",
  render      = "renderInteractivePlot(<reactive_data>, input$smd_interactive)",
  filters = list(
    smd_interactive = list(
      button_type = "materialSwitch",
      label = "Interactive",
      value = TRUE
    ),
    smd_color = list(
      button_type = "pickerInput",
      label = "Color",
      choices = c(
        'cdm_name', '<group>', '<strata>', '<additional>', '<settings>'
      ),
      selected = 'variable_level',
      multiple = TRUE
    ),
    smd_facet = list(
      button_type = "pickerInput",
      label = "Facet",
      choices = c(
        'cdm_name', '<group>', '<strata>', '<additional>', '<settings>'
      ),
      selected = 'cdm_name',
      multiple = TRUE
    )
  ),
  download = downloadPlot("plot_asmd.png")
)

## Propensity Score ----
panelDetails$propensity_score_coefficients$title <- "PS Coefficients"
panelDetails$propensity_score_coefficients$content$table <- NULL
panelDetails$propensity_score_distribution$title <- "PS Distribution"
panelDetails$propensity_score_distribution$content <- NULL
panelDetails$propensity_score_distribution$content$plot <- list(
  title       = "Plot",
  output_type = "ui",
  reactive    = "<filtered_data> |> visOmopResults::scatterPlot(x = 'ps_x', y = 'ps_y', line = TRUE, point = FALSE, ribbon = FALSE, facet = input$ps_facet, facet = input$ps_color)",
  render      = "renderInteractivePlot(<reactive_data>, input$ps_interactive)",
  filters = list(
    ps_interactive = list(
      button_type = "materialSwitch",
      label = "Interactive",
      value = TRUE
    ),
    ps_color = list(
      button_type = "pickerInput",
      label = "Color",
      choices = c(
        'cdm_name', '<group>', '<strata>', '<additional>', '<settings>'
      ),
      selected = 'exposure',
      multiple = TRUE
    ),
    ps_facet = list(
      button_type = "pickerInput",
      label = "Facet",
      choices = c(
        'cdm_name', '<group>', '<strata>', '<additional>', '<settings>'
      ),
      selected = 'cdm_name',
      multiple = TRUE
    )
  ),
  download = downloadPlot("plot_ps_distribution.png")
)
panelDetails$propensity_score_weight_distribution$title <- "PS Fine Stratification Weights"

## Outcome ----
panelDetails$outcome_model$title <- "Odds Ratio"
panelDetails$outcome_model$icon <- "chart-bar"
panelDetails$outcome_model$content$plot <- list(
  title       = "Plot",
  output_type = "ui",
  reactive    = "<filtered_data> |> visOmopResults::scatterPlot(x = input$odds_ratio_plot_y, y = 'coef', ymin = 'lower_ci', ymax = 'upper_ci', line = FALSE, point = TRUE, ribbon = FALSE, facet = input$odds_ratio_facet, facet = input$odds_ratio_color)",
  render      = "renderInteractivePlot(<reactive_data>, input$odds_ratio_interactive)",
  filters = list(
    odds_ratio_interactive = list(
      button_type = "materialSwitch",
      label = "Interactive",
      value = TRUE
    ),
    odds_ratio_plot_y = list(
      button_type = "pickerInput",
      label = "Y-axis",
      choices = c(
        'cdm_name', '<group>', '<strata>', '<additional>', '<settings>'
      ),
      selected = 'outcome',
      multiple = TRUE
    ),
    odds_ratio_color = list(
      button_type = "pickerInput",
      label = "Color",
      choices = c(
        'cdm_name', '<group>', '<strata>', '<additional>', '<settings>'
      ),
      selected = 'outcome_group',
      multiple = TRUE
    ),
    odds_ratio_facet = list(
      button_type = "pickerInput",
      label = "Facet",
      choices = c(
        'cdm_name', '<group>', '<strata>', '<additional>', '<settings>'
      ),
      selected = 'cdm_name',
      multiple = TRUE
    )
  ),
  download = downloadPlot("plot_odds_ratio.png")
)
panelDetails$propensity_score_c_statistic <- NULL

# Static shiny ----
exportStaticApp(
  result = res,
  directory = dirname(here::here()),
  logo = NULL,
  title = "",
  background = TRUE,
  summary = FALSE,
  report = FALSE,
  panelDetails = panelDetails,
  panelStructure = list(
    "Study population" = c("summarise_table", "time_distributions", "standardised_mean_difference"),
    "Propensity Score" = c("propensity_score_coefficients", "propensity_score_distribution", "propensity_score_weight_distribution"),
    "Odds Ratio" = "outcome_model"
  ),
  theme = "default",
  template = NULL,
  updateButtons = TRUE,
  includeOneChoiceFilters = TRUE
)
