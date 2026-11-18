# Create results folder ----
resultsFolder <- here("Results")
dir.create(resultsFolder, recursive = TRUE, showWarnings = FALSE)

source(here("Analysis", "functions.R"), local = TRUE)

# Variables ----
categoryPrefixes <- c(
  age_group = "Age_", 
  infant_sex = "Infant_Sex_", 
  ocs_category = "OCS_",
  race = "race_", 
  region = "region_", 
  state = "state_",
  delivery_year_category = "Year_",
  number_diagnosis_category = "Num_Dx3_",
  number_ed_visits_category = "Num_ED_Visits_",
  number_hospitalisations_category = "Num_Hosp_",
  number_generics_no_antibiotics_category = "Num_generics_no_antibiotic_",
  number_outpatient_visits_category = "Num_Out_Visits_Dx_"
)
indicationGroups <- list(
  rti_ear = "rti_ear", 
  rti_other_urti = "rti_other_urti",
  rti_phrayngitis = "rti_phrayngitis", 
  rti_sinusitis = "rti_sinusitis",
  rti_lower = c("rti_other_lrti", "rti_pneumonia"),
  chlamydial_infections = "chlamydial_infections",
  sti_other = c(
    "other_sti", "endocervical_infection", "gonoccoccal_infections",
    "chancroid", "syphilis", "granuloma_inguinale"
  ),
  skin_infection = "skin_infection", 
  skin_other = "skin_other", 
  uti = "uti",
  gastrointestinal_infections = c(
    "gastroenteritis", "h_pylori", "cholera",
    "clostridial_infection", "acute_intestinal_amebiasis"
  ),
  rare_infections = c(
    "actinomycosis", "anthrax", "bartonellosis", "brucellosis", "yaws",
    "listeriosis", "lyme_disease", "malaria", "plague", "psittacosis",
    "relapsing_fever", "rickettsial_infections", "trachoma", "tularemia",
    "vincent_infection"
  )
)
studyVariables <- list(
  "Pregnancy" = c("age", "age_category", "lmp_year", "delivery_year", "delivery_year_category", "infant_sex"),
  "Exposure" = c(
    "exposure_start", "exposure_end", "exposure", "exposure_days", 
    "pregnant_exposure_days", "exposure_before_lmp",
    "exposure_start_gestational_week", "exposure_start_gestational_day"
  ),
  "Other demographics" = c("race", "state", "region"),
  "Obstetric conditions" = c("ocs", "ocs_category", "repeated_preg"),
  "Substance Use/Abuse" = c("alcohol_abuse", "smoking", "substance_abuse"),
  "Comorbidities" = c(
    "anemia", "diabetes_pregest", "epilepsy_convulsions",
    "hypertension_pregest", "obesity_overweight",
    "renal_disease", "thyroid_disease", "anxiety", "depression"
  ),
  "Medications" = c(
    "acetaminophen", "antidiabetics", "antihypertensives",
    "corticosteroids", "folate_suppl", "immunosuppressant",
    "insulin", "nsaids", "teratogen_suspected"
  ),
  "Indications ([LMP-90, LMP-1])" = paste0("w1_", names(indicationGroups)),
  "Indications ([LMP, LMP+90])" = paste0("w2_", names(indicationGroups)),
  "Indications ([LMP-90, LMP+90])" = names(indicationGroups),
  "Healthcare utilization" = c(
    "num_ed_visits", "num_dx3", "num_hosp", "num_out_visits_dx",
    "num_generics_no_antibiotic",
    "number_ed_visits_category", "number_generics_no_antibiotics_category",
    "number_hospitalisations_category", "number_outpatient_visits_category" 
  ),
  "Outcomes" =  c(
    'abdominal_wall', 'asd', 'av_septal_defect', 'cardiac', 
    'central_nervous_system', 'cleft_lip_all', 'cleft_palate_alone', 
    'conotruncal_defects', 'ear_anomalies', 'eye_anomalies', 'gastrointestinal',
    'genital', 'hypospadias', 'limb_defects', 'main_malform_overall', 
    'musculoskeletal', 'oral_cleft', 'oral_cleft_new_2', 'other', 
    'other_vascular', 'pda', 'respiratory_malformations', 'single_ventricle',
    'urinary', 'ventricular_septal_defect'
  )
)

exposure <- "DOXYCYCLINE"
comparators <- c("AMOXICILLIN", "AZITHROMYCIN") 

for (db in databases) {
  if (!db %in% names(dataFiles) || !nzchar(dataFiles[[db]])) {
    stop("Missing filename for database: ", db)
  }
  # DB-specific results folder ----
  dbResultsFolder <- file.path(resultsFolder, db)
  dir.create(dbResultsFolder, recursive = TRUE, showWarnings = FALSE)
  logFile <- file.path(dbResultsFolder, paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
  
  tryCatch({
    logMessage("START: clean_data ----")
    cleanedPath <- file.path(dbResultsFolder, paste0(db, ".rds"))
    if (cleanData) {
      source(here("Analysis", "01_CleanData.R"), local = TRUE)
    } else {
      logMessage("Load clean data")
      if (!file.exists(cleanedPath)) stop("Missing cleaned data: ", cleanedPath)
      data <- readRDS(cleanedPath)
    }
    steps <- list(
      propensity_score = list(run = runPropensityScore, file = "02_PropensityScore.R"),
      characterisation = list(run = characterisation, file = "03_Characterisation.R"),
      outcome_model = list(run = outcomeModel, file = "04_OutcomeModel.R")
    )
    for (step in names(steps)) {
      if (!isTRUE(steps[[step]]$run)) next
      logMessage("START ", step, " ----")
      t0 <- Sys.time()
      source(here("Analysis", steps[[step]]$file), local = TRUE)
      logMessage("DONE (", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 2), " min) ----")
    }
    logMessage("COMPLETED ", db)
  }, error = function(e) {
    logMessage("FAILED ", db, ": ", conditionMessage(e))
    stop(e)
  })
  rm(data)
  invisible(gc(verbose = FALSE))
}

logMessage("All selected databases completed.")

zip::zipr(
  zipfile = paste0("Results_", format(Sys.Date(), "%Y%m%d"), ".zip"),
  files = resultsFolder,
  recurse = TRUE
)
