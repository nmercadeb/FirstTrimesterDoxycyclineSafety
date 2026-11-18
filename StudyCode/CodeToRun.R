library(zip)
library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(haven)
library(Hmisc)
library(glmnet)
library(lubridate)
library(omopgenerics)
library(PatientProfiles)

dataFolder <- here("Data")

dataFiles <- c(
  MarketScan = "ms_master_file_nov13.sas7bdat",
  Medicaid = "max_master_file_nov13.sas7bdat"
)
databases <- c("MarketScan", "Medicaid")
minCellCount <- c(
  MarketScan = 0,
  Medicaid = 11
)

# Job switches
cleanData <- TRUE
runPropensityScore <- TRUE
characterisation <- TRUE
outcomeModel <- TRUE

source(here("RunStudy.R"), local = TRUE)
