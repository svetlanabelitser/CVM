# #set directory with input data

setwd("..")
setwd("..")
dirbase<-getwd()
# dirinput <- paste0(dirbase,"/CDMInstances/CVM2205_EFFICACY_CHILDREN/")

# dirinput <- paste0(thisdir,"/i_input/")
dirinput <- paste0(thisdir,"/i_input_subpop/")

set_and_create_dir <- function(x) {
  x <- paste0(thisdir, x)
  dir.create(file.path(x), showWarnings = F)
  return(x)
}

# set other directories
diroutput <- set_and_create_dir("/g_output/")
dirtemp <- set_and_create_dir("/g_intermediate/")
dirconceptsets <- set_and_create_dir("/g_intermediate/concept_sets/")
direxp <- set_and_create_dir("/g_export/")
dirmacro <- set_and_create_dir("/p_macro/")
dirfigure <- set_and_create_dir("/g_figure/")
extension <- c(".csv")
dirpargen <- set_and_create_dir("/g_parameters/")
direvents <- set_and_create_dir("/g_intermediate/events/")
dircomponents <- set_and_create_dir("/g_intermediate/components/")
PathOutputFolder <- set_and_create_dir("/g_describeHTML")

# load packages
if (!require("MASS")) install.packages("MASS")
library(MASS)
if (!require("haven")) install.packages("haven")
library(haven)
if (!require("tidyverse")) install.packages("tidyverse")
library(dplyr)
if (!require("lubridate")) install.packages("lubridate")
library(lubridate)
if (!require("AdhereR")) install.packages("AdhereR")
library(AdhereR)
if (!require("stringr")) install.packages("stringr")
library(stringr)
if (!require("purrr")) install.packages("purrr")
library(purrr)
if (!require("readr")) install.packages("readr")
library(readr)
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)
if (!require("survival")) install.packages("survival")
library(survival)
if (!require("rmarkdown")) install.packages("rmarkdown")
library(rmarkdown)
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)
if (!require("data.table")) install.packages("data.table")
library(data.table)
if (!require("qpdf")) install.packages("qpdf")
library(qpdf)
if (!require("parallel")) install.packages("parallel")
library(parallel)
if (!require("readxl")) install.packages("readxl")
library(readxl)

# load macros

source(paste0(dirmacro,"CreateConceptSetDatasets_v20.R"))
#source(paste0(dirmacro,"RetrieveRecordsFromEAVDatasets.R"))
# source(paste0(dirmacro,"CreateItemsetDatasets.R"))
source(paste0(dirmacro,"CreateItemsetDatasets.R"))
source(paste0(dirmacro,"MergeFilterAndCollapse_v5.R"))
source(paste0(dirmacro,"CreateSpells_v15.R"))
source(paste0(dirmacro,"CreateFlowChart.R"))
#source(paste0(dirmacro,"CountPersonTimeV12.4.R"))

source(paste0(dirmacro,"CleanOutcomes.R"))
source(paste0(dirmacro,"CreateAgebandIntervals.R"))
source(paste0(dirmacro,"CreateTimeIntervals.R"))
source(paste0(dirmacro,"CheckAndPrepareDates.R"))
source(paste0(dirmacro,"CalculateSubtractionDenominator.R"))
source(paste0(dirmacro,"CalculateNumeratorAggregated.R"))
source(paste0(dirmacro,"SplitSpellsAgeBands.R"))
source(paste0(dirmacro,"CalculateNumeratorNotRecurrent.R"))
source(paste0(dirmacro,"SetToInteger.R"))
source(paste0(dirmacro,"CountPersonTimeV13.9.R"))

source(paste0(dirmacro,"ApplyComponentStrategy_v13_2.R"))
source(paste0(dirmacro,"CreateFigureComponentStrategy_v4.R"))
source(paste0(dirmacro,"DRECountThresholdV4.R"))
source(paste0(dirmacro,"table1.R"))
source(paste0(dirmacro,"scri_tools.R"))
source(paste0(dirmacro,"df_to_list_of_list.R"))
source(paste0(dirmacro,"test_cleaning.R"))
source(paste0(dirmacro,"launch_step.R"))

#other parameters

date_format <- "%Y%m%d"

firstjan2021 <- ymd(20210101)
#---------------------------------------
# understand which datasource the script is querying

CDM_SOURCE<- fread(paste0(dirinput,"CDM_SOURCE.csv"))
thisdatasource <- as.character(CDM_SOURCE[1,3])

study_start <- as.Date(as.character(20190101), date_format)
start_lookback <- as.Date(as.character(20180101), date_format)

study_end <- min(as.Date(as.character(CDM_SOURCE[1,"date_creation"]), date_format),
                 as.Date(as.character(CDM_SOURCE[1,"recommended_end_date"]), date_format), na.rm = T)

instance_creation <- as.Date(as.character(CDM_SOURCE[1,"date_creation"]), date_format)

start_COVID_vaccination_date <- fifelse(thisdatasource == 'CPRD',
                                        as.Date(as.character(20201206), date_format),
                                        as.Date(as.character(20201227), date_format))

# TODO ask SIDIAP or search date (same as BIFAP?)
start_COVID_diagnosis_date <- case_when((thisdatasource == 'TEST') ~ ymd(20200131),
                                        (thisdatasource == 'ARS') ~ ymd(20200131),
                                        (thisdatasource == 'PHARMO') ~ ymd(20200227),
                                        (thisdatasource == 'CPRD') ~ ymd(20200123),
                                        (thisdatasource == 'BIFAP') ~ ymd(20200131),
                                        (thisdatasource == 'SIDIAP') ~ ymd(20200131),
                                        TRUE ~ ymd(20200131))

###################################################################
# CREATE EMPTY FILES
###################################################################

files<-sub('\\.csv$', '', list.files(dirinput))

if (!any(str_detect(files,"^SURVEY_ID"))) {
  print("Creating empty SURVEY_ID since none were found")
  fwrite(data.table(person_id = character(0), survey_id = character(0), survey_date = character(0),
                    survey_meaning = character(0)),
         paste0(dirinput, "SURVEY_ID", ".csv"))
}

if (!any(str_detect(files,"^SURVEY_OBSERVATIONS"))) {
  print("Creating empty SURVEY_OBSERVATIONS since none were found")
  fwrite(data.table(person_id = character(0), so_date = character(0), so_source_table = character(0),
                    so_source_column = character(0), so_source_value = character(0), so_unit = character(0),
                    survey_id = character(0)),
         paste0(dirinput, "SURVEY_OBSERVATIONS", ".csv"))
}

if (!any(str_detect(files,"^MEDICINES"))) {
  print("Creating empty MEDICINES since none were found")
  fwrite(data.table(person_id = character(0), medicinal_product_id = integer(0),
                    medicinal_product_atc_code = character(0), date_dispensing = integer(0),
                    date_prescription = logical(0), disp_number_medicinal_product = numeric(0),
                    presc_quantity_per_day = logical(0), presc_quantity_unit = logical(0),
                    presc_duration_days = logical(0), product_lot_number = logical(0),
                    indication_code = logical(0), indication_code_vocabulary = logical(0),
                    meaning_of_drug_record = character(0), origin_of_drug_record = character(0),
                    prescriber_speciality = logical(0), prescriber_speciality_vocabulary = logical(0),
                    visit_occurrence_id = character(0)),
         paste0(dirinput, "MEDICINES_FED", ".csv"))
}

#############################################
#SAVE METADATA TO direxp
#############################################

file.copy(paste0(dirinput,'/METADATA.csv'), direxp, overwrite = T)
file.copy(paste0(dirinput,'/CDM_SOURCE.csv'), direxp, overwrite = T)
file.copy(paste0(dirinput,'/INSTANCE.csv'), direxp, overwrite = T)

#############################################
#SAVE to_run.R TO direxp
#############################################

file.copy(paste0(thisdir,'/to_run.R'), direxp, overwrite = T)

#study_years_datasource

study_years <- c("2019", "2020","2021")

# TODO should add 2020?
firstYearComponentAnalysis = "2019"
secondYearComponentAnalysis = "2020"

days<-ifelse(thisdatasource %in% c("ARS","TEST"),180,1)

#############################################
#RECODING FOR OUTPUT TABLES
#############################################

vect_recode_dap <- c(TEST = "Italy_ARS",
                              ARS = "Italy_ARS",
                              PHARMO = "NL_PHARMO",
                              CPRD = "UK_CPRD",
                              BIFAP = "ES_BIFAP")

vect_recode_dap <- data.table(ori = names(vect_recode_dap), new = vect_recode_dap)

export_dap_name <- as.character(as.data.table(thisdatasource)[vect_recode_dap,
                                                              on = .(thisdatasource = ori),
                                                              "thisdatasource" := .(i.new)])

vect_new_severity <- c("covid_severity_1_plus", "covid_severity_2_plus", "covid_severity_3_plus",
                       "covid_severity_4_plus", "covid_severity_1", "covid_severity_2", "covid_severity_3",
                       "covid_severity_4")

#############################################
#FUNCTION TO COMPUTE AGE
#############################################

Agebands = c(-1, 4, 11, 17, 24, 29, 39, 49, 59, 69, 79, Inf)
Agebands_countpersontime = c(0, 4, 11, 17, 24, 29, 39, 49, 59, 69, 79)
Agebands_labels = c("0-4","5-11","12-17","18-24","25-29", "30-39", "40-49","50-59","60-69", "70-79","80+")
Agebands_children = c("0-4","5-11","12-17")

Agebands60 <- c("60-69", "70-79","80+")
Agebands059 <- c("0-4","5-11","12-17","18-24","25-29", "30-39", "40-49","50-59")

age_fast = function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age = to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}

`%not in%` = Negate(`%in%`)

find_last_monday <- function(tmp_date, monday_week) {
  
  tmp_date <- as.Date(lubridate::ymd(tmp_date))
  
  while (tmp_date %not in% monday_week) {
    tmp_date <- tmp_date - 1
  }
  return(tmp_date)
}

find_first_monday_year <- function(tmp_date, monday_week) {
  
  tmp_date <- as.Date(lubridate::ymd(tmp_date))
  
  while (tmp_date %not in% monday_week) {
    tmp_date <- tmp_date + 1
  }
  return(tmp_date)
}

correct_difftime <- function(t1, t2, t_period = "days") {
  return(difftime(t1, t2, units = t_period) + 1)
}

calc_precise_week <- function(time_diff) {
  # correction in case a person exit the same date it enter
  time_diff <- fifelse(time_diff == 1, time_diff + 1, time_diff)
  weeks_frac <- time_length(time_diff - 1, "week")
  fifelse(weeks_frac%%1==0, weeks_frac, floor(weeks_frac) + 1)
}

join_and_replace <- function(df1, df2, join_cond, old_name) {
  temp <- merge(df1, df2, by.x = join_cond[1], by.y = join_cond[2])
  temp[, join_cond[1] := NULL]
  setnames(temp, old_name, join_cond[1])
}

import_concepts <- function(dirtemp, concept_set) {
  concepts<-data.table()
  for (concept in concept_set) {
    load(paste0(dirtemp, concept,".RData"))
    if (exists("concepts")) {
      concepts <- rbind(concepts, get(concept))
    } else {
      concepts <- get(concept)
    }
  }
  return(concepts)
}

exactPoiCI <- function (df, X, PT, conf.level = 0.95) {
  alpha <- 1 - conf.level
  IR <- df[, get(X)]
  upper <- df[, 0.5 * qchisq((1-(alpha/2)), 2*(get(X)+1))]
  lower <- df[, 0.5 * qchisq(alpha/2, 2*get(X))]
  temp_list <- lapply(list(IR, lower, upper), `/`, df[, get(PT)/365.25])
  temp_list <- lapply(temp_list, `*`, 100000)
  temp_list <- lapply(temp_list, function(x) {fifelse(x == Inf, 0, x)})
  return(lapply(temp_list, round, 2))
}

correct_col_type <- function(df) {
  for (i in names(df)){
    df[is.na(get(i)), (i) := 0]
    if (!inherits(df[, get(i)], "IDate")) {
      df[is.integer(get(i)), (i) := as.numeric(get(i))]
    }
    df[is.logical(get(i)), (i) := as.numeric(get(i))]
  }
  return(df)
}

bc_divide_60 <- function(df, by_cond, cols_to_sums, only_old = F, col_used = "ageband_at_study_entry") {
  older60 <- copy(df)[get(col_used) %in% Agebands60,
                      lapply(.SD, sum, na.rm=TRUE), by = by_cond, .SDcols = cols_to_sums]
  older60 <- unique(older60[, c(col_used) := "60+"])
  if (!only_old) {
    younger60 <- copy(df)[get(col_used) %in% Agebands059,
                          lapply(.SD, sum, na.rm=TRUE), by = by_cond, .SDcols = cols_to_sums]
    younger60 <- unique(younger60[, c(col_used) := "0-59"])
    
    df <- rbind(df, younger60)
  }
  df <- rbind(df, older60)
  return(df)
}

prop_to_total <- function(x){paste0(round(x / total_doses * 100, 2), "%")}

smart_save <- function(df, folder, subpop = "") {
  qsave(df, paste0(folder, deparse(substitute(df)), suffix[[subpop]], ".qs"), nthreads = parallel::detectCores())
}

smart_load <- function(df, folder, subpop = "") {
  qread(paste0(folder, deparse(substitute(df)), suffix[[subpop]], ".qs"), nthreads = parallel::detectCores())
}