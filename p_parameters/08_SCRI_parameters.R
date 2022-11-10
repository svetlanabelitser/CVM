# Program Information  ----------------------------------------------------

# Program:      07_scri_inputs.R 
# Author:       Anna Schultze, Svetlana Belitser; Ema Alsina, Sophie Bots, Ivonne Martens 
# Description:  sets a series of "parameters" (i.e, input values) 
# Requirements: none
#
# INPUT LISTS  -----------------------------------------------------------


# package list 
packages <- c("survival")   

# functions list:
func_names <- c("scri.R", "formatdata2.R", "table1.R")
func_dir   <- paste0(thisdir,"/p_steps/")


# names of input data 
raw_data <- "D3_study_variables_for_SCRI.RData"
raw_data_name <- "D3_study_variables_for_SCRI"
intermediate_data <- "scri_data_extract"

# age categories
age_cat <- c(-1, 30, 120)

# SCRI variables
SCRI_variables <- c("E_GOUT_COV", "C_MYOCARD_AESI", "C_PERICARD_AESI", "SO_OTITISEXT_COV", "C_VALVULAR_COV")
