# Program Information  ----------------------------------------------------

# Program:      07_scri_inputs.R 
# Author:       Anna Schultze, Svetlana Belitser; Ema Alsina, Sophie Bots, Ivonne Martens 
# Description:  sets a series of "parameters" (i.e, input values) 
# Requirements: none
#
# INPUT LISTS  -----------------------------------------------------------


# package list 
packages<-  c( "survival" )   

# functions list:
func_names <- c("standardsccs2.R", "formatdata2.R", "table1.R")
func_dir   <- paste0(getwd(),"/p_steps/")


# names of input data 
raw_data <- "D3_study_variables_for_SCRI.RData"
raw_data_name <- "D3_study_variables_for_SCRI"
intermediate_data <- "scri_data_extract"

# age categories
age_cat <- c(-1,18,24,29,39,49,55,65,80,120)






