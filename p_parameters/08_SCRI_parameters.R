# Program Information  ----------------------------------------------------

# Program:      07_scri_inputs.R 
# Author:       Anna Schultze, Svetlana Belitser; Ema Alsina, Sophie Bots, Ivonne Martens 
# Description:  sets a series of "parameters" (i.e, input values) 
# Requirements: none
#
# INPUT LISTS  -----------------------------------------------------------

# functions list:
func_names <- c("scri_tools.R")
func_dir   <- paste0(thisdir,"/p_steps/")

# Load functions
source(paste0(dirmacro, "scri_tools.R"))

# names of input data 
raw_data <- "D3_study_population_SCRI.RData"
raw_data_name <- "D3_study_population_SCRI"

# SCRI variables
SCRI_variables_vocabulary <- data.table(vac4eu = c("E_GOUT_COV", "C_MYOCARD_AESI", "C_PERICARD_AESI",
                                                   "SO_OTITISEXT_COV", "C_VALVULAR_COV"),
                                        scri = c("gout", "myocarditis", "pericarditis",
                                                 "otitis_externa", "valvular_heart_disease"))

#############################
  #
  # events and outcome variables:
  #
  ae_events <-  c("myocarditis", "pericarditis", "myopericarditis", "otitis_externa", "valvular_heart_disease", "gout")

  #############################
  #
  # specify the length and the start moment of the calendar time intervals:
  #
  time_interval_width  <- c( 30, 30 )
  time_interval_starts <- c(-10, -1 )
  

  #############################
  #
  #   dataset variable
  #
  id <- "person_id"

  ###########################
  #
  # define which part to run:
  #
  lmain  <- T
  lcovid <- T
  ldist  <- T
  
  ###########################
  #
  # some parameters for the 'scri'function
  #
  
  # colors for plots:
  col_list <- c("red", "green3", "orange",  "deepskyblue", "magenta2", "cyan2", "chocolate1", "gray" ) 
  
  # parallel 
  lparal     = T    # if T ==> library(parallel) is started in function 'scri'
  n_cores    = NA
  
  leventplot = T 
  max_n_points = 1000 
  lplot      = T
  CI_draw    = T
  lforest    = T
