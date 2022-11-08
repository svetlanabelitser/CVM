# Program Information  ----------------------------------------------------

# Program:      07_scri_inputs.R 
# Author:       Anna Schultze, Svetlana Belitser; Ema Alsina, Sophie Bots, Ivonne Martens 
# Description:  sets a series of "parameters" (i.e, input values) 
# Requirements: none
#
# INPUT LISTS  -----------------------------------------------------------


# package list 
packages <- c("survival", "MASS", "qpdf", "metafor", "markdown")   

# functions list:
func_names <- c("scri_tools.R")
func_dir   <- paste0(thisdir,"/p_steps/")


# names of input data 
raw_data <- "D3_study_population_SCRI.RData"
raw_data_name <- "D3_study_population_SCRI"

#############################
  #
  # events and outcome variables:
  #
  ae_events <-  c( "myocarditis", "pericarditis", ??"myopericarditis", "otitis", ???"valvular_heart_disease", "gout"  )  

  #############################
  #
  # specify the length and the start moment of the calendar time intervals:
  #
  time_interval_width  <- c( 21, 30, 30, 45)
  time_interval_starts <- c( -8,-10, -1, -5)
  

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
  lparal     = F    # if T ==> library(parallel) is started in function 'scri'
  n_cores    = NA
  
  leventplot = F 
  lplot      = T
  CI_draw    = T
  lforest    = T
  
  
