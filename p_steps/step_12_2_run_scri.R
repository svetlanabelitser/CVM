# Program Information  ----------------------------------------------------

# Program:      step_12_2_run_scri.R 
# Author:       Svetlana Belitser, Anna Schultze; Ema Alsina, Sophie Bots, Ivonne Martens 
# Description:  calls a function which runs an SCRI on specified datasets 
#               runs on all datasets in g_intermediate/scri                  ?
# Requirements: 
#               dependencies: preceding steps, package "survival" 
#               input:  g_intermediate/scri/*                                ?
#               parameters: in 07_scri_inputs.R 
#               output: g_output/scri/*model_output.csv                     ?

# Housekeeping  -----------------------------------------------------------
# install and load packages
# if(!require("survival")) install.packages("survival")
library("survival")   
# load functions:
for(ifunc in func_names)
  source(paste0(func_dir,ifunc))

# ensure required folders are created  
dir.create(file.path("./g_intermediate/scri"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path("./g_output/scri"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path("./log_files/scri"), showWarnings = FALSE, recursive = TRUE)

# SCCS output_directory  
sdr <- paste0(getwd(),"/g_output/scri/SCCS/")
dir.create(sdr, showWarnings = FALSE, recursive = TRUE)

load(file="./g_intermediate/scri/scri_data_extract.RData")

scri_input <- scri_data_extract
summary(scri_input)

#####
# change time scale to days since 01-01-19                #??????????
scri_input$start_study_date <- as.Date("2019-01-01")
date_vars <- c("study_entry_date", "study_exit_date", "date_vax1", "date_vax2","date_vax3", 
               "myocarditis_date", "pericarditis_date", "myopericarditis_date", "birth_date","death_date" )
date_vars_days <- gsub("date","days",date_vars ) 
scri_input[,date_vars_days] <- apply(scri_input[,date_vars],2,function(x) round(as.numeric(difftime(x,scri_input$start_study_date,units="days"))) ) 
scri_input$age_start_study <- (-scri_input$birth_days/365.25)

scri_input$study_entry_date0 <- scri_input$study_entry_date
scri_input$study_entry_date  <- pmin( scri_input$study_entry_date, scri_input$start_study_date )

######## strata variables:
#  brands
table1(scri_input$type_vax1)
# age categories
age_cat <- c(-1,18,24,29,39,49,55,65,80,120)
scri_input$age_start_study_cat <- cut(scri_input$age_start_study,age_cat)
table1(scri_input$age_start_study_cat,title="agecat")


#####  define lengths of the risk windows  #####
#
# define length of pre-vaccin. period:
prevax_per_start <- -92   # let op negative sign!!!
prevax_per_end   <- -30   # let op negative sign!!!
#
# define risk period of vax 1:
vax1_start <- 0  
vax1_end   <- 28 
#
# define risk period of vax 2:
vax2_start <- 0  
vax2_end   <- 28 

expogrp <- c(0,1,8,14)

######################################################################
# define start and end of observation
#   start0,end0 - all obs period
#   start,end   - 1y before vax 1 and 1y after vax 2
scri_input$start   <- pmax( scri_input$study_entry_days, scri_input$days_vax1 + prevax_per_start, na.rm=T)   #?
scri_input$end     <- pmin( scri_input$study_exit_days,  scri_input$days_vax2 + vax2_end,         na.rm=T)   #? 

scri_input$start1y <- pmax( scri_input$study_entry_days, scri_input$days_vax1 - 365,              na.rm=T)   #?
scri_input$end1y   <- pmin( scri_input$study_exit_days,  scri_input$days_vax2 + 365,              na.rm=T)   #? 

scri_input$start0  <- scri_input$study_entry_days
scri_input$end0    <- scri_input$study_exit_days 

####################################################################
#
#        specify risk windows without separate overlap period
#
#############
#   the risk window of dose 2 takes precedence over the risk window of dose 1
#
# prevax risk period: 
scri_input$vd0     <- pmax( scri_input$study_entry_days, scri_input$days_vax1 + prevax_per_start, na.rm=T) 
scri_input$evd0    <- pmin( scri_input$study_exit_days,  scri_input$days_vax1 + prevax_per_end,   na.rm=T) 
scri_input$vd      <- scri_input$vd0
#
# vax 1 risk period: 
scri_input$vd1  <- scri_input$days_vax1 + vax1_start
scri_input$evd1 <- pmin( scri_input$days_vax1 + vax1_end, 
                         scri_input$days_vax2 - 1, 
                         scri_input$study_exit_days,   na.rm=T) 
# vax 2 risk period: 
scri_input$vd2  <- scri_input$days_vax2 + vax2_start 
scri_input$evd2 <- pmin( scri_input$days_vax2 + vax2_end, scri_input$study_exit_days) 

##########
#         specify risk windows with separate overlap period
#
# prevax risk period: 
scri_input$ovd0     <- scri_input$vd0 
scri_input$eovd0    <- scri_input$evd0 
scri_input$ovd      <- scri_input$ovd0
#
# vax 1 risk period
scri_input$ovd1  <- scri_input$vd1
scri_input$eovd1 <- scri_input$evd1
#
# overlap between risk windows of vax 1 and vax 2
scri_input$ovd12  <- scri_input$vd2
scri_input$eovd12 <- pmin( scri_input$days_vax1 + vax1_end, 
                           scri_input$study_exit_days,   na.rm=T) 
scri_input$ovd12[  scri_input$eovd12 < scri_input$ovd12 ] <- NA
scri_input$eovd12[ is.na(scri_input$ovd12) ] <- NA
#
# vax 2 risk period
scri_input$ovd2  <- pmax( scri_input$vd2, scri_input$eovd12+1, na.rm=T )
scri_input$eovd2 <- scri_input$evd2 

##############
#    the risk window of dose 1 takes precedence over the risk window of dose 2
#
# prevax risk period: 
scri_input$v1d0     <- scri_input$vd0 
scri_input$ev1d0    <- scri_input$evd0 
scri_input$v1d      <- scri_input$v1d0
#
# vax 1 risk period: 
scri_input$v1d1  <- scri_input$days_vax1 + vax1_start
scri_input$ev1d1 <- pmin( scri_input$days_vax1 + vax1_end, 
                          #scri_input$days_vax2 - 1, 
                          scri_input$study_exit_days,   na.rm=T) 
# vax 2 risk period: 
scri_input$v1d2  <- pmax( scri_input$ev1d1, scri_input$days_vax2 + vax2_start ) 
scri_input$ev1d2 <- pmin( scri_input$days_vax2 + vax2_end, scri_input$study_exit_days) 

######
#                   risk windows names
#
rw_names <- c( paste0( "pre-exposure [",prevax_per_start,";",prevax_per_end,"]" ), 
               paste0( "dose 1 [0;",vax1_end,"]" ),  
               paste0( "dose 2 [0;",vax2_end,"]" )
)
rw_names_day0 <- c( paste0( "pre-exposure [",prevax_per_start,";",prevax_per_end,"]" ), 
                     paste0( "dose 1 day 0" ),  
                     paste0( "dose 1 [1;",vax1_end,"]" ),  
                     paste0( "dose 2 day 0" ), 
                     paste0( "dose 2 [1;",vax2_end,"]" )
)
overlap_rw_names <- c( paste0( "pre-exposure [",prevax_per_start,";",prevax_per_end,"]" ), 
                       paste0( "dose 1 [0;",vax1_end,"]" ),  
                       paste0( "dose 2 during dose 1 window [0;max(overlap)]" ),  
                       paste0( "dose 2 after  dose 1 risk window [0;",vax2_end,"]" )
)
overlap_rw_names_day0 <- c( paste0( "pre-exposure [",prevax_per_start,";",prevax_per_end,"]" ), 
                            paste0( "dose 1 day 0" ),  
                            paste0( "dose 1 [1;",vax1_end,"]" ),  
                            paste0( "dose 2 during dose 1 window day 0" ),  
                            paste0( "dose 2 during dose 1 window [1;max(overlap)]" ),  
                            paste0( "dose 2 after  dose 1 risk window day 0" ), 
                            paste0( "dose 2 after  dose 1 risk window [1;",vax2_end,"]" )
)
rw1_names <- c( paste0( "pre-exposure [",prevax_per_start,";",prevax_per_end,"]" ), 
                paste0( "dose 1 [0;",vax1_end,"]" ),  
                paste0( "dose 2 after dose 1 risk window [0;",vax2_end,"]" )
)
rw1_names_day0 <- c( paste0( "pre-exposure [",prevax_per_start,";",prevax_per_end,"]" ), 
                     paste0( "dose 1 day 0" ),  
                     paste0( "dose 1 [1;",vax1_end,"]" ),  
                     paste0( "dose 2 day 0 after dose 1 risk window" ), 
                     paste0( "dose 2 after dose 1 risk window [1;",vax2_end,"]" )
)
#
############################################################################


#############   SCCS models ############################
#
#

##########
#              model A  (vax2 takes precedence over vax1)
#
#   the risk window of dose 2 takes precedence over the risk window of dose 1

SCCS_models_A <- list()

scri_input$vd <- scri_input$vd0
res<-standardsccs2(  event ~ vd, 
                     indiv = person_id, 
                     astart = start, 
                     aend = end,   
                     aevent = myopericarditis_days,
                     adrug  =cbind( vd,  vd1,  vd2),
                     aedrug =cbind(evd0, evd1, evd2), 
                     adrugnames = rw_names,
                     #agegrp = age_groups, 
                     #expogrp=c(0,8,14), 
                     dataformat = "multi",
                     sameexpopar = F, 
                     data = scri_input) 
print(res[[1]],digits=3)
SCCS_models_A <- c( SCCS_models_A,  list( model_A_all = res) )

# model A per brand
res<-standardsccs2(  event ~ type_vax1/vd, 
                     indiv = person_id, 
                     astart = start, 
                     aend = end,   
                     aevent = myopericarditis_days,
                     adrug  =cbind( vd,  vd1,  vd2),
                     aedrug =cbind(evd0, evd1, evd2), 
                     adrugnames = rw_names,
                     #agegrp = age_groups, 
                     #expogrp=c(0,8,14), 
                     dataformat = "multi",
                     sameexpopar = F, 
                     data = scri_input) 
print(res[[1]],digits=3)
SCCS_models_A <- c( SCCS_models_A,  list( model_A_per_brand = res) )

# model A per brand without Moderna
res<-standardsccs2(  event ~ type_vax1/vd, 
                     indiv = person_id, 
                     astart = start, 
                     aend = end,   
                     aevent = myopericarditis_days,
                     adrug  =cbind( vd,  vd1,  vd2),
                     aedrug =cbind(evd0, evd1, evd2), 
                     adrugnames = rw_names,
                     #agegrp = age_groups, 
                     #expogrp=c(0,8,14), 
                     dataformat = "multi",
                     sameexpopar = F, 
                     data = scri_input[scri_input$type_vax1!="Moderna",]) 
print(res[[1]],digits=3)
print(res[[1]][order(res[[1]][,"type_vax1"]),],digits =2)
SCCS_models_A <- c( SCCS_models_A,  list( model_A_per_Pf_AZ = res) )

save(SCCS_models_A, file = paste0(sdr, "SCCS_models_A.RData" ))

