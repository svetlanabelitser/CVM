#-------------------------------
# ECVM script

# authors: Rosa Gini, Olga Paoletti, Davide Messina, Giorgio Limoncella

# v 6.8.1
# Modified step 12_2 and created 12_4

# v 6.8
# Modified step 12_1 and 12_2

# v 6.7
# added step 12_3 for SCRI

# v 6.6
# added step 12_2, and created 12_2 for SCRI

# v 6.5.1
# removed aggregations and filters for poisson dataset
# added step 06_13 and 12_1, initial part for SCRI

# v 6.5
# added calculation of dataset for poisson analysis
# bugfix for date_death

# v 6.4.4
# added steps which create input table for poisson
# bugfixes regarding conflicts arisen after merging

# v 6.4.3
# updated code list for ALI
# added modification for step 07_9 to decrease RAM utilization

# v 6.4.2
# list of codes for concepts moved to p_steps/archive_parameters
# gap between OBS PERIODS to 180dd for ARS

# v 6.4.1
# bugfix and incident cases in 11/11_3 now counted only if person in study

# v 6.4
# Inclusion of use of hypertensive drugs in cardiovascular risk
# bug fix in final tables and insufficient run_in
# PT -> Counts in final tables
# Risk factors calculated from 1/1/2019
# Added any_risk_factors in final tables

#v 6.3.1
#DO NOT include use of hypertensive drugs in cardiovascular risk,k small bug fix in final tables

#v 6.3
#changes in final tables for October report

#v 6.2.2
#changes in the vaccines lables and small fix for the MIS final tables

#v.6.2.1
##small fix on final table 7, fix on filter for covid dates 

#v.6.2
##small changes on final tables,addition of KD as a conceptset and PERICARD in CVM report

#v.6.1
#small changes on final tables

# v6.0 - 29 September 2021
# adjustment for subpopulations and change of agebands

# changelog V5.3_MIS:
# addition of final tables 1-7

# changelog V5.2_MIS:
# correction: start of countpersontime at cohort entry and not at study entry


# changelog V5.1_MIS:
# changed codes for MIS and added a specifi to_run for BIFAP (to correct for subpopulations)

# changelog V5_MIS:
# added MIS section

# changelog V4.3.2:
# changed severity level algorithm for BIFAP

# changelog V4.3.1:
# fixed numerator for coverage. Now with vaccinated excluded when exit study
# bugfix for table 3_4_5_6

# changelog V4.3:
# fixed denominator for coverage. Now with dynamic population

# changelog V4.2:
# new codes ICPC and italian ICD9 codes
# improved BIFAP covid registry

# changelog V4.0.2:
# bugfix

# changelog V4.0.1:
# - SAP tables included until 8
# - Added ageband 60+
# - Added column for age at 1 Jan 2021
# - Implementation of exact Poisson confidence intervals for IR (Ulm, 1990)

# changelog V4.0:
# included ACCESS codes

# changelog V3.6:
# - SAP tables included until 6

# changelog V3.5:
# - Support for recurrent events
# - CreateConceptSetDatasets V18
# - support for COVID severity in BIFAP

# changelog V3.4:
# - Cumulative results for risks time since vaccination
# - Inclusion of column ageband and numerator in dashboard tables

# changelog V3.3:
# - Risk factors now are in wide format, not long, for countpersontime

# changelog V3.2:
# - Debugged Createconceptset
# - IR dataset now in Rdata too.

rm(list=ls(all.names=TRUE))

#set the directory where the file is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir<-setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir<-setwd(dirname(rstudioapi::getSourceEditorContext()$path))


#load parameters
source(paste0(thisdir,"/p_parameters/01_parameters_program.R"))
source(paste0(thisdir,"/p_parameters/02_parameters_CDM.R"))
source(paste0(thisdir,"/p_parameters/03_concept_sets.R"))
source(paste0(thisdir,"/p_parameters/04_itemsets.R"))
source(paste0(thisdir,"/p_parameters/05_subpopulations_restricting_meanings.R"))
source(paste0(thisdir,"/p_parameters/06_algorithms.R"))
source(paste0(thisdir,"/p_parameters/07_scri_inputs.R"))

#run scripts

#SCRI section
#create D3 MIS population
# system.time(source(paste0(thisdir,"/p_steps/step_06_13_SCRI_population.R")))
system.time(source(paste0(thisdir,"/p_steps/step_12_1_create_scri_dataset.R")))
system.time(source(paste0(thisdir,"/p_steps/step_12_2_run_scri.R")))
system.time(source(paste0(thisdir,"/p_steps/step_12_3_count_subgroup_numbers.R")))
system.time(source(paste0(thisdir,"/p_steps/step_12_4_prepare_meta_dataset.R")))
