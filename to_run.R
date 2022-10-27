#-------------------------------
# CVM script - Readiness

# authors Readiness: Rosa Gini, Davide Messina

# v 2.0 - 27 October 2022
# Readiness
# updated codelist and variable names to adapt to the VAC4EU standards
# Major changes in most of the steps

# v 1.4 - 09 June 2022
# fixed bug about covid severity
# additional covid severity levels

# v 1.3 - 06 June 2022
# fixed drug proxies (except PEDIANET)
# fixed covid itemset for PEDIANET
# bugfix for end of cohort d
# fixed folder of final table in case of subpopulations

# v 1.2 - 01 June 2022
# mapped codelists of diagnosis and drug proxies to VAC4EU codelists
# added time dependent age in IR
# add MEDICINES if does not exist
# Itemset for PEDIANET

# v 1.1 - 27 May 2022
# Completed covid severity and relative IR
# Bugfixes

# v 1.0 - 20 May 2022
# Initial release
# Revised covid severity

rm(list=ls(all.names=TRUE))

#set the directory where the file is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir<-setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir<-setwd(dirname(rstudioapi::getSourceEditorContext()$path))

##%######################################################%##
#                                                          #
####                     PARAMETERS                     ####
#                                                          #
##%######################################################%##

source(paste0(thisdir,"/p_parameters/01_parameters_program.R"))
source(paste0(thisdir,"/p_parameters/02_parameters_CDM.R"))
source(paste0(thisdir,"/p_parameters/03_concept_sets.R"))
source(paste0(thisdir,"/p_parameters/04_itemsets.R"))
source(paste0(thisdir,"/p_parameters/05_subpopulations_restricting_meanings.R"))
source(paste0(thisdir,"/p_parameters/06_variable_lists.R"))
source(paste0(thisdir,"/p_parameters/07_algorithms.R"))
source(paste0(thisdir,"/p_parameters/08_SCRI_parameters.R"))
source(paste0(thisdir,"/p_parameters/09_design_parameters.R"))
source(paste0(thisdir,"/p_parameters/99_saving_all_parameters.R"))


##%######################################################%##
#                                                          #
####                    MAIN SCRIPT                     ####
#                                                          #
##%######################################################%##

launch_step("p_steps/01_T2_10_create_persons.R")
launch_step("p_steps/01_T2_20_apply_CreateSpells.R")
launch_step("p_steps/01_T2_31_CreateConceptSetDatasets.R")
launch_step("p_steps/01_T2_32_CreateItemSetDatasets.R")
launch_step("p_steps/01_T2_33_CreatePromptSetDatasets.R")
launch_step("p_steps/01_T2_40_clean_vaccines.R")
launch_step("p_steps/01_T2_41_apply_criteria_for_doses.R")
launch_step("p_steps/01_T2_50_clean_spells.R")
launch_step("p_steps/01_T2_60_selection_criteria_from_PERSON_to_study_population.R")

launch_step("p_steps/02_T3_10_create_study_population.R")

launch_step("p_steps/03_T2_10_create_D3_outcomes_simple_algorithm.R")
launch_step("p_steps/03_T2_11_create_D3_outcomes_complex_algorithm.R")
launch_step("p_steps/03_T2_12_create_D3_event_outcomes_ALL.R")
launch_step("p_steps/03_T2_20_create_D3_covid_episodes.R")
launch_step("p_steps/03_T2_30_create_covariates.R")

launch_step("p_steps/04_T3_10_create_total_study_population.R")

launch_step("p_steps/05_T3_10_count_events_windows.R")
launch_step("p_steps/05_T3_11_aggregate_events_windows.R")
launch_step("p_steps/05_T3_20_create_person_time_monthly.R")
launch_step("p_steps/05_T3_21_aggregate_person_time_monthly.R")
launch_step("p_steps/05_T3_30_create_person_time_background.R")
launch_step("p_steps/05_T3_31_aggregate_person_time_background.R")

launch_step("p_steps/06_T4_10_create_D5_IR_background.R")

launch_step("p_steps/07_T5_10_final_tables.R")
