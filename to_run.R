#-------------------------------
# CVM script - Efficacy in children

# authors: Rosa Gini, Olga Paoletti, Davide Messina, Giorgio Limoncella
# authors: Anna Schultze, Svetlana Belitser; Ema Alsina, Sophie Bots, Ivonne Martens 

# v 1.0 - 20 May 2022
# Initial release
# Revised covid severity

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

# 01 RETRIEVE RECORDS FRM CDM

system.time(source(paste0(thisdir,"/p_steps/step_01_1_T2.1_create_conceptset_datasets.R")))
system.time(source(paste0(thisdir,"/p_steps/step_01_2_T2.1_create_spells.R")))
system.time(source(paste0(thisdir,"/p_steps/step_01_3_T2.1_create_dates_in_PERSONS.R")))
system.time(source(paste0(thisdir,"/p_steps/step_01_4_T2.1_create_prompt_and_itemset_datasets.R")))

#02 quality checks

system.time(source(paste0(thisdir,"/p_steps/step_02_1_T2_create_QC_criteria.R")))
system.time(source(paste0(thisdir,"/p_steps/step_02_2_T3_apply_QC_exclusion_criteria.R")))

#03 create exclusion criteria
system.time(source(paste0(thisdir,"/p_steps/step_03_1_T2_create_exclusion_criteria.R")))
system.time(source(paste0(thisdir,"/p_steps/step_03_2_T2_merge_persons_concept.R")))

#04 apply exclusion criteria
system.time(source(paste0(thisdir,"/p_steps/step_04_1_T3_apply_exclusion_criteria.R")))
system.time(source(paste0(thisdir,"/p_steps/step_04_2_T3_apply_quality_check_exclusion_criteria_doses.R")))
##use flowchart (apply also quality checks)

#05 create D3 for doses and coverage
system.time(source(paste0(thisdir,"/p_steps/step_05_1_T2.2_components.R")))
system.time(source(paste0(thisdir,"/p_steps/step_05_2_T2.2_secondary_components.R")))
system.time(source(paste0(thisdir,"/p_steps/step_05_3_T2_create_events_ALL_OUTCOMES.R")))
system.time(source(paste0(thisdir,"/p_steps/step_05_5_QC_apply_component_strategy.R")))
system.time(source(paste0(thisdir,"/p_steps/step_05_6_T2.2_covariates_at_baseline.R")))
system.time(source(paste0(thisdir,"/p_steps/step_05_7_T2.2_DP_at_baseline.R")))
system.time(source(paste0(thisdir,"/p_steps/step_05_8_T2.3_baseline_characteristics.R")))
system.time(source(paste0(thisdir,"/p_steps/step_05_9_T2.3_ALL_covariates_at_baseline_V2.R")))
system.time(source(paste0(thisdir,"/p_steps/step_05_12_T2.2_COVID_repeated_events.R")))
system.time(source(paste0(thisdir,"/p_steps/step_05_14_T3_check_IR_severity_children.R")))

#06 create D3 for doses and coverage
system.time(source(paste0(thisdir,"/p_steps/step_06_1_T2_create_D3_datasets.R")))
system.time(source(paste0(thisdir,"/p_steps/step_06_2_T2.2_covariates_at_vaccination.R")))
system.time(source(paste0(thisdir,"/p_steps/step_06_3_T2.2_DP_at_vaccination.R")))
system.time(source(paste0(thisdir,"/p_steps/step_06_4_T2.3_vaccination_characteristics.R")))
system.time(source(paste0(thisdir,"/p_steps/step_06_5_T2.3_ALL_covariates_at_vaccination_V2.R")))
system.time(source(paste0(thisdir,"/p_steps/step_06_6_T2_create_D3_datasets.R")))
system.time(source(paste0(thisdir,"/p_steps/step_06_7_MIS_population.R")))
system.time(source(paste0(thisdir,"/p_steps/step_06_8_T2.2_covariates_at_covid.R")))
system.time(source(paste0(thisdir,"/p_steps/step_06_9_T2.2_DP_at_covid.R")))
system.time(source(paste0(thisdir,"/p_steps/step_06_10_T2.3_covid_characteristics.R")))
system.time(source(paste0(thisdir,"/p_steps/step_06_11_T2.3_ALL_covariates_at_covid_V2.R")))
system.time(source(paste0(thisdir,"/p_steps/step_06_12_MIS_population_c.R")))

# system.time(source(paste0(thisdir,"/p_steps/step_07_9_T3_create_person_time_MIS_year.R")))
# system.time(source(paste0(thisdir,"/p_steps/step_07_10_T3_aggregate_monthly_MIS.R")))
# system.time(source(paste0(thisdir,"/p_steps/step_08_2_T4_IR_MIS.R")))
# 
#descriptive
system.time(source(paste0(thisdir,"/p_steps/step_09_4_T3_create_D4_descriptive_tables_MIS.R")))
system.time(source(paste0(thisdir,"/p_steps/step_11_2_T4_create_dummy_tables_MIS_KD.R")))

system.time(source(paste0(thisdir,"/p_steps/step_06_13_Poisson_population.R")))
system.time(source(paste0(thisdir,"/p_steps/step_07_11_T3_create_person_time_poisson.R")))
system.time(source(paste0(thisdir,"/p_steps/step_07_12_T3_aggregate_monthly_Poisson.R")))
