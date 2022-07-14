#-------------------------------
# CVM script - Efficacy in children

# authors: Rosa Gini, Olga Paoletti, Davide Messina, Giorgio Limoncella
# authors: Anna Schultze, Svetlana Belitser; Ema Alsina, Sophie Bots, Ivonne Martens 

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


#load parameters
source(paste0(thisdir,"/p_parameters/01_parameters_program.R"))
source(paste0(thisdir,"/p_parameters/02_parameters_CDM.R"))
source(paste0(thisdir,"/p_parameters/03_concept_sets.R"))
source(paste0(thisdir,"/p_parameters/04_itemsets.R"))
source(paste0(thisdir,"/p_parameters/05_subpopulations_restricting_meanings.R"))
source(paste0(thisdir,"/p_parameters/06_algorithms.R"))
source(paste0(thisdir,"/p_parameters/07_scri_inputs.R"))
source(paste0(thisdir,"/p_parameters/08_saving_all_parameters.R"))


#run scripts

# 01 RETRIEVE RECORDS FRM CDM

launch_step("step_01_1_T2.1_create_conceptset_datasets.R")
launch_step("step_01_2_T2.1_create_dates_in_PERSONS.R")
launch_step("step_01_3_T2.1_create_spells.R")
launch_step("step_01_4_T2.1_create_prompt_and_itemset_datasets.R")

#03 create exclusion criteria
system.time(source(paste0(thisdir,"/p_steps/step_03_1_T2_create_exclusion_criteria.R")))
system.time(source(paste0(thisdir,"/p_steps/step_03_2_T2_merge_persons_concept.R")))


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
system.time(source(paste0(thisdir,"/p_steps/step_05_13_T2.2_component_COVID_severity_hospitalised.R")))
system.time(source(paste0(thisdir,"/p_steps/step_05_14_T2.2_component_COVID_severity_ICU.R")))
system.time(source(paste0(thisdir,"/p_steps/step_05_15_T2.2_component_COVID_severity_DEATH.R")))
system.time(source(paste0(thisdir,"/p_steps/step_05_16_T2.3_COVID_severity_repeated_events.R")))

#06 create D3 for doses and coverage
system.time(source(paste0(thisdir,"/p_steps/step_06_1_T2_create_D3_datasets.R")))
system.time(source(paste0(thisdir,"/p_steps/step_06_2_T2.2_covariates_at_vaccination.R")))
system.time(source(paste0(thisdir,"/p_steps/step_06_3_T2.2_DP_at_vaccination.R")))
system.time(source(paste0(thisdir,"/p_steps/step_06_4_T2.3_vaccination_characteristics.R")))
system.time(source(paste0(thisdir,"/p_steps/step_06_5_T2.3_ALL_covariates_at_vaccination_V2.R")))
system.time(source(paste0(thisdir,"/p_steps/step_06_6_T2_create_D3_datasets.R")))
system.time(source(paste0(thisdir,"/p_steps/step_06_7_children_population.R")))
system.time(source(paste0(thisdir,"/p_steps/step_06_8_T2.2_covariates_at_covid.R")))
system.time(source(paste0(thisdir,"/p_steps/step_06_9_T2.2_DP_at_covid.R")))
system.time(source(paste0(thisdir,"/p_steps/step_06_10_T2.3_covid_characteristics.R")))
system.time(source(paste0(thisdir,"/p_steps/step_06_11_T2.3_ALL_covariates_at_covid_V2.R")))
system.time(source(paste0(thisdir,"/p_steps/step_06_12_children_population_c.R")))

system.time(source(paste0(thisdir,"/p_steps/step_07_9_T3_create_person_time_children_year.R")))
system.time(source(paste0(thisdir,"/p_steps/step_07_10_T3_aggregate_monthly_children.R")))
system.time(source(paste0(thisdir,"/p_steps/step_08_2_T4_IR_children.R")))
# 
#descriptive
system.time(source(paste0(thisdir,"/p_steps/step_09_4_T3_create_D4_descriptive_tables_children.R")))
system.time(source(paste0(thisdir,"/p_steps/step_11_2_T4_create_dummy_tables_children.R")))

system.time(source(paste0(thisdir,"/p_steps/step_06_13_Poisson_population.R")))
system.time(source(paste0(thisdir,"/p_steps/step_07_11_T3_create_person_time_poisson.R")))
system.time(source(paste0(thisdir,"/p_steps/step_07_12_T3_aggregate_monthly_Poisson.R")))
##end MIS and Myocarditis section------------

#07 create persontime
system.time(source(paste0(thisdir,"/p_steps/step_07_1_T3_create_person_time_risks.R")))
system.time(source(paste0(thisdir,"/p_steps/step_07_2_T3_create_person_time_risks_year.R")))
system.time(source(paste0(thisdir,"/p_steps/step_07_3_T3_create_person_time_benefits.R")))
system.time(source(paste0(thisdir,"/p_steps/step_07_4_T3_create_person_time_benefits_year.R")))
system.time(source(paste0(thisdir,"/p_steps/step_07_5_T3_aggregate_sex_birth_cohort.R")))
system.time(source(paste0(thisdir,"/p_steps/step_07_6_T3_aggregate_sex_risk_factor.R")))
system.time(source(paste0(thisdir,"/p_steps/step_07_7_T3_create_person_time_vax_cohort.R")))
system.time(source(paste0(thisdir,"/p_steps/step_07_8_T3_aggregate_monthly.R")))



#08 Calculate Incidence Rates
system.time(source(paste0(thisdir,"/p_steps/step_08_1_T4_IR.R")))


#09 create D4 for doses and coverage
system.time(source(paste0(thisdir,"/p_steps/step_09_1_T3_create_D4_doses_weeks.R")))
system.time(source(paste0(thisdir,"/p_steps/step_09_2_T3_create_D4_descriptive_tables.R")))
system.time(source(paste0(thisdir,"/p_steps/step_09_3_T3_create_dashboard_tables.R")))


# system.time(source(paste0(thisdir,"/p_steps/step_10_2_Coverage_description.R")))
# system.time(source(paste0(thisdir,"/p_steps/step_10_3_Doses_description.R")))
# system.time(source(paste0(thisdir,"/p_steps/step_10_4_benefit_description.R")))


#11 Create descriptive tables
system.time(source(paste0(thisdir,"/p_steps/step_11_1_T4_create_dummy_tables.R")))
system.time(source(paste0(thisdir,"/p_steps/step_11_3_T4_create_dummy_tables_October.R")))


#POISSON section
system.time(source(paste0(thisdir,"/p_steps/step_06_13_Poisson_population.R")))
system.time(source(paste0(thisdir,"/p_steps/step_07_11_T3_create_person_time_poisson.R")))
system.time(source(paste0(thisdir,"/p_steps/step_07_12_T3_aggregate_monthly_Poisson.R")))


#SCRI section
#create D3 MIS population
system.time(source(paste0(thisdir,"/p_steps/step_06_14_SCRI_population.R")))
system.time(source(paste0(thisdir,"/p_steps/step_12_1_create_scri_dataset.R")))
system.time(source(paste0(thisdir,"/p_steps/step_12_2_run_scri.R")))
system.time(source(paste0(thisdir,"/p_steps/step_12_3_count_subgroup_numbers.R")))
# system.time(source(paste0(thisdir,"/p_steps/step_12_4_prepare_meta_dataset.R")))

#MYOCARD cohort
system.time(source(paste0(thisdir,"/p_steps/step_06_15_Cohort_population.R")))
system.time(source(paste0(thisdir,"/p_steps/step_07_13_T3_create_person_time_windows.R")))
system.time(source(paste0(thisdir,"/p_steps/step_07_14_T3_aggregate_windows.R")))
system.time(source(paste0(thisdir,"/p_steps/step_08_3_T4_IR_windows.R")))


system.time(source(paste0(thisdir,"/p_steps/step_07_15_T3_create_person_time_simplified_severity.R")))
system.time(source(paste0(thisdir,"/p_steps/step_07_16_T3_aggregate_simplified_severity.R")))

system.time(source(paste0(thisdir,"/p_steps/step_11_4_T4_create_dummy_tables_April.R")))
