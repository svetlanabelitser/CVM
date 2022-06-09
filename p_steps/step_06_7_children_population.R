# ----------------------------------
# Calculate the cohort D3 datasets for children, For cohort c the dataset is temporary since it doesn't contains the risks at covid diagnosis.

# input: D3_events_ALL_OUTCOMES, D3_outcomes_severity_episodes_covid , D3_study_population
# output: D3_study_variables_for_children, D4_population_b, D3_selection_criteria_c, D4_population_c_no_risk, D3_selection_criteria_d, D4_population_d, D4_population_d_long, D4_population_d_28

for (subpop in subpopulations_non_empty) {
  print(subpop)
  
  first_jan_2020<-as_date("20200101")
  first_jan_2021<-as_date("20210101")
  
  load(paste0(dirtemp,"D3_events_ALL_OUTCOMES",suffix[[subpop]],".RData"))
  load(paste0(dirtemp,"D3_outcomes_severity_episodes_covid",suffix[[subpop]],".RData")) #L1plus
  load(paste0(dirtemp,"D3_study_population",suffix[[subpop]],".RData"))
  
  events_ALL_OUTCOMES<-get(paste0("D3_events_ALL_OUTCOMES", suffix[[subpop]]))
  rm(list=paste0("D3_events_ALL_OUTCOMES", suffix[[subpop]]))
  outcomes_covid<-get(paste0("D3_outcomes_severity_episodes_covid", suffix[[subpop]]))
  rm(list=paste0("D3_outcomes_severity_episodes_covid", suffix[[subpop]]))
  study_population<-get(paste0("D3_study_population", suffix[[subpop]]))
  rm(list=paste0("D3_study_population", suffix[[subpop]]))
  
  #add date of first covid to the population
  covid_L1<-unique(outcomes_covid[, .(person_id,date_event)])
  covid_L1<-covid_L1[,min(date_event,na.rm = T),by="person_id"]
  
  setnames(covid_L1,"V1","covid_date")
  D3_study_variables_for_children <- merge(study_population, covid_L1, all.x = T, by="person_id")
  rm(covid_L1, study_population)
  
  tempname<-paste0("D3_study_variables_for_children",suffix[[subpop]])
  assign(tempname,D3_study_variables_for_children)
  save(tempname, file = paste0(dirtemp, tempname,".RData"),list=tempname)
  
  #COHORT B
  #add the study entry date for children
  D3_study_variables_for_children[,study_entry_date_children_b:=max(first_jan_2020,study_entry_date,na.rm = T),by="person_id"]
  #add the cohort entry date for children
  D3_study_variables_for_children[,cohort_entry_date_children_b:=study_entry_date_children_b,by="person_id"]
  #add the study exit date for children
  D3_study_variables_for_children[,study_exit_date_children_b:=min(study_exit_date,covid_date, date_vax1-1,na.rm = T),by="person_id"]
  # calculate correct fup_days
  D3_study_variables_for_children[, fup_days := correct_difftime(study_exit_date_children_b, cohort_entry_date_children_b)]
  #select the variables and save
  
  D4_population_b<-D3_study_variables_for_children[,.(person_id,sex,age_at_study_entry,ageband_at_study_entry,
                                                 date_of_birth,study_entry_date_children_b, cohort_entry_date_children_b,
                                                 study_exit_date_children_b, fup_days)]
  
  tempname<-paste0("D4_population_b",suffix[[subpop]])
  assign(tempname,D4_population_b)
  save(tempname, file = paste0(diroutput, tempname,".RData"),list=tempname)
  rm(list=tempname)
  if (this_datasource_has_subpopulations==T) rm(D4_population_b)
  
  #---------------------------------
  #COHORT C
  #add the study entry date for children
  D3_study_variables_for_children[,study_entry_date_children_c:=first_jan_2020,by="person_id"]
  #add the cohort entry date for children
  D3_study_variables_for_children[,cohort_entry_date_children_c:=covid_date,by="person_id"]
  #add the study exit date for children
  D3_study_variables_for_children[,study_exit_date_children_c:=min(study_exit_date,date_vax1-1,na.rm = T),by="person_id"]
  # calculate correct fup_days
  D3_study_variables_for_children[, fup_days := correct_difftime(study_exit_date_children_c, cohort_entry_date_children_c)]
  #select the variables and save                           
  
  D3_selection_criteria_c <- D3_study_variables_for_children[is.na(covid_date) | study_exit_date_children_c <= cohort_entry_date_children_c, not_in_cohort_c:=1]
  D3_selection_criteria_c <- D3_selection_criteria_c[, age_at_covid := floor(lubridate::time_length(difftime(cohort_entry_date_children_c, date_of_birth, units = "days"), "years"))]
  D3_selection_criteria_c <- D3_selection_criteria_c[, ageband_at_covid := as.character(cut(age_at_covid, breaks = Agebands, labels = Agebands_labels))]
  
  D3_selection_criteria_c <- D3_selection_criteria_c[, not_in_cohort_c := replace(.SD, is.na(.SD), 0), .SDcols = "not_in_cohort_c"]

  
  tempname<-paste0("D3_selection_criteria_c",suffix[[subpop]])
  assign(tempname,D3_selection_criteria_c)
  save(tempname, file = paste0(dirtemp, tempname,".RData"),list=tempname)
  
  D4_population_c_no_risk <- CreateFlowChart(
    dataset = D3_selection_criteria_c[,.(person_id,sex,age_at_covid,ageband_at_covid, date_of_birth,
                                         study_entry_date_children_c, cohort_entry_date_children_c, study_exit_date_children_c,
                                         not_in_cohort_c, fup_days)],
    listcriteria = c("not_in_cohort_c"),
    flowchartname = "Flowchart_cohort_c")
  
  tempname<-paste0("D4_population_c_no_risk",suffix[[subpop]])
  assign(tempname,D4_population_c_no_risk)
  save(tempname, file = paste0(dirtemp, tempname,".RData"),list=tempname)
  rm(list=tempname)
  rm(D3_selection_criteria_c)
  if (this_datasource_has_subpopulations==T) rm(D4_population_c_no_risk)
  #---------------------------------
  #COHORT D
  
  #d crea: covid prima vaccinazione (usa in strato countpersontime) e e covdi dopo(usa per exit)
  
  #covid_d<-unique(outcomes_covid[name_event=="COVID_L1plus",.(person_id,date_event)])
  #add the study entry date for children
  
  D3_study_variables_for_children[,study_entry_date_children_d:=first_jan_2020]
  #add the cohort entry date for children
  D3_study_variables_for_children[,cohort_entry_date_children_d:=max(date_vax1, first_jan_2020), by="person_id"]
  
  D3_selection_criteria_d <- D3_study_variables_for_children[is.na(date_vax1) | study_exit_date < cohort_entry_date_children_d, not_in_cohort_d:=1]
  rm(D3_study_variables_for_children)
  
  D3_selection_criteria_d <- D3_selection_criteria_d[, not_in_cohort_d := replace(.SD, is.na(.SD), 0), .SDcols = "not_in_cohort_d"]
  
  tempname<-paste0("D3_selection_criteria_d",suffix[[subpop]])
  assign(tempname,D3_selection_criteria_d)
  save(tempname, file = paste0(dirtemp, tempname,".RData"),list=tempname)
  
  D4_population_d <- CreateFlowChart(
    dataset = D3_selection_criteria_d,
    listcriteria = c("not_in_cohort_d"),
    flowchartname = "Flowchart_cohort_d")
  
  #add the study exit date for children
  #D3_study_variables_for_children[covid_date>date_vax1,study_exit_date_children_d:=min(covid_date,study_exit_date,na.rm = T)]
  D4_population_d[covid_date<=date_vax1,history_covid:=1][covid_date>date_vax1 | is.na(covid_date),history_covid:=0]
  D4_population_d[, study_exit_date_children_d:=min(study_end,study_exit_date),by="person_id"]
  
  D4_population_d<-D4_population_d[study_exit_date_children_d > study_entry_date_children_d, ]
  D4_population_d<-D4_population_d[, covid_date := NULL]
  
  D4_population_d <- D4_population_d[study_exit_date_vax1 > study_exit_date_children_d,
                                               c("study_exit_date_vax1", "study_exit_date_vax2") := list(study_exit_date_children_d, NA)]
  D4_population_d <- D4_population_d[study_entry_date_vax2 > study_exit_date_children_d, study_entry_date_vax2 := NA]
  D4_population_d <- D4_population_d[study_exit_date_vax2 > study_exit_date_children_d, study_exit_date_vax2 := study_exit_date_children_d]
  
  D4_population_d_28gg <- copy(D4_population_d)
  
  D4_population_d_28gg[, c("study_entry_date", "study_exit_date") := NULL]
  colA = paste("study_entry_date_vax", 1:2, sep = "")
  colB = paste("study_exit_date_vax", 1:2, sep = "")
  colC = paste("type_vax", 1:2, sep = "_")
  D4_population_d_28gg <- data.table::melt(D4_population_d_28gg, measure = list(colA, colB, colC), variable.name = "Dose",
                           value.name = c("study_entry_date", "study_exit_date", "type_vax"), na.rm = T)
  
  study_population <- copy(D4_population_d_28gg)
  
  D4_population_d_28gg <- D4_population_d_28gg[,.(person_id, sex, age_at_date_vax_1, ageband_at_date_vax_1, date_of_birth,
                                                  study_entry_date, study_exit_date,  history_covid, type_vax, Dose)]
  
  D4_population_d_28gg <- D4_population_d_28gg[, days_28 := study_entry_date + 27]
  D4_population_d_28gg <- D4_population_d_28gg[study_exit_date > days_28 & Dose == 1, study_exit_date := days_28]
  D4_population_d_28gg <- D4_population_d_28gg[, days_28 := NULL][, Dose := as.character(Dose)]
  setnames(D4_population_d_28gg, c("study_entry_date", "study_exit_date"), c("start_period", "end_period"))
  
  tempname<-paste0("D4_population_d_28",suffix[[subpop]])
  assign(tempname,D4_population_d_28gg)
  save(tempname, file = paste0(diroutput, tempname,".RData"),list=tempname)
  
  rm(D4_population_d_28gg)
  rm(list=tempname)
  

  
  
  
  
  
  
  
  
  


  
  setnames(study_population,
           c("CV_at_date_vax_1", "COVCANCER_at_date_vax_1", "COVCOPD_at_date_vax_1", "COVHIV_at_date_vax_1",
             "COVCKD_at_date_vax_1", "COVDIAB_at_date_vax_1", "COVOBES_at_date_vax_1", "COVSICKLE_at_date_vax_1",
             "immunosuppressants_at_date_vax_1", "at_risk_at_date_vax_1"),
           c("CV", "COVCANCER", "COVCOPD", "COVHIV", "COVCKD", "COVDIAB", "COVOBES", "COVSICKLE",
             "IMMUNOSUPPR", "any_risk_factors"))

  # study_population <- study_population %>%
  #   select(person_id, sex, study_entry_date, study_exit_date, ageband_at_date_vax_1, type_vax, Dose, CV, COVCANCER,
  #          COVCOPD, COVHIV, COVCKD, COVDIAB, COVOBES, COVSICKLE, IMMUNOSUPPR, any_risk_factors)
  
  study_population <- study_population %>%
    select(person_id, sex, study_entry_date, study_exit_date, ageband_at_date_vax_1,
           date_of_birth, type_vax, Dose, history_covid)
  
  study_population <- study_population[, Dose := as.character(Dose)]
  
  end_2020 <- ymd(20201231)
  
  vector_periods <- c(28, 60, 180, 365)
  effective_starts <- c(0, head(vector_periods, -1))
  
  study_entry_names <- paste0("study_entry_date_", effective_starts)
  study_exit_names <- paste0("study_exit_date_", vector_periods)
  study_population <- study_population[, (study_entry_names) := lapply(effective_starts, `+`, study_entry_date)]
  study_population <- study_population[, (study_exit_names) := lapply(vector_periods - 1, `+`, study_entry_date)]
  setnames(study_population, c("study_entry_date", "study_exit_date"), c("orig_study_entry_date", "orig_study_exit_date"))
  
  study_population <- data.table::melt(study_population, measure = list(study_entry_names, study_exit_names),
                                         variable.name = "Period", variable.factor = F,
                                         value.name = c("study_entry_date", "study_exit_date"))
  study_population[.(Period = as.character(seq_along(vector_periods)), to = as.character(vector_periods)), on = "Period", Period := i.to]
  study_population <- study_population[study_entry_date <= orig_study_exit_date, ]
  study_population <- study_population[study_exit_date > orig_study_exit_date, study_exit_date := orig_study_exit_date]
  
  study_population <- study_population[, c("orig_study_entry_date", "orig_study_exit_date") := NULL]
  setnames(study_population, c("study_entry_date", "study_exit_date"), c("start_date_of_period", "end_date_of_period"))
  
  tempname <- paste0("D4_population_d_long", suffix[[subpop]])
  assign(tempname, study_population)
  save(tempname, file = paste0(diroutput, tempname,".RData"),list=tempname)
  
  rm(list=paste0("D4_population_d_long", suffix[[subpop]]))
  rm(study_population)
  
  
  # calculate correct fup_days
  D4_population_d[, fup_days := correct_difftime(study_exit_date_children_d, cohort_entry_date_children_d)]
  
  D4_population_d<-D4_population_d[,.(person_id,sex,age_at_date_vax_1,ageband_at_date_vax_1, date_of_birth,
                                      study_entry_date_children_d, cohort_entry_date_children_d,study_exit_date_children_d,date_vax1,
                                      date_vax2, history_covid, type_vax_1,type_vax_2,not_in_cohort_d, fup_days,
                                      CV_at_date_vax_1, COVCANCER_at_date_vax_1, COVCOPD_at_date_vax_1,
                                      COVHIV_at_date_vax_1, COVCKD_at_date_vax_1, COVDIAB_at_date_vax_1,
                                      COVOBES_at_date_vax_1, COVSICKLE_at_date_vax_1, immunosuppressants_at_date_vax_1,
                                      at_risk_at_date_vax_1)]
  
  tempname<-paste0("D4_population_d",suffix[[subpop]])
  assign(tempname,D4_population_d)
  save(tempname, file = paste0(diroutput, tempname,".RData"),list=tempname)
  
  rm(list=tempname)
  rm(tempname)
  rm(D3_selection_criteria_d)
  if (this_datasource_has_subpopulations==T) rm(D4_population_d)
  rm(events_ALL_OUTCOMES,outcomes_covid)
  
}


