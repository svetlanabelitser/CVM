# COUNT PERSON TIME PER COVID PER MIS/MYOCARD
#-----------------------------------------------
# To estimate the weekly incidence rates of risks in 2020 by data source for MIS/Myocard

# input: D3_outcomes_covid, D4_population_b, D4_population_c, D4_population_d
# output: D4_persontime_b, D4_persontime_monthly_b, D4_persontime_c, D4_persontime_monthly_c, D4_persontime_d, D4_persontime_monthly_d

#COHORT B

for (subpop in subpopulations_non_empty) {  
  print(subpop)
  start_persontime_studytime = "20200101"
  
  # load(paste0(dirtemp,"D3_events_ALL_OUTCOMES",suffix[[subpop]],".RData"))
  load(paste0(diroutput,"D4_population_b",suffix[[subpop]],".RData"))
  
  # events_ALL_OUTCOMES<-get(paste0("D3_events_ALL_OUTCOMES", suffix[[subpop]]))
  # rm(list=paste0("D3_events_ALL_OUTCOMES", suffix[[subpop]]))
  population_b<-get(paste0("D4_population_b", suffix[[subpop]]))
  rm(list=paste0("D4_population_b", suffix[[subpop]]))
  
  endyear<- substr(population_b[,max(study_exit_date_MIS_b)], 1, 4)
  end_persontime_studytime<-as.character(paste0(endyear,"1231"))
  
  load(paste0(dirtemp,"D3_outcomes_severity_episodes_covid",suffix[[subpop]],".RData"))
  outcomes_covid<-get(paste0("D3_outcomes_severity_episodes_covid", suffix[[subpop]]))
  rm(list=paste0("D3_outcomes_severity_episodes_covid", suffix[[subpop]]))
  
  # covid_L1<-copy(outcomes_covid)[, date_event := date_event %m+% months(sample(c(1, 2, 3),
  #                                                                              nrow(outcomes_covid),
  #                                                                              replace = T))]
  # 
  # outcomes_covid <- rbind(outcomes_covid, covid_L1)
  # rm(covid_L1)
  
  nameoutput <- paste0("Output_file")
  assign(nameoutput, CountPersonTime(
    Dataset_events = outcomes_covid,
    Dataset = population_b,
    Person_id = "person_id",
    Start_study_time = start_persontime_studytime,
    End_study_time = end_persontime_studytime,
    Start_date = "cohort_entry_date_MIS_b",
    End_date = "study_exit_date_MIS_b",
    #Birth_date = "date_of_birth",
    Strata = c("sex","ageband_at_study_entry"),
    Name_event = "name_event",
    Date_event = "date_event",
    #Age_bands = c(0,19,29,39,49,59,69,79),
    Increment="month",
    Outcomes_rec = vect_new_severity,
    Rec_period = rep(60, length(vect_new_severity)),
    #Unit_of_age = "year",
    #include_remaning_ages = T,
    Aggregate = T
  ))
  
  Output_file[, which(duplicated(names(Output_file))) := NULL]
  
  nameoutput<-paste0("D4_persontime_b",suffix[[subpop]])
  assign(nameoutput ,Output_file)
  rm(Output_file)
  save(nameoutput, file = paste0(diroutput, nameoutput,".RData"),list=nameoutput)
  
  nameoutput2<-paste0("D4_persontime_b")
  assign(nameoutput2 ,get(nameoutput))
  thisdirexp <- ifelse(this_datasource_has_subpopulations == FALSE,direxp,direxpsubpop[[subpop]])
  fwrite(get(nameoutput2),file=paste0(thisdirexp,nameoutput2,".csv"))
  
  rm(list=c("nameoutput", "nameoutput2"))
  
  rm(population_b)
}

for (subpop in subpopulations_non_empty){
  tempname<-paste0("D4_persontime_b")
  thisdirexp <- ifelse(this_datasource_has_subpopulations == FALSE,direxp,direxpsubpop[[subpop]])
  assign(tempname,fread(paste0(thisdirexp,tempname,".csv")))
  thisdirsmallcountsremoved <- ifelse(this_datasource_has_subpopulations == FALSE,dirsmallcountsremoved,dirsmallcountsremovedsubpop[[subpop]])
  col<-colnames(get(tempname))[-(1:6)]
  temp<-paste0(col,"=5")
  temp2<-paste("c(",paste(temp, collapse = ','),")")
  suppressWarnings(
    DRE_Treshold(
      Inputfolder = thisdirexp,
      Outputfolder = thisdirsmallcountsremoved,
      Delimiter = ",",
      Varlist = c(eval(parse(text=(temp2)))),
      FileContains = "D4_persontime_b"
    )
  )
  rm(list=tempname)
}

#COHORT C
for (subpop in subpopulations_non_empty) {  
  print(subpop)
  start_persontime_studytime = "20200101"
  
  load(paste0(diroutput,"D4_population_c",suffix[[subpop]],".RData"))
  
  population_c<-get(paste0("D4_population_c", suffix[[subpop]]))
  rm(list=paste0("D4_population_c", suffix[[subpop]]))
  
  endyear<- substr(population_c[,max(study_exit_date_MIS_c)], 1, 4)
  end_persontime_studytime<-as.character(paste0(endyear,"1231"))
  
  nameoutput <- paste0("Output_file")
  assign(nameoutput, CountPersonTime(
    Dataset_events = outcomes_covid,
    Dataset = population_c,
    Person_id = "person_id",
    Start_study_time = start_persontime_studytime,
    End_study_time = end_persontime_studytime,
    Start_date = "cohort_entry_date_MIS_c",
    End_date = "study_exit_date_MIS_c",
    #Birth_date = "date_of_birth",
    Strata = c("sex","ageband_at_covid"),
    Name_event = "name_event",
    Date_event = "date_event",
    #Age_bands = c(0,19,29,39,49,59,69,79),
    Increment="month",
    Outcomes_rec = vect_new_severity,
    Rec_period = rep(60, length(vect_new_severity)),
    #Unit_of_age = "year",
    #include_remaning_ages = T,
    Aggregate = T
  ))
  
  Output_file[, which(duplicated(names(Output_file))) := NULL]
  
  nameoutput<-paste0("D4_persontime_c",suffix[[subpop]])
  assign(nameoutput ,Output_file)
  rm(Output_file)
  save(nameoutput, file = paste0(diroutput, nameoutput,".RData"),list=nameoutput)
  
  nameoutput2<-paste0("D4_persontime_c")
  assign(nameoutput2 ,get(nameoutput))
  thisdirexp <- ifelse(this_datasource_has_subpopulations == FALSE,direxp,direxpsubpop[[subpop]])
  fwrite(get(nameoutput2),file=paste0(thisdirexp,nameoutput2,".csv"))
  rm(list=c("nameoutput", "nameoutput2"))
  
  rm(population_c)
}

for (subpop in subpopulations_non_empty){
  tempname<-paste0("D4_persontime_c")
  thisdirexp <- ifelse(this_datasource_has_subpopulations == FALSE,direxp,direxpsubpop[[subpop]])
  assign(tempname,fread(paste0(thisdirexp,tempname,".csv")))
  thisdirsmallcountsremoved <- ifelse(this_datasource_has_subpopulations == FALSE,dirsmallcountsremoved,dirsmallcountsremovedsubpop[[subpop]])
  col<-colnames(get(tempname))[-(1:6)]
  temp<-paste0(col,"=5")
  temp2<-paste("c(",paste(temp, collapse = ','),")")
  suppressWarnings(
    DRE_Treshold(
      Inputfolder = thisdirexp,
      Outputfolder = thisdirsmallcountsremoved,
      Delimiter = ",",
      Varlist = c(eval(parse(text=(temp2)))),
      FileContains = "D4_persontime_c"
    )
  )
  rm(list=tempname)
}

#COHORT D
for (subpop in subpopulations_non_empty) {  
  print(subpop)
  start_persontime_studytime = "20200101"
  
  load(paste0(diroutput,"D4_population_d_28",suffix[[subpop]],".RData"))
  
  population_d<-get(paste0("D4_population_d_28", suffix[[subpop]]))
  rm(list=paste0("D4_population_d_28", suffix[[subpop]]))
  
  endyear<- substr(population_d[,max(end_period)], 1, 4)
  end_persontime_studytime<-as.character(paste0(endyear,"1231"))
  
  nameoutput <- paste0("Output_file")
  assign(nameoutput, CountPersonTime(
    Dataset_events = outcomes_covid,
    Dataset = population_d,
    Person_id = "person_id",
    Start_study_time = start_persontime_studytime,
    End_study_time = end_persontime_studytime,
    Start_date = "start_period",
    End_date = "end_period",
    #Birth_date = "date_of_birth",
    Strata = c("sex","ageband_at_date_vax_1","type_vax","Dose","history_covid"), #add covid before vaccine
    Name_event = "name_event",
    Date_event = "date_event",
    #Age_bands = c(0,19,29,39,49,59,69,79),
    Increment="month",
    Outcomes_rec = vect_new_severity,
    Rec_period = rep(60, length(vect_new_severity)),
    #Unit_of_age = "year",
    #include_remaning_ages = T,
    Aggregate = T
  ))
  
  Output_file[, which(duplicated(names(Output_file))) := NULL]
  
  nameoutput<-paste0("D4_persontime_d",suffix[[subpop]])
  assign(nameoutput ,Output_file)
  rm(Output_file)
  save(nameoutput, file = paste0(diroutput, nameoutput,".RData"),list=nameoutput)
  
  nameoutput2<-paste0("D4_persontime_d")
  assign(nameoutput2 ,get(nameoutput))
  thisdirexp <- ifelse(this_datasource_has_subpopulations == FALSE,direxp,direxpsubpop[[subpop]])
  fwrite(get(nameoutput2),file=paste0(thisdirexp,nameoutput2,".csv"))
  rm(list=c("nameoutput", "nameoutput2"))
  
  rm(population_d)
}


for (subpop in subpopulations_non_empty){
  tempname<-paste0("D4_persontime_d")
  thisdirexp <- ifelse(this_datasource_has_subpopulations == FALSE,direxp,direxpsubpop[[subpop]])
  assign(tempname,fread(paste0(thisdirexp,tempname,".csv")))
  thisdirsmallcountsremoved <- ifelse(this_datasource_has_subpopulations == FALSE,dirsmallcountsremoved,dirsmallcountsremovedsubpop[[subpop]])
  col<-colnames(get(tempname))[-(1:6)]
  temp<-paste0(col,"=5")
  temp2<-paste("c(",paste(temp, collapse = ','),")")
  suppressWarnings(
    DRE_Treshold(
      Inputfolder = thisdirexp,
      Outputfolder = thisdirsmallcountsremoved,
      Delimiter = ",",
      Varlist = c(eval(parse(text=(temp2)))),
      FileContains = "D4_persontime_d"
    )
  )
  rm(list=tempname)
}

#COHORT D
for (subpop in subpopulations_non_empty) {  
  print(subpop)
  start_persontime_studytime = "20200101"
  
  load(paste0(diroutput,"D4_population_d_long",suffix[[subpop]],".RData"))
  
  population_d<-get(paste0("D4_population_d_long", suffix[[subpop]]))
  rm(list=paste0("D4_population_d_long", suffix[[subpop]]))
  
  endyear<- substr(population_d[,max(end_date_of_period)], 1, 4)
  end_persontime_studytime<-as.character(paste0(endyear,"1231"))
  
  nameoutput <- paste0("Output_file")
  assign(nameoutput, CountPersonTime(
    Dataset_events = outcomes_covid,
    Dataset = population_d,
    Person_id = "person_id",
    Start_study_time = start_persontime_studytime,
    End_study_time = end_persontime_studytime,
    Start_date = "start_date_of_period",
    End_date = "end_date_of_period",
    #Birth_date = "date_of_birth",
    # Strata = c("sex","ageband_at_date_vax_1","type_vax","Dose", "Period", "CV", "COVCANCER", "COVCOPD", "COVHIV", "COVCKD", 
    #            "COVDIAB", "COVOBES", "COVSICKLE", "IMMUNOSUPPR", "any_risk_factors"), #add covid before vaccine
    Strata = c("sex","ageband_at_date_vax_1","type_vax","Dose", "Period"), #add covid before vaccine
    Name_event = "name_event",
    Date_event = "date_event",
    #Age_bands = c(0,19,29,39,49,59,69,79),
    Increment="month",
    Outcomes_rec = vect_new_severity,
    Rec_period = rep(60, length(vect_new_severity)),
    #Unit_of_age = "year",
    #include_remaning_ages = T,
    Aggregate = T
  ))
  
  Output_file[, which(duplicated(names(Output_file))) := NULL]
  
  nameoutput<-paste0("D4_persontime_d_long",suffix[[subpop]])
  assign(nameoutput ,Output_file)
  rm(Output_file)
  save(nameoutput, file = paste0(diroutput, nameoutput,".RData"),list=nameoutput)
  
  nameoutput2<-paste0("D4_persontime_d_long")
  assign(nameoutput2 ,get(nameoutput))
  thisdirexp <- ifelse(this_datasource_has_subpopulations == FALSE,direxp,direxpsubpop[[subpop]])
  fwrite(get(nameoutput2),file=paste0(thisdirexp,nameoutput2,".csv"))
  rm(list=c("nameoutput", "nameoutput2"))
  
  rm(population_d)
}


for (subpop in subpopulations_non_empty){
  tempname<-paste0("D4_persontime_d_long")
  thisdirexp <- ifelse(this_datasource_has_subpopulations == FALSE,direxp,direxpsubpop[[subpop]])
  assign(tempname,fread(paste0(thisdirexp,tempname,".csv")))
  thisdirsmallcountsremoved <- ifelse(this_datasource_has_subpopulations == FALSE,dirsmallcountsremoved,dirsmallcountsremovedsubpop[[subpop]])
  col<-colnames(get(tempname))[-(1:6)]
  temp<-paste0(col,"=5")
  temp2<-paste("c(",paste(temp, collapse = ','),")")
  suppressWarnings(
    DRE_Treshold(
      Inputfolder = thisdirexp,
      Outputfolder = thisdirsmallcountsremoved,
      Delimiter = ",",
      Varlist = c(eval(parse(text=(temp2)))),
      FileContains = "D4_persontime_d_long"
    )
  )
  rm(list=tempname)
}

rm(outcomes_covid)