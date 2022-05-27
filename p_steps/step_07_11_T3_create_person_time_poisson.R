# COUNT PERSON TIME PER COVID PER POISSON
#-----------------------------------------------
# To estimate the weekly incidence rates of risks in 2020 by data source for poisson

# input: D3_events_ALL_OUTCOMES, D3_vaxweeks_poisson
# output: D4_persontime_risk_month_poisson

print("COUNT PERSON TIME by month for risks (Poisson)")

persontime_risk_year <- vector(mode = 'list')

for (subpop in subpopulations_non_empty) {  
  print(subpop)
  start_persontime_studytime = "20200101"
  
  load(paste0(dirtemp,"D3_vaxweeks_poisson",suffix[[subpop]],".RData"))
  study_population<-get(paste0("D3_vaxweeks_poisson", suffix[[subpop]]))
  
  load(paste0(dirtemp,"D3_outcomes_severity_episodes_covid",suffix[[subpop]],".RData"))
  outcomes_covid<-get(paste0("D3_outcomes_severity_episodes_covid", suffix[[subpop]]))
  rm(list=paste0("D3_outcomes_severity_episodes_covid", suffix[[subpop]]))
  
  endyear<- substr(study_population[,max(end_date_of_period)], 1, 4)
  end_persontime_studytime<-as.character(paste0(endyear,"1231"))
  
  print("recurrent")
  
  nameoutput <- paste0("Output_file",suffix[[subpop]])
  assign(nameoutput,CountPersonTime(
    Dataset_events = outcomes_covid,
    Dataset = study_population,
    Person_id = "person_id",
    Start_study_time = start_persontime_studytime,
    End_study_time = end_persontime_studytime,
    Start_date = "start_date_of_period",
    End_date = "end_date_of_period",
    Birth_date = "date_of_birth",
    Strata = c("DAP", "Gender",
               "COVID19", "Vaccine1", "Vaccine2", "Dose1", "Dose2", "CV_at_study_entry",
               "COVCANCER_at_study_entry", "COVCOPD_at_study_entry", "COVHIV_at_study_entry",
               "COVCKD_at_study_entry", "COVDIAB_at_study_entry", "COVOBES_at_study_entry",
               "COVSICKLE_at_study_entry", "IMMUNOSUPPR_at_study_entry",
               "any_risk_factors_at_study_entry", "all_risk_factors_at_study_entry",
               "CV_at_date_vax", "COVCANCER_at_date_vax", "COVCOPD_at_date_vax", "COVHIV_at_date_vax",
               "COVCKD_at_date_vax", "COVDIAB_at_date_vax", "COVOBES_at_date_vax",
               "COVSICKLE_at_date_vax", "IMMUNOSUPPR_at_date_vax", "any_risk_factors_at_date_vax",
               "all_risk_factors_at_date_vax"),
    Name_event = "name_event",
    Date_event = "date_event",
    Age_bands = Agebands_countpersontime,
    Increment = "month",
    Outcomes_rec = vect_new_severity,
    Aggregate = T,
    Rec_period = rep(60, length(vect_new_severity))
  ))
  
  for (i in names(get(paste0("Output_file",suffix[[subpop]])))){
    get(paste0("Output_file",suffix[[subpop]]))[is.na(get(i)), (i):=0]
  }
  
  persontime_risk_year <- get(paste0("Output_file",suffix[[subpop]]))
  
  persontime_risk_year[, which(duplicated(names(persontime_risk_year))) := NULL]
  
  thisdirexp <- ifelse(this_datasource_has_subpopulations == FALSE,direxp,direxpsubpop[[subpop]])
  fwrite(persontime_risk_year,file=paste0(thisdirexp,"D4_persontime_risk_month_poisson.csv"))
  
  nameoutput<-paste0("D4_persontime_risk_month_poisson", suffix[[subpop]])
  assign(nameoutput,persontime_risk_year)
  save(nameoutput,file=paste0(diroutput,nameoutput,".RData"),list=nameoutput)
  
  rm(list=paste0("Output_file",suffix[[subpop]]) )
  rm(list=nameoutput)
  rm(list=paste0("D3_vaxweeks_poisson", suffix[[subpop]]))
}

for (subpop in subpopulations_non_empty){
  tempname<-paste0("D4_persontime_risk_month_poisson")
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
      FileContains = "D4_persontime_risk_month_poisson"
    )
  )
  rm(list=tempname)
}
# rm(list = nameobject)
rm(persontime_risk_year,outcomes_covid,study_population)
