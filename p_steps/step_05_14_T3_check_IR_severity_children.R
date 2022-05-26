# COUNT PERSON TIME PER COVID
#-----------------------------------------------
#To estimate the weekly incidence rates of COVID-19 (overall and by severity level) in 2020 by data source

# input: D3_vaxweeks_including_not_vaccinated, D3_outcomes_covid ,list_outcomes_observed_COVID
# output: D4_persontime_covid_week (exported to csv)

print("COUNT PERSON TIME PER COVID by week benefits")

persontime_covid_week <- vector(mode = 'list')
for (subpop in subpopulations_non_empty) {
  print(subpop)
  
  start_week=seq.Date(as.Date("20200106","%Y%m%d"),Sys.Date(),by = "week")
  
  load(paste0(diroutput,"D4_study_population",suffix[[subpop]],".RData"))
  study_population<-get(paste0("D4_study_population", suffix[[subpop]]))
  
  load(paste0(dirtemp,"D3_covid_episodes",suffix[[subpop]],".RData")) #L1plus
  outcomes_covid<-get(paste0("D3_covid_episodes", suffix[[subpop]]))
  rm(list=paste0("D3_covid_episodes", suffix[[subpop]]))
  
  setnames(outcomes_covid, "date", "date_event")
  outcomes_covid[, name_event := "COVID"]
  
  max_exit<-study_population[,max(study_exit_date)]
  if (nrow(outcomes_covid) != 0) {
    last_event<-outcomes_covid[,max(date_event)]
    if (last_event<ymd("20200101")) {
      next
    }
    end_persontime_studytime<-min(max_exit,last_event)
  } else {
    end_persontime_studytime<-max_exit
  }
  start_week=start_week[start_week<=end_persontime_studytime]
  end_week=start_week+6
  start_week=gsub("-","", start_week)
  end_week=gsub("-","", end_week)
  
  
  persontime_covid_week <- CountPersonTime(
    Dataset_events = outcomes_covid,
    Dataset = study_population,
    Person_id = "person_id",
    Start_study_time = "20191230",
    End_study_time = end_week[length(end_week)],
    Start_date = "study_entry_date",
    End_date = "study_exit_date",
    Birth_date = "date_of_birth",
    Strata = c("sex"),
    Name_event = "name_event",
    Date_event = "date_event",
    Age_bands = c(0, 4, 11, 17, 24, 29, 39, 49, 59, 69, 79),
    Increment="week",
    Outcomes_nrec = "COVID", 
    # Unit_of_age = "year",
    # include_remaning_ages = T,
    Aggregate = T
  )
  
  thisdirexp <- ifelse(this_datasource_has_subpopulations == FALSE,direxp,direxpsubpop[[subpop]])
  fwrite(persontime_covid_week,file=paste0(thisdirexp,"D4_persontime_covid_week.csv"))
  
  nameoutput<-paste0("D4_persontime_covid_week",suffix[[subpop]])
  assign(nameoutput,persontime_covid_week)
  save(nameoutput,file=paste0(diroutput,nameoutput,".RData"),list=nameoutput)
  
  rm(list=paste0("D4_persontime_covid_week",suffix[[subpop]]))
  rm(outcomes_covid)
}


for (subpop in subpopulations_non_empty){
  tempname<-paste0("D4_persontime_covid_week")
  thisdirexp <- ifelse(this_datasource_has_subpopulations == FALSE,direxp,direxpsubpop[[subpop]])
  assign(tempname,fread(paste0(thisdirexp,tempname,".csv")))
  thisdirsmallcountsremoved <- ifelse(this_datasource_has_subpopulations == FALSE,dirsmallcountsremoved,dirsmallcountsremovedsubpop[[subpop]])
  col<-colnames(get(tempname))[-(1:3)]
  temp<-paste0(col,"=5")
  temp2<-paste("c(",paste(temp, collapse = ','),")")
  suppressWarnings(
    DRE_Treshold(
      Inputfolder = thisdirexp,
      Outputfolder = thisdirsmallcountsremoved,
      Delimiter = ",",
      Varlist = c(eval(parse(text=(temp2)))),
      FileContains = "D4_persontime_covid_week"
    )
  )
  rm(list=tempname)
}

for (subpop in subpopulations_non_empty) {
  load(paste0(diroutput,"D4_persontime_covid_week",suffix[[subpop]],".RData"))
  
  D4_persontime_covid_week<-get(paste0("D4_persontime_covid_week", suffix[[subpop]]))
  list_outcomes_observed<-"COVID"
  
  events<-c("COVID")
  
  for (ev in events) {
    name_cols <- paste0(c("IR_", "lb_", "ub_"), ev)
    name_count <- paste0(ev,"_b")
    name_pt <- paste0("Persontime_",ev)
    D4_persontime_covid_week[, (name_cols) := exactPoiCI(D4_persontime_covid_week, name_count, name_pt)]
  }
  
  nameoutput<-paste0("D4_persontime_covid_week")
  assign(nameoutput,D4_persontime_covid_week[, !grep("^Person", names(D4_persontime_covid_week)) , with = FALSE])
  
  thisdirexp <- ifelse(this_datasource_has_subpopulations == FALSE,direxp,direxpsubpop[[subpop]])
  
  save(nameoutput,file=paste0(thisdirexp,nameoutput,".RData"),list=nameoutput)
  
  fwrite(get(nameoutput),file=paste0(thisdirexp,nameoutput,".csv"))
  rm(list=nameoutput)
  rm(D4_persontime_covid_week)
}


rm(persontime_covid_week,outcomes_covid,study_population)
