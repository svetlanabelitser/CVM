# CREATE ALGORITHMS FOR COVID SEVERITY REPEATED
#-----------------------------------------------
# input: D4_study_population, D3_events_COVID_narrow, D3_events_DEATH, covid_registry, COVID_test
# output: D3_covid_episodes, D3_covid_episodes_description and QC_covid_episodes


print("CREATE ALGORITHMS FOR COVID SEVERITY FOR REPEATED EVENTS")

load(paste0(dirtemp,"covid_registry.RData")) 
load(paste0(dirtemp,"COVID_test.RData")) 
load(paste0(dirtemp,"emptydataset"))

# data sources having positive tests
datasources_positive_tests <- c("TEST","SIDIAP","PEDIANET")

# data sources including all records with a covid diagnosis
datasources_covid_diagnosis_all <- c("FISABIO","SIDIAP")

# data sources including only records of covid diagnosis from hospitals
datasources_covid_diagnosis_only_hosp <- c("TEST","ARS","CASERTA")



# OVERALL STRATEGY 
# 1 rbind all files that imply covid, rename date to 'date', name list_all_covid_notifications
# 2 create copy of list_all_covid_notifications with date date_previous with itself and only keep records having no match or date >= date_previous - 60, name D3_covid_episodes
# 3 use this D3_covid_episodes as the list of unique covid episodes: generate for each record all components needed to assess severity of that specific episode
# 4 combine the components to label severity

# in this step we enact 1 and 2

#---------------------------------
#---------------------------------
# 1 create list_all_covid_notifications
#---------------------------------
#---------------------------------

list_all_covid_notifications <- emptydataset

#-------------------------------------
# add to list_all_covid_notifications positive results from covid test (after data source-specific processing)

covid_test_positive <- emptydataset

if (thisdatasource %in% datasources_positive_tests){
  covid_test_positive <- COVID_test[,.(person_id,date, mo_source_value)][mo_source_value == "positive"]
  rm(COVID_test)
  covid_test_positive <- covid_test_positive[,.(person_id,date)]
  covid_test_positive <- covid_test_positive[,origin_case := "covid_test_positive" ]
}

list_all_covid_notifications <- rbind(list_all_covid_notifications,covid_test_positive,fill = T)

#-------------------------------------
# add to list_all_covid_notifications records from covid registry

covid_registry <- covid_registry[,.(person_id,date)]
covid_registry <- covid_registry[,origin_case := "covid_registry" ]

list_all_covid_notifications <- rbind(list_all_covid_notifications,covid_registry,fill = T)


#-------------------------------------
# add to list_all_covid_notifications diagnoses of covid (only in selected data sources, and sometimes only in hospital) (this is specific per subpopulation, therefore from this moment on we only split the computation per subpopulation)

for (subpop in subpopulations_non_empty) {
  load(paste0(dirtemp,"D3_events_COVID_narrow",suffix[[subpop]],".RData"))
  load(paste0(diroutput,"D4_study_population",suffix[[subpop]],".RData"))
  
  dia_COVID_narrow <- as.data.table(get(paste0("D3_events_COVID_narrow",suffix[[subpop]])))
  persons_in_pop <- as.data.table(get(paste0("D4_study_population",suffix[[subpop]])))[,.(person_id)]
  

  list_all_covid_notificationssubpop <- list_all_covid_notifications
  
  if (thisdatasource %in% datasources_covid_diagnosis_all) {
    dia_COVID_narrow <- dia_COVID_narrow[,origin_case := paste0("covid_narrow_m_",meaning_of_event)]
    dia_COVID_narrow <- dia_COVID_narrow[,.(person_id,date,origin_case)]
    list_all_covid_notificationssubpop <- rbind(list_all_covid_notificationssubpop,dia_COVID_narrow,fill = TRUE)
  }
  if (thisdatasource %in% datasources_covid_diagnosis_only_hosp) {
    dia_COVID_narrow <- dia_COVID_narrow[eval(parse(text = condmeaning[["HOSP"]])),]
    dia_COVID_narrow <- dia_COVID_narrow[,origin_case := paste0("covid_narrow_hosp_m_",meaning_of_event)]
    dia_COVID_narrow <- dia_COVID_narrow[,.(person_id,date,origin_case)]
    list_all_covid_notificationssubpop <- rbind(list_all_covid_notificationssubpop,dia_COVID_narrow,fill = TRUE)
  }
  
  # step 2: create a second copy of the list, and use it to remove records having a previous 
  
  # DT <- DT[, date_next_record := shift(date, n = 1, fill = NA, type=c("lead")), by = "pers_id"] 
  setorder(list_all_covid_notificationssubpop,"person_id","date")
  # list_all_covid_notificationssubpop <- list_all_covid_notificationssubpop[,date_next_record := shift(date, n = 1, fill = NA, type=c("lead")), by = "person_id"]
  # clean to keep only persons who are in fact in the study population (to avoid using too much memory during the next merge)
  list_all_covid_notificationssubpop <- merge(list_all_covid_notificationssubpop,persons_in_pop, all = FALSE, by = "person_id") 
  
  list_all_covid_notificationssubpop <- list_all_covid_notificationssubpop[,n:=seq_along(.I), by = "person_id"]
  
  copy <- copy(list_all_covid_notificationssubpop)
  copy <- copy[,.(person_id,date,n)]
  setnames(copy,c("date","n"),c("date_previous","n_previous"))
  listtodrop <- merge(list_all_covid_notificationssubpop,copy, all.x = TRUE, by = "person_id",allow.cartesian = TRUE)[date < date_previous + 60 & date > date_previous, ]
  listtodrop <- unique(listtodrop[,.(person_id,n)])
  listtodrop <- listtodrop[,todrop:= 1]
  listunique <- merge(list_all_covid_notificationssubpop,listtodrop, all.x = TRUE, by = c("person_id","n"))
  listunique <- listunique[ is.na(todrop), ]
  listunique <- unique(listunique[,.(person_id,date) ])

  
  tempname <- paste0("D3_covid_episodes",suffix[[subpop]])
  assign(tempname,listunique)
  save(list = tempname, file = paste0(dirtemp,tempname,".RData"))

  #----------------------------------------
  # describe how each episode was detected: create D3_covid_episodes_description and QC_covid_episodes
  
  setorder(listunique,"person_id","date")
  listdescr <- listunique[,n:=seq_along(.I), by = "person_id"]
  listdescr <- listdescr[,date_next_record := shift(date, n = 1, fill = NA, type = c("lead")), by = "person_id"]
  list_all_covid_notificationssubpop <- list_all_covid_notificationssubpop[,.(person_id,date,origin_case)]
  setnames(list_all_covid_notificationssubpop,c("date"),c("date_descr"))
  listdescr <- merge(listdescr,list_all_covid_notificationssubpop, all.x = TRUE, by = "person_id",allow.cartesian = TRUE)[date_descr >= date & (date_descr < date_next_record | is.na(date_next_record)), ]
  listdescr <- unique(listdescr[,.(person_id,date,n,origin_case)])
  listdescr <- listdescr[, component := 1]
  listdescr <- dcast(listdescr,person_id + date + n  ~ origin_case, value.var = "component", fill = 0 )
  listdescr <- listdescr[, year := year(date)]
  
  tempname <- paste0("D3_covid_episodes_description",suffix[[subpop]])
  assign(tempname,listdescr)
  save(list = tempname, file = paste0(dirtemp,tempname,".RData"))
  
  columns_listdescr <- colnames(listdescr)[colnames(listdescr) %not in% c("person_id","date")]
  listdescr <- listdescr[, .N, by = columns_listdescr]

  tempname <- paste0("QC_covid_episodes",suffix[[subpop]])
  assign(tempname,listdescr)
  save(list = tempname, file = paste0(diroutput,tempname,".RData"))
  
  thisdirexp <- ifelse(this_datasource_has_subpopulations == FALSE,direxp,direxpsubpop[[subpop]])
  fwrite(listdescr, file = paste0(thisdirexp,tempname,".csv"))
  
  
  rm(list_all_covid_notificationssubpop,copy,listunique,listtodrop,persons_in_pop,columns_listdescr,listdescr)
  rm(list = paste0("D3_events_COVID_narrow",suffix[[subpop]]))
  rm(list = paste0("D4_study_population", suffix[[subpop]]))
  rm(list = tempname)
}

#rm(list_all_covid_notifications)






