# -----------------------------------------------------------------------
# RETRIEVE PROMPT DATASETS and ITEMSET DATASETS

# input: SURVEY_ID, SURVEY_OBSERVATIONS
# output: prompt datasets and itemset datasets

print("RETRIEVE PROMPTSETS")

# RETRIEVE FROM SURVEY_ID ALL prompt datasets corresponding to "covid_registry" 

SURVEY_ID_COVID <- data.table(person_id = character(),survey_date  = character(), survey_meaning = character())

for (file in files_ConcePTION_CDM_tables[["SURVEY_ID"]]) {
  SURVEY_ID_COVID <-rbind(SURVEY_ID_COVID,fread(paste0(dirinput,file,".csv"), colClasses = list( character="person_id"))[survey_meaning =="covid_registry" | survey_meaning == "covid19_registry",], fill = TRUE)  
}
covid_registry <- SURVEY_ID_COVID[,date:=ymd(survey_date)]
covid_registry <- covid_registry[,-"survey_date"]



# RETRIEVE FROM VISIT_OCCURRENCE ALL prompt datasets corresponding to "hospitalisation_automatically_referred_to_PC" (data source PEDIANET)

hospitalisation_automatically_referred_to_PC <- data.table(person_id = character(),visit_start_date  = character(), meaning_of_visit = character())

for (file in files_ConcePTION_CDM_tables[["VISIT_OCCURRENCE"]]) {
  print('in')
  hospitalisation_automatically_referred_to_PC <- rbind(VISIT_OCCURRENCE_hosp,fread(paste0(dirinput,file,".csv"), colClasses = list( character="person_id"))[meaning_of_visit == "hospitalisation_automatically_referred_to_PC",], fill = TRUE)  
}

hospitalisation_automatically_referred_to_PC <- hospitalisation_automatically_referred_to_PC[,date:=ymd(visit_start_date)]
hospitalisation_automatically_referred_to_PC <- hospitalisation_automatically_referred_to_PC[,-"visit_start_date"]


print("RETRIEVE ITEMSETS")


# RETRIEVE FROM SURVEY_OBSERVATIONS ALL itemset datasets from source_table,source_column (all study variables: if one has no itemset, the dataset is empty)
#-----------------------------------------------------


CreateItemsetDatasets(EAVtables = ConcePTION_CDM_EAV_tables_retrieve_source,
                      datevar= ConcePTION_CDM_datevar_retrieve,
                      dateformat= "YYYYmmdd",
                      rename_col = list(person_id=person_id_retrieve,date=date_retrieve),
                      study_variable_names = study_variables_of_our_study,
                      itemset = itemset_AVpair_our_study_this_datasource,
                      dirinput = dirinput,
                      diroutput = dirtemp,
                      extension = c("csv"))

# RETRIEVE FROM SURVEY_OBSERVATIONS ALL itemset datasets from origin,meaning (only study variables having this specification in the datasource are retrieved)
#-----------------------------------------------------

CreateItemsetDatasets(EAVtables = ConcePTION_CDM_EAV_tables_retrieve_meaning,
                      datevar= ConcePTION_CDM_datevar_retrieve,
                      dateformat= "YYYYmmdd",
                      rename_col = list(person_id=person_id_retrieve,date=date_retrieve),
                      study_variable_names = study_variables_this_datasource_meaning,
                      itemset = itemset_AVpair_our_study_this_datasource_meaning,
                      dirinput = dirinput,
                      diroutput = dirtemp,
                      extension = c("csv"))


save(covid_registry,file = paste0(dirtemp,"covid_registry.RData"))
save(hospitalisation_automatically_referred_to_PC,file = paste0(dirtemp,"hospitalisation_automatically_referred_to_PC.RData"))

suppressWarnings(rm(SURVEY_ID_COVID, covid_registry,hospitalisation_automatically_referred_to_PC, files))
suppressWarnings(rm(list=names(itemset_AVpair_our_study_this_datasource)))
