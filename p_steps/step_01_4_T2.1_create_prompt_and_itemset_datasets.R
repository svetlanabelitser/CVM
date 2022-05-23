# -----------------------------------------------------------------------
# RETRIEVE PROMPT DATASETS and ITEMSET DATASETS

# input: SURVEY_ID, SURVEY_OBSERVATIONS
# output: prompt datasets and itemset datasets

print("RETRIEVE RECORDS FROM SURVEY")

# RETRIEVE FROM SURVEY_ID ALL prompt datasets corresponding to "covid_registry" 

# collect and rbind from all files whose name starts with 'SURVEY_ID'
SURVEY_ID_COVID <- data.table(person_id = character(),survey_date  = character(), survey_meaning = character())

for (file in files_ConcePTION_CDM_tables[["SURVEY_ID"]]) {
  SURVEY_ID_COVID <-rbind(SURVEY_ID_COVID,fread(paste0(dirinput,file,".csv"), colClasses = list( character="person_id"))[survey_meaning =="covid_registry",])  
}
covid_registry <- SURVEY_ID_COVID[,date:=ymd(survey_date)]
covid_registry <- covid_registry[,-"survey_date"]


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

suppressWarnings(rm(SURVEY_ID_COVID, covid_registry, files))
suppressWarnings(rm(list=names(itemset_AVpair_our_study_this_datasource)))
