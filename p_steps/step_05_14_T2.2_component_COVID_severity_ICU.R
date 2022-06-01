# CREATE COMPONENTS FOR COVID SEVERITY - ICU
#-----------------------------------------------
# input: D4_study_population, D3_events_COVID_narrow, COVID_symptoms, COVID_test
# output: D3_covid_severity_components_ICU, QC_covid_severity_components_ICU

# in this step the list of unique episodes of covid is labelled with the components indicating that the episode was admitted to ICU. The list of components is data source-specific

# data sources including records of ARDS diagnosis
datasources_ICU_from_ARDS <- c("TEST","ARS","CASERTA","FISABIO","SIDIAP")

# data sources including records of procedures of mechanical ventilation
datasources_proc_mechanical_ventilation <- c("TEST","ARS","CASERTA")

# data sources including records of access to ICU from hospitalisation
datasources_access_ICU <- c("TEST","FISABIO")

# data sources including access to ICU from covid registry (to be handled in a data source-tailored manner below)
datasources_ICU_from_covid_registry <- c("TEST","ARS","BIFAP","CASERTA")
 

print("CREATE COMPONENTS FOR COVID SEVERITY - ICU")

load(paste0(dirtemp,"COVID_symptoms.RData")) 
load(paste0(dirtemp,"emptydataset"))

# "covid_severity_1" "covid_severity_2" "covid_severity_3" "covid_severity_4" "covid_severity_5"


# OVERALL STRATEGY 
# 1 rbind all files that imply ICU
# 2 associate each record to the corresponding episode (from the date of the episode until the date of the next episode)
# 3 reshape the dataset to obtain one record per episode, labelled with each component indicating that the episode was admitted to ICU
# 4 create a descriptive of all the components that contributed to classify the episode as admitted to ICU

for (subpop in subpopulations_non_empty) { 
  #---------------------------------
  #---------------------------------
  # 1 rbind all files that imply admission to ICU
  #---------------------------------
  #---------------------------------
  

  load(paste0(dirtemp,"D3_covid_episodes",suffix[[subpop]],".RData"))
  COVID_episodes <- as.data.table(get(paste0("D3_covid_episodes",suffix[[subpop]])))
  
  load(paste0(dirtemp,"D3_events_COVID_narrow",suffix[[subpop]],".RData"))
  dia_COVID_narrow <- as.data.table(get(paste0("D3_events_COVID_narrow",suffix[[subpop]])))
  

  #-------------------------
  # inizialise the dataset of components for severity level 'covid ICU'
  components_covid_ICU <- emptydataset

  #-------------------------
  # ARDS after covid

  if (thisdatasource %in% datasources_ICU_from_ARDS){
    load(paste0(dirtemp,"D3_events_ARDS_narrow",suffix[[subpop]],".RData"))
    dia_ARDS_narrow <- as.data.table(get(paste0("D3_events_ARDS_narrow",suffix[[subpop]])))
    setnames(dia_ARDS_narrow,c("date"),c("date_event"))
    dia_ARDS_narrow <- dia_ARDS_narrow[,.(person_id,date_event,meaning_of_event)]
    dia_ARDS_narrow <-  merge(COVID_episodes,dia_ARDS_narrow, by = "person_id",allow.cartesian = TRUE)[date_event >= date & date_event <= date_event + 14 , ]
    dia_ARDS_narrow <- dia_ARDS_narrow[,origin_component := paste0("dia_ARDS_narrow_m_",meaning_of_event)]
    dia_ARDS_narrow <- dia_ARDS_narrow[,.(person_id,date,origin_component)]
    components_covid_ICU <- rbind(components_covid_ICU, dia_ARDS_narrow, fill = TRUE)[,.(person_id, date, origin_component)]
    rm(dia_ARDS_narrow)
  }
  
  #-------------------------
  # mechanical ventilation after covid
  
  if (thisdatasource %in% datasources_proc_mechanical_ventilation){
    load(paste0(dirtemp,"ICU_VENTILATION.RData"))
    mech_ventilation <- as.data.table(ICU_VENTILATION)
    setnames(mech_ventilation,c("date"),c("date_event"))
    mech_ventilation <- mech_ventilation[,.(person_id,date_event,meaning_of_procedure)]
    mech_ventilation <-  merge(COVID_episodes,mech_ventilation, by = "person_id",allow.cartesian = TRUE)[date_event >= date & date_event <= date_event + 14 , ]
    mech_ventilation <- mech_ventilation[,origin_component := paste0("mech_ventilation_m_",meaning_of_procedure)]
    mech_ventilation <- mech_ventilation[,.(person_id,date,origin_component)]
    components_covid_ICU <- rbind(components_covid_ICU, mech_ventilation, fill = TRUE)[,.(person_id, date, origin_component)]
    rm(mech_ventilation,ICU_VENTILATION)
  }

  #-------------------------
  # ICU from EVENTS in hospitalisation due to covid
  if (thisdatasource %in% datasources_access_ICU){
    dia_COVID_narrow_ICU <-  dia_COVID_narrow[meaning_of_event == "hospitalisation_ICU_unspecified",]
    dia_COVID_narrow_ICU <- dia_COVID_narrow_ICU[,origin_component := paste0("covid_narrow_ICU_m_",meaning_of_event)]
    components_covid_ICU <- rbind(components_covid_ICU, dia_COVID_narrow_ICU, fill = TRUE)[,.(person_id, date, origin_component)]
  }
  

  #-------------------------
  # ICU from free text (data source-tailored)
  
  # for PEDIANET: add itemset extracted_from_free_text with so_source_values == "ACCESS_ICU"
  
  #-------------------------
  # ICU from covid registry (data source-tailored)
  
  if (thisdatasource %in% datasources_hosp_from_covid_registry){
    ICU_from_covid_registry <- emptydataset
    
    if (thisdatasource %in% c("TEST","ARS","CASERTA")){
      load(paste0(dirtemp,"COVID_symptoms.RData"))
      ICU_from_covid_registrythisdatasource <- COVID_symptoms[so_source_value == 'Critico' | so_source_value == 'CRITICO',]
      if (this_datasource_has_subpopulations == TRUE){ 
        ICU_from_covid_registrythisdatasource <- ICU_from_covid_registrythisdatasource[eval(parse(text = select_in_subpopulationsSO[[subpop]])),]
      }
      ICU_from_covid_registry = rbind(ICU_from_covid_registry,ICU_from_covid_registrythisdatasource, fill = TRUE)
      rm(COVID_symptoms, ICU_from_covid_registrythisdatasource)
    }
    
    if (thisdatasource %in% c("TEST","BIFAP")){
      load(paste0(dirtemp,"COVID_ICU.RData"))
      ICU_from_covid_registrythisdatasource <- COVID_ICU[so_source_column == 'Ingreso_uci' & so_source_value == '1',]
      if (this_datasource_has_subpopulations == TRUE){ 
        ICU_from_covid_registrythisdatasource <- ICU_from_covid_registrythisdatasource[eval(parse(text = select_in_subpopulationsSO[[subpop]])),]
      }
      ICU_from_covid_registry = rbind(ICU_from_covid_registry,ICU_from_covid_registrythisdatasource, fill = TRUE)
      rm(COVID_ICU, ICU_from_covid_registrythisdatasource)
    }
    
    ICU_from_covid_registry <- ICU_from_covid_registry[,origin_component := "ICU_from_covid_registry"]
    components_covid_ICU <- rbind(components_covid_ICU, ICU_from_covid_registry, fill = TRUE)[,.(person_id, date, origin_component)]
    
    rm(ICU_from_covid_registry)
    
  }
  
  #---------------------------------
  #---------------------------------
  # 2 MERGE ALL COMPONENTS TO LIST OF EPISODES
  #---------------------------------
  #---------------------------------

  COVID_episodes <- COVID_episodes[,date_next_record := shift(date, n = 1, fill = NA, type = c("lead")), by = "person_id"]
  components_covid_ICU <- components_covid_ICU[,.(person_id,date,origin_component)]
  setnames(components_covid_ICU,c("date"),c("date_component"))
  components_covid_ICU <- merge(COVID_episodes,components_covid_ICU, all.x = TRUE, by = "person_id",allow.cartesian = TRUE)[date_component >= date & (date_component < date_next_record | is.na(date_next_record)), ]
  components_covid_ICU <- unique(components_covid_ICU[,.(person_id,date,origin_component)])

  #---------------------------------
  #---------------------------------
  # 3 reshape the dataset to obtain one record per episode
  #---------------------------------
  #---------------------------------
  
    
  components_covid_ICU <- components_covid_ICU[, component := 1]
  if (nrow(components_covid_ICU) > 0 ){
    components_covid_ICU <- dcast(components_covid_ICU,person_id + date ~ origin_component, value.var = "component", fill = 0 )
  }else{
    components_covid_ICU <- components_covid_ICU[,.(person_id,date)]
  }
    
  tempname <- paste0("D3_covid_severity_components_ICU",suffix[[subpop]])
  assign(tempname,components_covid_ICU)
  save(list = tempname, file = paste0(dirtemp,tempname,".RData"))

  #---------------------------------
  #---------------------------------
  # 4 count occurrence of each combination of components
  #---------------------------------
  #---------------------------------

  components_covid_ICU <- components_covid_ICU[, year := year(date)]
  columns_components_covid_ICU <- colnames(components_covid_ICU)[colnames(components_covid_ICU) %not in% c("person_id","date","date_next_record","date_component")]
  components_covid_ICU <- components_covid_ICU[, .N, by = columns_components_covid_ICU]
  
  tempname <- paste0("QC_covid_severity_components_ICU",suffix[[subpop]])
  assign(tempname,components_covid_ICU)
  save(list = tempname, file = paste0(diroutput,tempname,".RData"))
  
  thisdirexp <- ifelse(this_datasource_has_subpopulations == FALSE,direxp,direxpsubpop[[subpop]])
  fwrite(components_covid_ICU, file = paste0(thisdirexp,tempname,".csv"))
  
  rm(dia_COVID_narrow, components_covid_ICU)
  
}
