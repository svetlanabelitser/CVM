#------------------------------------------------------------------
# create events and components of OUTCOMES 

# input: concept set datasets of outcomes (narrow and possible), D4_study_population
# output: for each outcome OUTCOME, D3_components_OUTCOME.RData and D3_events_OUTCOME_type.RData, for type = narrow, possible

print('create events and create components of OUTCOMES and CONTROLS')

for (COVARIATE in COV_variables) {
  
  print(COVARIATE)
  
  for (subpop in subpopulations_non_empty) {
    
    print(subpop)
    
    name_D4_study_population <- paste0("D4_study_population", suffix[[subpop]])
    load(paste0(diroutput, name_D4_study_population, ".RData")) 
    study_population <- get(name_D4_study_population)[,.(person_id, study_entry_date)]
    
    selectionOUTCOME <- "date >= study_entry_date - 365 & date < study_entry_date"
    
    # delete records that are not observed in this whole subpopulation
    if (this_datasource_has_subpopulations == TRUE){
      selectionOUTCOME <- paste0(selectionOUTCOME,' & ',select_in_subpopulationsEVENTS[[subpop]])
    }
    
    nameconceptsetdatasetOUTCOMEtype <- variable_definition[[COVARIATE]]
    conceptsets_list <- lapply(nameconceptsetdatasetOUTCOMEtype,
                               function(x) get(load(paste0(dirconceptsets, x,".RData"))[[1]]))
    components <- MergeFilterAndCollapse(listdatasetL= conceptsets_list,
                                         datasetS = study_population,
                                         condition = selectionOUTCOME,
                                         key = "person_id",
                                         strata=c("person_id", "conceptset"),
                                         summarystat = list(
                                           list(c("exist"), "date", "at_least_one")
    
    
    
    load(paste0(dirtemp,'tempfile.RData') )
    
    tempOUTCOME <- tempfile
    componentsOUTCOME<- components 
    
    nameobjectOUTCOMEtype <- paste0('D3_events',"_",OUTCOME,suffix[[subpop]])
    foroutput <- tempOUTCOME
    assign(nameobjectOUTCOMEtype,foroutput)
    
    
    
    rm(nameconceptsetdatasetOUTCOMEtype)
    
    nameobjectOUTCOME <- paste0("D3_components","_",OUTCOME,suffix[[subpop]])
    componentsOUTCOMEfinal <- vector(mode = 'list')
    OUTCOME_narrow <- componentsOUTCOME
    
    temp2 <- merge(COHORT_TMP,OUTCOME_narrow, by="person_id",all.x  = T)
    for (i in names(temp2)) temp2[is.na(get(i)), (i):=0]
    componentsOUTCOMEfinal <- temp2
    
    assign(nameobjectOUTCOME, componentsOUTCOMEfinal)
    
    save(nameobjectOUTCOME,file=paste0(dircomponents,paste0(nameobjectOUTCOME,".RData")),list= nameobjectOUTCOME)
  }
}
