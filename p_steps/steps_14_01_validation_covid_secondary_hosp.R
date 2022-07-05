for (subpop in subpopulations_non_empty) {

load(paste0(dirtemp, "/D3_covid_severity_components_hospitalisation",suffix[[subpop]],".RData"))
covid_severity_components <- as.data.table(get(paste0("D3_covid_severity_components_hospitalisation",suffix[[subpop]])))
rm(list = paste0("D3_covid_severity_components_hospitalisation",suffix[[subpop]]))

load(paste0(dirtemp, "/COVID_narrow",suffix[[subpop]],".RData"))
covid_narrow <- as.data.table(get(paste0("COVID_narrow",suffix[[subpop]])))
rm(list = paste0("COVID_narrow",suffix[[subpop]]))

covid_severity_components <- covid_severity_components[covid_narrow_hosp_m_hospitalisation_primary == 0 &
                                                         covid_narrow_hosp_m_hospitalisation_secondary == 1 &
                                                         hospitalisation_from_covid_registry == 0,]

ids_to_keep <- copy(covid_severity_components)[, person_id]

covid_narrow <- covid_narrow[person_id %in% ids_to_keep, ]

setorder(covid_narrow, person_id, date)
covid_narrow <- covid_narrow

EVENTS_SDO <- fread(paste0(dirinput, "/EVENTS_SDO.csv"))
EVENTS_PS <- fread(paste0(dirinput, "/EVENTS_PS.csv"))
SURVEY_ID_COVIDDATASET <- fread(paste0(dirinput, "/SURVEY_ID_COVIDDATASET.csv"))
SURVEY_OBSERVATIONS_COVIDDATASET <- fread(paste0(dirinput, "/SURVEY_OBSERVATIONS_COVIDDATASET.csv"))
PROCEDURES_SDO <- fread(paste0(dirinput, "/PROCEDURES_SDO.csv"))

test <- copy(covid_narrow)
test1 <- merge(test[str_detect(visit_occurrence_id, "SDO_"), .(person_id, visit_occurrence_id)],
               EVENTS_SDO, all.x = T, by = c("person_id", "visit_occurrence_id"))
test2 <- merge(test[str_detect(visit_occurrence_id, "PS_"), .(person_id, visit_occurrence_id)],
               EVENTS_PS, all.x = T, by = c("person_id", "visit_occurrence_id"))
test <- rbind(test1, test2)

test5 <- PROCEDURES_SDO[person_id %in% ids_to_keep, ]
test5 <- dcast(test5,  person_id + visit_occurrence_id~ rowid(person_id, visit_occurrence_id),
               value.var = c("procedure_date", "procedure_code", "procedure_code_vocabulary", "meaning_of_procedure",
                             "origin_of_procedure"))

test <- merge(test1, test5, all.x = T, by = c("person_id", "visit_occurrence_id"))

# test <- dcast(test, person_id ~ rowid(person_id),
#               value.var = c("visit_occurrence_id", "start_date_record", "end_date_record", "event_code", "event_record_vocabulary",
#                             "text_linked_to_event_code", "event_free_text", "present_on_admission", "laterality_of_event",
#                             "meaning_of_event", "origin_of_event"))





test3 <- merge(SURVEY_ID_COVIDDATASET[person_id %in% ids_to_keep, ],
               SURVEY_OBSERVATIONS_COVIDDATASET[person_id %in% ids_to_keep, ], all = T, by = c("person_id", "survey_id"))
# test3 <- dcast(test3, person_id ~ rowid(person_id),
#                value.var = c("survey_id", "survey_date", "survey_date", "survey_origin", "so_date",
#                              "so_source_table", "so_source_column", "so_unit", "so_source_value",
#                              "so_meaning", "so_meaning"))

test <- merge(test, test3, all.x = T, by = c("person_id"), allow.cartesian = T)

fwrite(test, paste0(dirtemp, "/COVID_secondary_hosp_no_registry.csv"))

}
