library(magrittr)

for (subpop in subpopulations_non_empty) {
  print(subpop)
  
  dataset_name <- paste0("D3_study_population", suffix[[subpop]])
  load(paste0(dirtemp, dataset_name, ".RData"))
  study_population <- get(dataset_name)
  rm(list=dataset_name)
  
  dataset_name <- paste0("D3_outcomes_covid", suffix[[subpop]])
  load(paste0(dirtemp, dataset_name, ".RData"))
  outcomes_covid <- get(dataset_name)
  rm(list = dataset_name)
  
  outcomes_covid <- outcomes_covid[, .(person_id, date_event)]
  outcomes_covid <- outcomes_covid[outcomes_covid[,.I[which.min(date_event)], by = c("person_id")]$V1]
  
  n_doses <- 2
  colnames(study_population)
  study_population %<>%
    select(person_id, sex, study_entry_date, ageband_at_study_entry, type_vax_1, type_vax_2,
           study_entry_date_vax1, study_exit_date_vax1, study_entry_date_vax2, study_exit_date_vax2)
  
  study_population <- study_population[, study_exit_date := study_entry_date_vax1 - 1]
  colA = c(paste("study_entry_date_vax", 1:n_doses, sep = ""), "study_entry_date")
  colB = c(paste("study_exit_date_vax", 1:n_doses, sep = ""), "study_exit_date")
  
  colD = paste("type_vax", 1:n_doses, sep = "_")
  temp <- data.table::melt(study_population, measure = list(colA, colB, colD),
                           variable.name = "Dose", variable.factor = F,
                           value.name = c("study_entry_date", "study_exit_date", "type_vax"),
                           na.rm = F)
  
  temp <- temp[Dose == as.character(n_doses + 1), Dose := "0"]
  temp <- merge(temp, outcomes_covid, all.x = T, by = "person_id")
  
  end_2020 <- ymd(20201231)
  temp[3, study_exit_date := ymd(20200531)]
  temp_0 <- temp[Dose == "0", study_exit_date := pmin(study_exit_date, end_2020, na.rm = T)]
  
}