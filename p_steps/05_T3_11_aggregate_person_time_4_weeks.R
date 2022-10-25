
for (subpop in subpopulations_non_empty) {  
  print(subpop)
  
  load(paste0(diroutput, "D4_persontime_risk_4_weeks", suffix[[subpop]], ".RData"))
  persontime_windows <- get(paste0("D4_persontime_risk_4_weeks", suffix[[subpop]]))
  rm(list = paste0("D4_persontime_risk_4_weeks", suffix[[subpop]]))
  
  cols_to_sums <- names(persontime_windows)[grepl("^Persontime|_b$", names(persontime_windows))]
  
  persontime_windows <- persontime_windows[, lapply(.SD, sum, na.rm=TRUE),
                                           by = c("sex", "dose", "type_vax", "period", "Ageband"),
                                           .SDcols = cols_to_sums]
  
  nameoutput <- paste0("D4_persontime_windows_aggregated", suffix[[subpop]])
  assign(nameoutput, persontime_windows)
  save(nameoutput, file = paste0(diroutput, nameoutput, ".RData"), list = nameoutput)
}
