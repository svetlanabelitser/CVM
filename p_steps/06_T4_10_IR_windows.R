for (subpop in subpopulations_non_empty) {  
  print(subpop)
  
  #D4_persontime_risk_year-----------------------------------------------------
  
  load(paste0(diroutput,"D4_persontime_windows_aggregated",suffix[[subpop]],".RData"))
  persontime_windows<-get(paste0("D4_persontime_windows_aggregated", suffix[[subpop]]))
  rm(list=paste0("D4_persontime_windows_aggregated", suffix[[subpop]]))
  
  for (ev in c(OUTCOME_variables, CONTROL_variables)) {
    name_cols <- paste0(c("IR_", "lb_", "ub_"), ev)
    name_count <- paste0(ev,"_b")
    name_pt <- paste0("Persontime_",ev)
    persontime_windows[, (name_cols) := exactPoiCI(persontime_windows, name_count, name_pt)]
  }
  
  nameoutput <- paste0("RES_IR_persontime_windows")
  assign(nameoutput, persontime_windows)
  save(nameoutput, file = paste0(direxpsubpop[[subpop]], nameoutput, ".RData"), list = nameoutput)
  
  nameoutput <- paste0("RES_IR_windows")
  assign(nameoutput, persontime_windows[, !grep("^Person", names(persontime_windows)), with = FALSE])
  save(nameoutput, file = paste0(direxpsubpop[[subpop]], nameoutput, ".RData"), list = nameoutput)
  
  fwrite(get(nameoutput), file = paste0(direxpsubpop[[subpop]], nameoutput, ".csv"))
}
