## Reference population
pop.eustat<- read.csv(paste0(dirmacro,"/ESP_ageband.csv"),sep = "")

for (subpop in subpopulations_non_empty) {  
  print(subpop)
  
  #D4_persontime_risk_year-----------------------------------------------------
  
  load(paste0(diroutput,"D4_persontime_background_aggregated",suffix[[subpop]],".RData"))
  persontime_windows<-get(paste0("D4_persontime_background_aggregated", suffix[[subpop]]))
  rm(list=paste0("D4_persontime_background_aggregated", suffix[[subpop]]))
  persontime_windows <- persontime_windows[Ageband != "total" & sex == 'total' & year == "2019/2020",]
  colstokeep <- c('COVID19')
  
  for (ev in c(OUTCOME_variables, CONTROL_variables)) {
    name_count <- paste0(ev,"_b")
    name_pt <- paste0("Persontime_",ev)
    my_results_CVM <- dsr(data = persontime_windows,
                          event = get(name_count),
                          fu = get(name_pt),
                          subgroup = COVID19,
                          Ageband,
                          refdata = pop.eustat,
                          method = "gamma",
                          sig = 0.95,
                          mp = 36525000, # 100,000 * 365.25
                          decimals = 2)
    # Subgroup <- COVID19
    # paste0 ("IR_std_",ev) <- "Std Rate (per 36525000)"   
    # paste0 ("lb_std_",ev) <- "95% LCL (Std)" 
    # paste0 ("ub_std_",ev) "95% UCL (Std)"
    my_results_CVM <- my_results_CVM[,c(1,7,8,9) ]
    colnames(my_results_CVM) <- c('COVID19',paste0(c("IR_std_", "lb_std_", "ub_std_"), ev))
    persontime_windows <- merge(persontime_windows,my_results_CVM,by = 'COVID19')
    colstokeep <- c(colstokeep,paste0(c("IR_std_", "lb_std_", "ub_std_"), ev))
  }
  persontime_windows <- persontime_windows[,..colstokeep]
  persontime_windows <- unique(persontime_windows)
  
  nameoutput <- paste0("D5_IR_background_std")
  assign(nameoutput, persontime_windows)
  save(nameoutput, file = paste0(dirD4D5subpop[[subpop]], nameoutput, ".RData"), list = nameoutput)
  
  fwrite(get(nameoutput), file = paste0(dirD4D5subpop[[subpop]], nameoutput, ".csv"))
}

# data <- D4_persontime_background_aggregated_HOSP[Ageband != "total" & sex == 'total' & year == "2019/2020",]
# 
# my_results_CVM <- dsr(data = data,
#                       event = B_COAGDIS_AESI_b,
#                       fu = Persontime_B_COAGDIS_AESI,
#                       subgroup = COVID19,
#                       Ageband,
#                       refdata = pop.eustat,
#                       method = "gamma",
#                       sig = 0.95,
#                       mp = 36525000, # 100,000 * 365.25
#                       decimals = 2)