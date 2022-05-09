#-----------------------------------------------
# Aggregate persontime datasets by birth cohorts

# input: D4_persontime_risk_week, D4_persontime_benefit_week, D4_persontime_risk_year, D4_persontime_benefit_year
# output: D4_persontime_risk_week_BC, D4_persontime_benefit_week_BC, D4_persontime_risk_year_BC, D4_persontime_benefit_year_BC

for (subpop in subpopulations_non_empty) {  
  print(subpop)
  
  namedataset2<-paste0("D4_persontime_severity_week",suffix[[subpop]])
  load(paste0(diroutput,"D4_persontime_severity_week",suffix[[subpop]],".RData"))
  
  cols_to_sums <- names(get(namedataset2))[7:length(get(namedataset2))]
  
  assign(namedataset2, get(namedataset2)[, lapply(.SD, sum, na.rm=TRUE),
                                         by = c("sex", "Ageband", "Dose", "type_vax", "week"),
                                         .SDcols = cols_to_sums])
  
  all_sex <- copy(get(namedataset2))[, lapply(.SD, sum, na.rm=TRUE),
                                     by = c("Ageband", "Dose", "type_vax", "week"),
                                     .SDcols = cols_to_sums]
  all_sex <- all_sex[, sex := "both_sexes"]
  
  assign(namedataset2, rbind(get(namedataset2), all_sex))
  
  all_ages <- copy(get(namedataset2))[, lapply(.SD, sum, na.rm=TRUE),
                                      by = c("sex", "Dose", "type_vax", "week"),
                                      .SDcols = cols_to_sums]
  all_ages <- unique(all_ages[, Ageband := "all_birth_cohorts"])
  
  assign(namedataset2,rbind(get(namedataset2), all_ages))
  
  all_dose <- copy(get(namedataset2))[Dose %in% c(1, 2, 3), lapply(.SD, sum, na.rm=TRUE),
                                       by = c("Ageband", "sex", "type_vax", "week"),
                                       .SDcols = cols_to_sums]
  all_dose <- all_dose[, Dose := "both_doses"]
  
  assign(namedataset2,rbind(get(namedataset2), all_dose))
  
  all_man <- copy(get(namedataset2))[, lapply(.SD, sum, na.rm=TRUE),
                                      by = c("Ageband", "sex", "week", "Dose"),
                                      .SDcols = cols_to_sums]
  all_man <- all_man[, type_vax := "all_manufacturer"]
  
  nameoutput2<-paste0("D4_persontime_severity_week_BC",suffix[[subpop]])
  assign(nameoutput2,rbind(get(namedataset2), all_man))
  
  save(nameoutput2,file=paste0(diroutput,nameoutput2,".RData"),list=nameoutput2)
  rm(list=nameoutput2)
  rm(list=namedataset2)
  rm(namedataset2,nameoutput2)
  
  rm(all_ages, all_sex, cols_to_sums)
  
}
