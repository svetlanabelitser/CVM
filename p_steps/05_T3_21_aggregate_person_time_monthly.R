#-----------------------------------------------
# Aggregate persontime datasets by month

# input: D4_persontime_risk_month
# output: D4_persontime_risk_month_RFBC

for (subpop in subpopulations_non_empty) {  
  print(subpop)
  
  load(paste0(diroutput, "D4_persontime_risk_month", suffix[[subpop]], ".RData"))
  PT_monthly <- get(paste0("D4_persontime_risk_month", suffix[[subpop]]))
  rm(list = paste0("D4_persontime_risk_month", suffix[[subpop]]))
  
  # Find the columns for counts and PT
  cols_to_sums <- names(PT_monthly)[grepl("^Persontime|_b$", names(PT_monthly))]
  
  # Sums by sex to get data for both sexes together
  PT_monthly_sex <- PT_monthly[, lapply(.SD, sum, na.rm=TRUE),
                               by = c("dose", "type_vax", "month", "Ageband", "COVID19"),
                               .SDcols = cols_to_sums][, sex := "total"]
  
  PT_monthly <- rbindlist(list(PT_monthly, PT_monthly_sex), use.names = T)
  
  # Sums by Ageband to get data for all ages
  PT_monthly_age <- PT_monthly[, lapply(.SD, sum, na.rm=TRUE),
                               by = c("sex", "dose", "type_vax", "month", "COVID19"),
                               .SDcols = cols_to_sums][, Ageband := "total"]
  
  PT_monthly <- rbindlist(list(PT_monthly, PT_monthly_age), use.names = T)
  
  PT_monthly[, c("year", "month") := tstrsplit(month, "-")]
  PT_monthly[, month := month.name[as.integer(month)]]
  
  # Sums by year 2019/2020 for table 6
  PT_monthly_year <- PT_monthly[year %in% c("2019", "2020"), ][, lapply(.SD, sum, na.rm=TRUE),
                                                               by = c("sex", "dose", "type_vax", "Ageband", "COVID19"),
                                                               .SDcols = cols_to_sums][, year := "2019/2020"][, month := "total"]
  
  PT_monthly <- rbindlist(list(PT_monthly, PT_monthly_year), use.names = T)
  
  nameoutput <- paste0("D4_persontime_risk_month_aggregated", suffix[[subpop]])
  assign(nameoutput, PT_monthly)
  save(nameoutput, file = paste0(diroutput, nameoutput, ".RData"), list = nameoutput)
}



