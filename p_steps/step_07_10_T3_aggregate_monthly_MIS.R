#-----------------------------------------------
# Aggregate persontime datasets for MIS/Myocard

# input: D4_persontime_monthly_b, D4_persontime_monthly_c, D4_persontime_monthly_d
# output: D4_persontime_monthly_b_BC, D4_persontime_monthly_c_BC, D4_persontime_monthly_d_BC

for (subpop in subpopulations_non_empty) {  
  print(subpop)
  
  namedataset1<-paste0("D4_persontime_b",suffix[[subpop]])
  load(paste0(diroutput,"D4_persontime_b",suffix[[subpop]],".RData"))
  
cols_to_sums = names(get(namedataset1))[4:length(get(namedataset1))]
setnames(get(namedataset1), "ageband_at_study_entry", "Ageband")

all_sex <- copy(get(namedataset1))[, lapply(.SD, sum), by = c("Ageband", "year"),
                                          .SDcols = cols_to_sums]
all_sex <- all_sex[, sex := "both_sexes"]
assign(namedataset1, rbind(get(namedataset1), all_sex))

all_year <- copy(get(namedataset1))[, lapply(.SD, sum), by = c("sex", "Ageband"),
                                          .SDcols = cols_to_sums]
all_year <- all_year[, year := "all_years"]
assign(namedataset1, rbind(get(namedataset1), all_year))

all_ages <- copy(get(namedataset1))[, lapply(.SD, sum), by = c("sex", "year"),
                                           .SDcols = cols_to_sums]
all_ages <- unique(all_ages[, Ageband := "all_birth_cohorts"])

nameoutput1<-paste0("D4_persontime_b_BC",suffix[[subpop]])
assign(nameoutput1,rbind(get(namedataset1), all_ages))

save(nameoutput1,file=paste0(diroutput,nameoutput1,".RData"),list=nameoutput1)
rm(list=nameoutput1)
rm(list=namedataset1)
rm(namedataset1,nameoutput1)



namedataset2<-paste0("D4_persontime_c",suffix[[subpop]])
load(paste0(diroutput,"D4_persontime_c",suffix[[subpop]],".RData"))

cols_to_sums = names(get(namedataset2))[4:length(get(namedataset2))]
setnames(get(namedataset2), "ageband_at_covid", "Ageband")

all_sex <- copy(get(namedataset2))[, lapply(.SD, sum), by = c("Ageband", "year"),
                                         .SDcols = cols_to_sums]
all_sex <- all_sex[, sex := "both_sexes"]
assign(namedataset2,rbind(get(namedataset2), all_sex))

all_year <- copy(get(namedataset2))[, lapply(.SD, sum), by = c("sex", "Ageband"),
                                          .SDcols = cols_to_sums]
all_year <- all_year[, year := "all_years"]
assign(namedataset2,rbind(get(namedataset2), all_year))

all_ages <- copy(get(namedataset2))[, lapply(.SD, sum), by = c("sex", "year"),
                                          .SDcols = cols_to_sums]
all_ages <- unique(all_ages[, Ageband := "all_birth_cohorts"])

nameoutput2<-paste0("D4_persontime_c_BC",suffix[[subpop]])
assign(nameoutput2,rbind(get(namedataset2), all_ages))

save(nameoutput2,file=paste0(diroutput,nameoutput2,".RData"),list=nameoutput2)
rm(list=nameoutput2)
rm(list=namedataset2)
rm(namedataset2,nameoutput2)




namedataset3<-paste0("D4_persontime_d",suffix[[subpop]])
load(paste0(diroutput,"D4_persontime_d",suffix[[subpop]],".RData"))

cols_to_sums = names(get(namedataset3))[7:length(get(namedataset3))]
setnames(get(namedataset3), "ageband_at_date_vax_1", "Ageband")

all_sex <- copy(get(namedataset3))[, lapply(.SD, sum), by = c("Ageband", "year", "type_vax", "Dose", "history_covid"),
                                         .SDcols = cols_to_sums]
all_sex <- all_sex[, sex := "both_sexes"]
assign(namedataset3,rbind(get(namedataset3), all_sex))

all_year <- copy(get(namedataset3))[, lapply(.SD, sum), by = c("sex", "Ageband", "type_vax", "Dose", "history_covid"),
                                          .SDcols = cols_to_sums]
all_year <- all_year[, year := "all_years"]
assign(namedataset3,rbind(get(namedataset3), all_year))

all_ages <- copy(get(namedataset3))[, lapply(.SD, sum), by = c("sex", "year", "type_vax", "Dose", "history_covid"),
                                          .SDcols = cols_to_sums]
all_ages <- unique(all_ages[, Ageband := "all_birth_cohorts"])

nameoutput3<-paste0("D4_persontime_d_BC",suffix[[subpop]])
assign(nameoutput3,rbind(get(namedataset3), all_ages))

save(nameoutput3,file=paste0(diroutput,nameoutput3,".RData"),list=nameoutput3)
rm(list=nameoutput3)
rm(list=namedataset3)
rm(namedataset3,nameoutput3)
rm(all_sex,all_ages,all_year)


namedataset4<-paste0("D4_persontime_d_long",suffix[[subpop]])
load(paste0(diroutput,"D4_persontime_d_long",suffix[[subpop]],".RData"))

cols_to_sums = names(get(namedataset4))[7:length(get(namedataset4))]
setnames(get(namedataset4), "ageband_at_date_vax_1", "Ageband")

D4_persontime_poisson$year = as.integer(lapply(strsplit(D4_persontime_poisson$month, split = "-"), "[", 1))
D4_persontime_poisson$month = as.integer(lapply(strsplit(D4_persontime_poisson$month, split = "-"), "[", 2))

all_sex <- copy(get(namedataset4))[, lapply(.SD, sum), by = c("Ageband", "year", "type_vax", "Dose", "Period"),
                                   .SDcols = cols_to_sums]
all_sex <- all_sex[, sex := "both_sexes"]
assign(namedataset4,rbind(get(namedataset4), all_sex))

all_year <- copy(get(namedataset4))[, lapply(.SD, sum), by = c("sex", "Ageband", "type_vax", "Dose", "Period"),
                                    .SDcols = cols_to_sums]
all_year <- all_year[, year := "all_years"]
assign(namedataset4,rbind(get(namedataset4), all_year))

all_ages <- copy(get(namedataset4))[, lapply(.SD, sum), by = c("sex", "year", "type_vax", "Dose", "Period"),
                                    .SDcols = cols_to_sums]
all_ages <- unique(all_ages[, Ageband := "all_birth_cohorts"])

nameoutput4<-paste0("D4_persontime_d_long_BC",suffix[[subpop]])
assign(nameoutput4,rbind(get(namedataset4), all_ages))

save(nameoutput4,file=paste0(diroutput,nameoutput4,".RData"),list=nameoutput4)
rm(list=nameoutput4)
rm(list=namedataset4)
rm(namedataset4,nameoutput4)
rm(all_sex,all_ages,all_year)
}
