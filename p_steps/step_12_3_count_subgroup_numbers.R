# Program Information  ----------------------------------------------------

# Program:      step_12_3_count subgroup numbers.R 
# Author:       Svetlana Belitser, Anna Schultze; Ema Alsina, Sophie Bots, Ivonne Martens 
# Description:  calculates the number of individuals per subgroup of interest for analyses
#               runs on g_intermediate/scri/scri_extract_data                
# Requirements: 
#               dependencies: none
#               input:  g_intermediate/scri/*  
#               output:  g_intermediate/scri/*  
#
#               parameters: in 07_scri_inputs.R 
#

####       print_during_running <- F    #  T or F


# Housekeeping  -----------------------------------------------------------

if(!any(ls()=="thisdir"))   thisdir   <- getwd()
if(!any(ls()=="dirtemp"))   dirtemp   <- paste0(thisdir,"/g_intermediate/")
if(!any(ls()=="direxp")) direxp <- paste0(thisdir,"/g_export/")

for (subpop in subpopulations_non_empty) {
thisdirexp <- ifelse(this_datasource_has_subpopulations == FALSE, direxp, direxpsubpop[[subpop]])

# ensure required folders are created  
dir.create(file.path(paste0(thisdirexp, "scri")),  showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(paste0(thisdir,"/log_files/scri")), showWarnings = FALSE, recursive = TRUE)

# SCCS output_directory  
sdr <- paste0(thisdirexp, "scri/scri/")
dir.create(sdr, showWarnings = FALSE, recursive = TRUE)


# Import Data -------------------------------------------------------------
load(paste0(dirtemp, "scri/", intermediate_data, suffix[[subpop]], ".RData"))
temp_name<-get(paste0(intermediate_data, suffix[[subpop]]))
rm(list=paste0(intermediate_data, suffix[[subpop]]))
assign(intermediate_data, temp_name)
rm(temp_name)

scri_data_extract <- eval(parse(text = intermediate_data))


# count the things and make the table 

info_dosediff <- summary(scri_data_extract$dose_diff, na.rm=T)
median_dosediff <- paste(info_dosediff[2], info_dosediff[3], info_dosediff[5], sep = "_")
under_28days <- plyr::count(scri_data_extract$dose_diff < 28)[2,2]
covid19 <- plyr::count(!is.na(scri_data_extract$covid_19_date))[2,2]
comorb <- plyr::count(scri_data_extract$comorbidity == 1)[2,2]
immuno <- plyr::count(scri_data_extract$immunosuppressants_at_study_entry == 1)[2,2]

output <- data.frame(
  subgroup = c("Median time between doses", "n_under28days", "n_priorcovid", "n_comorbidity", "n_immunocompromised"),
  number = c(median_dosediff, under_28days, covid19, comorb, immuno
  )
)

save(output,
     file = paste0(sdr, "subgroup_numbers_table.RData"))
}
