# Program Information  ----------------------------------------------------

# Program:      step_12_1_create_scri_dataset.R 
# Author:       Anna Schultze, Svetlana Belitser; Ema Alsina, Sophie Bots, Ivonne Martens 
# Description:  calls a function which creates SCRI dataset in wide format 
#               takes user provided brands and outcomes
#               outputs datasets provided N is >5. 
#               if N is < 5 no output data is provided and a warning printed to console 
# Requirements: 
#               dependencies: 07_scri_inputs.R
#               input:  D3_study_variables_for_SCRI (g_intermediate) in RData format 
#               parameters: in 07_scri_inputs.R
#               output: g_output/scri/scri_input_[brand]_[outcome]_flowchart.txt
#                       g_intermediate/scri/scri_input_[brand]_[outcome]_input.csv               

# Housekeeping  -----------------------------------------------------------
# load functions:
#for(ifunc in func_names)
#  source(paste0(func_dir,ifunc))

# ensure required folders are created 
dir.create(file.path("./g_intermediate/scri"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path("./g_output/scri"),       showWarnings = FALSE, recursive = TRUE)

 

# Import Data -------------------------------------------------------------
load(paste0(getwd(),"/g_intermediate/", raw_data))
scri_data_extract <- eval(parse(text = raw_data_name))


names(scri_data_extract)[names(scri_data_extract)=="type_vax_1"] <- "type_vax1"
names(scri_data_extract)[names(scri_data_extract)=="type_vax_2"] <- "type_vax2"


# create variable 'myopericarditis_date'  
print("Freq table for non-missing(T) values:")
scri_data_extract$myopericarditis_date <- pmin(scri_data_extract$myocarditis_date,scri_data_extract$pericarditis_date,na.rm=T)
print(table1( paste0(  "myoperi:", !is.na(scri_data_extract$myopericarditis_date),
                       " myo:",!is.na(scri_data_extract$myocarditis_date),
                       " peri:", !is.na(scri_data_extract$pericarditis_date) )
))
#print(table( myo_perio=paste( !is.na(scri_data_extract$myocarditis_date),!is.na(scri_data_extract$pericarditis_date)), myoperi= !is.na(scri_data_extract$myopericarditis_date)))

# create 'myocarditis', 'pericarditis' and 'myopericarditis'  variables
scri_data_extract$myocarditis     <- as.numeric(!is.na(scri_data_extract$myocarditis_date ))
scri_data_extract$pericarditis    <- as.numeric(!is.na(scri_data_extract$pericarditis_date))
scri_data_extract$myopericarditis <- as.numeric(!is.na(scri_data_extract$myopericarditis_date))

# distribution of vaccine dates between vax1 and vax2. The minimum should not be small !!!!
scri_data_extract$dose_diff = as.numeric(difftime(scri_data_extract$date_vax2, scri_data_extract$date_vax1,units="days"))
summary(scri_data_extract$dose_diff)
if(any(min(scri_data_extract$dose_diff[!is.na(scri_data_extract$dose_dif)])<14)) 
  warning(paste("There are ", sum(scri_data_extract$dose_diff<14, na.rm=T), " persons with (date_vax2 - date_vax1) < 14 days."))


# change time scale to days since 01-09-2020              
scri_data_extract$start_study_date <- as.Date("2020-09-01")
scri_data_extract$start_study_date <- scri_data_extract$study_entry_date
# age at start_study (01-09-2020)
scri_data_extract$age_start_study <- scri_data_extract$age_at_study_entry
#scri_data_extract$age_start_study <- as.numeric(difftime(scri_data_extract$start_study_date,scri_data_extract$birth_date,units="days")) / 365.25 
summary(scri_data_extract$age_start_study)

#if( any(min(scri_data_extract$age_start_study,na.rm=T) < as.numeric(difftime(scri_data_extract$start_study_date,Sys.Date(),units="days")) / 365.25) ) 
#  warning("check the range of variable 'age_start_study'")

############
#
# flowchart:
#
dap <- ifelse( any(names(scri_data_extract)=="DAP"), scri_data_extract$DAP[1], "")
sink(paste0('./g_output/scri/flowchart_',dap,'.txt'),append = F)
  cat("\n\n\n")
  dim(scri_data_extract)
  # non-missing vax1 dates
  na_date_vax1 <- c("no_vax1","vax1")[ 1 + as.numeric(!is.na(scri_data_extract$date_vax1)) ]
  print(table1(na_date_vax1))
  cat("___________________________\n\n\n")
  # .. and brand
  flowchart <- cbind.data.frame(na_date_vax1=na_date_vax1, brand=scri_data_extract$type_vax1) 
  print(table( flowchart$brand))
  print(table1( flowchart ))
  cat('___________________________\n\n\n')
  # .. and sex
  flowchart <- cbind.data.frame(flowchart, sex=scri_data_extract$sex) 
  print(table1(flowchart$sex))
  print(table1(flowchart))
  print(tapply(flowchart$sex, paste0(flowchart$na_date_vax1," & ",flowchart$brand),  table1) )
  cat("___________________________\n\n\n")
  # .. and age at start_study (01-09-2020)
  age_cat_fc <- age_cat
  if( min(scri_data_extract$age_start_study,na.rm=T) < age_cat[1] ) age_cat_fc <- c(min(scri_data_extract$age_start_study,na.rm=T), age_cat_fc)
  if( max(scri_data_extract$age_start_study,na.rm=T) > age_cat[length(age_cat)] ) age_cat_fc <- c(age_cat_fc, max(scri_data_extract$age_start_study,na.rm=T) )
  flowchart <- cbind.data.frame(flowchart, age_start_fu=cut(scri_data_extract$age_start_study, age_cat_fc) ) 
  print(table1(flowchart))
  print(tapply(paste(flowchart$sex," & ",flowchart$age_start_fu), paste0(flowchart$na_date_vax1," & ",flowchart$brand),  table1) )
  cat("___________________________\n\n\n")
  # obs.start after 1-09-2020:
  obs_before_fu_or_baby <- rep("", nrow(scri_data_extract))
  obs_before_fu_or_baby[ is.na(scri_data_extract$study_entry_date)  ] <- "start_obs is missing"
  obs_before_fu_or_baby[ scri_data_extract$study_entry_date <  scri_data_extract$start_study_date ] <- "start_obs before fu_start"
  obs_before_fu_or_baby[ scri_data_extract$study_entry_date >= scri_data_extract$start_study_date ] <- "start_obs after fu_start (no_vax1)"
  obs_before_fu_or_baby[ scri_data_extract$study_entry_date >= scri_data_extract$start_study_date & 
                           scri_data_extract$study_entry_date < scri_data_extract$date_vax1-90 ] <- "start_obs in [start_fu;vax1-90d)"
  obs_before_fu_or_baby[ !is.na(scri_data_extract$age_start_study) & scri_data_extract$age_start_study<=0 ] <- "born after fu_start"
                           
  flowchart <- cbind.data.frame(flowchart, obs_before_fu_or_baby = obs_before_fu_or_baby)
                                
  print(table1(flowchart$obs_before_fu_or_baby))
  print(table1(flowchart))
  print(tapply(paste(flowchart$obs_before_fu_or_baby," & ",flowchart$age_start_fu), paste0(flowchart$na_date_vax1," & ",flowchart$brand, " & ",flowchart$sex),  table1) )
  print(table1(!is.na(scri_data_extract$study_entry_date)))
  print(summary(scri_data_extract$study_entry_date))
  print(summary(as.numeric(difftime(scri_data_extract$start_study_date, scri_data_extract$study_entry_date, units="days"))))
  cat("___________________________\n\n\n")
  # myoperi dates in fu period:
  myoperi_before_fu <- rep("", nrow(scri_data_extract))
  myoperi_before_fu[ is.na(scri_data_extract$myopericarditis_date)  ] <- "myoperi is missing"
  myoperi_before_fu[ scri_data_extract$myopericarditis_date < scri_data_extract$start_study_date ] <- "myoperi   before fu"
  myoperi_before_fu[ scri_data_extract$myopericarditis_date >= scri_data_extract$start_study_date ] <- "myoperi after start_fu (no_vax1)"
  myoperi_before_fu[ scri_data_extract$myopericarditis_date >= scri_data_extract$date_vax1-90 ] <- "myoperi after (vax1-90d)"
  myoperi_before_fu[ scri_data_extract$myopericarditis_date >= scri_data_extract$start_study_date & 
                       scri_data_extract$myopericarditis_date < scri_data_extract$date_vax1-90 ] <- "myoperi  in [start_fu;vax1-90d)"
  #myoperi_before_fu[ scri_data_extract$myopericarditis_date > scri_data_extract$date_vax1-90 ] <- "myoperi in [start_fu;vax1-90d]"
  flowchart <- cbind.data.frame(flowchart, myoperi_before_fu = myoperi_before_fu)
  
  print(table1(flowchart$myoperi_before_fu))
  print(table1(flowchart[,c("obs_before_fu_or_baby","myoperi_before_fu")]))
  print(table1(flowchart))
  print(tapply(paste(flowchart$myoperi_before_fu," & ",flowchart$age_start_fu), paste0(flowchart$na_date_vax1," & ",flowchart$brand, " & ",flowchart$sex),  table1) )
  print(table1(!is.na(scri_data_extract$study_entry_date)))
  print(summary(scri_data_extract$study_entry_date))
  print(summary(as.numeric(difftime(scri_data_extract$start_study_date, scri_data_extract$study_entry_date, units="days"))))
  cat("___________________________\n\n\n")


  # condition
  cond.scri <- with( scri_data_extract,
                         !is.na(date_vax1) & 
                         !is.na(type_vax1) &   
                         !is.na(sex) &   
                         !is.na(age_start_study) &   
                         #obs_before_fu_or_baby %in% c( "start_obs before fu_start",  "born after fu_start" ) &
                         #myoperi_before_fu == "myoperi after (vax1-90d)"   &
                         !is.na(myopericarditis_date) & 
                         myopericarditis_date >= date_vax1-90 
  )
  
  flowchart <- cbind.data.frame( cond.scri=c("no_use","use")[1+as.numeric(cond.scri)], flowchart )   
  
  cat("\n\n\nFull Flowchart:\n\n")
  print(table1( flowchart$cond.scri ))
  print(tapply( flowchart$cond.scri, flowchart$brand, table1 ))
  print(table1( flowchart ))
  cat("___________________________\n\n\n")
  
  cat("\n\n\nFull Flowchart only for vaccinated:\n\n")
  print(table1( flowchart$cond.scri[flowchart$na_date_vax1!="no_vax1"] ))
  print(with(flowchart[flowchart$na_date_vax1!="no_vax1",], tapply( cond.scri, brand, table1) ))
  print(table1( flowchart[flowchart$na_date_vax1!="no_vax1",] ))
  cat("___________________________\n\n\n")
  
  
  print(table1(flowchart[,c("obs_before_fu_or_baby","myoperi_before_fu")]))
  cat("___________________________\n\n\n")
  
  
  #selection:
  print("Selection:")
  nrow0<-nrow(scri_data_extract)
  scri_data_extract <- scri_data_extract[cond.scri, ]
  print(cbind.data.frame( all_rows=nrow0,selected_rows=nrow(scri_data_extract), percent_selected= 100*nrow(scri_data_extract)/nrow0 ), digits=2)
  
sink()
save(flowchart, file = paste0('./g_output/scri/flowchart_',dap,'.RData') )

save(scri_data_extract, file = "./g_intermediate/scri/scri_data_extract.RData" )






