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
for(ifunc in func_names)
  source(paste0(func_dir,ifunc))

# ensure required folders are created 
dir.create(file.path("./g_intermediate/scri"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path("./g_output/scri"),       showWarnings = FALSE, recursive = TRUE)



# Import Data -------------------------------------------------------------
load(paste0(getwd(),"/g_intermediate/", raw_data))
scri_data_extract <- eval(parse(text = raw_data_name))



scri_data_extract <- scri_CPRD

names(scri_data_extract)[names(scri_data_extract)=="type_vax_1"] <- "type_vax1"
names(scri_data_extract)[names(scri_data_extract)=="type_vax_2"] <- "type_vax2"
names(scri_data_extract)[names(scri_data_extract)=="type_vax_3"] <- "type_vax3"
names(scri_data_extract)[names(scri_data_extract)=="type_vax_4"] <- "type_vax4"

scri_data_extract[1:2,]
summary(scri_data_extract)


# simulation for CPRD: 
if(F){
  # simulate binomial variables:
  covs <- c( "comorbidity", "pregnant", "myoc_elevated_risk" )
  p_covs <- c( 0.2, 0.1, 0.1 )
  for(icov in 1:length(covs))
    if( !any(names(scri_data_extract)==covs[icov]) )
      scri_data_extract[,covs[icov]] <- rbinom(nrow(scri_data_extract),1,p_covs[icov])
  #
  # simulate date variables:
  date_covs <- c( "covid_19_date", "otitis_externa_date", "myocarditis_date"  ) #  "pericarditis_date" is created also if "myocarditis_date" is changed
  p_date_covs <- c( 0.3, 0.1, 0.1, 0.4 )
  for(icov in 1:length(date_covs)){
    cond <- !any(names(scri_data_extract)==date_covs[icov])
    if( any(names(scri_data_extract)==date_covs[icov]) ) cond <- cond | mean(!is.na(scri_data_extract[, date_covs[icov]]))<0.01
    if( cond) {
      if(any(names(scri_data_extract)==date_covs[icov])){
        print(paste("variable '",date_covs[icov],"' is changes because there are not enough cases (<1%) for test!"))
        print(table(!is.na(scri_data_extract[, date_covs[icov]])))
      }
      tmp <- as.logical(rbinom(nrow(scri_data_extract),1,p_date_covs[icov]))
      scri_data_extract[ tmp, date_covs[icov]] <- sample(seq(as.Date('2020/12/01'), as.Date('2021/05/31'), by="day"), sum(tmp),  replace = TRUE)
      if(date_covs[icov]=="myocarditis_date") 
        scri_data_extract[ !tmp, "pericarditis_date"] <- sample(seq(as.Date('2020/12/01'), as.Date('2021/05/31'), by="day"), sum(!tmp),  replace = TRUE)
    } 
  }  
  
  print(summary(scri_data_extract))
}



# create variable 'myopericarditis_date'  
{
  print("Freq table for non-missing(T) values:")
  scri_data_extract$myopericarditis_date <- pmin(scri_data_extract$myocarditis_date,scri_data_extract$pericarditis_date,na.rm=T)
  print(table1( paste0(  "myoperi:", !is.na(scri_data_extract$myopericarditis_date),
                         " myo:",!is.na(scri_data_extract$myocarditis_date),
                         " peri:", !is.na(scri_data_extract$pericarditis_date) )
  ))
  #print(table( myo_perio=paste( !is.na(scri_data_extract$myocarditis_date),!is.na(scri_data_extract$pericarditis_date)), myoperi= !is.na(scri_data_extract$myopericarditis_date)))
}

# create 'myocarditis', 'pericarditis' and 'myopericarditis'  variables
scri_data_extract$myocarditis     <- as.numeric(!is.na(scri_data_extract$myocarditis_date ))
scri_data_extract$pericarditis    <- as.numeric(!is.na(scri_data_extract$pericarditis_date))
scri_data_extract$myopericarditis <- as.numeric(!is.na(scri_data_extract$myopericarditis_date))

# distribution of vaccine dates between vax1 and vax2. The minimum should not be small !!!!
scri_data_extract$dose_diff = as.numeric(difftime(scri_data_extract$date_vax2, scri_data_extract$date_vax1,units="days"))
summary(scri_data_extract$dose_diff)
if(any(min(scri_data_extract$dose_diff[!is.na(scri_data_extract$dose_dif)])<14)) 
  warning(paste("There are ", sum(scri_data_extract$dose_diff<14, na.rm=T), " persons with (date_vax2 - date_vax1) < 14 days."))


# check: missing values:
dim(scri_data_extract)
scri_data_extract <- scri_data_extract[!is.na(scri_data_extract$study_entry_date),]
dim(scri_data_extract)
scri_data_extract <- scri_data_extract[!is.na(scri_data_extract$study_exit_date),]
dim(scri_data_extract)
scri_data_extract <- scri_data_extract[!is.na(scri_data_extract$date_vax1),]
dim(scri_data_extract)
scri_data_extract <- scri_data_extract[!is.na(scri_data_extract$myopericarditis_date),]
dim(scri_data_extract)




# check: vaccine dates. If (date_vax2 - date_vax1) <= 5 ==> look for next vaccine and replace. 
# If there are also information about vax3 (and vax4)
if(F){
  print("summary( date_vax2 - date_vax1 ) before:")
  print(summary(scri_data_extract$dose_diff))
  while( sum( (cond <- !is.na(scri_data_extract$date_vax2) & scri_data_extract$dose_diff<=5), na.rm=T )>0 ){
    
    print("Number of day between vaccine 1 and vaccine 2 if less or equal to 5:")
    print(scri_data_extract$dose_diff[cond])
    
    # check: date_vax2 -date_vax1 <= 5 ==> change date_vax2 = date_vax3 if vax3 available:
    pats_dose_diff_le5 <- scri_data_extract$person_id[ cond ]
    scri_data_extract$dose_diff[scri_data_extract$person_id %in% pats_dose_diff_le5]
    
    # replace vax2 by vax3; vax3 by vaX4:
    #vaccine dates:
    scri_data_extract$date_vax2[scri_data_extract$person_id %in% pats_dose_diff_le5]  <- scri_data_extract$date_vax3[scri_data_extract$person_id %in% pats_dose_diff_le5]
    scri_data_extract$date_vax3[scri_data_extract$person_id %in% pats_dose_diff_le5]  <- scri_data_extract$date_vax4[scri_data_extract$person_id %in% pats_dose_diff_le5]
    scri_data_extract$date_vax4[scri_data_extract$person_id %in% pats_dose_diff_le5]  <- NA
    # vaccine type
    scri_data_extract$type_vax2[scri_data_extract$person_id %in% pats_dose_diff_le5] <- scri_data_extract$type_vax3[scri_data_extract$person_id %in% pats_dose_diff_le5]
    scri_data_extract$type_vax3[scri_data_extract$person_id %in% pats_dose_diff_le5] <- scri_data_extract$type_vax4[scri_data_extract$person_id %in% pats_dose_diff_le5]
    scri_data_extract$type_vax4[scri_data_extract$person_id %in% pats_dose_diff_le5] <- NA
    
    # recalculate dose_diff, vax2
    scri_data_extract$dose_diff <- as.numeric(difftime(scri_data_extract$date_vax2, scri_data_extract$date_vax1,units="days"))
    scri_data_extract$vax2      <- as.numeric(!is.na(scri_data_extract$date_vax2))
    
    print("AFTER CHANGE ('NA' means no second vaccine): Number of day between vaccine 1 and vaccine 2 if less or equal to 5:")
    print(scri_data_extract$dose_diff[scri_data_extract$person_id %in% pats_dose_diff_le5])
  }
  print("summary( date_vax2 - date_vax1 ) after:")
  print(summary(scri_data_extract$dose_diff))
}

save(scri_data_extract, file = "./g_intermediate/scri/scri_data_extract.RData" )




