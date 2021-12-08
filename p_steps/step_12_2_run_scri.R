# Program Information  ----------------------------------------------------

# Program:      step_12_2_run_scri.R 
# Author:       Svetlana Belitser, Anna Schultze; Ema Alsina, Sophie Bots, Ivonne Martens 
# Description:  calls a function which runs an SCRI on specified datasets 
#               runs on all datasets in g_intermediate/scri                  
# Requirements: 
#               dependencies: preceding steps, package "survival" 
#               input:  g_intermediate/scri/*  
#               output:  g_intermediate/scri/*  
#
#               parameters: in 07_scri_inputs.R 
#  
#               function: p_macro/scri.R                     
#               function: p_macro/formatdata2.R                     
#               function: p_macro/table1.R                     
#
#  

####       print_during_running <- F    #  T or F


# Housekeeping  -----------------------------------------------------------
# install and load packages
#for(ifunc in c("scri.R","formatdata2.R","tabl1.R") ) source(paste0(func_dir,ifunc))

if(!any(ls()=="thisdir"))   thisdir   <- getwd()
if(!any(ls()=="dirtemp"))   dirtemp   <- paste0(thisdir,"/g_intermediate/")
if(!any(ls()=="diroutput")) diroutput <- paste0(thisdir,"/g_output/")

# ensure required folders are created  
dir.create(file.path(paste0(dirtemp, "scri")), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(paste0(diroutput, "scri")),  showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(paste0(thisdir,"/log_files/scri")), showWarnings = FALSE, recursive = TRUE)

# SCCS output_directory  
sdr <- paste0(diroutput, "scri/scri/")
dir.create(sdr, showWarnings = FALSE, recursive = TRUE)


# Import Data -------------------------------------------------------------
load(paste0(dirtemp, "scri/", intermediate_data, ".RData"))
scri_data_extract <- eval(parse(text = intermediate_data))


scri_input <- scri_data_extract


####################################
#  function 'scri_create_input'
scri_create_input <- function(
        data = scri_input0 ,                     
        prevax   = c( -90, -30 ), 
        buffer   = c( -29,  -1 ), 
        rw_vax1  = c(   0,  28 ),
        rw_vax2  = c(   0,  28 ),
        delete_buffer           = F,
        delete_between_rw1_rw2  = F
){
  #####  define lengths of the risk windows  #####
  #
  # define length of pre-vaccin. periods:
  prevax_per_start  <- prevax[1]   # -90   # let op negative sign!!!
  prevax_per_end    <- prevax[2]   # -30   # let op negative sign!!!
  
  buffer_per_start  <- buffer[1]   # -29   # let op negative sign!!!
  buffer_per_end    <- buffer[2]   # -1    # let op negative sign!!!
  #
  # define risk period of vax 1:
  vax1_start <- rw_vax1[1]  # 0  
  vax1_end   <- rw_vax1[2]  # 28 
  #
  # define risk period of vax 2:
  vax2_start <- rw_vax2[1]  # 0  
  vax2_end   <- rw_vax2[2]  # 28 
  
  # define extra sub-risk windows:
  #expogrp <- c(0,8,14)
  #expogrp_with_day0 <- c(0,1,8,14)
  
  #####  create variables with days from 'start_study_date'
  # calculate #days from 'start_study_date'  for the following variables:
  if(!any(names(data)=="study_entry_days")){
    date_vars <- c("study_entry_date", "study_exit_date", "date_vax1", "date_vax2",#"date_vax3", 
                   "myocarditis_date", "pericarditis_date", "myopericarditis_date","death_date" )  #, "birth_date")
    if(any( !(date_vars %in% names(data)) )) 
       warning( paste0("Variable[s] ", paste0(date_vars[!(date_vars %in% names(data))],collapse=",")," not found in the dataset."))
    date_vars <- date_vars[date_vars %in% names(scri_data_extract)]   # founded column names
    date_vars_days <- gsub("date","days",date_vars )                  # new var-names with 'days' instead of 'date'
    for(icol in 1:length(date_vars))
      scri_data_extract[,date_vars_days[icol]] <- round(as.numeric( difftime( scri_data_extract[,date_vars[icol]], scri_data_extract$start_study_date, units="days") ))  
  }
  ######################################################
  #  check the 'study_entry_days' and 'study_exit_days'
  #
  if(any( data$study_entry_days > data$days_vax1 & !is.na(data$days_vax1) & !is.na(data$study_entry_days), na.rm=T )){
    warning(paste("'study_entry_days' after the vax1 for ", sum( data$start > data$days_vax1, na.rm=T), "rows. They are deleted!"))
    nrow0<-nrow(data)  
    data <- data[ data$study_entry_days <= data$days_vax1 & data$vax1==1 & !is.na(data$study_entry_days),]  # if study_entry_days > vax1 ==>  delete rows
    print(c(new_nrow=nrow(data), old_nrow=nrow0)) 
  }

  if(any( data$study_exit_days < data$days_vax1, na.rm=T )){
    warning(paste0("There are ",sum( data$study_exit_days < data$days_vax1,na.rm=T)," persons with 'study_exit_time' before vax1. They are deleted!"))
    nrow0<-nrow(data)  
    data <- data[ data$days_vax1<=data$study_exit_days & data$vax1==1 & !is.na(data$study_entry_days),]  # if study_exit_days < vax1 ==>  delete rows
    print(c(new_nrow=nrow(data), old_nrow=nrow0)) 
  }  
  
  if(any( (cond <- !is.na(data$days_vax2) & data$study_exit_days < data$days_vax2 & !is.na(data$dstudy_exit_days) ) )){
    warning(paste0("There are ",sum( cond, na.rm=T)," persons with 'study_exit_time' before vax2 ==> study_exit_days <- days_vax2 !"))
    data$study_exit_days[cond]  <- data$days_vax2[cond] 
  } 

  if( sum( cond <- !is.na(data$days_vax2) & (data$days_vax2 - data$days_vax1) <= 5) > 0 ){
    print(paste(sum(cond),"'dose2' were deleted because dose2 is less than 6 days after dose1."))
    data$date_vax2[cond] <- NA
    data$days_vax2[cond] <- NA
    data$type_vax2[cond] <- NA
    data$vax2[cond] <- 0
  }  
  

  ######################################################################
  # define start and end of observation
  #   start,end   - 90 days before vax1, 28 (or 180) days after vax2 (or vax1 if no vax2) or study_exit_days
  data$start   <- pmax( data$study_entry_days, data$days_vax1 + prevax_per_start, na.rm=T)   
  data$end     <- pmax( data$days_vax1 + vax1_end,  data$days_vax2 + vax2_end,  na.rm=T)   
  data$end     <- pmin( data$end,  data$study_exit_days,    na.rm=T)  

  ####################################################################
  #
  #        specify risk windows without separate overlap period
  #
  #############
  #   model A
  #   the risk window of dose 2 takes precedence over the risk window of dose 1
  #
  # prevax risk period: 
  data$vd0     <- pmax( data$study_entry_days, data$days_vax1 + prevax_per_start, na.rm=T) 
  data$evd0    <- pmin( data$study_exit_days,  data$days_vax1 + prevax_per_end,   na.rm=T) 
  data$vd      <- data$vd0
  summary(data$vd0-data$days_vax1)
  summary(data$evd0-data$days_vax1)
  
  # buffer period (for all models):
  data$b0     <- pmax( data$study_entry_days, data$days_vax1 + buffer_per_start, na.rm=T) 
  data$eb0    <- pmin( data$study_exit_days,  data$days_vax1 + buffer_per_end,   na.rm=T) 
  #
  # vax 1 risk period: 
  data$vd1  <- data$days_vax1 + vax1_start
  data$evd1 <- pmin( data$days_vax1 + vax1_end, 
                     data$study_exit_days,   na.rm=T) 
  if(any(data$evd1 < data$vd1 + vax1_end)){
    warning(paste0("There are ",sum(data$evd1 < data$vd1+vax1_end, na.rm=T)," persons with 'study_exit_time' before (vax1 + ",vax1_end,")."))
    print(table( (data$evd1-data$vd1)[data$evd1-data$vd1<vax1_end] ))
  }
  data$evd1 <- pmin( data$evd1, data$days_vax2 - 1, na.rm=T)
  summary(data$evd1-data$days_vax1)
  #
  # vax 2 risk period: 
  data$vd2  <- data$days_vax2 + vax2_start 
  data$evd2 <- pmin( data$days_vax2 + vax2_end, data$study_exit_days) 
  summary(data$evd2-data$vd2)
  
  ##########
  #         specify risk windows with separate overlap period
  #
  # prevax risk period: 
  data$ovd0     <- data$vd0 
  data$eovd0    <- data$evd0 
  data$ovd      <- data$ovd0
  #
  # vax 1 risk period
  data$ovd1  <- data$vd1
  data$eovd1 <- data$evd1
  #
  # overlap between risk windows of vax 1 and vax 2
  data$ovd12  <- data$vd2
  data$eovd12 <- pmin( data$days_vax1 + vax1_end, 
                             data$study_exit_days,   na.rm=T) 
  data$ovd12[  data$eovd12 < data$ovd12 ] <- NA
  data$eovd12[ is.na(data$ovd12) ] <- NA
  #
  # vax 2 risk period
  data$ovd2  <- pmax( data$vd2, data$eovd12+1, na.rm=T )
  data$eovd2 <- data$evd2 
  
  ##############
  #    the risk window of dose 1 takes precedence over the risk window of dose 2
  #
  # prevax risk period: 
  data$v1d0     <- data$vd0 
  data$ev1d0    <- data$evd0 
  data$v1d      <- data$v1d0
  #
  # vax 1 risk period: 
  data$v1d1  <- data$days_vax1 + vax1_start
  data$ev1d1 <- pmin( data$days_vax1 + vax1_end, 
                      #data$days_vax2 - 1, 
                      data$study_exit_days,   na.rm=T) 
  summary(data$ev1d1-data$v1d1)
  
  # vax 2 risk period: 
  data$v1d2  <- pmax( data$ev1d1, data$days_vax2 + vax2_start ) 
  data$ev1d2 <- pmin( data$days_vax2 + vax2_end, data$study_exit_days ) 
  summary(data$ev1d2-data$v1d2)
  
  ##############
  #    two independent risk window for dose_1 and dose_2
  #
  # prevax risk period: 
  data$ind0     <- data$vd0 
  data$eind0    <- data$evd0 
  data$ind      <- data$ind0
  #
  # vax 1 risk period: 
  data$ind1  <- data$days_vax1 + vax1_start
  data$eind1 <- pmin( data$days_vax1 + vax1_end, 
                      data$study_exit_days,   na.rm=T) 
  summary(data$eind1-data$ind1)
  
  # vax 2 risk period: 
  data$ind2  <- data$days_vax2 + vax2_start 
  data$eind2 <- pmin( data$days_vax2 + vax2_end, data$study_exit_days ) 
  summary(data$eind2-data$ind2)

  summary(data)
  # list2DF( apply(data,2, function(x) summary(as.numeric(x)-data$days_vax1) ) )
  
  ##########################
  # create datasets for the current risk periods:
  #
  # condition for myoperi before start (vax1-90):
  nrow00 <- nrow0 <- nrow(data)
  data <- data[ data$start <= data$myopericarditis_days & !is.na(data$days_vax1) & !is.na(data$myopericarditis_days),  ]
  if(nrow0-nrow(data)>0)
    print( c(myoperi_after_start_prevax=nrow(data), old_nrow=nrow0, percent=100*nrow(data)/nrow0), digits=2 ) # without myoperi before vax1-90
 
  # condition for myoperi after last risk window (of vax1 or vax2):
  nrow0 <- nrow(data)
  end_last_rw <- pmax(data$days_vax1 + vax1_end, data$days_vax2 + vax2_end, na.rm=T  )
  end_last_rw <- pmin(end_last_rw, data$study_exit_days, na.rm=T  )
  data <- data[ data$myopericarditis_days <= end_last_rw ,  ]
  if(nrow0-nrow(data)>0)
    print( c(myoperi_before_end_last_rw=nrow(data), old_nrow=nrow0, percent=100*nrow(data)/nrow0), digits=2 ) # without myoperi after last risk window

  # condition myoperi during buffer interval
  data$cond_buffer <-  !is.na(data$days_vax1) & !is.na(data$myopericarditis_days) &
    data$days_vax1 + buffer_per_start <=  data$myopericarditis_days  & 
    data$myopericarditis_days         <   data$days_vax1
  
  # condition for between risk windows of vax1 and vax2
  data$cond_between_rw1_rw2 <-  
    !is.na(data$days_vax1)     &   !is.na(data$days_vax2) & !is.na(data$myopericarditis_days ) &
    data$days_vax1 + vax1_end < data$myopericarditis_days  & 
    data$myopericarditis_days <   data$days_vax2 
  
  # delete period between vax1 and vax2
  if(  delete_between_rw1_rw2 ){
    nrow0<-nrow(data)
    data <- data[ !data$cond_between_rw1_rw2 , ]
    if(nrow0-nrow(data)>0)
      print( c(no_myoperi_between_risk_windows=nrow(data), old_nrow=nrow0, percent=100*nrow(data)/nrow0), digits=2 ) # without period between risk windows of vax1 and vax2 with buffer period
  }

 # delete buffer period:
  if(delete_buffer ){
    nrow0<-nrow(data)
    data <- data[ !data$cond_buffer  ,   ]
    if(nrow0-nrow(data)>0)
      print( c(no_myoperi_in_buffer=nrow(data), old_nrow=nrow0, percent=100*nrow(data)/nrow0), digits=2 ) # without buffer period
  }
  
  tit <- paste0("\n\nDataset for SCRI analyses with prevax:[",paste0(prevax,collapse=";"),"],")
  if(delete_buffer) tit <- paste0( tit, " buffer:[", paste0(buffer,collapse=";"),"], ")
  tit <- paste0( tit, "vax1:[",paste0(rw_vax1,collapse=";"),"], vax2:[",paste0(rw_vax2,collapse=";"),"]:\n\n")
  
  cat(tit)
  print( c(data=nrow(data), old_nrow=nrow00, percent=100*nrow(data)/nrow00), digits=2 )                      # without buffer period
  
  list(data             = data,
       parameters = list(  prevax                  = prevax, 
                           buffer                  = buffer, 
                           rw_vax1                 = rw_vax1,
                           rw_vax2                 = rw_vax2,
                           delete_buffer           = delete_buffer,
                           delete_between_rw1_rw2  = delete_between_rw1_rw2,
                           call                    = match.call()
      ))
  
}  # end of function 'scri_create_input'
#
###############################

###############################
#
#  functions to add results to report list and model list
#
add_to_report_list <- function(x, name, list=report_list, add=T){ 
  if(!add) list <- list()
  else if(!is.list(list)) stop("'list' must be a list") 
  if( is.list(x) ){
    x <- x[[1]][, !(names(x[[1]]) %in% c("relative_rate","event_percent","all_cat2")) ]
    #if(any(names(x)=="n_events"))
    #  x <- x[,c(1:2, ((1:ncol(x))[names(x)=="n_events"]) : ncol(x) ),]
  }
  list <- c( list, list(x) )
  if(!missing(name)) names(list)[length(list)] <- name
  list
}

add_to_models_list <- function(x, name, list=models_list, add=T){ 
  if(!add) list <- list()
  else if(!is.list(list)) stop("'list' must be a list") 
  list <- c( list, list(x) )
  if(!missing(name)) names(list)[length(list)] <- name
  list
}
#
#####################

####################
#
#     risk windows names are created for different 
#
#       * 'vax1_end' and 'vax1_end';
#       * 'prevax_per_start', 'prevax_per_en'd'
#                  
#
if(F){ 
  rw_names <- c( paste0( "pre-exposure [",prevax_per_start,";",prevax_per_end,"]" ), 
                 paste0( "dose 1 [0;",vax1_end,"]" ),  
                 paste0( "dose 2 [0;",vax2_end,"]" )
  )
  rw_names_day0 <- c( paste0( "pre-exposure [",prevax_per_start,";",prevax_per_end,"]" ), 
                      paste0( "dose 1 day 0" ),  
                      paste0( "dose 1 [1;",vax1_end,"]" ),  
                      paste0( "dose 2 day 0" ), 
                      paste0( "dose 2 [1;",vax2_end,"]" )
  )
  rw_names_day0_buff_betw <- c( paste0( "pre-exposure [",prevax_per_start,";",prevax_per_end,"]" ), 
                                paste0( "buffer" ),  
                                paste0( "dose 1 day 0" ),  
                                paste0( "dose 1 [1;",vax1_end,"]" ),  
                                paste0( "between" ), 
                                paste0( "dose 2 day 0" ), 
                                paste0( "dose 2 [1;",vax2_end,"]" )
  )
  rw_names_day0_between <- c( paste0( "pre-exposure [",prevax_per_start,";",prevax_per_end,"]" ), 
                              paste0( "dose 1 day 0" ),  
                              paste0( "dose 1 [1;",vax1_end,"]" ),  
                              paste0( "between" ), 
                              paste0( "dose 2 day 0" ), 
                              paste0( "dose 2 [1;",vax2_end,"]" )
  )
  overlap_rw_names <- c( paste0( "pre-exposure [",prevax_per_start,";",prevax_per_end,"]" ), 
                         paste0( "dose 1 [0;",vax1_end,"]" ),  
                         paste0( "dose 2 during dose 1 window [0;max(overlap)]" ),  
                         paste0( "dose 2 after  dose 1 risk window [0;",vax2_end,"]" )
  )
  overlap_rw_names_day0 <- c( paste0( "pre-exposure [",prevax_per_start,";",prevax_per_end,"]" ), 
                              paste0( "dose 1 day 0" ),  
                              paste0( "dose 1 [1;",vax1_end,"]" ),  
                              paste0( "dose 2 during dose 1 window day 0" ),  
                              paste0( "dose 2 during dose 1 window [1;max(overlap)]" ),  
                              paste0( "dose 2 after  dose 1 risk window day 0" ), 
                              paste0( "dose 2 after  dose 1 risk window [1;",vax2_end,"]" )
  )
  rw1_names <- c( paste0( "pre-exposure [",prevax_per_start,";",prevax_per_end,"]" ), 
                  paste0( "dose 1 [0;",vax1_end,"]" ),  
                  paste0( "dose 2 after dose 1 risk window [0;",vax2_end,"]" )
  )
  rw1_names_day0 <- c( paste0( "pre-exposure [",prevax_per_start,";",prevax_per_end,"]" ), 
                       paste0( "dose 1 day 0" ),  
                       paste0( "dose 1 [1;",vax1_end,"]" ),  
                       paste0( "dose 2 day 0 after dose 1 risk window" ), 
                       paste0( "dose 2 after dose 1 risk window [1;",vax2_end,"]" )
  )
}
#######################




#############   SCRI models ############################
#
#
#

##########
#              model A  (vax2 takes precedence over vax1)
#
#   the risk window of dose 2 takes precedence over the risk window of dose 1

old_width = options(width=200)
print_during_running <- F


for(ianalysis in 1:2){
  
  if(ianalysis==1) glob_analysis_name <- "_all"
  if(ianalysis==2) glob_analysis_name <- "_noCovid_before_myoperi"


#######################################
#  create dataset:
#

vax1_end         <-  28    # 60
vax2_end         <-  28    # 60

prevax_per_start <- -90
prevax_per_end   <- -30

buffer_per_start  <- -29   # let op negative sign!!!
buffer_per_end    <- -1   # let op negative sign!!!


d90_30_28_28 <- scri_create_input(data = scri_input, 
                                      prevax = c(prevax_per_start, prevax_per_end), 
                                      buffer = c(buffer_per_start, buffer_per_end), 
                                      rw_vax1 = c(0, vax1_end),   # c(0, 60)
                                      rw_vax2 = c(0, vax2_end),    # c(0, 60)
                                      delete_buffer           = F,
                                      delete_between_rw1_rw2  = F   ) 
data_scri  <- d90_30_28_28$data



if(ianalysis==2){
  dim(data_scri)
  cond <- !is.na(data_scri$covid19_date) &  data_scri$myopericarditis_date  < data_scri$covid19_date    
  cond <- cond | is.na(data_scri$covid19_date) 
  data_scri <- data_scri[ cond ,]
  dim(data_scri)
}


#####
## create names for scri output:
#
rw_names <- c( paste0( "pre-exposure [",prevax_per_start,";",prevax_per_end,"]" ), 
               paste0( "dose 1 [0;",vax1_end,"]" ),  
               paste0( "dose 2 [0;",vax2_end,"]" )
)
rw_names_day0 <- c( paste0( "pre-exposure [",prevax_per_start,";",prevax_per_end,"]" ), 
                    paste0( "dose 1 day 0" ),  
                    paste0( "dose 1 [1;",vax1_end,"]" ),  
                    paste0( "dose 2 day 0" ), 
                    paste0( "dose 2 [1;",vax2_end,"]" )
)
rw_names_day0_buff_betw <- c( paste0( "pre-exposure [",prevax_per_start,";",prevax_per_end,"]" ), 
                              paste0( "buffer" ),  
                              paste0( "dose 1 day 0" ),  
                              paste0( "dose 1 [1;",vax1_end,"]" ),  
                              paste0( "between" ), 
                              paste0( "dose 2 day 0" ), 
                              paste0( "dose 2 [1;",vax2_end,"]" )
)
rw_names_day0_between <- c( paste0( "pre-exposure [",prevax_per_start,";",prevax_per_end,"]" ), 
                            paste0( "dose 1 day 0" ),  
                            paste0( "dose 1 [1;",vax1_end,"]" ),  
                            paste0( "between" ), 
                            paste0( "dose 2 day 0" ), 
                            paste0( "dose 2 [1;",vax2_end,"]" )
)

##########################################


#print_during_running <- T

models_list <- list()
report_list <- list()

# model A all data with day 0 for vax1 and vax2                         
# (vax2 preced. over vax1) 
data_scri$vd <- data_scri$vd0
if(any(ls()=="res")) rm(res)
res <- scri(  event ~ vd, 
            indiv = person_id, 
            astart = start, 
            aend = end,   
            aevent = myopericarditis_days,
            adrug  =cbind( vd,  vd1, vd1+1, vd2, vd2+1),
            aedrug =cbind(evd0, vd1, evd1,  vd2, evd2), 
            adrugnames = rw_names_day0,
            dataformat = "multi",
            sameexpopar = F, 
            data = data_scri)

if(print_during_running){
  cat(paste("\n\nAll brands together:\n\n"))
  print(format(res[[1]],justify="left",digits=3))
}
report_list <- add_to_report_list(res,"all_brands")
models_list <- add_to_models_list(res,"all_brands")

###########  with buffer and separate period between rw1 and rw2:
data_scri$vd <- data_scri$vd0
rm(res)
res <- scri(  event ~ vd, 
            indiv = person_id, 
            astart = start, 
            aend = end,   
            aevent = myopericarditis_days,
            adrug  =cbind( vd,  evd0+1, vd1, vd1+1, evd1+1, vd2, vd2+1),
            aedrug =cbind(evd0, vd1-1,  vd1, evd1,  vd2-1,  vd2, evd2), 
            adrugnames = rw_names_day0_buff_betw,
            dataformat = "multi",
            sameexpopar = F, 
            data = data_scri ) 

if(print_during_running){
  cat(paste("\n\nAll brands together also with the buffer and between two risk windows periods:\n\n"))
  print(format(res[[1]],justify="left",digits=3))
}
report_list <- add_to_report_list(res,"all_brands_buf_betw")
models_list <- add_to_models_list(res,"all_brands_buf_betw")

########### per brand: #############

for(ibrand in sort(unique(data_scri$type_vax1))){
  data_scri$vd <- data_scri$vd0
  rm(res)
  res <- scri(  event ~ vd, 
              indiv = person_id, 
              astart = start, 
              aend = end,   
              aevent = myopericarditis_days,
              adrug  =cbind( vd,  vd1, vd1+1, vd2, vd2+1),
              aedrug =cbind(evd0, vd1, evd1,  vd2, evd2), 
              adrugnames = rw_names_day0,
              #expogrp=c(0,8,14), 
              dataformat = "multi",
              sameexpopar = F, 
              data = data_scri[data_scri$type_vax1 == ibrand, ])
  
  if(print_during_running){
    cat(paste("\n\n",ibrand,":\n\n"))
    print(format(res[[1]],justify="left",digits=3))
  }
  report_list <- add_to_report_list(res,ibrand)
  models_list <- add_to_models_list(res,ibrand)

}

###########  with buffer and separate period between rw1 and rw2:
for(ibrand in sort(unique(data_scri$type_vax1))){
  data_scri$vd <- data_scri$vd0
  rm(res)
  res <- scri(  event ~ vd, 
              indiv = person_id, 
              astart = start, 
              aend = end,   
              aevent = myopericarditis_days,
              adrug  =cbind( vd,  evd0+1, vd1, vd1+1, evd1+1, vd2, vd2+1),
              aedrug =cbind(evd0, vd1-1,  vd1, evd1,  vd2-1,  vd2, evd2), 
              adrugnames = rw_names_day0_buff_betw,
              #expogrp=c(0,8,14), 
              dataformat = "multi",
              sameexpopar = F, 
              data = data_scri[data_scri$type_vax1 == ibrand, ]) 
  
  if(print_during_running){
    cat(paste("\n\n",ibrand," also with the buffer and between two risk windows periods:\n\n"))
    print(format(res[[1]],justify="left",digits=3))
  }
  report_list <- add_to_report_list(res,paste0(ibrand, "_buf_betw"))
  models_list <- add_to_models_list(res,paste0(ibrand, "_buf_betw"))
}
# report_list


###########
#  save report_list and model_list
dap <- ifelse( any(names(scri_input)=="DAP"), scri_input$DAP[1], "")

# copy report and models lists to lists with other names:
assign(paste0(dap,"_report_models_A",glob_analysis_name),  report_list )
assign(paste0(dap,"_scri_models_A",  glob_analysis_name),  models_list )

# save report, models list as .RData; report also in .txt file:
save(list=paste0(dap,"_scri_models_A",  glob_analysis_name), file = paste0(sdr, dap, "_scri_models_A",  glob_analysis_name,".RData" ))
save(list=paste0(dap,"_report_models_A",glob_analysis_name), file = paste0(sdr, dap, "_report_models_A",glob_analysis_name,".RData" ))

sink(paste0(sdr, dap, "_scri_models_A",glob_analysis_name,".txt" ))
  cat(paste("\n\nNumber of rows in the dataset =", nrow(data_scri),".\n\n\n"))
  print(lapply(report_list,format,justify="left",digits=3))
sink()
#
#######################################################################################



#                Age & sex


##########################################


#print_during_running <- T

models_list <- list()
report_list <- list()

if(!any(ls()=="age_cat_scri")) age_cat_scri <- c(-1,30,120)

data_scri$age_cat <- cut(data_scri$age_at_study_entry, age_cat_scri )

# model A all data with day 0 for vax1 and vax2                         
# (vax2 preced. over vax1) 
#
data_scri$vd <- data_scri$vd0
if(any(ls()=="res")) rm(res)
res <- scri(  event ~ sex/age_cat/vd, 
                indiv = person_id, 
                astart = start, 
                aend = end,   
                aevent = myopericarditis_days,
                #adrug  =cbind( vd,  vd1, vd1+1, vd2, vd2+1),
                #aedrug =cbind(evd0, vd1, evd1,  vd2, evd2), 
                #adrugnames = rw_names_day0,
                adrug  =cbind( vd,  evd0+1, vd1, vd1+1, evd1+1, vd2, vd2+1),
                aedrug =cbind(evd0, vd1-1,  vd1, evd1,  vd2-1,  vd2, evd2), 
                adrugnames = rw_names_day0_buff_betw,
                dataformat = "multi",
                sameexpopar = F, 
                data = data_scri)

if(print_during_running){
  cat(paste("\n\nAll brands together per sex and age_cat:\n\n"))
  print(format(res[[1]],justify="left",digits=3))
  print("In another order:")
  print(format(res[[1]][order(res[[1]][,"all_cat"]),],justify="left",digits=3))
}
report_list <- add_to_report_list(res,"all_brands_sex_age")
models_list <- add_to_models_list(res,"all_brands_sex_age")

########### per brand: #############

###########  with buffer and separate period between rw1 and rw2:
for(ibrand in sort(unique(data_scri$type_vax1)))
  for(isex in sort(unique(data_scri$sex)))
    for(iage in sort(unique(data_scri$age_cat))){
      data_scri$vd <- data_scri$vd0
      rm(res)
      res <- scri(  event ~ vd, 
                      indiv = person_id, 
                      astart = start, 
                      aend = end,   
                      aevent = myopericarditis_days,
                      adrug  =cbind( vd,  evd0+1, vd1, vd1+1, evd1+1, vd2, vd2+1),
                      aedrug =cbind(evd0, vd1-1,  vd1, evd1,  vd2-1,  vd2, evd2), 
                      adrugnames = rw_names_day0_buff_betw,
                      #expogrp=c(0,8,14), 
                      dataformat = "multi",
                      sameexpopar = F, 
                      data = data_scri[  data_scri$type_vax1 == ibrand & 
                                           data_scri$sex       == isex &
                                           data_scri$age_cat   == iage     , ]) 
      if(print_during_running){
        cat(paste("\n\n",ibrand,"  for sex=",isex," and age_cat:",iage,"  also with the buffer and between two risk windows periods:\n\n"))
        print(format(res[[1]],justify="left",digits=3))
      }
      report_list <- add_to_report_list(res,paste0(ibrand, "_sex",isex,"_age",iage,"_buf_betw"))
      models_list <- add_to_models_list(res,paste0(ibrand, "_sex",isex,"_age",iage,"_buf_betw"))
    }
# report_list
# print:
# lapply(report_list,function(x) format(x[order(x[,"all_cat"]),],justify="left",digits=3) )



###########
#  save report_list and model_list
dap <- ifelse( any(names(scri_input)=="DAP"), scri_input$DAP[1], "")

assign(paste0(dap,"_report_models_A_sex_age",glob_analysis_name),report_list )
assign(paste0(dap,"_scri_models_A_sex_age"  ,glob_analysis_name),  models_list )

save(list=paste0(dap,"_scri_models_A_sex_age",  glob_analysis_name), file = paste0(sdr, dap, "_scri_models_A_sex_age",  glob_analysis_name,".RData" ))
save(list=paste0(dap,"_report_models_A_sex_age",glob_analysis_name), file = paste0(sdr, dap, "_report_models_A_sex_age",glob_analysis_name,".RData" ))

sink(paste0(sdr, dap, "_scri_models_A_sex_age",glob_analysis_name,".txt" ))
print("Sorted:")
print(lapply(report_list[1],function(x) format(x[order(x[,"all_cat"]),],justify="left",digits=3) ))
print("original order:")
print(lapply(report_list,format,justify="left",digits=3))
sink()


#
#######################################################################################


#######################################################################################



#                Age 


##########################################


#print_during_running <- T

models_list <- list()
report_list <- list()

if(!any(ls()=="age_cat")) age_cat <- c(-1,30,120)

# age_cat <- c(-1,30,120)

data_scri$age_cat <- cut(data_scri$age_at_study_entry, age_cat )

# model A all data with day 0 for vax1 and vax2                         
# (vax2 preced. over vax1) 
#
data_scri$vd <- data_scri$vd0
if(any(ls()=="res")) rm(res)
res <- scri(  event ~ age_cat/vd, 
                indiv = person_id, 
                astart = start, 
                aend = end,   
                aevent = myopericarditis_days,
                adrug  =cbind( vd,  vd1, vd1+1, vd2, vd2+1),
                aedrug =cbind(evd0, vd1, evd1,  vd2, evd2), 
                adrugnames = rw_names_day0,
                dataformat = "multi",
                sameexpopar = F, 
                data = data_scri)

if(print_during_running){
  cat(paste("\n\nAll brands together per age_cat:\n\n"))
  print(format(res[[1]],justify="left",digits=3))
  print("In another order:")
  print(format(res[[1]][order(res[[1]][,"all_cat"]),],justify="left",digits=3))
}
report_list <- add_to_report_list(res,"all_brands_age")
models_list <- add_to_models_list(res,"all_brands_age")

########### per brand: #############

###########  with buffer and separate period between rw1 and rw2:
for(ibrand in sort(unique(data_scri$type_vax1)))
    for(iage in sort(unique(data_scri$age_cat))){
      data_scri$vd <- data_scri$vd0
      rm(res)
      res <- scri(  event ~ vd, 
                      indiv = person_id, 
                      astart = start, 
                      aend = end,   
                      aevent = myopericarditis_days,
                      adrug  =cbind( vd,  evd0+1, vd1, vd1+1, evd1+1, vd2, vd2+1),
                      aedrug =cbind(evd0, vd1-1,  vd1, evd1,  vd2-1,  vd2, evd2), 
                      adrugnames = rw_names_day0_buff_betw,
                      #expogrp=c(0,8,14), 
                      dataformat = "multi",
                      sameexpopar = F, 
                      data = data_scri[  data_scri$type_vax1 == ibrand & 
                                           data_scri$age_cat   == iage     , ]) 
      if(print_during_running){
        cat(paste("\n\n",ibrand,"  for age_cat:",iage,"  also with the buffer and between two risk windows periods:\n\n"))
        print(format(res[[1]],justify="left",digits=3))
      }
      report_list <- add_to_report_list(res,paste0(ibrand, "_age",iage,"_buf_betw"))
      models_list <- add_to_models_list(res,paste0(ibrand, "_age",iage,"_buf_betw"))
    }
# report_list
# print:
# lapply(report_list,function(x) format(x[order(x[,"all_cat"]),],justify="left",digits=3) )



###########
#  save report_list and model_list
dap <- ifelse( any(names(scri_input)=="DAP"), scri_input$DAP[1], "")

assign(paste0(dap,"_report_models_A_age",glob_analysis_name), report_list )
assign(paste0(dap,"_scri_models_A_age",  glob_analysis_name), models_list )

save(list=paste0(dap,"_scri_models_A_age",  glob_analysis_name), file = paste0(sdr, dap, "_scri_models_A_age",  glob_analysis_name,".RData" ))
save(list=paste0(dap,"_report_models_A_age",glob_analysis_name), file = paste0(sdr, dap, "_report_models_A_age",glob_analysis_name,".RData" ))

sink(paste0(sdr, dap, "_scri_models_A_age",glob_analysis_name,".txt" ))
print("Sorted:")
print(lapply(report_list[1],function(x) format(x[order(x[,"all_cat"]),],justify="left",digits=3) ))
print("original order:")
print(lapply(report_list,format,justify="left",digits=3))
sink()


#
#######################################################################################


} # close of for(ianalysis ... )


##########
# restore options:
options(old_width)



