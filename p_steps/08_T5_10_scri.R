# Program Information  ----------------------------------------------------
#
# Program:      step_12_2_run_scri.R 
# Author:       Svetlana Belitser
# Description:  calls functions which runs an SCRI on specified datasets 
#               runs on all datasets in g_output/scri                  
# Requirements: 
#               dependencies: preceding steps, package "survival" 
#               input:  g_output/scri/*  
#               output:  g_output/scri/*  
#
#               parameters: in 07_scri_inputs.R 
#  
#               function: p_macro/scri_tools.R                    
#               function: p_macro/table1.R  
#
#

# Housekeeping  -----------------------------------------------------------
# install and load packages

if(!any(ls()=="thisdir"))   thisdir   <- getwd()
if(!any(ls()=="dirtemp"))   dirtemp   <- paste0(thisdir,"/g_intermediate/")
if(!any(ls()=="diroutput")) diroutput <- paste0(thisdir,"/g_output/")
if(!any(ls()=="diroutput")) direxp    <- paste0(thisdir,"/g_output/")

# ensure required folders are created  
dir.create(file.path(paste0(dirtemp,   "scri")),            showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(paste0(thisdir,   "/log_files/scri")), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(paste0(diroutput, "scri")),            showWarnings = FALSE, recursive = TRUE)


for (subpop in subpopulations_non_empty) {
  
  # scri output_directory for export:  
  sdr0 <- direxpsubpop[[subpop]]
  dir.create(file.path(paste0(sdr0, "scri")), showWarnings = FALSE, recursive = TRUE)
  
  # SCCS output_directory for models not for export:
  sdr_models0 <- paste0(diroutput, "scri/")
  dir.create(sdr_models0, showWarnings = FALSE, recursive = TRUE)
  
  
  
  
  
  # Import Data -------------------------------------------------------------
  
  
  
  # Load the D3_study_population_SCRI
  load(paste0(dirtemp, "D3_study_population_SCRI", suffix[[subpop]], ".RData"))
  scri_input <- as.data.frame(get(paste0("D3_study_population_SCRI", suffix[[subpop]])))
  rm(list = paste0("D3_study_population_SCRI", suffix[[subpop]]))
  
  # scri_input <- D3_study_population_SCRI  
  
  #################
  # create dataset 'data_vax' (with multiple rows per person) from 'scri_input' (with one row per person)
  #
  if(any(c("date_vax1","vax1_date") %in% names(scri_input))){
    date_var <- "date_vax"; type_var <- "type_vax"
    if(any(c("vax1_date") %in% names(scri_input))) date_var <- "vax_date"
    if(any(c("vax1_type") %in% names(scri_input))) type_var <- "vax_type"
    scri_n_vax_var <- as.integer(substring(names(scri_input),9)[substring(names(scri_input),1,8)==date_var]) # ==> 1,2,3  (from "data_vax1", "date_vax2", ...)
    for(ivax in scri_n_vax_var){
      data_tmp <- scri_input[, !substring(names(scri_input),1,8) %in% c(type_var,date_var) | 
                               ( substring(names(scri_input),1,8) %in% c(type_var,date_var) & names(scri_input) %in% paste0(c(type_var,date_var),ivax) ) ]
      names(data_tmp)[names(data_tmp)==paste0(date_var,ivax)] <- "vax_date" 
      names(data_tmp)[names(data_tmp)==paste0(type_var,ivax)] <- "vax_brand"
      data_tmp$vax_n <- ivax
      data_tmp <- data_tmp[,c( names(data_tmp)[!names(data_tmp) %in% c("vax_brand","vax_date")], "vax_brand", "vax_date" )]
      
      if(ivax==1) data_vax <- data_tmp
      else data_vax <- rbind.data.frame(data_vax, data_tmp)
    }
  } else {
    data_vax <- scri_input
    names(data_vax)[names(data_vax)  ==    "date_vax"            ]  <- "vax_date" 
    names(data_vax)[names(data_vax) %in% c("type_vax","vax_type")]  <- "vax_brand" 
  }
  #
  #######
  
  
  data_vax[,"vax_days"]  <- as.integer( difftime( data_vax[,"vax_date"], as.Date("2020-08-31"), units="days"))
  # sort per id, vax_days
  data_vax <- data_vax[order(data_vax[,id],data_vax[,"vax_days"]),]
  
  # dap:
  if(!any(ls()=="dap")) dap <- ifelse( any(tolower(names(data_vax))=="dap"), data_vax[1,tolower(names(data_vax))=="dap"], "" )
  if(dap=="" & any(tolower(names(data_vax))=="datasource")) dap <- data_vax[1,tolower(names(data_vax))=="datasource"]
  
  # calculate the 'days'-variables:
  for(idate_vars in substring(names(data_vax),6)[substring(names(data_vax),1,5)=="date_"])
    data_vax[,paste0(idate_vars,"_days")]  <- as.integer( difftime( data_vax[,paste0("date_",idate_vars)], as.Date("2020-08-31"), units="days"))
  for(idate_vars in substring(names(data_vax),1,nchar(names(data_vax))-5)[substring(names(data_vax),nchar(names(data_vax))-4,nchar(names(data_vax)))=="_date"])
    data_vax[,paste0(idate_vars,"_days")]  <- as.integer( difftime( data_vax[,paste0(idate_vars,"_date")], as.Date("2020-08-31"), units="days"))
  names(data_vax)[names(data_vax)=="of_death_days"] <- "death_days" 
  
  
  # check:
  tb<-table(data_vax$study_entry_days < data_vax$study_exit_days )
  if(length(tb)>1)stop(paste("There are ",tb["FALSE"],"rows with 'study_entry_date' >= 'study_exit_date' !"))
  
  data_vax <- data_vax[data_vax$study_entry_days < data_vax$study_exit_days,]
  
  if(sum(data_vax$study_entry_days > data_vax$vax_days,na.rm=T)>0) stop(paste( sum(data_vax$study_entry_days > data_vax$vax_days,na.rm=T), "rows with 'study_entry_days' > 'vax_days'"))
  
  data_vax <- data_vax[data_vax$study_entry_days <= data_vax$vax_days,]
  
  
  
  
  #############   SCRI models ############################
  #
  #
  old_width = options(width=300)
  
  #
  ########################################################
  
  ########################################
  #
  # create a list with calendar time interval:
  #
  time_seq <- vector("list",length=length(time_interval_width))
  for(i in 1:length(time_interval_width))
    time_seq[[i]] <- seq(min(data_vax[,"study_entry_days"],na.rm=T)-time_interval_starts[i],max(data_vax[,"study_exit_days"],na.rm=T)+time_interval_width[i]-1,by=time_interval_width[i])
  names(time_seq) <- paste0("period",time_interval_width,"d_start_",-time_interval_starts,"d")
  
  ########################################
  
  
  ##########################################
  #
  #   create variables: 
  #   dose number: 'vax_n'      (1,2,...) 
  #                'vax_number' ("dose 1" ,"dose 2", "dose 3", ...)
  #
  
  #### 'vax_n':
  data_vax <- data_vax[order(data_vax[,id],data_vax[,"vax_days"]),]
  ivax <- 1; 
  data_vax$vax_n <- data_vax[,"vax_days"]
  data_vax$vax_n[!is.na(data_vax$vax_n)] <- ivax 
  while(T){
    cond_vax_i <- data_vax$vax_n == ivax & !is.na(data_vax$vax_n) 
    cond2<-duplicated(data_vax[cond_vax_i,id])
    if(!any(cond2)) break
    ivax <- ivax + 1
    data_vax$vax_n[cond_vax_i][cond2] <- ivax
  }
  #### 'vax_number':
  data_vax$vax_number <- paste0("dose ",data_vax$vax_n," ")
  #### 'vax_brand_short':
  data_vax$vax_brand_short <- format(substring(data_vax[,"vax_brand"],1,5))
  
  data_vax <- data_vax[!is.na(data_vax$vax_date),]
  
  
  ##########################################################################################
  #
  #   these parameters are used for all analyses:
  #
  extra_options <- list( paral                = lparal,    # if T ==> library(parallel) is started in function 'scri'
                         n_cores              = n_cores,   # Don't define it! Define it only if it doesn't work on its own! number of cores/threads to use.
                         time_seq             = time_seq, 
                         print_during_running = F,
                         lprint               = F,
                         plot_during_running  = F, 
                         leventplot           = leventplot, 
                         lplot                = lplot,
                         CI_draw              = CI_draw,
                         lforest              = lforest,
                         col                  = col_list
  )
  
  # default in function 'define_rws': (vax2 takes precedence over vax1) the risk window of dose 2 takes precedence over the risk window of dose 1
  
  for(iae in ae_events){
    
    print(iae)
    
    if(!(paste0(iae,"_days") %in% names(data_vax))) { cat(paste("\nevent",iae,"not found.\n")); next }
    
    if(!any(names(data_vax)==iae)) data_vax[,iae] <- as.integer(!is.na(data_vax[,paste0(iae,"_days")]))
    
    if(lmain){
      
      # SCCS output_directory for the event:  EXPORT 
      sdr <- paste0(sdr0, iae,"/")
      dir.create(sdr, showWarnings = FALSE, recursive = TRUE)
      
      # SCCS output_directory for the event:  LOCAL 
      sdr_models <- paste0(sdr_models0, iae,"/")
      dir.create(sdr_models, showWarnings = FALSE, recursive = TRUE)
      
      ##########################################################################################
      #
      #   add 'sdr_tabs' and 'sdr_models' parameters
      #
      extra_options$sdr_tabs   <- sdr              
      extra_options$sdr_models <- sdr_models 
      
      
      ###################################################
      #
      #  baseline tables
      # 
      characteristics(data=data_vax, event=iae, path_file_name=paste0(sdr,"baseline.txt"), condition_value="" )
      
      
      ###########################################################################################
      ###################################  event  ############################################### 
      #   
      event_info <- list( event=iae, event_time =paste0(iae,"_days"), event_date =paste0(iae,"_date") )
      
      
      ###########################################################################################
      #
      #             vax_name="vax_number": dose1, dose2, dose3, ...
      #
      vax_def0 <- scri_data_parameters( data =  data_vax,   vax_name  = "vax_number",       vax_time = "vax_days",        vax_date     = "vax_date", 
                                        id   = "person_id", start_obs = "study_entry_days", end_obs  = "study_exit_days", censored_vars = "death_days" )
      
      ###########  vax_number & no split  ##### 
      # 
      ## cut_points_name="28d" :  { [-91;-30], [-29;-1], [0;0], [1;28], [28;61], [62;181], >181 } 
      vax_def  <- define_rws(vax_def0,  cut_points_before = c(-90,-29,0), cut_points_after = c(0,1,29,62,182), cut_points_name="28d",  data=data_vax )
      res <- scri( formula = "~ lab", vax_def = vax_def, data = data_vax, event_info=event_info,  extra_parameters = extra_options, add_to_itself=F  )        
      
      ## cut_points_name="7d" { [-91;-30], [-29;-1], [0;0], [1;7], [8;14], [15;21], [22;28] } 
      vax_def  <- define_rws(vax_def0,  cut_points_before = c(-90,-29,0), cut_points_after = c(0,1,8,15,22,29), cut_points_name="7d", no_last_interval_after=T, data=data_vax )
      res <- scri( formula = "~ lab", vax_def = vax_def, data = data_vax, event_info=event_info,  extra_parameters = extra_options, add_to_itself=T   )        
      
      
      ###########  vax_number & brand ( no distance):  ##### 
      # 
      ## cut_points_name="28d" :  { [-91;-30], [-29;-1], [0;0], [1;28], [28;61], [62;181]  } 
      vax_def <- define_rws(vax_def0,  cut_points_before = c(-90,-29,0), cut_points_after = c(0,1,29,62,182), cut_points_name="28d", no_last_interval_after=T, 
                            data=data_vax, vax_dep = c( before="vax_brand_short" ))
      res <- scri( formula = "~ brand:lab", vax_def = vax_def, data = data_vax, event_info=event_info, extra_parameters = extra_options, add_to_itself=F)
      
      ## cut_points_name="7d"
      vax_def <- define_rws(vax_def0,  cut_points_before = c(-90,-29,0), cut_points_after = c(0,1,8,15,22,29), cut_points_name="7d", no_last_interval_after=T, 
                            data=data_vax, vax_dep = c( before="vax_brand_short" ))
      res <- scri( formula = "~ brand:lab", vax_def = vax_def, data = data_vax, event_info=event_info, extra_parameters = extra_options, add_to_itself=T )
      
      
      
      #######################################################################
      #
      #       strata analyse with brand for: age30, age30_50, sex, sex_age30 
      #
      
      data_vax$age30     <- paste0("age",as.character(cut(data_vax$age_at_study_entry, c(-1,30,   Inf)))); data_vax$age30[   is.na(data_vax$age_at_study_entry)] <- NA
      data_vax$age30_50  <- paste0("age",as.character(cut(data_vax$age_at_study_entry, c(-1,30,50,Inf)))); data_vax$age30_50[is.na(data_vax$age_at_study_entry)] <- NA
      
      if("o" %in% tolower(data_vax$sex))  data_vax$sex[tolower(data_vax$sex)=="o"]<-NA
      data_vax$sexc      <- paste0("sex:",data_vax$sex );                     data_vax$sexc[     is.na(data_vax$sex)                        ] <- NA
      data_vax$sex_age30 <- paste0("sex:",data_vax$sex, " ", data_vax$age30); data_vax$sex_age30[is.na(data_vax$sex) | is.na(data_vax$age30)] <- NA
      
      
      # strata variables:
      for( strata_var in c( "age30","age30_50", "sexc", "sex_age30") ){
        
        # values of the current strata variable
        strata_values <- unique(data_vax[,strata_var]); strata_values <- strata_values[!is.na(strata_values)]
        
        # delete strata "age(-1,30]" if variable 'age30_50'  because it is also in 'age30'
        if(strata_var=="age30_50") strata_values <- strata_values[strata_values!="age(-1,30]"]
        
        for(strata_value in strata_values){ 
          
          ###########  vax_number & brand ( no distance) per stratum  ##### 
          # 
          ## cut_points_name="28d" :  { [-91;-30], [-29;-1], [0;0], [1;28], [28;61], [62;181]  } 
          vax_def <- define_rws(vax_def0,  cut_points_before = c(-90,-29,0), cut_points_after = c(0,1,29,62,182), cut_points_name="28d", no_last_interval_after=T, 
                                data=data_vax, vax_dep = c( before="vax_brand_short" ))
          # in models with calendar time intervals use only events from the current stratum:
          res <- scri( formula = "~ brand:lab", vax_def = vax_def, data = data_vax, strata_var=strata_var, strata_value=strata_value, use_all_events=F,
                       event_info=event_info, extra_parameters = extra_options, add_to_itself=F )
          # in models with calendar time intervals use events from all strata:
          res <- scri( formula = "~ brand:lab", vax_def = vax_def, data = data_vax, strata_var=strata_var, strata_value=strata_value, use_all_events=T, 
                       event_info=event_info, extra_parameters = extra_options, add_to_itself=T )
          
          ## cut_points_name="7d"
          vax_def <- define_rws(vax_def0,  cut_points_before = c(-90,-29,0), cut_points_after = c(0,1,8,15,22,29), cut_points_name="7d", no_last_interval_after=T, 
                                data=data_vax, vax_dep = c( before="vax_brand_short" ))
          res <- scri( formula = "~ brand:lab", vax_def = vax_def, data = data_vax, strata_var=strata_var, strata_value=strata_value, use_all_events=F,
                       event_info=event_info, extra_parameters = extra_options, add_to_itself=T )
          res <- scri( formula = "~ brand:lab", vax_def = vax_def, data = data_vax, strata_var=strata_var, strata_value=strata_value, use_all_events=T,
                       event_info=event_info, extra_parameters = extra_options, add_to_itself=T )
          
        }
      }
    } # end lmain
    
    
    ################################################################################
    #
    #            for covid subsets
    #
    if(lcovid){
      if(any(tolower(names(data_vax)) != "covid19_date") ){
        
        # SCCS output_subdirectory 'covid' in 'event' directory 
        sdr_covid <- paste0(sdr, "covid","/")
        dir.create(sdr_covid, showWarnings = FALSE, recursive = TRUE)
        
        # SCCS output_subdirectory 'covid' in the event directory: 
        sdr_covid_models <- paste0(sdr_models, "covid","/")
        dir.create(sdr_covid_models, showWarnings = FALSE, recursive = TRUE)
        
        #########################
        #   copy extra_options  and 
        #   change  'sdr_tabs' and 'sdr_models' parameters    
        #
        extra_options_covid            <- extra_options
        extra_options_covid$sdr_tabs   <- sdr_covid
        extra_options_covid$sdr_models <- sdr_covid_models
        ##############
        
        # all data:
        # cat("\n\nALL DATA: table for variables:\n")
        # print(substring(iae,1,7))
        # table1( unique(data_vax[ ,c(id, iae)]) [ , iae]  )
        
        
        for(icovid in c( "covidplus30d","nocovid")){
          
          ############# create covid variables: ##############.
          data_vax$covid <- as.numeric(!is.na(data_vax[,"covid19_date"]))
          if(icovid == "covidplus30d"){
            #####
            # select only those who did not have COVID before (the event date + 30 days). These 30 days added extra to make sure that 
            # if someone had COVID then there was no connection to the event.
            # create covid selection variables: no_covid_before_myocard_30d, no_covid_before_pericar_30d, no_covid_before_myoperi_30d, ...
            data_vax[,"no_covid_before_event_plus30d"] <- ( (as.Date(data_vax[,paste0(iae,"_date")]) + 30) < as.Date(data_vax$covid19_date) )  &  !is.na(data_vax$covid19_date) & !is.na(data_vax[,paste0(iae,"_date")])
            data_vax[is.na(data_vax$covid19_date) | is.na(data_vax[,paste0(iae,"_date")]),"no_covid_before_event_plus30d"] <- T
            
            data_vax[                                          , "covid_selection_name"] <- ""
            data_vax[data_vax[,"no_covid_before_event_plus30d"], "covid_selection_name"] <- paste0("no_covid_before_",iae,"_plus30d")
            covid_value <- paste0("no_covid_before_",iae,"_plus30d")
            
            #cat(paste0("\n\nno_covid_before ",iae," plus30d: table for variables:\n"))
            #print(c("covid","no_covid_before_event_plus30d"))
            #print(table1( unique( data_vax[ ,c(id, "covid","covid_selection_name",iae)] )[ ,c(iae,"covid","covid_selection_name")] ))
          }
          
          if(icovid =="nocovid"){
            #####
            # no covid in [start_of_observation; stop_of_observation]:
            data_vax[                                , "covid_selection_name"] <- ""
            data_vax[is.na(data_vax[,"covid19_date"]), "covid_selection_name"] <- "no_covid_observed"
            covid_value <- "no_covid_observed"
            
            #cat(paste0("\n\nno covid in [start_of_observation; stop_of_observation]:\n"))
            #print(c("covid","no_covid_observed"))
            #print(table1( unique( data_vax[ ,c(id, "covid","covid_selection_name",iae)] )[ ,c(iae,"covid","covid_selection_name")] ))
          }
          
          characteristics(data=data_vax, event=iae, path_file_name=paste0(sdr_covid,"baseline_",icovid,".txt"), condition_value=icovid )
          
          ###########################################################################################
          #
          #             vax_name="vax_number": dose1, dose2, dose3, ...
          #
          vax_def0 <- scri_data_parameters(data=data_vax, vax_name="vax_number", vax_time="vax_days", vax_date="vax_date" )
          
          ###########  vax_number & no split  ##### 
          # 
          ## cut_points_name="28d" :  { [-91;-30], [-29;-1], [0;0], [1;28], [28;61], [62;181], >181 } 
          vax_def  <- define_rws(vax_def0,  cut_points_before = c(-90,-29,0), cut_points_after = c(0,1,29,62,182), cut_points_name="28d",  data=data_vax )
          res <- scri( formula = "~ lab", vax_def = vax_def, data = data_vax, strata_var="covid_selection_name", strata_value=covid_value, use_all_events=F,
                       event_info=event_info,  extra_parameters = extra_options_covid, add_to_itself=F  ) 
          ## cut_points_name="7d"
          vax_def  <- define_rws(vax_def0,  cut_points_before = c(-90,-29,0), cut_points_after = c(0,1,8,15,22,29), cut_points_name="7d", no_last_interval_after=T, data=data_vax )
          res <- scri( formula = "~ lab", vax_def = vax_def, data = data_vax, strata_var="covid_selection_name", strata_value=covid_value, use_all_events=F,
                       event_info=event_info,  extra_parameters = extra_options_covid, add_to_itself=T )        
          
          
          ###########  vax_number & brand ( no distance):  ##### 
          # 
          ## cut_points_name="28d" :  { [-91;-30], [-29;-1], [0;0], [1;28], [28;61], [62;181]  } 
          vax_def <- define_rws(vax_def0,  cut_points_before = c(-90,-29,0), cut_points_after = c(0,1,29,62,182), cut_points_name="28d", no_last_interval_after=T, 
                                data=data_vax, vax_dep = c( before="vax_brand_short" ))
          res <- scri( formula = "~ brand:lab", vax_def = vax_def, data = data_vax, strata_var="covid_selection_name", strata_value=covid_value, use_all_events=F,
                       event_info=event_info, extra_parameters = extra_options_covid, add_to_itself=F )
          ## cut_points_name="7d"
          vax_def <- define_rws(vax_def0,  cut_points_before = c(-90,-29,0), cut_points_after = c(0,1,8,15,22,29), cut_points_name="7d", no_last_interval_after=T, 
                                data=data_vax, vax_dep = c( before="vax_brand_short" ))
          res <- scri( formula = "~ brand:lab", vax_def = vax_def, data = data_vax, strata_var="covid_selection_name", strata_value=covid_value, use_all_events=F,
                       event_info=event_info, extra_parameters = extra_options_covid, add_to_itself=T )
          
          
        }   # end  covid 'for'
      }# end codid variable
    }  # end lcovid
    
    
    
    
    
    
    
    #########################################################################
    #
    #  this part:  without time adjustment
    #
    if(ldist){
      
      
      ##########################################
      #
      #   create additional variables:
      #
      if( !all( c("dist","vax_name","type_with_prev","type_history","type_history_sorted") %in% names(data_vax)) ){
        #### 'dist'
        data_vax$dist <- c(NA,  diff(data_vax[,"vax_days"])  )
        data_vax$dist[-1][ data_vax[-1,id]!=data_vax[-nrow(data_vax),id]  ] <- NA
        #### 'dist_gt_60'
        data_vax$dist_gt_60 <- c("<=60d"," >60d")[(data_vax$dist > 60) +1 ]
        data_vax$dist_gt_60[is.na(data_vax$dist_gt_60)] <- ""
        
        #### 'vax_name'
        data_vax$vax_name <- data_vax$vax_number
        data_vax$vax_name[ data_vax$vax_name==paste0("dose ",1," ") ] <- "dose  1.1 " 
        data_vax$vax_name[ data_vax$vax_name==paste0("dose ",2," ") ] <- "dose  1.2 " 
        data_vax$vax_name[ data_vax$vax_name==paste0("dose ",3," ") ] <- "booster 1" 
        data_vax$vax_name[ data_vax$vax_name==paste0("dose ",4," ") ] <- "boost1or2" 
        data_vax$vax_name[ data_vax$vax_n==2 & data_vax$dist>60 ]     <- "booster 1"
        
        # create variable with tow brands: from the previous and current doses :
        data_vax$type_with_prev <- format(data_vax[,"vax_brand_short"])
        cond_prev_exists <- c( F, data_vax[-1,id] == data_vax[-nrow(data_vax),id] ) 
        cond_next_exists <- c( data_vax[-nrow(data_vax),id] == data_vax[-1,id], F ) 
        if(any(cond_prev_exists)) 
          data_vax$type_with_prev[ cond_prev_exists ] <- paste0( data_vax$type_with_prev[cond_next_exists],"-", format(data_vax[,"vax_brand_short"])[cond_prev_exists] )
        
        # create variable with history of brands:
        data_vax$type_history <- format(data_vax[,"vax_brand_short"])
        prev_steps <- 1
        while(T){
          cond_prev_exists <- c( F, data_vax[-1,id] == data_vax[-nrow(data_vax),id] ) & data_vax$vax_n==prev_steps+1
          cond_next_exists <- c( data_vax[-nrow(data_vax),id] == data_vax[-1,id], F )  & data_vax$vax_n==prev_steps  
          if(!any(cond_prev_exists)) break
          data_vax$type_history[ cond_prev_exists ] <- paste0( data_vax$type_history[cond_next_exists],"-", format(data_vax[,"vax_brand_short"])[cond_prev_exists] )
          prev_steps <- prev_steps + 1
        }
        # table1(data_vax[,,"type_history"])
        
        # create variable with sorted history of brands:
        data_vax$type_history_sorted <- unlist(lapply( strsplit(data_vax$type_history, "[ |-]+"), function(x)paste0(sort(x), collapse ="-") ))
        # table1(data_vax[,c("type_history_sorted","type_history")])
      }
      #
      ##################################
      
      
      # SCCS output_subdirectory 'distance_combi' in 'event' directory 
      sdr_dist <- paste0(sdr0, iae, "/distance_combi/")
      dir.create(sdr_dist, showWarnings = FALSE, recursive = TRUE)
      
      # SCCS output_subdirectory 'distance_combi' in the event directory: 
      sdr_dist_models <- paste0(sdr_models0, iae,"/distance_combi/")
      dir.create(sdr_dist_models, showWarnings = FALSE, recursive = TRUE)
      
      #########################
      #   copy extra_options  and 
      #   change  'sdr_tabs' and 'sdr_models' parameters    
      #
      extra_options_dist            <- extra_options
      extra_options_dist$sdr_tabs   <- sdr_dist
      extra_options_dist$sdr_models <- sdr_dist_models
      extra_options_dist$time_seq   <- NULL
      
      
      ###########################################################################################
      ###################################  event  ############################################### 
      #   
      event_info <- list( event=iae, event_time =paste0(iae,"_days"), event_date =paste0(iae,"_date") )
      
      
      ###########################################################################################
      #
      #             vax_name="vax_number": dose1, dose2, dose3, ...
      #
      vax_def0 <- scri_data_parameters( data =  data_vax,   vax_name  = "vax_number",       vax_time = "vax_days",        vax_date     = "vax_date", 
                                        id   = "person_id", start_obs = "study_entry_days", end_obs  = "study_exit_days", censored_vars = "death_days" )
      
      extra_options_dist$extra_name <- vax_def0$data_parameters$vax_name
      characteristics(data=data_vax, event=iae, path_file_name=paste0(sdr_dist,"baseline_vax_number.txt"), vax_name="vax_number", condition_value=vax_def0$data_parameters$vax_name )
      
      ###########  vax_number & dist  ##### 
      # 
      ## cut_points_name="28d" :  { [-91;-30], [-29;-1], [0;0], [1;28], [28;61] } 
      vax_def  <- define_rws(vax_def0,  cut_points_before = c(-90,-29,0), cut_points_after = c(0,1,29,62), cut_points_name="28d",   no_last_interval_after=T, 
                             data=data_vax, vax_dep = c( after="dist_gt_60" ) )
      res <- scri( formula = "~ lab", vax_def = vax_def, data = data_vax, event_info=event_info,  extra_parameters = extra_options_dist, add_to_itself=F  )        
      
      ###########  vax_number & brand wih distance:  ##### 
      # 
      ## cut_points_name="28d" :  { [-91;-30], [-29;-1], [0;0], [1;28], [28;61]  } 
      vax_def <- define_rws(vax_def0,  cut_points_before = c(-90,-29,0), cut_points_after = c(0,1,29,62), cut_points_name="28d", no_last_interval_after=T, 
                            data=data_vax, vax_dep = c( before="vax_brand_short", after="dist_gt_60"  ))
      res <- scri( formula = "~ brand:lab", vax_def = vax_def, data = data_vax, event_info=event_info, extra_parameters = extra_options_dist, add_to_itself=F )
      
      ## cut_points_name="7d"
      vax_def <- define_rws(vax_def0,  cut_points_before = c(-90,-29,0), cut_points_after = c(0,1,8,15,22,29), cut_points_name="7d", no_last_interval_after=T, 
                            data=data_vax, vax_dep = c( before="vax_brand_short", after="dist_gt_60"  ))
      res <- scri( formula = "~ brand:lab", vax_def = vax_def, data = data_vax, event_info=event_info, extra_parameters = extra_options_dist, add_to_itself=T )
      
      extra_options_dist$extra_name <- ""
      
      ###########################################################################################
      #
      #             vax_name="vax_name": dose1.1, dose1.2, boost1, boost2, ...
      #
      vax_def0 <- scri_data_parameters( data =  data_vax,   vax_name  = "vax_name",         vax_time = "vax_days",        vax_date     = "vax_date", 
                                        id   = "person_id", start_obs = "study_entry_days", end_obs  = "study_exit_days", censored_vars = "death_days" )
      extra_options_dist$extra_name <- vax_def0$data_parameters$vax_name
      characteristics(data=data_vax, event=iae, path_file_name=paste0(sdr_dist,"baseline_vax_name.txt"), vax_name="vax_name", condition_value=vax_def0$data_parameters$vax_name )
      
      ###########  vax_name & no split  ##### 
      # 
      ## cut_points_name="28d" :  { [-91;-30], [-29;-1], [0;0], [1;28], [28;61], [62;181], >181 }
      vax_def  <- define_rws(vax_def0,  cut_points_before = c(-90,-29,0), cut_points_after = c(0,1,29,62,182), cut_points_name="28d",   data=data_vax )
      res <- scri( formula = "~ lab", vax_def = vax_def, data = data_vax, event_info=event_info,  extra_parameters = extra_options_dist, add_to_itself=F  )        
      
      ###########  vax_name & brand ( wihout distance ):  ##### 
      # 
      ## cut_points_name="28d" :  { [-91;-30], [-29;-1], [0;0], [1;28], [28;61], [62;181]  } 
      vax_def <- define_rws(vax_def0,  cut_points_before = c(-90,-29,0), cut_points_after = c(0,1,29,62,182), cut_points_name="28d", no_last_interval_after=T, 
                            data=data_vax, vax_dep = c( before="vax_brand_short" ))
      res <- scri( formula = "~ brand:lab", vax_def = vax_def, data = data_vax, event_info=event_info, extra_parameters = extra_options_dist, add_to_itself=F )
      
      ## cut_points_name="7d"
      vax_def <- define_rws(vax_def0,  cut_points_before = c(-90,-29,0), cut_points_after = c(0,1,8,15,22,29), cut_points_name="7d", no_last_interval_after=T, 
                            data=data_vax, vax_dep = c( before="vax_brand_short" ))
      res <- scri( formula = "~ brand:lab", vax_def = vax_def, data = data_vax, event_info=event_info, extra_parameters = extra_options_dist, add_to_itself=T )
      
      
      ###########  vax_name & brand wih distance:  ##### 
      # 
      ## cut_points_name="28d" :  { [-91;-30], [-29;-1], [0;0], [1;28], [28;61]  } 
      vax_def <- define_rws(vax_def0,  cut_points_before = c(-90,-29,0), cut_points_after = c(0,1,29,62), cut_points_name="28d", no_last_interval_after=T, 
                            data=data_vax, vax_dep = c( before="vax_brand_short", after="dist_gt_60"  ))
      res <- scri( formula = "~ brand:lab", vax_def = vax_def, data = data_vax, event_info=event_info, extra_parameters = extra_options_dist, add_to_itself=F )
      
      ## cut_points_name="7d"
      vax_def <- define_rws(vax_def0,  cut_points_before = c(-90,-29,0), cut_points_after = c(0,1,8,15,22,29), cut_points_name="7d", no_last_interval_after=T, 
                            data=data_vax, vax_dep = c( before="vax_brand_short", after="dist_gt_60"  ))
      res <- scri( formula = "~ brand:lab", vax_def = vax_def, data = data_vax, event_info=event_info, extra_parameters = extra_options_dist, add_to_itself=T )
      
      extra_options_dist$extra_name <- ""
      
      
      ###########################################################################################
      #
      #             for combination of brands (historical)
      #
      #
      vax_def0 <- scri_data_parameters( data =  data_vax,   vax_name  = "vax_number",       vax_time = "vax_days",        vax_date     = "vax_date", 
                                        id   = "person_id", start_obs = "study_entry_days", end_obs  = "study_exit_days", censored_vars = "death_days" )
      extra_options_dist$extra_name <- vax_def0$data_parameters$vax_name
      
      ###########  vax_number & combination of previous and current brand :  ##### 
      # 
      ## cut_points_name="28d" :  { [-91;-30], [-29;-1], [0;28]  } 
      vax_def <- define_rws(vax_def0,  cut_points_before = c(-90,-29,0), cut_points_after = c(0,29), cut_points_name="28d", no_last_interval_after=T, 
                            data=data_vax, vax_dep = c( before="type_with_prev"  ))
      # without formula:
      res <- try( scri( formula = "~ brand:lab", vax_def = vax_def, data = data_vax, event_info=event_info, extra_parameters = extra_options_dist, add_to_itself=F ) )
      if(class(res)[[1]]== "try-error") forest_plots_tab(res[[1]][[1]][[1]])      
      
      ###########  vax_number & brand history sorted, or ignore order, i.e. you don't know which one was the first, which one was the second, ...  ##### 
      # 
      ## cut_points_name="28d" :  { [-91;-30], [-29;-1], [0;28]  } 
      vax_def <- define_rws(vax_def0,  cut_points_before = c(-90,-29,0), cut_points_after = c(0,29), cut_points_name="28d", no_last_interval_after=T, 
                            data=data_vax, vax_dep = c( before="type_history_sorted"  ))
      res <- try( scri( formula = "~ brand:lab", vax_def = vax_def, data = data_vax, event_info=event_info, extra_parameters = extra_options_dist, add_to_itself=F ) )
      
      
      ###########  vax_number & brand history  ##### 
      # 
      ## cut_points_name="28d" :  { [-91;-30], [-29;-1], [0;28]  } 
      vax_def <- define_rws(vax_def0,  cut_points_before = c(-90,-29,0), cut_points_after = c(0,29), cut_points_name="28d", no_last_interval_after=T, 
                            data=data_vax, vax_dep = c( before="type_history"  ))
      res <- try( scri( formula = "~ brand:lab", vax_def = vax_def, data = data_vax, event_info=event_info, extra_parameters = extra_options, add_to_itself=F ) )
      
      extra_options_dist$extra_name <- ""
      
      
      
      
      ########################################################
      #
      #    analyse for "age30","age30_50","sexc","sex_age30"
      #
      if( !all( c("age30","age30_50","sexc","sex_age30") %in% names(data_vax)) ){
        data_vax$age30     <- paste0("age",as.character(cut(data_vax$age_at_study_entry, c(-1,30,   Inf)))); data_vax$age30[   is.na(data_vax$age_at_study_entry)] <- NA
        data_vax$age30_50  <- paste0("age",as.character(cut(data_vax$age_at_study_entry, c(-1,30,50,Inf)))); data_vax$age30_50[is.na(data_vax$age_at_study_entry)] <- NA
        
        if("o" %in% tolower(data_vax$sex))  data_vax$sex[tolower(data_vax$sex)=="o"]<-NA
        data_vax$sexc      <- paste0("sex:",data_vax$sex );                     data_vax$sexc[     is.na(data_vax$sex)                        ] <- NA
        data_vax$sex_age30 <- paste0("sex:",data_vax$sex, " ", data_vax$age30); data_vax$sex_age30[is.na(data_vax$sex) | is.na(data_vax$age30)] <- NA
      }
      
      # strata variables:
      for( strata_var in c( "age30","age30_50", "sexc", "sex_age30") ){
        
        # SCCS output_subdirectory 'distance_combi' in 'event' directory 
        sdr_dist_stratum <- paste0(sdr0, iae, "/distance_combi/",ifelse(strata_var%in%c("age30","age30_50"), "age", strata_var), "/" )
        dir.create(sdr_dist_stratum, showWarnings = FALSE, recursive = TRUE)
        
        # SCCS output_subdirectory 'distance_combi' in the event directory: 
        sdr_dist_stratum_models <- paste0(sdr_models0, iae,"/distance_combi/",ifelse(strata_var%in%c("age30","age30_50"), "age", strata_var), "/" )
        dir.create(sdr_dist_stratum_models, showWarnings = FALSE, recursive = TRUE)
        
        #########################
        #   copy extra_options  and 
        #   change  'sdr_tabs' and 'sdr_models' parameters    
        #
        extra_options_dist$sdr_tabs   <- sdr_dist_stratum
        extra_options_dist$sdr_models <- sdr_dist_stratum_models
        
        
        # values of the current strata variable
        strata_values <- unique(data_vax[,strata_var]); strata_values <- strata_values[!is.na(strata_values)]
        
        # delete strata "age(-1,30]" if variable 'age30_50'  because it is also in 'age30'
        if(strata_var=="age30_50") strata_values <- strata_values[strata_values!="age(-1,30]"]
        
        for(strata_value in strata_values){ 
          
          
          
          ###########################################################################################
          ###################################  event  ############################################### 
          #   
          event_info <- list( event=iae, event_time =paste0(iae,"_days"), event_date =paste0(iae,"_date") )
          
          
          ###########################################################################################
          #
          #             vax_name="vax_number": dose1, dose2, dose3, ...
          #
          vax_def0 <- scri_data_parameters( data =  data_vax,   vax_name  = "vax_number",       vax_time = "vax_days",        vax_date     = "vax_date", 
                                            id   = "person_id", start_obs = "study_entry_days", end_obs  = "study_exit_days", censored_vars = "death_days" )
          extra_options_dist$extra_name <- vax_def0$data_parameters$vax_name
          
          ###########  vax_number & dist  ##### 
          # 
          ## cut_points_name="28d" :  { [-91;-30], [-29;-1], [0;0], [1;28], [28;61] } 
          vax_def  <- define_rws(vax_def0,  cut_points_before = c(-90,-29,0), cut_points_after = c(0,1,29,62), cut_points_name="28d",   no_last_interval_after=T, 
                                 data=data_vax, vax_dep = c( after="dist_gt_60" ) )
          res <- scri( formula = "~ lab", vax_def = vax_def, data = data_vax, strata_var=strata_var, strata_value=strata_value, 
                       event_info=event_info,  extra_parameters = extra_options_dist, add_to_itself=F  )        
          
          ###########  vax_number & brand wih distance:  ##### 
          # 
          ## cut_points_name="28d" :  { [-91;-30], [-29;-1], [0;0], [1;28], [28;61]  } 
          vax_def <- define_rws(vax_def0,  cut_points_before = c(-90,-29,0), cut_points_after = c(0,1,29,62), cut_points_name="28d", no_last_interval_after=T, 
                                data=data_vax, vax_dep = c( before="vax_brand_short", after="dist_gt_60"  ))
          res <- scri( formula = "~ brand:lab", vax_def = vax_def, data = data_vax, strata_var=strata_var, strata_value=strata_value, 
                       event_info=event_info, extra_parameters = extra_options_dist, add_to_itself=F )
          
          ## cut_points_name="7d"
          vax_def <- define_rws(vax_def0,  cut_points_before = c(-90,-29,0), cut_points_after = c(0,1,8,15,22,29), cut_points_name="7d", no_last_interval_after=T, 
                                data=data_vax, vax_dep = c( before="vax_brand_short", after="dist_gt_60"  ))
          res <- scri( formula = "~ brand:lab", vax_def = vax_def, data = data_vax, strata_var=strata_var, strata_value=strata_value, 
                       event_info=event_info, extra_parameters = extra_options_dist, add_to_itself=T )
          
          extra_options_dist$extra_name <- ""
          
          
          ###########################################################################################
          #
          #             vax_name="vax_name": dose1.1, dose1.2, boost1, boost2, ...
          #
          vax_def0 <- scri_data_parameters( data =  data_vax,   vax_name  = "vax_name",         vax_time = "vax_days",        vax_date     = "vax_date", 
                                            id   = "person_id", start_obs = "study_entry_days", end_obs  = "study_exit_days", censored_vars = "death_days" )
          extra_options_dist$extra_name <- vax_def0$data_parameters$vax_name
          
          ###########  vax_name & no split  ##### 
          # 
          ## cut_points_name="28d" :  { [-91;-30], [-29;-1], [0;0], [1;28], [28;61], [62;181], >181 }
          vax_def  <- define_rws(vax_def0,  cut_points_before = c(-90,-29,0), cut_points_after = c(0,1,29,62,182), cut_points_name="28d",   data=data_vax )
          res <- scri( formula = "~ lab", vax_def = vax_def, data = data_vax, strata_var=strata_var, strata_value=strata_value,
                       event_info=event_info,  extra_parameters = extra_options_dist, add_to_itself=F  )        
          
          ###########  vax_name & brand ( wihout distance ):  ##### 
          # 
          ## cut_points_name="28d" :  { [-91;-30], [-29;-1], [0;0], [1;28], [28;61], [62;181]  } 
          vax_def <- define_rws(vax_def0,  cut_points_before = c(-90,-29,0), cut_points_after = c(0,1,29,62,182), cut_points_name="28d", no_last_interval_after=T, 
                                data=data_vax, vax_dep = c( before="vax_brand_short" ))
          res <- scri( formula = "~ brand:lab", vax_def = vax_def, data = data_vax, strata_var=strata_var, strata_value=strata_value,
                       event_info=event_info, extra_parameters = extra_options_dist, add_to_itself=F )
          
          ## cut_points_name="7d"
          vax_def <- define_rws(vax_def0,  cut_points_before = c(-90,-29,0), cut_points_after = c(0,1,8,15,22,29), cut_points_name="7d", no_last_interval_after=T, 
                                data=data_vax, vax_dep = c( before="vax_brand_short" ))
          res <- scri( formula = "~ brand:lab", vax_def = vax_def, data = data_vax, strata_var=strata_var, strata_value=strata_value,
                       event_info=event_info, extra_parameters = extra_options_dist, add_to_itself=T )
          
          
          ###########  vax_name & brand wih distance:  ##### 
          # 
          ## cut_points_name="28d" :  { [-91;-30], [-29;-1], [0;0], [1;28], [28;61]  } 
          vax_def <- define_rws(vax_def0,  cut_points_before = c(-90,-29,0), cut_points_after = c(0,1,29,62), cut_points_name="28d", no_last_interval_after=T, 
                                data=data_vax, vax_dep = c( before="vax_brand_short", after="dist_gt_60"  ))
          res <- scri( formula = "~ brand:lab", vax_def = vax_def, data = data_vax, strata_var=strata_var, strata_value=strata_value,
                       event_info=event_info, extra_parameters = extra_options_dist, add_to_itself=F )
          
          ## cut_points_name="7d"
          vax_def <- define_rws(vax_def0,  cut_points_before = c(-90,-29,0), cut_points_after = c(0,1,8,15,22,29), cut_points_name="7d", no_last_interval_after=T, 
                                data=data_vax, vax_dep = c( before="vax_brand_short", after="dist_gt_60"  ))
          res <- scri( formula = "~ brand:lab", vax_def = vax_def, data = data_vax, strata_var=strata_var, strata_value=strata_value,
                       event_info=event_info, extra_parameters = extra_options_dist, add_to_itself=T )
          
          extra_options_dist$extra_name <- ""
          
          
          ###########################################################################################
          #
          #             for combination of brands (historical)
          #
          #
          vax_def0 <- scri_data_parameters( data =  data_vax,   vax_name  = "vax_number",       vax_time = "vax_days",        vax_date     = "vax_date", 
                                            id   = "person_id", start_obs = "study_entry_days", end_obs  = "study_exit_days", censored_vars = "death_days" )
          extra_options_dist$extra_name <- vax_def0$data_parameters$vax_name
          
          ###########  vax_number & combination of previous and current brand :  ##### 
          # 
          ## cut_points_name="28d" :  { [-91;-30], [-29;-1], [0;28]  } 
          vax_def <- define_rws(vax_def0,  cut_points_before = c(-90,-29,0), cut_points_after = c(0,29), cut_points_name="28d", no_last_interval_after=T, 
                                data=data_vax, vax_dep = c( before="type_with_prev"  ))
          # without formula:
          res <- try( scri( formula = "~ brand:lab", vax_def = vax_def, data = data_vax, strata_var=strata_var, strata_value=strata_value,
                            event_info=event_info, extra_parameters = extra_options_dist, add_to_itself=F ) )
          if(class(res)[[1]]== "try-error") forest_plots_tab(res[[1]][[1]][[1]])      
          
          ###########  vax_number & brand history sorted, or ignore order, i.e. you don't know which one was the first, which one was the second, ...  ##### 
          # 
          ## cut_points_name="28d" :  { [-91;-30], [-29;-1], [0;28]  } 
          vax_def <- define_rws(vax_def0,  cut_points_before = c(-90,-29,0), cut_points_after = c(0,29), cut_points_name="28d", no_last_interval_after=T, 
                                data=data_vax, vax_dep = c( before="type_history_sorted"  ))
          res <- try( scri( formula = "~ brand:lab", vax_def = vax_def, data = data_vax, strata_var=strata_var, strata_value=strata_value,
                            event_info=event_info, extra_parameters = extra_options_dist, add_to_itself=F ) )
          
          
          ###########  vax_number & brand history  ##### 
          # 
          ## cut_points_name="28d" :  { [-91;-30], [-29;-1], [0;28]  } 
          vax_def <- define_rws(vax_def0,  cut_points_before = c(-90,-29,0), cut_points_after = c(0,29), cut_points_name="28d", no_last_interval_after=T, 
                                data=data_vax, vax_dep = c( before="type_history"  ))
          res <- try( scri( formula = "~ brand:lab", vax_def = vax_def, data = data_vax, strata_var=strata_var, strata_value=strata_value,
                            event_info=event_info, extra_parameters = extra_options, add_to_itself=F ) )
          
          extra_options_dist$extra_name <- ""
          
        }
      }
      
    } # end of ldist
    
  } # end iae      
  ##########
  # restore options:
  options(old_width)
  
  
}  # end of 'subpop' loop


