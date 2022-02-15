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
#               function: scri_create_input
#               function: add_to_report_list
#               function: add_to_models_list
#
#  

####       print_during_running <- F    #  T or F


# Housekeeping  -----------------------------------------------------------
# install and load packages

if(!any(ls()=="thisdir"))   thisdir   <- getwd()
if(!any(ls()=="dirtemp"))   dirtemp   <- paste0(thisdir,"/g_intermediate/")

# ensure required folders are created  
dir.create(file.path(paste0(dirtemp, "scri")),           showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(paste0(thisdir,"/log_files/scri")), showWarnings = FALSE, recursive = TRUE)

for (subpop in subpopulations_non_empty) {
  
  thisdirexp <- ifelse(this_datasource_has_subpopulations == FALSE, direxp, direxpsubpop[[subpop]])
  
  # SCCS output_directory  
  sdr <- paste0(thisdirexp, "scri/scri/")
  dir.create(sdr, showWarnings = FALSE, recursive = TRUE)
  

  # Import Data -------------------------------------------------------------
  load(paste0(dirtemp, "scri/", intermediate_data, suffix[[subpop]], ".RData"))
  temp_name<-get(paste0(intermediate_data, suffix[[subpop]]))
  rm(list=paste0(intermediate_data, suffix[[subpop]]))
  assign(intermediate_data, as.data.frame(temp_name))
  rm(temp_name)

  load(paste0(dirtemp, "nvax.RData"))

  if(F){
  
  dir.create(file.path(paste0(thisdirexp, "scri")),  showWarnings = FALSE, recursive = TRUE)
    
  # SCCS output_directory  
  sdr <- paste0(thisdirexp, "scri/scri/")
  dir.create(sdr, showWarnings = FALSE, recursive = TRUE)
  
   # Import Data -------------------------------------------------------------
  load(paste0(dirtemp, "scri/", intermediate_data, ".RData"))
  scri_data_extract <- eval(parse(text = intermediate_data))
  
  } 
  
  scri_input <- scri_data_extract

  dap <- ifelse( any(names(scri_input)=="DAP"), scri_input$DAP[1], "")
  


#############   SCRI models ############################
#
#
#

##########
#              (vax2 takes precedence over vax1)
#
#   the risk window of dose 2 takes precedence over the risk window of dose 1

old_width = options(width=200)
print_during_running <- T



##########################################
#
#      definitions of risk windows:
#

lab_pre  <- c( "pre-exposure",  "buffer" )

vax1_end_before_vax2 <- substitute(pmin(days_vax2-1, study_exit_days, na.rm=T))
vax2_end_before_vax3 <- substitute(pmin(days_vax3-1, study_exit_days, na.rm=T))

rws_def_2vax_28 <- substitute(list(  prevax  = list( t0 = days_vax1, cuts = c( -90, -29, 0),  lab  = lab_pre , lab_add_interval=T, no_last_interval=T   ), # also can be added:    add_interval = T 
                                     v1      = list( t0 = days_vax1, cuts = c( 0, 1, 29 ),    tend = vax1_end_before_vax2,  lab = "dose 1 "   ), 
                                     v2      = list( t0 = days_vax2, cuts = c( 0, 1, 29 ),    tend = study_exit_days,       lab = "dose 2 "   ))
                              #all      = list( t0 = study_entry_days, tend=study_exit_days,     lab = "all", lab_add_interval=F   )
)
rws_def_2vax_7  <- substitute(list(  prevax  = list( t0 = days_vax1, cuts = c( -90, -29, 0),  lab  = lab_pre , lab_add_interval=T, no_last_interval=T   ), # also can be added:    add_interval = T 
                                     v1      = list( t0 = days_vax1, cuts = c( 0, 1, 8, 15, 29, 61, 181 ),    tend = vax1_end_before_vax2,  lab = "dose 1 "   ), 
                                     v2      = list( t0 = days_vax2, cuts = c( 0, 1, 8, 15, 29, 61, 181 ),    tend = study_exit_days,       lab = "dose 2 "   ))
                              #all      = list( t0 = study_entry_days, tend=study_exit_days,     lab = "all", lab_add_interval=F   )
)

rws_def_3vax_28 <- substitute(list(prevax  = list( t0 = days_vax1, cuts = c( -90, -29, 0),               lab  = lab_pre , lab_add_interval=T, no_last_interval=T   ), # also can be added:    add_interval = T 
                                   v1      = list( t0 = days_vax1, cuts = c( 0, 1, 29 ), tend = vax1_end_before_vax2,  lab = "dose 1 "   ), 
                                   v2      = list( t0 = days_vax2, cuts = c( 0, 1, 29 ), tend = vax2_end_before_vax3,  lab = "dose 2 "   ), 
                                   v3      = list( t0 = days_vax3, cuts = c( 0, 1, 29 ), tend = study_exit_days,       lab = "dose 3 "   ))
)
rws_def_3vax_7  <- substitute(list(prevax  = list( t0 = days_vax1, cuts = c( -90, -29, 0),               lab  = lab_pre , lab_add_interval=T, no_last_interval=T   ), # also can be added:    add_interval = T 
                                   v1      = list( t0 = days_vax1, cuts = c( 0, 1, 8, 15, 29, 61, 181 ), tend = vax1_end_before_vax2,  lab = "dose 1 "   ), 
                                   v2      = list( t0 = days_vax2, cuts = c( 0, 1, 8, 15, 29, 61, 181 ), tend = vax2_end_before_vax3,  lab = "dose 2 "   ), 
                                   v3      = list( t0 = days_vax3, cuts = c( 0, 1, 8, 15, 29, 61, 181 ), tend = study_exit_days,       lab = "dose 3 "   ))
)


# with overlap & the last intervals last till the end of observation !!!
rws_def_3vax_overlap_28 <- substitute(list(  prevax  = list( t0 = days_vax1, cuts = c( -90, -29, 0),          lab  = lab_pre , no_last_interval=T   ), # also can be added:    add_interval = T 
                                            v1      = list( t0 = days_vax1, cuts = c( 0, 1, 29, 61 ), tend = study_exit_days,  lab = "dose 1 ", no_last_interval=F    ), 
                                            v2      = list( t0 = days_vax2, cuts = c( 0, 1, 29, 61 ), tend = study_exit_days,  lab = "dose 2 ", no_last_interval=F    ), 
                                            v3      = list( t0 = days_vax3, cuts = c( 0, 1, 29, 61 ), tend = study_exit_days,  lab = "dose 3 ", no_last_interval=F    ))
)
rws_def_3vax_overlap_7  <- substitute(list(  prevax  = list( t0 = days_vax1, cuts = c( -90, -29, 0),          lab  = lab_pre , no_last_interval=T   ), # also can be added:    add_interval = T 
                                            v1      = list( t0 = days_vax1, cuts = c( 0, 1, 8, 15, 29, 61 ), tend = study_exit_days,  lab = "dose 1 ", no_last_interval=F    ), 
                                            v2      = list( t0 = days_vax2, cuts = c( 0, 1, 8, 15, 29, 61 ), tend = study_exit_days,  lab = "dose 2 ", no_last_interval=F    ), 
                                            v3      = list( t0 = days_vax3, cuts = c( 0, 1, 8, 15, 29, 61 ), tend = study_exit_days,  lab = "dose 3 ", no_last_interval=F    ))
)
rws_def_2vax_overlap_28 <- substitute(list(  prevax  = list( t0 = days_vax1, cuts = c( -90, -29, 0),          lab  = lab_pre , no_last_interval=T   ), # also can be added:    add_interval = T 
                                            v1      = list( t0 = days_vax1, cuts = c( 0, 1, 29, 61 ), tend = study_exit_days,  lab = "dose 1 ", no_last_interval=F    ), 
                                            v2      = list( t0 = days_vax2, cuts = c( 0, 1, 29, 61 ), tend = study_exit_days,  lab = "dose 2 ", no_last_interval=F    )) 
)
rws_def_2vax_overlap_7  <- substitute(list(  prevax  = list( t0 = days_vax1, cuts = c( -90, -29, 0),          lab  = lab_pre , no_last_interval=T   ), # also can be added:    add_interval = T 
                                            v1      = list( t0 = days_vax1, cuts = c( 0, 1, 8, 15, 29, 61 ), tend = study_exit_days,  lab = "dose 1 ", no_last_interval=F    ), 
                                            v2      = list( t0 = days_vax2, cuts = c( 0, 1, 8, 15, 29, 61 ), tend = study_exit_days,  lab = "dose 2 ", no_last_interval=F    )) 
)

# with overlap & with cutting off the last intervals, i.e., not until the end of observation !!!
rws_def_3vax_overlap_7_bounded <- substitute(list(  
                                          prevax  = list( t0 = days_vax1, cuts = c( -90, -29, 0),          lab = lab_pre ,  no_last_interval=T   ), # also can be added:    add_interval = T 
                                          v1      = list( t0 = days_vax1, cuts = c( 0, 1, 8, 15, 29, 61 ), lab = "dose 1 ", no_last_interval=T    ), 
                                          v2      = list( t0 = days_vax2, cuts = c( 0, 1, 8, 15, 29, 61 ), lab = "dose 2 ", no_last_interval=T    ), 
                                          v3      = list( t0 = days_vax3, cuts = c( 0, 1, 8, 15, 29, 61 ), lab = "dose 3 ", no_last_interval=T    ))
)



##########################################
#
#      definitions of time_dependent variables :
#

########## variable 'brand'   ##############
brand12_def <- substitute(list( split_name  = "vax12", 
                                split       = cbind.data.frame( days_vax1, days_vax2),  
                                lab         = c("no_vax","dose 1","dose 2"),
                                change      = list( name         = "brand12",
                                                    #add_begin_sep = "_",
                                                    replace       = list( c(value="dose 1", var_name="type_vax1"),
                                                                          c(value="dose 2", var_name="type_vax2") )
                                              ),
                                ref         = "pre-"))


brand123_def <- substitute(list( split_name  = "vax123", 
                                 split       = cbind.data.frame( days_vax1, days_vax2, days_vax3),  
                                 lab         = c("no_vax","dose 1","dose 2","dose 3"),
                                 change      = list( name          = "brand123",
                                                     #add_begin_sep = "_",
                                                     replace       = list( c(value="dose 1", var_name="type_vax1"),
                                                                           c(value="dose 2", var_name="type_vax2"),
                                                                           c(value="dose 3", var_name="type_vax3") )
                                 ),     
                                 ref         = "pre-"))


########## variable 'cumulative brand'   ##############

scri_input$type_vax1_d1  <- paste0( "d1:",substring(scri_input$type_vax1,1,4) )
scri_input$type_vax12[ !is.na(scri_input$type_vax2)] <- paste0( scri_input$type_vax1_d1, "_d2:", substring(scri_input$type_vax2,1,4))[!is.na(scri_input$type_vax2)]
scri_input$type_vax123[!is.na(scri_input$type_vax3)] <- paste0( scri_input$type_vax12,   "_d3:", substring(scri_input$type_vax3,1,4))[!is.na(scri_input$type_vax3)]

brand123cum_def <- substitute(list( 
  split_name  = "vax123", 
  split       = cbind.data.frame( days_vax1, days_vax2, days_vax3),  
  lab         = c("no_vax","dose 1","dose 2","dose 3"),
  change      = list( name          = "brand123cum",
                      #add_begin_sep = "_",
                      replace       = list( c(value="dose 1", var_name="type_vax1_d1"),
                                            c(value="dose 2", var_name="type_vax12"),
                                            c(value="dose 3", var_name="type_vax123") )
  ),     
  ref         = "pre-"))

brand12cum_def <- substitute(list( 
  split_name  = "vax12", 
  split       = cbind.data.frame( days_vax1, days_vax2 ),  
  lab         = c("no_vax","dose 1","dose 2"),
  change      = list( name          = "brand12cum",
                      #add_begin_sep = "_",
                      replace       = list( c(value="dose 1", var_name="type_vax1_d1"),
                                            c(value="dose 2", var_name="type_vax12") )
  ),     
  ref         = "pre-"))

#
########################################################

########################################
#
# specify calendar time interval for adjusting:
#
lengths <- c( 7,14,10, 21,30)
#lengths <- c( 3,7,14,10, 21,30)
starts  <- c(-1,-2,-8,-1,-10,-1)
time_seq <- vector("list",length=length(lengths))
names(time_seq) <- paste0("period",lengths,"d")

for(i in 1:length(lengths))
  time_seq[[i]] <- seq(min(scri_input$study_entry_days,na.rm=T)-starts[i],max(scri_input$study_exit_days,na.rm=T)+lengths[i]-1,by=lengths[i])

########################################
#
#    define order of coefficient labels:
#
lab_orders <- list(  
  c("F", "M" ),
  c("sex0","sex:0","sexF","sex:F", "sex1", "sex:1" , "sexM", "sex:M" ),
  c(" (-1,30]","(30,40]","(30,50]","(30,Inf]",">=30",">30","(40,50]","(50,60]","(50,65]","(50,Inf]",">=50",">50",">=60",">60","(60,Inf]", ">65" ),
  c("d1:","d2:","d3:" ),
  c("Pfi","Mod","Ast", "JJ","J&J" ),
  c("Pfizer","Moderna","AstraZeneca", "JJ","J&J" ),
  c("pre-","buf", "dose 1", "dose 2", "dose 3" ),
  c("[0;0]","[1;7]","[1;28]","[1;14]","[8;14]","[15;28]",">28","[29;60]",">60","[61;180]", ">180")
)


# during testing: may use only one vector to adjust for calendar time, for example, time_seq[5]:
# time_seq <- time_seq[5]



#################################################
#################################################
#
#  run scri analysis
# 


print_during_running <- T
plot_during_running <- F

col_list <- c("red",palette()[-(1:2)] ) 



#ae_events <- c("myocarditis","pericarditis","myopericarditis")
ae_events <- c("myocarditis","pericarditis")

glob_analysis_name <-"all_data" 



########### no strata: #############
#

# for(iae in ae_events)
#   brand_images( plot_data, ae_event=iae, tit="")



models_list <- list()
report_list <- list()

first_plot <- T
for(iae in ae_events){
  ae_event_first <- T
  for(iii in ifelse(nvax>=3,4,2):1){
    
    if(iii==1) rws_def <- rws_def_2vax_28
    if(iii==2) rws_def <- rws_def_2vax_7
  
    if(iii==3) rws_def <- rws_def_3vax_28
    if(iii==4) rws_def <- rws_def_3vax_7
    
    vax_priority <- "_vax2_priority"
    specif_name<-"_no_split" 
    
    output_name <- paste0(substring(iae,1,7),vax_priority,specif_name,"_",ifelse(iii %in% (1:2), 2,3),"v", ifelse(iii %in% c(1,3), "_28","_7"))
    global_name <- paste0( specif_name )
    
    formula_text <-  "~ lab"

    res <- scri_strata( output_name  = output_name, 
                      formula_text = formula_text,       time_seq = time_seq, 
                      event_time = paste0(iae,"_days"), event = iae, id="person_id",
                      rws          = rws_def,
                      start_obs    = "study_entry_days", end_obs = "study_exit_days",
                      data         = scri_input,
                      image_plots = ae_event_first,
                      lab_orders = lab_orders,
                      lprint = print_during_running,
                      global_plot_name = global_name, add_global_plot = !first_plot
    )
  first_plot <- F
  ae_event_first <- F
  
  report_list <- add_to_report_list(res$tabs,     output_name)
  models_list <- add_to_models_list(res$scri_all, output_name)
  }
}
# plots:
if(plot_during_running) 
  for(istr in names(res$tabs) )
    if(!is.null(res$tabs[[istr]][[1]]))
      plot_res(res$tabs[[istr]], main=paste(formula_text," + cal_time_cat"), col=col_list)


###########
#  save report_list and model_list
save_results(global_name, report_list, models_list)
#
#######################################################################################


########### per first brand:  all brands in one model #############
#

# for(iae in ae_events){
#   
#   for(im_str in unique(scri_input$type_vax1) )
#     brand_images( scri_input[ scri_input$type_vax1==im_str, ], ae_event=iae, tit=paste("type_1=",im_str) )
#     #brand_3Dplots(plot_data, ae_event=iae, tit="")
# }

models_list <- list()
report_list <- list()

first_plot <- T
for(iae in ae_events){
  ae_event_first <- T
  for(iii in ifelse(nvax>=3,4,2):1){
    
    if(iii==1) rws_def <- rws_def_2vax_28
    if(iii==2) rws_def <- rws_def_2vax_7
    
    if(iii==3) rws_def <- rws_def_3vax_28
    if(iii==4) rws_def <- rws_def_3vax_7

    formula_text <-  "~ type_vax1:lab"
    
    vax_priority <- "_vax2_priority"
    specif_name<-"_first_brands" 
    
    output_name  <- paste0(substring(iae,1,7),vax_priority,specif_name,"_",ifelse(iii %in% (1:2), 2,3),"v", ifelse(iii %in% c(1,3), "_28","_7"))
    global_name  <- paste0( specif_name )
    
    res <- scri_strata( output_name  = output_name, 
                        formula_text = formula_text,       time_seq = time_seq, 
                        event_time = paste0(iae,"_days"), event = iae, id="person_id",
                        rws          = rws_def,
                        start_obs    = "study_entry_days", end_obs = "study_exit_days",
                        data         = scri_input,
                        image_plots = ae_event_first, image_strata="type_vax1", image_tit="type_1:",
                        lab_orders = lab_orders,
                        lprint = print_during_running,
                        global_plot_name = global_name, add_global_plot = !first_plot
    )
    first_plot <- F
    ae_event_first <- F
    
  report_list <- add_to_report_list(res$tabs,     output_name)
  models_list <- add_to_models_list(res$scri_all, output_name)
  }
}  

# plots:
if(plot_during_running) 
  for(istr in names(res$tabs) )
    if(!is.null(res$tabs[[istr]][[1]]))
      plot_res(res$tabs[[istr]], main=paste(formula_text," + cal_time_cat"), col=col_list)
###########
#  save report_list and model_list
save_results(global_name, report_list, models_list)
#
#######################################################################################



########### per brand:  all brands in one model #############

#output_name  <- "per_brand_one_model"
#formula_text <-  "~ brand123:lab"
#formula_text <-  "~ brand12:lab"
    

for(iae in ae_events){
 for(im_br in unique(c(scri_input$type_vax1)) )
    brand_images( scri_input, ae_event=iae, brand=im_br )
  #brand_3Dplots(plot_data, ae_event=iae, tit="")
}

models_list <- list()
report_list <- list()

first_plot <- T
for(iae in ae_events){
  ae_event_first <- T
  for(iii in ifelse(nvax>=3,4,2):1){
    
    if(iii==1) { rws_def <- rws_def_2vax_28;  time_dep <- list( brand12_def ); formula_text <-  "~ brand12:lab" }
    if(iii==2) { rws_def <- rws_def_2vax_7 ;  time_dep <- list( brand12_def ); formula_text <-  "~ brand12:lab" }
    
    if(iii==3) { rws_def <- rws_def_3vax_28;  time_dep <- list( brand123_def ); formula_text <-  "~ brand123:lab" }
    if(iii==4) { rws_def <- rws_def_3vax_7 ;  time_dep <- list( brand123_def ); formula_text <-  "~ brand123:lab" }
    
    vax_priority <- "_vax2priority"
    specif_name  <-"_brands" 
    
    global_name  <- paste0( vax_priority, specif_name )
    output_name  <- paste0( "_",substring(iae,1,7), global_name,"_",ifelse(iii %in% (1:2), 2,3),"v", ifelse(iii %in% c(1,3), "_28","_7"))
    
    res <- scri_strata( output_name  = output_name, 
                        formula_text = formula_text,       time_seq = time_seq, 
                        event_time = paste0(iae,"_days"), event = iae, id="person_id",
                        rws          = rws_def,
                        time_dep     = time_dep,                             # list( brand_def, brand12_def)
                        start_obs    = "study_entry_days", end_obs = "study_exit_days",
                        data         = scri_input,
                        image_plots = ae_event_first, image_brand=T,
                        lab_orders   = lab_orders,
                        lprint       = print_during_running,
                        global_plot_name = paste0( substring(iae,1,7),global_name), add_global_plot = !first_plot
    )
    first_plot <- F
    ae_event_first <- F
    
    report_list <- add_to_report_list(res$tabs,     output_name)
    models_list <- add_to_models_list(res$scri_all, output_name)
  }
}  



# plots:
if(plot_during_running) 
  for(istr in names(res$tabs) )
    if(!is.null(res$tabs[[istr]][[1]]))
      plot_res(res$tabs[[istr]], main=paste(formula_text," + cal_time_cat"), col=col_list)

####
#  save report_list and model_list
save_results(global_name, report_list, models_list)
#
#######################################################################################






########### per sex & age30: one model #############
#

if(all(names(scri_input) !="age30"))
  scri_input$age30 <- as.character(cut(scri_input$age_at_study_entry, c(-1,30,Inf)))

scri_input$sex_age30 <- paste0("sex:",scri_input$sex, " ", scri_input$age30)

models_list <- list()
report_list <- list()

first_plot <- T
for(iae in ae_events){
  
  for(istr in unique(scri_input$sex_age30) ){
    
    ae_event_first <- T  
    for(iii in ifelse(nvax>=3,4,2):1){
      
      if(iii==1) { rws_def <- rws_def_2vax_28;  time_dep <- list( brand12_def );  formula_text <-  "~ sex_age30:brand12:lab" }
      if(iii==2) { rws_def <- rws_def_2vax_7 ;  time_dep <- list( brand12_def );  formula_text <-  "~ sex_age30:brand12:lab" }
      
      if(iii==3) { rws_def <- rws_def_3vax_28;  time_dep <- list( brand123_def ); formula_text <-  "~ sex_age30:brand123:lab" }
      if(iii==4) { rws_def <- rws_def_3vax_7 ;  time_dep <- list( brand123_def ); formula_text <-  "~ sex_age30:brand123:lab" }
      
      vax_priority <- "_vax2priority"
      specif_name  <- "_sex_age30" # istr
      
      global_name  <- paste0( vax_priority, specif_name )
      output_name  <- paste0( "_",substring(iae,1,7), global_name,"_",ifelse(iii %in% (1:2), 2,3),"v", ifelse(iii %in% c(1,3), "_28","_7"))
      
      res <- scri_strata(  output_name  = output_name, 
                           formula_text = formula_text,       time_seq = time_seq, 
                           event_time = paste0(iae,"_days"), event = iae, id="person_id",
                           rws          = rws_def,
                           time_dep     = time_dep,                             # list( brand_def, brand12_def)
                           #combine_vars =  c("sex","age4"), 
                           start_obs    = "study_entry_days", end_obs = "study_exit_days", 
                           data         = scri_input[scri_input$sex_age30==istr,],
                           image_plots = ae_event_first, image_brand=T, image_tit=istr,
                           lab_orders   = lab_orders,
                           lprint       = print_during_running,
                           global_plot_name = paste0( substring(iae,1,7),global_name), add_global_plot = !first_plot
      )
      ae_event_first <- F
      first_plot <- F
      
      report_list <- add_to_report_list(res$tabs,     output_name)
      models_list <- add_to_models_list(res$scri_all, output_name)
    }
  }
}  

# plots:
if(plot_during_running) 
  for(istr in names(res$tabs) )
    if(!is.null(res$tabs[[istr]][[1]]))
      plot_res(res$tabs[[istr]], main=paste(formula_text," + cal_time_cat"), col=col_list)

####
#  save report_list and model_list
save_results(global_name, report_list, models_list)
#
#######################################################################################






########### per sex & age30_50: one model #############
#

if(all(names(scri_input) !="age30_50"))
  scri_input$age30_50 <- as.character(cut(scri_input$age_at_study_entry, c(-1,30,50,Inf)))

scri_input$sex_age30_50 <- paste0("sex:",scri_input$sex, " ", scri_input$age30_50)

models_list <- list()
report_list <- list()

first_plot <- T
for(iae in ae_events){
  
  for(istr in unique(scri_input$sex_age30_50) ){
    
    ae_event_first <- T  
    for(iii in ifelse(nvax>=3,4,2):1){
      
      if(iii==1) { rws_def <- rws_def_2vax_28;  time_dep <- list( brand12_def );  formula_text <-  "~ sex_age30_50:brand12:lab" }
      if(iii==2) { rws_def <- rws_def_2vax_7 ;  time_dep <- list( brand12_def );  formula_text <-  "~ sex_age30_50:brand12:lab" }
      
      if(iii==3) { rws_def <- rws_def_3vax_28;  time_dep <- list( brand123_def ); formula_text <-  "~ sex_age30_50:brand123:lab" }
      if(iii==4) { rws_def <- rws_def_3vax_7 ;  time_dep <- list( brand123_def ); formula_text <-  "~ sex_age30_50:brand123:lab" }
      
      vax_priority <- "_vax2priority"
      specif_name  <- "_sex_age30_50" # istr
      
      global_name  <- paste0( vax_priority, specif_name )
      output_name  <- paste0( "_",substring(iae,1,7), global_name,"_",ifelse(iii %in% (1:2), 2,3),"v", ifelse(iii %in% c(1,3), "_28","_7"))
      
      res <- scri_strata(  output_name  = output_name, 
                           formula_text = formula_text,       time_seq = time_seq, 
                           event_time = paste0(iae,"_days"), event = iae, id="person_id",
                           rws          = rws_def,
                           time_dep     = time_dep,                             # list( brand_def, brand12_def)
                           #combine_vars =  c("sex","age4"), 
                           start_obs    = "study_entry_days", end_obs = "study_exit_days", 
                           data         = scri_input[scri_input$sex_age30_50==istr,],
                           image_plots = ae_event_first, image_brand=T, image_tit=istr,
                           lab_orders   = lab_orders,
                           lprint       = print_during_running,
                           global_plot_name = paste0( substring(iae,1,7),global_name), add_global_plot = !first_plot
      )
      ae_event_first <- F
      first_plot <- F
      
      report_list <- add_to_report_list(res$tabs,     output_name)
      models_list <- add_to_models_list(res$scri_all, output_name)
    }
  }
}  

# plots:
if(plot_during_running) 
  for(istr in names(res$tabs) )
    if(!is.null(res$tabs[[istr]][[1]]))
      plot_res(res$tabs[[istr]], main=paste(formula_text," + cal_time_cat"), col=col_list)

####
#  save report_list and model_list
save_results(global_name, report_list, models_list)
#
#######################################################################################








########### per age30_50: one model #############
#

if(all(names(scri_input) !="age30_50"))
  scri_input$age30_50 <- as.character(cut(scri_input$age_at_study_entry, c(-1,30,50,Inf)))


models_list <- list()
report_list <- list()

first_plot <- T
for(iae in ae_events){
  
  for(istr in unique(scri_input$age30_50) ){
    
    ae_event_first <- T  
    for(iii in ifelse(nvax>=3,4,2):1){
      
      if(iii==1) { rws_def <- rws_def_2vax_28;  time_dep <- list( brand12_def );  formula_text <-  "~ age30_50:brand12:lab" }
      if(iii==2) { rws_def <- rws_def_2vax_7 ;  time_dep <- list( brand12_def );  formula_text <-  "~ age30_50:brand12:lab" }
      
      if(iii==3) { rws_def <- rws_def_3vax_28;  time_dep <- list( brand123_def ); formula_text <-  "~ age30_50:brand123:lab" }
      if(iii==4) { rws_def <- rws_def_3vax_7 ;  time_dep <- list( brand123_def ); formula_text <-  "~ age30_50:brand123:lab" }
      
      vax_priority <- "_vax2priority"
      specif_name  <- "_age30_50" # istr
      
      global_name  <- paste0( vax_priority, specif_name )
      output_name  <- paste0( "_",substring(iae,1,7), global_name,"_",ifelse(iii %in% (1:2), 2,3),"v", ifelse(iii %in% c(1,3), "_28","_7"))
      
      res <- scri_strata(  output_name  = output_name, 
                           formula_text = formula_text,       time_seq = time_seq, 
                           event_time = paste0(iae,"_days"), event = iae, id="person_id",
                           rws          = rws_def,
                           time_dep     = time_dep,                             # list( brand_def, brand12_def)
                           #combine_vars =  c("sex","age4"), 
                           start_obs    = "study_entry_days", end_obs = "study_exit_days", 
                           data         = scri_input[scri_input$age30_50==istr,],
                           image_plots = ae_event_first, image_brand=T, image_tit=istr,
                           lab_orders   = lab_orders,
                           lprint       = print_during_running,
                           global_plot_name = paste0( substring(iae,1,7),global_name), add_global_plot = !first_plot
      )
      ae_event_first <- F
      first_plot <- F
      
      report_list <- add_to_report_list(res$tabs,     output_name)
      models_list <- add_to_models_list(res$scri_all, output_name)
    }
  }
}  

# plots:
if(plot_during_running) 
  for(istr in names(res$tabs) )
    if(!is.null(res$tabs[[istr]][[1]]))
      plot_res(res$tabs[[istr]], main=paste(formula_text," + cal_time_cat"), col=col_list)

####
#  save report_list and model_list
save_results(global_name, report_list, models_list)
#
#######################################################################################







########### per age30  #############
#

if(all(names(scri_input) !="age30"))
  scri_input$age30 <- as.character(cut(scri_input$age_at_study_entry, c(-1,30,Inf)))


models_list <- list()
report_list <- list()

first_plot <- T
for(iae in ae_events){
  
  for(istr in unique(scri_input$age30) ){
    
    ae_event_first <- T  
    for(iii in ifelse(nvax>=3,4,2):1){
      
      if(iii==1) { rws_def <- rws_def_2vax_28;  time_dep <- list( brand12_def );  formula_text <-  "~ age30:brand12:lab" }
      if(iii==2) { rws_def <- rws_def_2vax_7 ;  time_dep <- list( brand12_def );  formula_text <-  "~ age30:brand12:lab" }
      
      if(iii==3) { rws_def <- rws_def_3vax_28;  time_dep <- list( brand123_def ); formula_text <-  "~ age30:brand123:lab" }
      if(iii==4) { rws_def <- rws_def_3vax_7 ;  time_dep <- list( brand123_def ); formula_text <-  "~ age30:brand123:lab" }
      
      vax_priority <- "_vax2priority"
      specif_name  <- "_age30" # istr
      
      global_name  <- paste0( vax_priority, specif_name )
      output_name  <- paste0( "_",substring(iae,1,7), global_name,"_",ifelse(iii %in% (1:2), 2,3),"v", ifelse(iii %in% c(1,3), "_28","_7"))
      
      res <- scri_strata(  output_name  = output_name,  
                           formula_text = formula_text,       time_seq = time_seq, 
                           event_time = paste0(iae,"_days"), event = iae, id="person_id",
                           rws          = rws_def,
                           time_dep     = time_dep,                             # list( brand_def, brand12_def)
                           #combine_vars =  c("sex","age4"), 
                           start_obs    = "study_entry_days", end_obs = "study_exit_days", 
                           data         = scri_input[scri_input$age30==istr,],
                           image_plots = ae_event_first, image_brand=T, image_tit=istr,
                           lab_orders   = lab_orders,
                           lprint       = print_during_running,
                           global_plot_name = paste0( substring(iae,1,7),global_name), add_global_plot = !first_plot
      )
      ae_event_first <- F
      first_plot <- F
      
      report_list <- add_to_report_list(res$tabs,     output_name)
      models_list <- add_to_models_list(res$scri_all, output_name)
    }
  }
}  

# plots:
if(plot_during_running) 
  for(istr in names(res$tabs) )
    if(!is.null(res$tabs[[istr]][[1]]))
      plot_res(res$tabs[[istr]], main=paste(formula_text," + cal_time_cat"), col=col_list)

####
#  save report_list and model_list
save_results(global_name, report_list, models_list)
#
#######################################################################################










########### per sex  #############
#

scri_input$sexc <- paste0("sex:",scri_input$sex )

models_list <- list()
report_list <- list()

first_plot <- T
for(iae in ae_events){
  
  for(istr in unique(scri_input$sexc) ){
    
    ae_event_first <- T  
    for(iii in ifelse(nvax>=3,4,2):1){
      
      if(iii==1) { rws_def <- rws_def_2vax_28;  time_dep <- list( brand12_def );  formula_text <-  "~ sexc:brand12:lab" }
      if(iii==2) { rws_def <- rws_def_2vax_7 ;  time_dep <- list( brand12_def );  formula_text <-  "~ sexc:brand12:lab" }
      
      if(iii==3) { rws_def <- rws_def_3vax_28;  time_dep <- list( brand123_def ); formula_text <-  "~ sexc:brand123:lab" }
      if(iii==4) { rws_def <- rws_def_3vax_7 ;  time_dep <- list( brand123_def ); formula_text <-  "~ sexc:brand123:lab" }
      
      vax_priority <- "_vax2priority"
      specif_name  <- "_sex" # istr
      
      global_name  <- paste0( vax_priority, specif_name )
      output_name  <- paste0( "_",substring(iae,1,7), global_name,"_",ifelse(iii %in% (1:2), 2,3),"v", ifelse(iii %in% c(1,3), "_28","_7"))
      
      res <- scri_strata(  output_name  = output_name,
                           formula_text = formula_text,       time_seq = time_seq, 
                           event_time = paste0(iae,"_days"), event = iae, id="person_id",
                           rws          = rws_def,
                           time_dep     = time_dep,                             # list( brand_def, brand12_def)
                           #combine_vars =  c("sex","age4"), 
                           start_obs    = "study_entry_days", end_obs = "study_exit_days", 
                           data         = scri_input[scri_input$sexc==istr,],
                           image_plots = ae_event_first, image_brand=T, image_tit=istr,
                           lab_orders   = lab_orders,
                           lprint       = print_during_running,
                           global_plot_name = paste0( substring(iae,1,7),global_name), add_global_plot = !first_plot
      )
      ae_event_first <- F
      first_plot <- F
      
      report_list <- add_to_report_list(res$tabs,     output_name)
      models_list <- add_to_models_list(res$scri_all, output_name)
    }
  }
}  

# plots:
if(plot_during_running) 
  for(istr in names(res$tabs) )
    if(!is.null(res$tabs[[istr]][[1]]))
      plot_res(res$tabs[[istr]], main=paste(formula_text," + cal_time_cat"), col=col_list)

####
#  save report_list and model_list
save_results(global_name, report_list, models_list)
#
#######################################################################################












if(F){

#                      distance between doses:

  
########### per age:  brand:  all brands in one model #############

brand12cum_def <- substitute(list( 
  split_name  = "vax12", 
  split       = cbind.data.frame( days_vax1, days_vax2, days_vax3),  
  lab         = c("no_vax","dose 1","dose 2"),
  change      = list( name          = "brand12cum",
                      #add_begin_sep = "_",
                      replace       = list( c(value="dose 1", var_name="type_vax1"),
                                            c(value="dose 2", var_name="type_vax12") )
  ),     
  ref         = "pre-"))


names(scri_input)

scri_input$dose_diff_cat <- scri_input$dose_diff
scri_input$dose_diff_cat[is.na(scri_input$dose_diff_cat)] <- -999999999
scri_input$dose_diff_cat <- cut( scri_input$dose_diff_cat, c(-Inf, -1,14,30,60,80,Inf) )
table(scri_input$dose_diff_cat)

scri_input$type_vax1_distance  <- paste(  "dist:", levels(scri_input$dose_diff_cat)[1], scri_input$type_vax1)
scri_input$type_vax12_distance <- paste(  "dist:", scri_input$dose_diff_cat, scri_input$type_vax12)


brand_distance_12_def <- substitute(list( 
  split_name  = "vax12", 
  split       = cbind.data.frame( days_vax1, days_vax2),  
  lab         = c("no_vax","dose 1","dose 2"),
  change      = list( name          = "brand_distance_12",
                      #add_begin_sep = "_",
                      replace       = list( c(value="dose 1", var_name="type_vax1_distance"),
                                            c(value="dose 2", var_name="type_vax12_distance") )
  ),     
  ref         = "pre-"))



output_name  <- "per_age_brand_model"
formula_text <-  "~ age30:brand_distance_12:lab"
scri_input$age30 <- cut(scri_input$age_at_study_entry,c(-1,30,120))

res <- scri_strata( strata_var   = "age30",  output_name  = output_name, 
                    formula_text = formula_text,       time_seq = time_seq, 
                    event_time = ae_event_time, event = ae_event, id="person_id",
                    rws          = rws_def_2vax,
                    time_dep     = brand_distance_12_def ,                             # list( brand_def, brand12_def)
                    start_obs    = "study_entry_days",   end_obs = "study_exit_days",
                    data         = scri_input,
                    lab_orders = lab_orders,
                    lprint = print_during_running,
                    global_plot_name = glob_analysis_name, add_global_plot = T
)

report_list <- add_to_report_list(res$tabs,     output_name)
models_list <- add_to_models_list(res$scri_all, output_name)

# plots:
if(plot_during_running) 
  for(istr in names(res$tabs) )
    if(!is.null(res$tabs[[istr]][[1]]))
      plot_res(res$tabs[[istr]], main=paste(formula_text," + cal_time_cat"), col=col_list)


}


##########
# restore options:
options(old_width)


}

