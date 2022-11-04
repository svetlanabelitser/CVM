
# Program Information  ----------------------------------------------------
#
#  functions for SCRI analysis 

# Functions:    scri_fit
#               refresh_event_variable
#               create_rws
#               split_intervals
#               summary_tab
#               factor_ref
#               combine_vars
#               plot_res
#               add_to_report_list
#               add_to_models_list
#               more functions .....
#
# Author:       Svetlana Belitser 
#               nov 2021 - feb 2022
#


scri_fit <- function( formula="",  
                      vax_def,  
                      event_time, event,
                      id,
                      rws,                          # list of risk/control windows definitions
                      time_seq, split_seq_name = "cal_time_cat", time_seq_ref="most events", 
                      time_dep = NA,                # list of risk/control windows definitions
                      combine_vars = c(),           # list of parameters to create new one variable from two other variables
                      start_obs, end_obs,
                      data, strata_var="", strata_value, use_all_events=F,
                      data_event,
                      nvax,                          # ???if missing ==> maximum of doses
                      lab_orders = NA,
                      ref=1,
                      rw_observed_percentage=0,     # 100% - the whole interval must be observed; 0% - even one day is enough to include this 'id' in the current risk window
                      censored_vars=c(),            # The rule 'rw_observed_percentage' does not work for variables 'censored_vars'. 
                      #  (for example, "death_days" ==> 'id' is included in the corresponding risk window till death date.)
                      event_in_rw=T,                # if event in rw ==> this rw should not be deleted even if not completely observed
                      delete_coef_no_events     = T,
                      delete_rows_start_with_no = T,
                      delete_no_ref_cols        = T,
                      delete_median_cols        = T,
                      lprint                    = T,       test = F,
                      save_data = F
){
  
  #if(missing(formula   )) stop("'formula' is missing.")
  if(missing(rws       )) stop("'rws' is missing.")
  if(missing(event_time)) stop("'event_time' is missing.")
  if(missing(event     )) stop("'event' is missing.")
  if(missing(id        )) stop("'id' is missing.")
  if(missing(data      )) stop("'data' is missing.")
  if(missing(start_obs )) stop("'start_obs' is missing.")
  if(missing(end_obs   )) stop("'end_obs' is missing.")
  if(missing(nvax      )) stop("'nvax' is missing.")
  
  
  
  if(nrow(data)==0)     
    return(  list( tabs     = NULL, 
                   tab_full = NULL,
                   model    = NULL,
                   call     = list( match.call())  ))
  if(strata_var!="") 
    if(sum(data[,strata_var]==strata_value,na.rm=T)==0)  
      return(  list( tab      = NULL, 
                     tab_full = NULL,
                     model    = NULL,
                     call     = list( match.call())  ))
  
  if(formula!=""){
    
    formula  <- formula(formula)
    tb       <- attributes(terms(formula))$factor
    tab_vars <- dimnames(tb)[[2]]
    
    if(missing(event)      & any(rowSums(tb)==0))  event      <- dimnames(tb)[[1]][rowSums(tb)==0]
    if(missing(event_time) & any(rowSums(tb)==0))  event_time <- dimnames(tb)[[1]][rowSums(tb)==0]
    
    no_formula <- F
  }
  else no_formula <- T
  
  
  ###########################################################################
  ################# create_rws (v3)  ###############
  #print(Sys.time())
  data_rws  <- create_rws(
    obj=vax_def,
    rws       = rws,
    data =data,
    start_obs = start_obs, end_obs = end_obs,
    event_time = event_time, event = event,
    id = id,
    lab_orders = lab_orders,
    ref="reference ",   #  ref=5 OR ref= "reference [-90;-30]"  or a part of a ref.category name
    #ref="pre-",       #  ref=5 OR ref= "pre-exposure [-90;-30]"  or a part of a ref.category name
    rw_observed_percentage = rw_observed_percentage,   
    censored_vars = censored_vars,           
    event_in_rw = event_in_rw               
  ) 
  if(!is.data.frame(data_rws)) {
    sep_vars <- data_rws$sep_vars
    data_rws <- data_rws$data_rws
  }
  else sep_vars <- c()
  #print(Sys.time())
  
  
  if(use_all_events & strata_var !="") {
    tmp <- levels(data_rws[,"lab"])
    data_rws[,"lab"] <- as.character(data_rws[,"lab"])
    data_rws[ data_rws[,strata_var]!=strata_value, "lab"] <- paste("no_stratum_",strata_value)  #"only_for_time_adj"
    #data_rws[,"lab"] <- factor(data_rws[,"lab"],levels=c(tmp,"only_for_time_adj"))
    data_rws[,"lab"] <- factor_ref(  data_rws[,"lab"],  lab=c(tmp,paste("no_stratum_",strata_value)), ref="reference") # lab=c(tmp,"only_for_time_adj"),  ref="reference")  
    #data_rws[,"lab"] <- factor_ref(  data_rws[,"lab"], lab=c(tmp,"only_for_time_adj"),  ref="pre-")  
  }  
  #??? data_rws <- refresh_event_variable( "rw_start", "rw_end", event, event_time, data_rws)
  
  
  #######
  #  create time intervals:
  #
  if(!missing(time_seq) & nrow(data_rws)>0){
    #if(!missing(time_seq) & split_seq_name %in% tab_vars & nrow(data_rws)>0){
    data_rws <- refresh_event_variable( "rw_start", "rw_end", data_rws, event, event_time)
    data_rws <- split_intervals( data =data_rws, 
                                 start_interval = "rw_start", end_interval = "rw_end", 
                                 splits_names = split_seq_name, # "cal_time_cat",
                                 splits       = time_seq,
                                 ref          = time_seq_ref, #"most events", 
                                 event        = event,        #   myopericarditis 
                                 event_time   = event_time 
    )
  }
  
  
  
  if(length(combine_vars)>0 & nrow(data_rws)>0){
    data_rws$lab  <- combine_vars_func( data_rws[, c(combine_vars, "lab"), drop=F ], lab_orders = lab_orders, ref=ref, event = data_rws[,event] )
    data_rws$lab  <- factor_ref(  data_rws$lab, lab_orders=lab_orders, ref=ref, event_var=data_rws[,event] )  
  }
  
  
  
  #####
  #  create 'event'and 'interval' variables
  #
  data_rws <- refresh_event_variable( "rw_start", "rw_end", data_rws, event, event_time)
  
  data_rws$interval <- data_rws$rw_end - data_rws$rw_start + 1
  data_rws <- data_rws[data_rws$interval>0,]
  
  # delete id's without events in the windows:
  id_no_events <- names((tb<-tapply(data_rws[,event],data_rws[,id], sum))[tb==0])
  sum(data_rws[,event]); length(unique(data_rws[,id])); nrow(data_rws)
  data_rws <- data_rws[ !(as.character(data_rws[,id]) %in% id_no_events),  ]
  sum(data_rws[,event]); length(unique(data_rws[,id])); nrow(data_rws)
  
  
  #  # delete id's without events in the windows:
  #  id_no_events <- as.numeric(names((tb<-tapply(data_rws[,event],data_rws[,id], sum))[tb==0]))
  #  sum(data_rws[,event]); length(unique(data_rws[,id])); nrow(data_rws)
  #  data_rws <- data_rws[ !(data_rws[,id] %in% id_no_events),  ]
  #  sum(data_rws[,event]); length(unique(data_rws[,id])); nrow(data_rws)
  
  
  
  if(nrow(data_rws)==0) return(  list( res_tab = NULL, 
                                       model   = NULL,
                                       call    = list( match.call())  ))
  
  
  if(any(names(data_rws)==split_seq_name))
    data_rws[,split_seq_name]  <- factor_ref(  as.character(data_rws[,split_seq_name]), 
                                               lab=levels(data_rws[,split_seq_name])[levels(data_rws[,split_seq_name]) %in% unique(as.character(data_rws[,split_seq_name])) ],
                                               ref=time_seq_ref, 
                                               event_var=data_rws[,event] )  
  
  
  # combine variables in formula with risk windows variable 'lab' (the begining of the code):
  if(!no_formula){
    tb <- attributes(terms(formula))$factor
    if(!missing(time_seq) & split_seq_name %in% dimnames(tb)[[2]])
      tb <- tb[, c( dimnames(tb)[[2]][dimnames(tb)[[2]]!=split_seq_name], split_seq_name ) ]
  }
  
  
  if(length(sep_vars)>0){
    
    mods <- vector("list",length=length(sep_vars))
    names(mods) <- names(sep_vars)
    
    for(isep in 1:length(sep_vars)){
      
      var_names <- sep_vars[[isep]]
      if(!missing(time_seq)) #& split_seq_name %in% dimnames(tb)[[2]])
        var_names <- c( var_names, split_seq_name )
      var_names <- var_names[ lapply(var_names, function(x)nlevels(data_rws[,x]))>1 ]
      
      
      #  data_rws$cal_time_mid <- I(unlist(lapply(strsplit(substring(as.character(data_rws$cal_time_cat),2,nchar(as.character(data_rws$cal_time_cat))-1),";",fixed=T),function(x)mean(as.numeric(x)))))
      Poisson_formula <- as.formula(paste(event,"~", 
                                          paste( var_names,  collapse=" + "), "+", 
                                          "strata(",id,")", "+", "offset(log(interval))")) 
      if(!no_formula){
        suppressWarnings(
          mod <- try( clogit(formula = Poisson_formula, data = data_rws, control=coxph.control(iter.max=1000) ), silent = T )
        )
      }
      else mod <- NA
      
      
      if(class(mod)[[1]]== "try-error" | no_formula){
        
        if(F){
          if(nrow(table1(data_rws[  data_rws[,event]==1 ,var_names]))<=8)
            print( table1(data_rws[  data_rws[,event]==1 ,var_names]) ) 
          print(cbind.data.frame(var_names, nrow=nrow(table1(data_rws[  data_rws[,event]==1 ,var_names])) ))
          if(any(names(data_rws)==split_seq_name))   #"cal_time_cat"))
            print(cbind.data.frame(var_names[var_names!=split_seq_name], nrow=nrow(table1(data_rws[  data_rws[,event]==1 ,var_names[var_names!=split_seq_name]])) ))
          
          if(!missing(time_seq) )
            warning("Error in Poisson regression. \ntime_seq=",median(diff(time_seq),na.rm=T),"; Formula: ", deparse(Poisson_formula))
          else warning("Error in Poisson regression. \nno time_seq; Formula: ", deparse(Poisson_formula))
        }        
        
        if(isep==1)
          res_tab <- summary_tab( var_names=var_names,#sep_vars[[isep]], 
                                  event=event, 
                                  data=data_rws, 
                                  id_name=id,  
                                  lab_orders=lab_orders,
                                  model_number = isep )
        else  
          res_tab <- summary_tab( var_names=var_names,#sep_vars[[isep]], 
                                  event=event, 
                                  data=data_rws, 
                                  id_name=id,  
                                  lab_orders=lab_orders,
                                  model_number = isep,
                                  res_tab = res_tab )
        
        mods[[isep]] <- list( mod ) 
        
      }
      else{
        
        
        if(isep==1)
          res_tab <- summary_tab( var_names=var_names,#sep_vars[[isep]], 
                                  event=event, 
                                  data=data_rws, 
                                  id_name=id,  
                                  mod=mod, 
                                  lab_orders=lab_orders,
                                  #coef_cond = coef_cond, 
                                  model_number = isep )
        else  
          res_tab <- summary_tab( var_names=var_names,#sep_vars[[isep]], 
                                  event=event, 
                                  data=data_rws, 
                                  id_name=id,  
                                  mod=mod, 
                                  lab_orders=lab_orders,
                                  #coef_cond=coef_cond, 
                                  model_number = isep,
                                  res_tab = res_tab )
        
        mods[[isep]] <- list(summary(mod)) 
      }
    }
    
    mod <- mods
    
    if(lprint)  print(format( res_tab, digits=3, nsmall=2, justify="left" ))
    
  } else {
    
    if(nrow(data_rws)==0) res_tab <- mod <- NULL
    else {
      
      if(any(cond<-(colSums(tb)>1)) & nrow(data_rws)>0)
        for(i in 1:sum(cond)){
          vars_tmp <- gsub(" ","",dimnames(tb)[[2]][cond][i])
          dimnames(tb)[[2]][cond][i] <- paste(strsplit( vars_tmp, ":" )[[1]],collapse="_")
          data_rws[,paste(strsplit( gsub(" ","",vars_tmp,fixed = TRUE) ,":")[[1]],collapse="_")]  <- combine_vars_func( 
            data_rws[, strsplit(gsub(" ","",vars_tmp,fixed = TRUE)  ,":")[[1]], drop=F ], lab_orders = lab_orders, event = data_rws[,event] )   #ref=ref, 
        }  
      
      var_names <- dimnames(tb)[[2]]
      if(!missing(time_seq) & split_seq_name %in% dimnames(tb)[[2]])
        var_names <- unique(c( var_names, split_seq_name ))
      var_names <- var_names[ lapply(var_names, function(x)nlevels(data_rws[,x]))>1 ]
      
      if(test){
        print(paste(event,"~", 
                    paste( var_names,  collapse=" + "), "+", 
                    "strata(",id,")", "+", "offset(log(interval))"))
        print("ref_cat:")
        for(ivar in var_names)
          print( c( ivar, dimnames(contrasts(data_rws[,ivar]))[[1]][ !( dimnames(contrasts(data_rws[,ivar]))[[1]] %in% dimnames(contrasts(data_rws[,ivar]))[[2]] )  ] ))
      }
      Poisson_formula <- as.formula(paste(event,"~", 
                                          paste( var_names,  collapse=" + "), "+", 
                                          "strata(",id,")", "+", "offset(log(interval))")) 
      if(!no_formula){
        suppressWarnings(
          mod <- try( clogit(formula = Poisson_formula, data = data_rws, control=coxph.control(iter.max=1000) ), silent = T )
        )
      }
      
      if(class(mod)[[1]]== "try-error" | no_formula) {
        
        if(F){
          if(nrow(table1(data_rws[  data_rws[,event]==1 ,var_names]))<=8)
            print( table1(data_rws[  data_rws[,event]==1 ,var_names]) ) 
          print(cbind.data.frame(var_names, nrow=nrow(table1(data_rws[  data_rws[,event]==1 ,var_names])) ))
          if(any(names(data_rws)==split_seq_name))   #"cal_time_cat"))
            print(cbind.data.frame(var_names[var_names!=split_seq_name], nrow=nrow(table1(data_rws[  data_rws[,event]==1 ,var_names[var_names!=split_seq_name]])) ))
          #print(cbind.data.frame(var_names[var_names!="cal_time_cat"], nrow=nrow(table1(data_rws[  data_rws[,event]==1 ,var_names[var_names!="cal_time_cat"]])) ))
          
          if(!missing(time_seq))
            warning("Error in Poisson regression. \ntime_seq=",median(diff(time_seq),na.rm=T),"; Formula: ", deparse(Poisson_formula))
          else warning("Error in Poisson regression. \nno time_seq; Formula: ", deparse(Poisson_formula))
        }   
        
        res_tab <- summary_tab( var_names=dimnames(tb)[[2]], event=event, data=data_rws, id_name=id, lab_orders=lab_orders )
      }
      else{
        res_tab <- summary_tab( var_names=dimnames(tb)[[2]], event=event, data=data_rws, id_name=id,  mod=mod, lab_orders=lab_orders  )
        mod <- summary(mod)
      }
      
      if(lprint)  print(format( res_tab, digits=2, justify="left" ))
    }
  }
  
  
  res_tab0 <- res_tab
  if(delete_coef_no_events) 
    res_tab[ is.na(res_tab$events_rw) | res_tab$events_rw==0 , c("events_rw","RR_data","RR") ] <- NA 
  if(delete_rows_start_with_no)
    res_tab <- res_tab[ !(substring(tolower(res_tab$all_cat),1,3) %in% c("no ","no_")), ]
  #if(delete_no_brand_rows & any(ls()=="brands"))
  #res_tab <- res_tab[ !grepl(paste0("no ",brands,collapse="|"),res_tab$all_cat,perl=T), ]
  if(delete_no_ref_cols & any(  !grepl("noref",names(res_tab)) ))
    res_tab <- res_tab[ ,!grepl("noref",names(res_tab)) ]
  if(delete_median_cols & any(  !grepl("median",names(res_tab)) ))
    res_tab <- res_tab[ ,!grepl("median",names(res_tab)) ]
  
  
  
  ret <- list( tab      = res_tab,
               tab_full = res_tab0,
               model    = mod,
               call     = list( match.call()) 
  )
  if(save_data) ret <- c( ret, data_rws=list(data_rws) )
  
  
  # ret <- list( tab     = res_tab)
  # attributes(ret) <- list( tab_all = res_tab0,
  #                          model   = mod,
  #                          call    = list( match.call()) 
  # )
  #if(save_data) attributes(ret) <- c( attributes(ret), data_rws=list(data_rws) )
  
  ret
  
}  # end of function 'scri_fit'



######################################
#######################################
######################################  
#  
#
#
refresh_event_variable <- function(  start_interval, end_interval, 
                                     data,
                                     event, event_time#, 
                                     #data_event
){
  
  data[ ,event] <- as.numeric( data[,start_interval]<=data[,event_time] & data[, event_time] <= data[,end_interval] )
  data[is.na(data[,event]),event] <- 0
  data
  
  
}  

refresh_event_variable_horiz <- function(  start_interval, end_interval, 
                                           data,
                                           event, event_time
){
  data[ ,event] <- as.numeric( data[,start_interval]<=data[,event_time] & data[, event_time] <= data[,end_interval] & data[,event]==1 )
  data[is.na(data[,event]),event] <- 0
  data
}  

######################################################################  
########################################################################  
########################################################################
#   !!!!!!!!!!!!!!!!!!!!
#
#
#             create rws (version 3)  31.01.2022
#
#
#
create_rws <- function( rws,                          # list of risk/control windows definitions
                        data,                         # dataset
                        obj,
                        start_obs, end_obs,           # start and end of observation variables
                        var_type,                     # if missing ==> vax1, vax2, vax3, ...
                        id,                           # id variable to get new start and end of observation  ('rw_min' and 'rw_max')
                        event_time, event,
                        lab_orders,
                        ref=1,                        # reference category for new variable: number OR [the beginning of ] category name OR "most events"
                        event_name="event",           # used if ref=="most events" to define reference category with most events
                        rw_observed_percentage=0,     # 100% - the whole interval must be observed; 0% - even one day is enough to include this 'id' in the current risk window
                        censored_vars=c(),            # The rule 'rw_observed_percentage' does not work for variables 'censored_vars'. 
                        #  (for example, "death_days" ==> 'id' is included in the corresponding risk window till death date.)
                        event_in_rw=T                 # if event in rw ==> this rw shouldnot be deleted even if not completely observed
){ 
  
  
  
  # create and calculate variable from formula from list 'rws' and dataset 'data': 'lab' (or another name), 'rw_start' and 'rw_end':
  calc_wind <- function(x, data, obj){ 
    
    cond_rws <- rep(T, nrow(data))
    if(!is.null(x$cond)) cond_rws <- eval(x$cond,data)
    else {
      if(!is.null(x$cond_var)){
        if(!is.null(x$cond_values)) cond_rws <- data[,x$cond_var] %in% x$cond_values               
        if(!is.null(x$cond_value))  cond_rws <- data[,x$cond_var] %in% x$cond_value                
        if( is.null(x$cond_values) & is.null(x$cond_value) )  cond_rws <- data[, x$cond_var ]      
      }
    }
    
    data <- data[cond_rws & !is.na(data[, x$t0]),]
    data$rws_cat <- x$name
    
    
    t0   <- data[ , x$t0] 
    if( !x$no_last_interval ){
      if(           x$overlap_priority      == "next"     ) tend <- data[ , obj$data_parameters$next_vax_time ] - 1 
      #      if(          x$overlap_priority      == "overlap"  ) ??? tend <- eval(  data[ , "next"     ] )
      #???      if(       x$overlap_priority      == "previous" ) tend <- eval(  data[ , obj$data_parameters$prev_vax_time ] )
      #      if( substring(x$overlap_priority,1,2) == "no"       ) ?? nothing do   tend <- eval(  data[ , "no"       ] )
    }      
    
    
    if(!any(names(x)=="cutpoints")) x$cutpoints <- 0
    
    #####
    # labels create
    if(!any(names(x)=="lab")) x$lab <- ""
    
    if( length(x$lab)<length(x$cutpoints) ) x$lab <- c( x$lab, rep(x$lab[length(x$lab)], length(x$cutpoints)-length(x$lab)) )
    
    
    if(x$lab_add_interval){
      x$lab[-length(x$lab)] <- paste0( x$lab[-length(x$lab)], "[", x$cutpoints[-length(x$cutpoints)],";",x$cutpoints[-1]-1,"]")
      if(!x$no_last_interval) x$lab[ length(x$lab)] <- paste0( x$lab[ length(x$lab)], ">", x$cutpoints[ length(x$lab)]-1)
    }
    
    if( any(names(x)=="no_last_interval")) x$no_last_interval <- eval(x$no_last_interval)
    else x$no_last_interval <- F
    
    
    
    
    
    for(i in 1:length(x$cutpoints)){
      
      if(any(ls()=="data_tmp")) rm(data_tmp)
      
      if(i==length(x$cutpoints) & x$no_last_interval) next
      
      
      if(any(ls()=="tend")) {
        
        if(i <length(x$cutpoints)) rw_enddd <- pmin(tend,t0 + x$cutpoints[i+1]-1,na.rm=T)
        if(i==length(x$cutpoints)) rw_enddd <- tend
        
        cond <- (t0+x$cutpoints[i]) <= tend  # start must be < the end of these intervals 'tend'
        cond[is.na(cond)] <- T  # id's without next dose; later, outside of the small function, check and adding the end of observation if necessary
        
        if(i<length(x$cutpoints) & rw_observed_percentage>0){  #  ?? replace??? from the small function
          cond <- cond &   ( 100 -  100 * ( (t0 + x$cutpoints[i+1]-1) - rw_enddd ) / ( x$cutpoints[i+1] - x$cutpoints[i] ) >= rw_observed_percentage )  
          
          if(event_in_rw) cond <- cond | ( data[,event]==1 &  t0+x$cutpoints[i] <= data[,event_time] & data[,event_time] <= t0+x$cutpoints[i+1]-1  ) 
          
          if(length(censored_vars)>0) 
            for(icensor in censored_vars)
              cond <- cond | ( !is.na(data[,icensor]) & rw_enddd == tend & tend == data[,icensor] & (t0+x$cutpoints[i]) <= tend )
        }
      }     
      else {
        if(i <length(x$cutpoints)) rw_enddd <- t0 + x$cutpoints[i+1]-1
        if(i==length(x$cutpoints)) rw_enddd <- rep(NA,nrow(data))
        cond <- rep(T,nrow(data))
      }  
      
      if( any(cond)) 
        data_tmp <- cbind.data.frame( data,
                                      rw_start = t0 + x$cutpoints[i],
                                      rw_end   = rw_enddd,
                                      lab      = x$lab[i]    )[cond,]
      
      if(any(ls()=="data_tmp")){
        if(any(ls()=="data_tmp0")) data_tmp0 <- rbind.data.frame(data_tmp0, data_tmp)  
        else                       data_tmp0 <- data_tmp
      }
    } # end of 'for'
    
    if(!any(ls()=="data_tmp0")) return(NULL)
    
    rownames(data_tmp0) <- NULL
    
    data_tmp0
  }   # end of sub-function 'calc_wind'
  
  # create and calculate variable from formula from list 'rws' and dataset 'data': 'lab' (or another name), 'rw_start' and 'rw_end':
  calc_wind_horiz <- function(x, data){ 
    
    
    
    t0   <- unlist(lapply(as.expression(x$t0),  eval, data))
    tend <- unlist(lapply(as.expression(x$tend),eval, data))
    if(any(is.na(tend))) tend <- pmin(tend, data[,end_obs], na.rm=T)
    
    if(!any(names(x)=="cuts")) x$cuts <- 0
    else x$cuts <- eval(x$cuts)
    
    #####
    # labels create
    if(any(names(x)=="lab")) x$lab <- eval(x$lab)
    else x$lab <- ""
    
    if(length(x$lab) < length(x$cuts)) x$lab <- c(x$lab, rep( x$lab[length(x$lab)],length(x$cuts)-length(x$lab)) )
    if( any(names(x)=="lab_add_interval")) x$lab_add_interval <- eval(x$lab_add_interval)
    else x$lab_add_interval <- T
    if(x$lab_add_interval){
      x$lab[-length(x$lab)] <- paste0( x$lab[-length(x$lab)],"[",x$cuts[-length(x$cuts)],";",x$cuts[-1]-1,"]")
      if(any(names(x)=="tend")) x$lab[length(x$lab)] <- paste0( x$lab[length(x$lab)],">",x$cuts[length(x$lab)]-1)
    }
    if( any(names(x)=="no_last_interval")) x$no_last_interval <- eval(x$no_last_interval)
    else x$no_last_interval <- F
    
    for(i in 1:length(x$cuts)){
      
      if(any(ls()=="data_tmp")) rm(data_tmp)
      
      if(i<length(x$cuts)){
        
        if(!is.null(tend)) {
          rw_enddd <- pmin(tend,t0 + x$cuts[i+1]-1,na.rm=T)
          cond <- (t0+x$cuts[i]) <= tend  # start must be < the end of these intervals 'tend'
          if(rw_observed_percentage>0){
            cond <- cond &   ( 100 -  100 * ( (t0 + x$cuts[i+1]-1) - rw_enddd ) / ( x$cuts[i+1] - x$cuts[i] ) >= rw_observed_percentage )  
            
            if(event_in_rw) cond <- cond | ( data[,event]==1 &  t0+x$cuts[i] <= data[,event_time] & data[,event_time] <= t0+x$cuts[i+1]-1  ) 
            
            if(length(censored_vars)>0) 
              for(icensor in censored_vars)
                cond <- cond | ( !is.na(data[,icensor]) & rw_enddd == tend & tend == data[,icensor] & (t0+x$cuts[i]) <= tend )
          }
        }     
        else {
          rw_enddd <- t0 + x$cuts[i+1]-1
          cond <- rep(T,nrow(data))
        }  
        cond[is.na(cond)] <- F
        
        if( any(cond)) 
          data_tmp <- cbind.data.frame( data,
                                        rw_start = t0 + x$cuts[i],
                                        rw_end   = rw_enddd,
                                        lab      = x$lab[i]    )[cond,]
      }  
      else {
        if(x$no_last_interval) 
          next
        else {
          if(!is.null(tend)) {
            cond <- (t0+x$cuts[i]) <= tend  # start must be < the end of these intervals 'tend'
            cond[is.na(cond)] <- F
            
            if( any(cond) )     #if( any(names(x)=="tend"))
              data_tmp <- cbind.data.frame( data,
                                            rw_start = t0 + x$cuts[i],
                                            rw_end   = tend,
                                            lab = x$lab[i])[cond,]
          }
        }    
      }  
      if(any(ls()=="data_tmp"))
        if(!any(ls()=="data_tmp0")) data_tmp0 <- data_tmp
      else data_tmp0 <- rbind.data.frame(data_tmp0, data_tmp)
    }
    
    if(!any(ls()=="data_tmp0")) return(NULL)
    
    data_tmp0 <- data_tmp0[!is.na(data_tmp0$rw_start),]
    
    data_tmp0
  }   # end of sub-function 'calc_wind_horiz' for horizontal data
  
  
  if(!is.list(rws)) rws <- eval(parse(text = rws))
  rws <- lapply(rws, function(x) {
    names(x)[ substring(names(x),1,3)=="cut" ] <- "cutpoints"
    if(is.null(x$lab_add_interval)) x$lab_add_interval <- T
    if(is.null(x$no_last_interval)) x$no_last_interval <- F
    if(is.null(x$overlap_priority)) x$overlap_priority <- "next"
    x
  } )
  
  if(any(lapply(rws, function(x) x$overlap_priority)=="next")){
    if(is.null(obj$data_parameters$next_vax_time)){
      if(any(names(data)=="next_vax_time"))   obj$data_parameters$next_vax_time <- "next_vax_time"
      else {
        if(any(names(data)=="next_vax_days")) obj$data_parameters$next_vax_time <- "next_vax_days"
        else                                  obj$data_parameters$next_vax_time <- "next_vax_time"
      }
    }
    if(!any(names(data)==obj$data_parameters$next_vax_time)){
      data <- data[order(data[,obj$data_parameters$id],data[,obj$data_parameters$vax_time]),]
      data[,obj$data_parameters$next_vax_time] <- c(data[-1,obj$data_parameters$vax_time], NA)
      data[ c( data[-nrow(data),obj$data_parameters$id] != data[-1,obj$data_parameters$id], T)   ,obj$data_parameters$next_vax_time] <- NA
      obj$data_parameters$next_vax_time <- obj$data_parameters$next_vax_time
    }
  }
  
  rws <- lapply(rws,function(x){ if(any(names(x)=="vax_dep"))if(is.null(names(x$vax_dep))) names(x$vax_dep)<-"before"; x} )
  
  
  if( is.null(names(rws)) ) names(rws) <- lapply(rws, function(x)x$name)
  
  if( is.null(names(rws)) ) names(rws) <- lapply(rws, function(x) gsub( ".", "_", gsub( " |,|-|_", "", tolower(x$lab[1]) ), fixed=T ) )
  if( any(names(rws)=="") ) names(rws)[names(rws)==""] <- lapply(rws, function(x) gsub( ".", "_", gsub( " |,|-|_", "", tolower(x$lab[1]) ), fixed=T ) )[names(rws)==""] 
  
  
  if(!is.null( tmp<-unlist( lapply(rws,function(x)x$name)) )) names(rws) <- tmp
  
  
  
  data$id_n <- 1:nrow(data)
  
  rws_datalist <- lapply( rws, calc_wind,       data,  obj )
  
  
  # create one dataset 'data_rws' from list of dataset 'rws_datalist':
  data_rws <- do.call("rbind.data.frame",rws_datalist)
  
  rownames(data_rws) <- NULL
  
  # check:
  if(!is.null(obj$data_parameters$start_obs) & !is.null(obj$data_parameters$end_obs)) 
    if(any(data_rws[,obj$data_parameters$start_obs] >= data_rws[,obj$data_parameters$end_obs])) stop("'start_obs' should be < 'end_obs'")
  
  # 1. fill in missing values in 'rw_end' 
  # 2. if necessary check 'rw_observed_percentage'
  # 3. delete [parts of] intervals if interval after 'end_obs'.
  if(!is.null(obj$data_parameters$end_obs)){
    
    # 1. fill in missing values in 'rw_end':
    if(any( (cond <- is.na(data_rws$rw_end) & data_rws$rw_start <= data_rws[,obj$data_parameters$end_obs]) )) 
      data_rws$rw_end[cond] <-  data_rws[cond,obj$data_parameters$end_obs]
    
    # 2. ??? if necessary check 'rw_observed_percentage'
    if(F){  # ???
      #if(i<length(x$cutpoints) & rw_observed_percentage>0){  # 
      cond <- cond &   ( 100 -  100 * ( (t0 + x$cutpoints[i+1]-1) - rw_enddd ) / ( x$cutpoints[i+1] - x$cutpoints[i] ) >= rw_observed_percentage )  
      
      if(event_in_rw) cond <- cond | ( data[,event]==1 &  t0+x$cutpoints[i] <= data[,event_time] & data[,event_time] <= t0+x$cutpoints[i+1]-1  ) 
      
      if(length(censored_vars)>0) 
        for(icensor in censored_vars)
          cond <- cond | ( !is.na(data[,icensor]) & rw_enddd == tend & tend == data[,icensor] & (t0+x$cutpoints[i]) <= tend )
    }
    
    # 3. delete [parts of] intervals if interval after 'end_obs':
    data_rws <- data_rws[ data_rws$rw_start <= data_rws[,obj$data_parameters$end_obs],  ]
    cond <-   data_rws$rw_start <= data_rws[,obj$data_parameters$end_obs] & data_rws[,obj$data_parameters$end_obs] < data_rws$rw_end & 
      !is.na(data_rws$rw_start) & !is.na(data_rws$rw_end) & !is.na(data_rws[,obj$data_parameters$end_obs])
    data_rws$rw_end[cond] <- data_rws[cond, obj$data_parameters$end_obs]
  }
  
  # delete [parts of] intervals if interval before 'start_obs'.
  if(!is.null(obj$data_parameters$start_obs)){ 
    data_rws <- data_rws[ data_rws[,obj$data_parameters$start_obs] <= data_rws$rw_end, ]
    cond <-     data_rws$rw_start < data_rws[,obj$data_parameters$start_obs] & data_rws[,obj$data_parameters$start_obs] <= data_rws$rw_end & 
      !is.na(data_rws$rw_start) & !is.na(data_rws$rw_end) & !is.na(data_rws[,obj$data_parameters$start_obs])
    data_rws$rw_start[cond] <- data_rws[cond, obj$data_parameters$start_obs]
  }
  
  labels   <- data_rws$lab[!duplicated(data_rws$lab)]
  
  # vax-dependend analysis (logical: T or F):    
  lvax_dep <- any( sapply(rws,function(x)!is.null(x$vax_dep)))
  
  if( !lvax_dep ){# no vax_dep variable ){
    
    if(is.null(rws$ref)) {
      if( !is.null( lapply(rws,function(x) is.null(x$ref))) ) 
        rws$ref <- unlist(lapply(rws,function(x) x$ref))[1]
      else rws$ref <- data_rws$lab[order(data_rws$rw_start)][1]
    }
    
    data_rws$lab <- factor_ref(  data_rws$lab, 
                                 #lab_orders = lab_orders,
                                 lab=labels, 
                                 lab_sort=F,
                                 ref=rws$ref 
                                 #event_var=data_rws[,event]
    ) 
    levels(data_rws$lab) <- format(levels(data_rws$lab))
  }
  else {  # with 'brand' or other vax_dependent_variable (e.g., vax_dep <- "type_vax_short")
    
    
    if(is.null(rws$ref)) {
      if( !is.null( lapply(rws,function(x) is.null(x$ref))) ) 
        rws$ref <- unlist(lapply(rws,function(x) x$ref))[1]
      else rws$ref <- data_rws$lab[order(data_rws$rw_start)][1]
    }
    
    
    ref_place <- data_rws$rws_cat[ grepl( unlist(lapply(rws[sapply(rws,length)>1],function(x) x$ref))[1]  ,  data_rws$lab ) ][1] # i.e., probably "v0" 
    #ref_place <- data_rws$rws_cat[ grepl(rws$ref,data_rws$lab) ][1] # i.e., probably "v0" 
    
    
    
    
    sep_vars <- vector("list",length=sum(sapply(rws,length)>1)); names(sep_vars) <- paste0("vars_for_",names(rws)[sapply(rws,length)>1])
    for(irw in (1:length(rws))[sapply(rws,length)>1]){
      
      x <- rws[[irw]]
      if(!any(names(data_rws) %in% x$vax_dep)) stop(paste0("The vaccine dependent variable '", x$vax_dep, "' not found. Check the risk windows definitions."))
      
      if( x$name != ref_place ){
        
        # add variable with the rws$vax_dep (e.g., variable with "Pfize","Moder",...) for x$name (e.g., "v1" or "v2" or "b1")
        tmp <- unique(data_rws[data_rws$rws_cat == x$name,c(obj$data_parameters$id, x$vax_dep)])
        names(tmp)[match(x$vax_dep,names(tmp))] <- paste0(x$name,"_",x$vax_dep) 
        if(any(duplicated(tmp[,obj$data_parameters$id]))) stop(paste0("There are more then one '", x$vax_dep, "' for dose name '", x$name, "'"))
        #dim(data_rws); sum(cond); dim(tmp)
        data_rws <- merge.data.frame( data_rws, tmp, by=obj$data_parameters$id, all.x= T )
        #dim(data_rws); sum(data_rws$rws_cat %in% c(ref_place,x$name))
        
        
        # create new variable for category x$name (e.g,"v1" or "v2" or "b1" ==> 'v1_lab' or ...)
        #     create new labels for all intervals corresponding to category with reference interval (probably, "v0" with "pre-exposure","buffer") 
        
        data_rws[, paste0(x$name,"_lab")] <- ""
        data_rws$tmp_cond <- T
        
        # create new labels for all categories where ref_cat (e.g., for all categories before the first dose, because one of the categories is reference category)
        cond <- data_rws$rws_cat == ref_place
        
        tmp  <- ""
        for(iplace in x$vax_dep[names(x$vax_dep)=="before"]){
          data_rws$tmp_cond[cond] <- data_rws$tmp_cond[cond] & !is.na(data_rws[cond,paste0(x$name,"_",iplace)])
          tmp  <- paste0( tmp, data_rws[cond,paste0(x$name,"_",iplace)], " " )
        }  
        tmp  <- paste0( tmp ,ifelse( length(x$lab)==1, x$lab, x$name ), data_rws[cond,"lab"])
        
        for(iplace in x$vax_dep[names(x$vax_dep)=="after"]){
          data_rws$tmp_cond[cond] <- data_rws$tmp_cond[cond] & !is.na(data_rws[cond,paste0(x$name,"_",iplace)])
          tmp  <- paste0( tmp, " ", data_rws[cond,paste0(x$name,"_",iplace)] )
        }
        data_rws[cond & data_rws$tmp_cond, paste0(x$name,"_lab")] <- tmp[ data_rws$tmp_cond[cond] ]
        
        #     create new labels for intervals with x$name (e.g, "v2")
        cond <- data_rws$rws_cat ==x$name
        
        tmp  <- ""
        for(iplace in x$vax_dep[names(x$vax_dep)=="before"])
          tmp  <- paste0( tmp, data_rws[cond,paste0(x$name,"_",iplace)], " " )
        tmp  <- paste0( tmp, data_rws[cond,"lab"] )
        
        for(iplace in x$vax_dep[names(x$vax_dep)=="after"])
          tmp  <- paste0( tmp, " ", data_rws[cond,paste0(x$name,"_",iplace)] )
        data_rws[cond, paste0(x$name,"_lab")] <- tmp
        
        
        
        
        
        
        #     create new labels (e.g., "no v2") for intervals without x$name (e.g, "v2")
        data_rws[ data_rws[, paste0(x$name,"_lab")]=="" , paste0(x$name,"_lab")] <- paste0("no ",x$name)
        #data_rws[!( data_rws$rws_cat %in% c(ref_place,x$name) ) | (is.na(data_rws[ ,paste0(x$name,"_",x$vax_dep)]) ) , paste0(x$name,"_lab")] <- paste0("no ",x$name)
        
        # create new 'brand'-variables from this variable (e.g., 'v2_lab'  ==> 'vaxdep1_v2_lab', 'vaxdep2_v2_lab', ...)
        data_rws[,"tmp_var"] <- data_rws[,paste0(x$name,"_",x$vax_dep[1])]
        if(length(x$vax_dep)>1) 
          for(i in 2:length(x$vax_dep)) data_rws[,"tmp_var"] <- paste( data_rws[,"tmp_var"], "&", data_rws[,paste0(x$name,"_",x$vax_dep[i])])
        #data_rws[ data_rws[,paste0(x$name,"_lab")] == paste0("no ",x$name), "tmp_var"] <- NA
        
        vax_dep_cat    <-  unique(data_rws[data_rws[,paste0(x$name,"_lab")] != paste0("no ",x$name),"tmp_var"])
        for(ibr in 1:length(vax_dep_cat) ){
          if(any(data_rws[data_rws$rws_cat==x$name,"tmp_var"]==vax_dep_cat[ibr])){
            
            cond <- data_rws[ ,"tmp_var"]==vax_dep_cat[ibr]  & data_rws[ ,paste0(x$name,"_lab")] != paste0("no ",x$name)
            data_rws[  cond, paste0("vaxdep",ibr,"_",x$name,"_lab")] <- data_rws[  cond, paste0(x$name,"_lab")]
            
            data_rws[ !cond, paste0("vaxdep",ibr,"_",x$name,"_lab")] <- paste0("no ",vax_dep_cat[ibr]," or ",x$name)  # e.g."no Pfize or v2"
            
            data_rws[, paste0("vaxdep",ibr,"_",x$name,"_lab")] <- factor_ref(  data_rws[, paste0("vaxdep",ibr,"_",x$name,"_lab")], lab_orders=lab_orders,  ref=x$ref )  
            
            sep_vars[[irw]] <- c( sep_vars[[irw]], paste0("vaxdep",ibr,"_",x$name,"_lab") )
          }
        }
        data_rws[,"tmp_var"] <- NULL
      }
    }
    sep_vars <- sep_vars[sapply(sep_vars,length)>0]
  }
  
  data_rws <- data_rws[ order(data_rws$id_n) , ]
  
  if(lvax_dep) data_rws <- list( data_rws = data_rws,
                                 sep_vars = sep_vars       )
  
  data_rws
} # end of function 'create_rws'  (version 3) 




#########################################
#########################################
#########################################
#
#  add time dependent variable  => split some intervals
#


split_intervals <- function(  data,                            # dataset
                              start_interval, end_interval,    # two variables in 'data'
                              splits_names,                    # name of the new variable
                              splits,                          # 1. a number; or 2. vector with numbers; or 
                              # 3. a variable in 'data'; or 4. 'cbind' or 'cbind.data.frame' of variables in 'data'
                              lab = c("before","after"), # labels for split intervals. The length should be (#splits + 1)
                              # if  2 intervals ==> c("before","after")
                              # if  3 intervals ==> c("before","during","after")
                              # if >3 intervals ==> paste0("(",time_seq[-length(time_seq)],";",time_seq[-1],"]")
                              lab_add_interval = T,            # add intervals at the end of labels 'lab'
                              lab_orders       = NA,
                              ref=1,                           # reference category for new variable: number OR category name OR "most events"
                              event,                           # used if ref=="most events" to define reference category with most events
                              event_time                       # used if ref=="most events" to define reference category with most events
                              #event_var = substitute(event)    # used if ref=="most events" to define reference category with most events
){
  
  # splits can be a value   or   a vector of values    or  a variable     or a vector of variables:
  if( is.data.frame(splits)){   # splits are variables from 'data' 
    names(splits) <- paste0("_split_",1:ncol(splits))
    
    data <- cbind.data.frame(data, as.data.frame(splits))
    splits <- names(splits)
  }
  else splits <- splits[ min( data[,start_interval], na.rm=T) < splits & splits < max( data[,end_interval], na.rm=T) ]
  
  
  data$i_ <- 1:nrow(data)
  
  # split intervals:
  for(isplit in 1:length(splits)){
    if(mode(splits)=="character") 
      split <- data[,paste0("_split_",isplit)] # for 4. 'cbind' or 'cbind.data.frame' of variables in 'data'
    else  split <- splits[[isplit]]   #for:  # 1. a number; or 2. vector with numbers; or # 3. a variable in 'data';
    
    start_int <- data[,start_interval]   # eval(substitute(data$start_interval))
    end_int   <- data[,end_interval  ]   # eval(substitute(data$end_interval))
    
    # "before" the split: entire intervals to the left of the split
    if(isplit==1) {
      data[,splits_names] <- NA
      data[ end_int <  split & !is.na(end_int), splits_names] <- isplit
      data[ end_int <  split & !is.na(end_int), splits_names] <- isplit
    }
    
    # "after" the split: entire intervlas to the right of the split
    data[ split <= start_int & !is.na(start_int) & !is.na(split) , splits_names] <- isplit + 1
    
    #######
    # split interval into 2 :
    cond <-  start_int < split & split <= end_int & !is.na(split) & !is.na(start_int) & !is.na(end_int)
    if(sum(cond)>0){
      if(length(split)==1) split <- rep(split,sum(cond)) 
      else                 split <- split[cond]
      
      # 'after' the split: part of the interval to the right of the split
      data_tmp <- data[cond,]
      data_tmp[ , splits_names ]   <- isplit + 1
      data_tmp[ , start_interval ] <- split                     #  deparse(substitute(start_interval)) ] <- split
      
      # 'before' the split: part of the interval to the left of the split
      if(isplit==1) data[ cond, splits_names ] <- 1
      data[ cond, end_interval ] <- split - 1    #      deparse(substitute(end_interval)) 
      
      data <- rbind.data.frame(data, data_tmp)
    }
  } 
  
  if(mode(splits)=="character") data <- data[,!( names(data) %in% paste0("_split_",1:length(splits)) ) ] 
  ####
  # add labels
  data[,splits_names] <- as.factor(data[,splits_names])
  if(missing(lab)){ 
    if(mode(splits)=="character"){
      if(nlevels(data[,splits_names])==2) lab <- c("before","after") 
      if(nlevels(data[,splits_names])==3) lab <- c("before","during","after") 
    }
    if(mode(splits)=="numeric")
      lab <- paste0("[",c(min(start_int,na.rm=T), splits),";",c(splits-1, max(end_int,na.rm=T)),"]")
  }  
  
  
  
  data              <- refresh_event_variable( start_interval, end_interval, data, event, event_time)
  if(length(lab_orders)==1 & is.na(lab_orders[1]))
    data[,splits_names] <- factor_ref(  data[,splits_names], lab=lab, ref=ref, event_var=data[,event] ) 
  else
    data[,splits_names] <- factor_ref(  data[,splits_names], lab_orders = lab_orders, lab=lab, ref=ref, event_var=data[,event] )  
  
  
  data <- data[order( data$i_, data[,start_interval] ), ]   #   eval(substitute(data$start_interval))),] 
  data$i_ <- NULL
  data
}  # end of function 'split_intervals_horiz'

# data_rws$interval <- data_rws$rw_end - data_rws$start_rw + 1
# 
# data_rws2 <- split_intervals(data_rws, start_rw, rw_end, "tdvar", interval+50, ref=2 )
# data_rws2 <- split_intervals(data_rws, start_rw, rw_end, "tdvar", cbind.data.frame(interval, interval+50), c("befo","dur","afte"), ref=2 )
# 
# data_rws2 <- split_intervals(data_rws, start_rw, rw_end, "tdvar", 170, ref=2 )
# data_rws2 <- split_intervals(data_rws, start_rw, rw_end, "tdvar", c(150,170), c("b","d","after") )
# 
# data_rws2 <- data_rws2[order(data_rws2$person_id,data_rws2$start_rw),]  
# data_rws2 
# 








################################################################
################################################################
################################################################
#    summary_tab
# summary per risk/control windows and results from Poisson regression
# in function 'var_sum': 'var_name', in 'data' must be variables: 'event','enterval',id (can be changed)
summary_tab <- function(  var_names, # var_names <- c("lab", "cal_time_cat")
                          event,
                          data, id_name = "id",
                          mod,
                          lab_orders,
                          #coef_cond, 
                          model_number=1,
                          res_tab  #,
                          #print = F, 
                          #digits = 3
){ 
  add <- F
  if(!missing(res_tab)) { res_tab0 <- res_tab; add <- T}
  # check:
  if(missing(var_names)) stop("'var_names' is missing")
  if(missing(data) & missing(mod)) stop("'data' and/or 'mod' must be specified. ")
  
  ######
  # create summary table for variable: 'var_names' from 'data' (variable 'interval', 'event',id_name must be in 'data')
  if(!missing(data)){
    
    cond <- !is.na(data$interval) & data$interval>0
    res_tab <- vector("list",length(var_names))
    for(ivar in var_names){
      
      if( is.factor(data[,ivar]) ) 
        if(any(cond2<-rowSums(contrasts(data[,ivar]))==0)) ivar_ref_cat <- levels(data[,ivar])[cond2]
      else  ivar_ref_cat <- levels(data[,ivar])[1]   # here m.b. study other contrasts (for the future?)
      else    ivar_ref_cat <- levels(data[,ivar])[1]
      
      n_events_per_id_pre <-  table( data[ cond &  data[,ivar]==ivar_ref_cat & data[,event]>0 , id_name] )
      ids_pre_names       <- unique( data[ cond &  data[,ivar]==ivar_ref_cat                  , id_name] )
      
      events_ref <- rep(NA,nlevels(data[,ivar])); names(events_ref) <- levels(data[,ivar])
      ids_rw  <- ids_ref <- observed_rw  <- observed_ref <- events_rw <- median_days_per_id_rw <- events_ref 
      ids_rw_noref <- observed_rw_noref  <- events_rw_noref <- median_days_per_id_rw_noref <- events_ref 
      for(ilab in levels(data[,ivar])){
        #  ids with events in the risk window:
        ids_event_rw_names  <- unique( data[ cond &  data[,ivar]==ilab & data[,event]>0 , id_name] )
        #  ids with events in the risk window but not observed in the ref.window:
        ids_event_rw_and_not_observed_ref <- ids_event_rw_names[ !(ids_event_rw_names %in% ids_pre_names) ]
        
        #  ids with events in the risk window AND observed in the ref.window:
        ids_event_rw_and_observed_ref <- ids_event_rw_names[ ids_event_rw_names %in% ids_pre_names ]
        #  ids with events in ref.window and observed in risk window:
        ids_event_ref_and_observed_rw <- names(n_events_per_id_pre)[ names(n_events_per_id_pre) %in% data[ cond & data[,ivar]==ilab, id_name]  ]
        
        #  both:
        if(ilab==ivar_ref_cat) ids_rw_names       <-    ids_event_rw_and_observed_ref                                     # ids_event_rw_names 
        else                   ids_rw_names       <- c( ids_event_rw_and_observed_ref, ids_event_ref_and_observed_rw )    # c( ids_event_rw_names, ids_event_ref_and_observed_rw )
        
        #  #events, #ids with events and #observed days in the risk window (and observed in the ref.window):
        events_rw[            ilab] <- sum(           data[ cond &  data[,ivar]==ilab & data[,id_name] %in% ids_rw_names                   ,   event    ] )
        ids_rw[               ilab] <- length(unique( data[ cond &  data[,ivar]==ilab & data[,id_name] %in% ids_rw_names &  data[,event]>0 ,   id_name  ] ) )
        observed_rw[          ilab] <- sum(           data[ cond &  data[,ivar]==ilab & data[,id_name] %in% ids_rw_names                   ,  "interval"] )
        median_days_per_id_rw[ilab] <- median(   with(data[ cond &  data[,ivar]==ilab & data[,id_name] %in% ids_rw_names, c(id_name,"interval")], tapply(interval,get(id_name),sum)) )
        #  #events, #ids with events and #observed days in ref.window (and observed in the risk windows):
        if(ilab!=ivar_ref_cat){ 
          events_ref[  ilab] <- sum(    n_events_per_id_pre[ names(n_events_per_id_pre) %in% ids_rw_names ] ) 
          ids_ref[     ilab] <- length( n_events_per_id_pre[ names(n_events_per_id_pre) %in% ids_rw_names ] ) 
          observed_ref[ilab] <- sum( data[ cond &  data[,ivar]==ivar_ref_cat &  data[,id_name] %in% ids_rw_names, "interval"] )
        }
        #  #events, #ids with events and #observed days in the risk window and NOT observed in the ref.window:
        if(length(ids_event_rw_and_not_observed_ref)>0){
          events_rw_noref[            ilab] <- sum(           data[ cond &  data[,ivar]==ilab & data[,id_name] %in% ids_event_rw_and_not_observed_ref                   ,   event    ] )
          ids_rw_noref[               ilab] <- length(unique( data[ cond &  data[,ivar]==ilab & data[,id_name] %in% ids_event_rw_and_not_observed_ref &  data[,event]>0 ,   id_name  ] ) )
          observed_rw_noref[          ilab] <- sum(           data[ cond &  data[,ivar]==ilab & data[,id_name] %in% ids_event_rw_and_not_observed_ref                   ,  "interval"] )
          median_days_per_id_rw_noref[ilab] <- median(   with(data[ cond &  data[,ivar]==ilab & data[,id_name] %in% ids_event_rw_and_not_observed_ref, c(id_name,"interval")], tapply(interval,get(id_name),sum)) )
        }
      }
      
      res_tab[ivar==var_names][[1]] <- cbind.data.frame(
        
        events_rw  = as.integer(events_rw),  # with(data[cond & data[,event] >0, ], tapply(get(id_name),get(ivar),function(x) sum( x %in% ids_pre_names ))),
        events_ref = events_ref,
        #events_ref = with(data[cond & data[,event]==0,], tapply(get(id_name),get(ivar),function(x) sum( n_events_per_id_pre[ names(n_events_per_id_pre) %in% unique(x) ])   )),
        
        days_rw    = observed_rw ,
        days_ref   = observed_ref, 
        
        ids_rw     = ids_rw,   #with(data[cond & data[,event] >0,], tapply(get(id_name),get(ivar),function(x)sum(unique(x) %in% ids_pre_names))),
        ids_ref    = ids_ref,  #with(data[cond & data[,event]==0,], tapply(get(id_name),get(ivar),function(x)sum(unique(x) %in% names(n_events_per_id_pre))))
        
        median_days_per_id_rw       = median_days_per_id_rw,
        median_days_per_id_rw_noref = median_days_per_id_rw_noref,  # in risk window but NOT observed in the reference window:
        
        # in risk window but NOT observed in the reference window:
        events_rw_noref = events_rw_noref,
        ids_rw_noref    = ids_rw_noref,
        days_rw_noref   = observed_rw_noref
      )
    }
    res_tab <- do.call("rbind.data.frame",res_tab)
    res_tab <- cbind.data.frame( i=1:nrow(res_tab), all_cat=rownames(res_tab) , res_tab)
    
    res_tab$events_ref[!is.na(res_tab$events_ref) & res_tab$events_ref==0] <- NA      
    res_tab$ids             <- rowSums(res_tab[,c("ids_rw", "ids_ref")], na.rm=T)
    
    res_tab$days_per_id_rw  <- round( res_tab$days_rw  / res_tab$ids , 2 )
    res_tab$days_per_id_ref <- round( res_tab$days_ref / res_tab$ids , 2 )
    
    # in risk window but NOT observed in the reference window:
    res_tab$days_per_id_rw_noref  <- round( res_tab$days_rw_noref  / res_tab$ids_rw_noref , 2 )
    
    res_tab$rw_portion           <- res_tab$events_rw  / res_tab$days_rw
    res_tab$ref_portion          <- res_tab$events_ref / res_tab$days_ref
    res_tab$RR_data              <- res_tab$rw_portion / res_tab$ref_portion
    
    res_tab <- res_tab[,c( "i", "all_cat", "events_rw", "events_ref","days_per_id_rw", "days_per_id_ref", "median_days_per_id_rw", 
                           "days_rw", "days_ref", "ids_rw", "ids_ref", "ids", 
                           "events_rw_noref", "ids_rw_noref", "days_rw_noref", "days_per_id_rw_noref", "median_days_per_id_rw_noref",    # these are added later 
                           "rw_portion", "ref_portion", "RR_data" )]
  }
  
  ######
  # create summary table for result from Poisson regression with variables 'var_names':
  #
  if(!missing(mod)){
    res_tab_model <- cbind.data.frame( summary(mod)$conf.int[,-2], 
                                       summary(mod)$coef[,match( c("Pr","co","se"), substring(dimnames(summary(mod)$coef)[[2]],1,2) ) ]  ) #  "Pr(>|z|)" "coef"   "se(coef)" 
    res_tab_model[ is.na(res_tab_model$coef), substring(dimnames(res_tab_model)[[2]],1,2)=="se" ] <- NA
    # delete variable names at the beginning of parameter names in results from Poisson regression:
    res_tab_model$all_cat <- dimnames(res_tab_model)[[1]]
    var_names <- unique(unlist(strsplit(var_names,":")))
    var_names <- var_names[order(nchar(var_names),decreasing = T)]
    for(ivar in var_names){
      res_tab_model$all_cat[cond] <- substring(res_tab_model$all_cat, nchar(ivar)+1)[(cond<-substring(res_tab_model$all_cat,1,nchar(ivar))==ivar)]
      res_tab_model$all_cat       <- gsub( paste0(":",ivar), ":", res_tab_model$all_cat)
    }
    names_matched <- match(c("exp","low","upp","Pr(","se("),substring(names(res_tab_model),1,3))
    if(any(!is.na(names_matched)))
      names(res_tab_model)[names_matched[!is.na(names_matched)]] <- c("RR","lci","uci","pval","se_coef")[!is.na(names_matched)]
    #names(res_tab_model)[match(c("exp","low","upp","Pr("),substring(names(res_tab_model),1,3))]] <- c("RR","2.5%","97.5%","pval")
    
    res_tab_model$model <- model_number
    
  }
  
  #####
  # combine (merge) these two tables:
  if(!missing(data) &  !missing(mod)){
    res_tab$all_cat_tmp       <- gsub(" ","",res_tab$all_cat)
    res_tab_model$all_cat_tmp <- gsub(" ","",res_tab_model$all_cat)
    res_tab <- merge.data.frame( res_tab, res_tab_model, by="all_cat_tmp", all=T, sort=F )
    res_tab <- res_tab[, names(res_tab)!="all_cat_tmp" ]
    
    if( any(names(res_tab)=="all_cat.x") ){
      if( any(names(res_tab)=="all_cat.y") ){
        res_tab$all_cat.x[ is.na(res_tab$all_cat.x) & !is.na(res_tab$all_cat.y) ] <- res_tab$all_cat.y[ is.na(res_tab$all_cat.x) & !is.na(res_tab$all_cat.y) ]
        res_tab <- res_tab[, names(res_tab)!="all_cat.y" ]
      }
      names(res_tab)[names(res_tab)=="all_cat.x"] <- "all_cat"
    }  
    res_tab <- res_tab[order(res_tab$i),]
  } 
  
  if(missing(data) & !missing(mod)) { 
    res_tab <- res_tab_model
    res_tab <- cbind.data.frame( i=1:nrow(res_tab), res_tab[, c(ncol(res_tab),1:(ncol(res_tab)-1))])
  }
  
  res_tab <- cbind.data.frame( event=substring(event,1,7), res_tab )
  
  res_tab$model <- model_number
  if(add){
    res_tab_new <- res_tab0
    res_tab_new$all_cat_without_space <- gsub(" ","",res_tab_new$all_cat)
    res_tab$all_cat_without_space     <- gsub(" ","",res_tab$all_cat)
    res_tab_new <- merge.data.frame(res_tab_new,res_tab, by=c("all_cat_without_space"), all=T, sort=F )
    names(res_tab_new)
    
    for(ivar in names(res_tab)[names(res_tab)!="all_cat_without_space"]){  # c( "event","all_cat","n_events","atrisk_days","atrisk_ids","days_pp","relative_rate","relative_perc")){
      #for(ivar in c( "event","all_cat","n_events","atrisk_days","atrisk_ids","days_pp","relative_rate","relative_perc")){
      names(res_tab_new)[names(res_tab_new)==paste0(ivar,".x")] <- ivar
      if( any( (cond<-is.na(res_tab_new[,ivar]) & !is.na(res_tab_new[,paste0(ivar,".y")])) ) )
        res_tab_new[cond,ivar] <- res_tab_new[cond,paste0(ivar,".y")]
      if( all( res_tab_new[,ivar] == res_tab_new[,paste0(ivar,".y")], na.rm=T ) )
        res_tab_new[,paste0(ivar,".y")] <- NULL
    }
    
    if(!missing(mod)){
      model_res_names <- c("RR","lci","uci","pval","coef","se_coef","model")
      #model_res_names <- c("i","RR","2.5%","97.5%","pval","coef","se(coef)","model")
      
      var_numbers <- match(paste0(model_res_names,".x"),names(res_tab_new))
      var_numbers <- var_numbers[!is.na(var_numbers)]
      names(res_tab_new)[var_numbers] <-  model_res_names[!is.na(var_numbers)]
      
      
      if( any( names(res_tab_new)=="pval") & any(names(res_tab_new)=="pval.y" ) ){
        # duplicated sets of columns with different values
        if(sum( cond<- !is.na( res_tab_new[,"pval"] ) & !is.na( res_tab_new[,paste0("pval.y")]) & res_tab_new[,paste0("pval")]!=res_tab_new[,paste0("pval.y")] )>0){
          res_tab_new_dupl <- res_tab_new[cond, , drop=F]
          res_tab_new_dupl[, model_res_names ] <- NULL
          names(res_tab_new_dupl)[match(paste0(model_res_names,".y"),names(res_tab_new_dupl))] <-  model_res_names
          if(any(names(res_tab_new_dupl)=="i.y")){
            res_tab_new_dupl <- res_tab_new_dupl[order(res_tab_new_dupl$i.y),]
            res_tab_new_dupl$i.y <- NULL  
          }
          res_tab_new_dupl <- res_tab_new_dupl[!duplicated(res_tab_new_dupl[,names(res_tab_new_dupl)!="i"]),]
        }
        
        if(sum( (cond <- is.na( res_tab_new[,"pval"] ) & !is.na( res_tab_new[,paste0("pval.y")])) )>0) 
          res_tab_new[cond, model_res_names ] <- res_tab_new[cond, paste0(model_res_names,".y") ]
        
        res_tab_new[,match(paste0(model_res_names,".y"),names(res_tab_new))] <-  NULL
      }
    }

    if(any(names(res_tab_new)=="i.y")) res_tab_new <- res_tab_new[,names(res_tab_new)!="i.y"]
    if( any(ls()=="res_tab_new_dupl") )
      res_tab_new <- rbind.data.frame(res_tab_new, res_tab_new_dupl[,match(names(res_tab_new),names(res_tab_new_dupl))]  )
    
    res_tab_new$all_cat_without_space <- NULL
    
    
    if(all(names(res_tab_new)!="model")){
      if(any(names(res_tab_new)=="i.x")) names(res_tab_new)[names(res_tab_new)=="i.x"    ] <-  "i"
      if(any(names(res_tab_new)=="i.x")) names(res_tab_new)[names(res_tab_new)=="i.model"] <-  "model"
      if(any(cond<-!is.na(match(names(res_tab_new),c("i.y","model.y")))))   res_tab_new[,cond] <-  NULL
      res_tab_new$model <- 1
    }
    
    # order table on 'all_cat'
    res_tab_new$tablab_order <- match( res_tab_new$all_cat, levels(factor_ref(res_tab_new$all_cat[ substring(res_tab_new$all_cat,1,1)!="[" ], lab_orders=lab_orders)) )
    res_tab_new$tablab_order[is.na(res_tab_new$tablab_order)] <- 100000
    res_tab_new <- res_tab_new[order(res_tab_new$tablab_order, res_tab_new$model),]
    #res_tab_new <- res_tab_new[order(res_tab_new$tablab_order, res_tab_new$model, res_tab_new$i),]
    res_tab_new$tablab_order <- NULL
    res_tab_new[,"ids"]   <- as.integer(res_tab_new[,  "ids"])
    res_tab_new[,"model"] <- as.integer(res_tab_new[,"model"])
    
    res_tab <- res_tab_new
    
    res_tab$i <- 1:nrow(res_tab)
    
    
  }
  
  res_tab
}  # end of function 'summary_tab'

#summary_tab(var_names=c("lab", "cal_time_cat"), data=data_rws, id_name="person_id",  print=T, digits=2)
#summary_tab(var_names=c("lab", "cal_time_cat"), data=data_rws, id_name="person_id", mod=mod, print=T,digits=1)
#summary_tab(var_names=c("lab", "cal_time_cat"),  mod=mod, print=T,digits=1)


#########################################
#########################################
#########################################
#
#  add a level in factor variable and choose reference category
#

factor_ref <- function(  var,
                         lab,
                         lab_sort   = F,
                         lab_orders = NA,
                         ref=1,          # reference category for new variable: number OR [beginning of ] category name OR "most events"
                         event_var,      # used if ref=="most events" to define reference category with most events
                         keep_ref = T,  # if 'var' is factor with reference category and 'ref' is missing, then keep this reference category in the updated variable 
                         na_replace = ""
){ 
  
  if( length(lab_orders)>1 | !is.na(lab_orders[1]) ){
    
    # lab_orders <- list( c("pre-","buf", "dose 1",     "dose 2" ),
    #                c("[0;0]","[1;28]",">28"),
    #                c("Pfizer","Moderna","AstraZeneca", ... ) )
    
    if(!is.list(lab_orders)) lab_orders <- list(lab_orders)
    
    var_unique <- unique(var)
    big_number <- max(sapply(lab_orders,length))+10
    orders <- as.data.frame( matrix( big_number, nrow=length(var_unique), ncol=length(lab_orders),
                                     dimnames=list(var_unique, 1:length(lab_orders)) ))
    
    for(itype in 1:length(lab_orders))
      for(icat in 1:length(lab_orders[[itype]]))
        if(any( cond<-(grepl( lab_orders[[itype]][icat], var_unique, fixed=T)  &  orders[,itype]==big_number) ))
          orders[ cond, itype] <- icat
    
    
    orders <- eval(parse(text=paste0("orders[ order(", paste( paste0("orders[,", 1:length(lab_orders)),collapse="],") ,"]),]")) )
    orders$order <- 1:nrow(orders)
    
    var <- factor(var, levels=rownames(orders), labels=rownames(orders) )
  }
  else {
    if(is.factor(var)){
      if(nlevels(var)>1) ref0 <- levels(var)[rowSums(contrasts(var))==0]
      levels0 <- levels(var)
      var <- as.character(var)
      if( all(levels0==as.character((1:length(levels0)))) ) var <- as.numeric(var)
    }  
    if(mode(var)=="character") var[is.na(var)] <- ""
    if(missing(lab)) lab <- var[!duplicated(var)] 
    if(lab_sort)  lab <- sort(lab)
    
    if(is.numeric(var)) {
      if(length(lab)== length(unique(var)))
        var <- factor(var, labels=lab)
      else {
        if(max(var,na.rm=T)<=length(lab) & all(!is.na(var)))
          var <- factor(var, labels=lab[sort(unique(var))])
        else
          var <- factor(var)
      }    
    }  
    else var <- factor(var, levels=lab, labels=lab)
  }
  
  # choose reference category ref
  if(!missing(ref)){
    if(is.character(ref)){
      if( any( cond<-grepl(ref,levels(var),fixed=T) ) ) 
        ref <- (1:nlevels(var))[cond][1]
      if(ref=="most events"){
        tb <- table( var[ !is.na(event_var) & event_var>0 ])
        ref <- match(names(tb)[  tb == max(tb) ][1], levels(var))
      }
    }
  }
  else{  # 'ref' is missing
    if(keep_ref & any(ls()=="ref0")) 
      if( ref0 %in% levels(var)) ref <- (1:nlevels(var))[levels(var)==ref0]
  }
  if(is.numeric(ref) & nlevels(var)>1){
    contrasts(var) <- contr.treatment(nlevels(var),base=ref)
    dimnames(contrasts(var))[[2]] <- levels(var)[-ref]
  } 
  var
} # end of function 'factor_ref'

#
###########################################
###########################################
###########################################



###########################################
###########################################
#
#
combine_vars_func <- function(var1, var2, sep=" & ", 
                              lab_orders,
                              ref, event          # reference category for new variable: number OR [beginning of ] category name OR "most events"
){
  
  if(!missing(var2)) var0 <- cbind.data.frame(var1, var2)
  else var0 <- var1
  
  for(icol in 2:ncol(var0)){
    var2 <- var0[,icol]
    var1 <- var0[,icol-1]
    
    ref_cat <- ref_cat1 <- ref_cat2 <- c()  
    if(is.factor(var1)) ref_cat1 <- levels(var1)[rowSums(contrasts(var1))==0]
    if(is.factor(var2)) ref_cat2 <- levels(var2)[rowSums(contrasts(var2))==0]
    
    if(!is.factor(var1)) var1 <- as.factor(var1)
    if(!is.factor(var2)) var2 <- as.factor(var2)
    
    if(length(unique(var1))==1) ref_cat1 <- unique(var1)
    if(length(unique(var2))==1) ref_cat2 <- unique(var2)
    
    all_combi <- paste( rep( levels(var1),  each=nlevels(var2)),  
                        rep( levels(var2),       nlevels(var1)), sep=sep)
    var12 <- paste(var1, var2, sep=sep)
    var_levels <- all_combi[ all_combi %in% unique(var12) ]
    
    # search for ref category:
    if(!missing(ref)){
      if(length(ref)>1){
        if(!is.na(ref[[1]])){
          if(is.numeric(ref[1])) ref_cat1 <- var1[ref[1]]
          if(is.character(ref[1])) {
            if(ref[1]=="most events") ref_cat2 <- names(table(var2))[table(var2)==max(table(var2))]
            else
              if(any(grepl(ref[1],var1)))
                ref_cat1 <- grep(ref[1],var1, value=T)
          }  
        }  
        if(!is.na(ref[[2]])){
          if(is.numeric(ref[2])) ref_cat2 <- var2[ref[2]]
          if(is.character(ref[2])) {
            if(ref[2]=="most events") ref_cat2 <- names(table(var2))[table(var2)==max(table(var2))]
            else  
              if(any(grepl(ref[2],var2))) ref_cat2 <- grep(ref[2],var2, value=T) 
          }    
        }  
      }  
      else {
        if(is.numeric( ref )) ref_cat <- var_levels[ref]
        if(is.character(ref)) {
          if(ref=="most events") ref_cat <- ref
          else  
            if(any(grepl(ref,var_levels)))    ref_cat <- grep(ref, var_levels, value=T)
        }    
      }
    }
    
    if(length(ref_cat1)>0) ref_cat1 <- grep( ref_cat1, var_levels, value=T, fixed=T)
    if(length(ref_cat2)>0) ref_cat2 <- grep( ref_cat2, var_levels, value=T, fixed=T)
    
    if(length(ref_cat)==0){
      if(length(ref_cat1)>0  & length(ref_cat2) >0) ref_cat <- var_levels[ var_levels %in% ref_cat1 & var_levels %in% ref_cat2 ]
      if(length(ref_cat1)>0  & length(ref_cat2)==0) ref_cat <- var_levels[ var_levels %in% ref_cat1                            ]
      if(length(ref_cat1)==0 & length(ref_cat2) >0) ref_cat <- var_levels[                            var_levels %in% ref_cat2 ]
    }  
    
    if(length(ref_cat)>1 & length(ref_cat1)>0){
      tmp <- c()
      for(icat in ref_cat1)
        tmp <- c( tmp, grep(icat, ref_cat, value=T, fixed=T) )
      if(length(tmp)>0) ref_cat <- tmp
    }  
    if(length(ref_cat)>1 & length(ref_cat2)>0){
      tmp <- c()
      for(icat in ref_cat2)
        tmp <- c( tmp, grep(icat, ref_cat, value=T, fixed=T) )
      if(length(tmp)>0) ref_cat <- tmp
    }  
    
    #if(length(ref_cat)>0 & !is.na(ref_cat))
    #  ref <- (1:length(var_levels))[var_levels==ref_cat[1]]
    
    #ref <- (1:nlevels(var_levels))[var_levels==ref_cat[1]]
    if(length(ref_cat)==0) ref_cat <- 1
    
    var12   <- factor_ref( var12, lab_orders = lab_orders, lab=var_levels, ref=ref_cat[1], event_var = event )
    var0[,icol] <- var12
  }
  var12
}   # end of function 'combine_vars'
#table1(data_rws$brand_lab)
#contrasts(data_rws$brand_lab)


###############################
#
plot_res <- function(res, main="", col=col_list, time_cat_i=length(strata), ylim, CI = T, 
                     correct_max_time_adj =  T){
  
  if(all(unlist(lapply(res, function(x)sum(!is.na(x$RR))))==0)) return(NULL) 
  
  ncoef     <- nrow(res[[1]])
  ncoef_max <- max(unlist( lapply(res,nrow) ))
  if(ncoef_max>ncoef) # +1 because for ref.category of cal_time_var 'model' is NA
    ncoef_max <- ncoef + 1 + max(unlist( lapply(res,function(x) {
      cond <- (1:nrow(x))[(ncoef+1):nrow(x)]; 
      if(all(dimnames(x)[[2]]!="model")) return(0)
      if(all(is.na(x[cond,"model"])))    return(0)
      tapply( x[cond,"i"], x[cond,"model"], length ) 
    } )))
  
  if(missing(ylim)){
    ymax   <- unlist(lapply(res,function(x) {
      if(all(dimnames(x)[[2]]!="RR")) return(NA) 
      x_tmp <- x$RR[x$RR<1000]; 
      if(any(!is.na(x_tmp))) max(x_tmp,na.rm=T) else NA 
    } ))
    
    if(any(!is.na(ymax))) ymax <- max(ymax, na.rm=T)
    else                  ymax <- 10
    ylim <- c( 0, ymax )
  }
  
  
  text_col_cond <- 1 + as.numeric(!is.na(res[[1]]$RR[1:ncoef]))
  tmp_1         <-    tmp_05    <-    rep(F,length(text_col_cond)) 
  for(imod in 1:length(res)){
    if(any(dimnames(res[[imod]])[[2]]=="RR")){
      tmp_1  <-  tmp_1  | ( !is.na(res[[imod]]$RR[1:ncoef]) & !is.na(res[[imod]]$pval[1:ncoef]) & res[[imod]]$pval[1:ncoef]<=0.1  )
      tmp_05 <-  tmp_05 | ( !is.na(res[[imod]]$RR[1:ncoef]) & !is.na(res[[imod]]$pval[1:ncoef]) & res[[imod]]$pval[1:ncoef]<=0.05 )
    }
  }
  text_col_cond <- text_col_cond + as.numeric(tmp_1 )
  text_col_cond <- text_col_cond + as.numeric(tmp_05)
  
  x_deltas <- 0
  if(length(res)>1) x_deltas <- 0.4* (1/(length(res)-1) * (0:(length(res)-1)) - 0.5)
  
  # function for colors:
  col_alpha <- function(col,alpha=0) rgb(t(col2rgb(col)/255),alpha=alpha)
  
  ###########
  #
  # plot 1: all coefficients:
  #
  if(ncoef_max > ncoef){ 
    
    plot( c(0,ncoef_max), ylim, type="n", main=main, xlab="effect number  |     time adjustment effects", ylab="RR", axes=F)
    axis(2); box()
    axis(1, at=1:ncoef )
    
    if(ncoef_max > ncoef+1) { #res[[i]]$all_cat[substring(res[[i]]$all_cat,1,1)=="["]
      suppressWarnings(
        numbers <- as.numeric(unique(unlist(lapply(res, function(x, ncoef) 
          strsplit( x$all_cat[ substring(x$all_cat,1,1)=="[" ], "\\[|;|,|\\]" ), ncoef=ncoef ))))
      )  
      axis(1, at=c(ncoef+0.5, ncoef_max+0.5), labels = paste0(c(min(numbers, na.rm=T), max(numbers, na.rm=T)),"days"), padj=1, tck=-0.04  )
      axis(1, at=c(ncoef+0.5, ncoef_max+0.5), labels = paste0(c(min(numbers, na.rm=T), max(numbers, na.rm=T)),"days"), padj=1, tck=1  )
    }  
    
    grid();abline(h=1, col="darkgray",lty=1)
    #abline(v=ncoef+0.5, col="darkgray", lty=1)   # "orange"
    abline(v=10*(1:(ncoef%/%10)), col="lightgray", lty=1)
    abline(v= 5*(1:(ncoef%/% 5)), col="lightgray", lty=2)
    
    #  CI's for unadjusted and adjusted RR's:
    if(CI)
      for(imod in 1:length(res)){
        if(any(dimnames(res[[imod]])[[2]]=="RR"))
          matlines( rbind( (1:ncoef+x_deltas[imod]),(1:ncoef+x_deltas[imod]))[,!is.na(res[[imod]]$RR[1:ncoef])],
                    t(res[[imod]][1:ncoef,][ !is.na(res[[imod]]$RR[1:ncoef])  ,c("lci","uci")]),
                    lty=1, lwd=1, col=col_alpha(col[imod],0.15), type="o", pch="-", cex=2 )
      }
    
    # RR's:
    for(imod in 1:length(res)){
      if(all(dimnames(res[[imod]])[[2]]!="RR")) next
      lines( 1:ncoef+x_deltas[imod],res[[imod]]$RR[1:ncoef], type="o", col=col[imod],lwd=ifelse(imod==1,2,1)); 
      if(imod==1) text( 1:ncoef,res[[1]]$RR, labels=as.character(res[[1]]$i), pos=3, col=col[1], cex= ifelse(ncoef<=50,1,0.7) ) 
      if(any( (cond <- !is.na(res[[imod]]$pval[1:ncoef]) & res[[imod]]$pval[1:ncoef]<=0.05) ))                # check for significant p-values
        points( (1:ncoef+x_deltas[imod])[cond], res[[imod]]$RR[1:ncoef][cond], pch="*",cex=3, col=col[imod]) 
    }
    
    # calendar time adjusted
    if(length(res)>1){
      for(imod in 2:length(res)){
        
        if(all(dimnames(res[[imod]])[[2]]!="RR")) next
        
        cond_after_ncoef <- (1:nrow(res[[imod]]))>ncoef
        
        if(ncoef_max>ncoef){
          if(!correct_max_time_adj){
            # CI's for adjusted RR's:
            if(CI)
              matlines( rbind( ncoef +  1:(nrow(res[[imod]])-ncoef), ncoef +  1:(nrow(res[[imod]])-ncoef)  )[,!is.na(res[[imod]]$RR[ cond_after_ncoef ])],
                        t(res[[imod]][cond_after_ncoef,][ !is.na(res[[imod]]$RR[ cond_after_ncoef ])  ,c("lci","uci")]),
                        lty=1, lwd=1, col=col_alpha(col[imod],0.15), type="o", pch="-", cex=2 )
            # RR's:
            lines( ncoef +  1:(length(res[[imod]]$RR)-ncoef), res[[imod]]$RR[ cond_after_ncoef ], type="o", col=col[imod],cex=0.5)
          }  
          else
            if(correct_max_time_adj){
              
              cond_time_adj <- substring(res[[imod]][cond_after_ncoef,"all_cat"],1,1)=="["
              
              suppressWarnings(
                mids <- unlist(   lapply( strsplit(  res[[imod]]$all_cat[cond_after_ncoef][ cond_time_adj ], "\\[|;|,|\\]") , 
                                          function(x) mean(as.numeric(x),na.rm=T) ) )
              )
              
              xx_time_adj    <-  ( (mids - min(numbers, na.rm=T) ) * ncoef_max  +( max(numbers,na.rm=T) - mids) * (ncoef+1) ) / ( max(numbers,na.rm=T) -  min(numbers, na.rm=T) )              
              #xx_time_adj    <- ncoef +  1   +   (mids - min(numbers, na.rm=T) ) * ( ncoef_max - ncoef ) / ( max(numbers,na.rm=T) -  min(numbers, na.rm=T) )              
              RR_time_adj    <- res[[imod]]$RR[   cond_after_ncoef][cond_time_adj] 
              CI_time_adj    <- res[[imod]][cond_after_ncoef,][cond_time_adj,c("lci","uci")] 
              pval_time_adj  <- res[[imod]]$pval[ cond_after_ncoef][cond_time_adj] 
              model_time_adj <- res[[imod]]$model[cond_after_ncoef][cond_time_adj] 
              
              if(any(!is.na(model_time_adj))){
                ref_models <- sort(unique( model_time_adj[!is.na(model_time_adj)] ))
                x_ref_deltas <- 0
                if(length(ref_models)>1) x_ref_deltas <- 0.2* (1/(length(ref_models)-1) * (0:(length(ref_models)-1)) - 0.5)
                
                for(ii in ref_models ){
                  cond_ref_model <- !is.na(model_time_adj) & model_time_adj==ii
                  # CI's:
                  if(CI)
                    matlines( rbind( xx_time_adj, xx_time_adj )[, cond_ref_model & !is.na(RR_time_adj) ]+x_ref_deltas[ii], 
                              t(CI_time_adj)[                   , cond_ref_model & !is.na(RR_time_adj) ],
                              lty=1, lwd=1, col=col_alpha(col[imod],0.15), type="o", pch="-", cex=2 )
                  # RR's:
                  lines( xx_time_adj[ cond_ref_model ] + x_ref_deltas[ii], RR_time_adj[ cond_ref_model ], type="o", col=col[imod],
                         pch=ifelse( length(unique( model_time_adj[!is.na(model_time_adj)] ))==1, 1, as.character(ii)), cex=0.5)
                  
                  if(any( (cond <- !is.na(pval_time_adj[cond_ref_model]) & pval_time_adj[cond_ref_model]<=0.05) )){  # check for significant p-values
                    if( !correct_max_time_adj)
                      points( ((ncoef+1):nrow(res[[imod]]))[cond_ref_model][cond], RR_time_adj[cond_ref_model][cond], pch="*",cex=3, col=col[imod]) 
                    else
                      points( xx_time_adj[cond_ref_model][cond], RR_time_adj[cond_ref_model][cond], pch="*",cex=3, col=col[imod])
                  }
                }
              }  
            }
        }  
      }  
    } 
    
    legend("topright",legend= paste(res[[1]]$i,res[[1]]$all_cat), 
           text.col=c("gray65","black","blue","red")[text_col_cond], cex=0.6,
           pt.cex=0.6, box.lty=0,bg="transparent", ncol= (ncoef %/% 38) + as.numeric((ncoef %% 38)>0) )
    #legend("topright",legend= paste(res[[1]]$i,res[[1]]$all_cat), cex=0.6, box.lty=0, bg="transparent", ncol= (ncoef %/% 38) + as.numeric((ncoef %% 38)>0) )
    legend("topleft", legend= c("significant","",names(res)), cex=0.7, 
           pt.cex=c( 3,0,rep(1,length(res)) ),
           pch=c( 42,32,rep(1,length(res)) ), lwd=2, lty=c( 0,0,rep(1,length(res)) ), 
           col=c("black","black",col[1:length(res)]), box.lty=0,bg="transparent")  
  }
  
  
  
  ############
  #
  # plot 2 en 3(if ymax>30): coefficients without coefficients for time adjusting:
  #
  for(i in 2:3) {   # plot 2    and   plot 3
    
    if(i==3 & max(ylim)<=30) next
    if(i==3 & max(ylim)>30 ) ylim=c(0,20)
    
    if(i==2 & missing(ylim)){
      ymax   <- unlist(lapply(res,function(x) {
        if(all(dimnames(x)[[2]]!="RR")) return(NA) 
        x_tmp <- x$RR[x$RR<1000]; 
        if(any(!is.na(x_tmp))) max(x_tmp,na.rm=T) else NA 
      } ))
      
      if(any(!is.na(ymax))) ymax <- max(ymax, na.rm=T)
      else                  ymax <- 10
      ylim <- c( 0, ymax )
    }  
    
    
    plot( c(0,ncoef), ylim, type="n", main=main, xlab="effect number", ylab=ifelse(i==2, "RR", "RR under 20") )
    
    grid();abline(h=1, col="darkgray",lty=1)
    abline(v=length(res[[1]]$RR[1:ncoef])+0.5, col="orange", lty=3)
    
    #  CI's for unadjusted and adjusted RR's:
    if(CI)
      for(imod in 1:length(res)){
        if(all(dimnames(res[[imod]])[[2]]!="RR")) next
        matlines( rbind( (1:ncoef+x_deltas[imod]),(1:ncoef+x_deltas[imod]))[,!is.na(res[[imod]]$RR[1:ncoef])],
                  t(res[[imod]][1:ncoef,][ !is.na(res[[imod]]$RR[1:ncoef])  ,c("lci","uci")]),
                  lty=1, lwd=1, col=col_alpha(col[imod],0.15), type="o", pch="-", cex=2 )
      }
    # RR's:
    for(imod in 1:length(res)){
      if(all(dimnames(res[[imod]])[[2]]!="RR")) next
      lines( 1:ncoef+x_deltas[imod],res[[imod]]$RR[1:ncoef], type="o", col=col[imod],lwd=ifelse(imod==1,2,1)); 
      if(imod==1) text( 1:ncoef,res[[1]]$RR, labels=as.character(res[[1]]$i), pos=3, col=col[1], cex= ifelse(ncoef<=50,1,0.7) ) 
      if(any( (cond <- !is.na(res[[imod]]$pval[1:ncoef]) & res[[imod]]$pval[1:ncoef]<=0.05) ))                # check for significant p-values
        points( (1:ncoef+x_deltas[imod])[cond], res[[imod]]$RR[1:ncoef][cond], pch="*",cex=3, col=col[imod]) 
    }
    
    
    legend("topright",legend= paste(res[[1]]$i,res[[1]]$all_cat), 
           text.col=c("gray65","black","blue","red")[text_col_cond], cex=0.6,
           pt.cex=0.6, box.lty=0,bg="transparent", ncol= (ncoef %/% 38) + as.numeric((ncoef %% 38)>0) )
    #legend("topright",legend= paste(res[[1]]$i,res[[1]]$all_cat), cex=0.6, box.lty=0, bg="transparent", ncol= (ncoef %/% 38) + as.numeric((ncoef %% 38)>0) )
    legend("topleft", legend= c("significant","",names(res)), cex=0.7, 
           pt.cex=c( 3,0,rep(1,length(res)) ),
           pch=c( 42,32,rep(1,length(res)) ), lwd=2, lty=c( 0,0,rep(1,length(res)) ), 
           col=c("black","black",col[1:length(res)]), box.lty=0,bg="transparent")
  }
  
}  # end of plot_res

###############################
#
plot_res2 <- function(res, tit="", col=col_list ){
  par(mfrow=c(2,1))
  ymax <- max(unlist(lapply(res,function(x)max(x$RR[x$RR<500],na.rm=T))))
  
  
  plot(res[[2]]$RR, type="o", col="blue",ylim=c(0,  ymax ), 
       main=tit,xlab="coefficients number",ylab="RR"); 
  grid();abline(h=1, col="gray",lty=3)
  abline(v=length(res[[1]]$RR)+0.5, col="orange", lty=3)
  lines(1:length(res[[1]]$RR)+0.15,res[[1]]$RR, type="o", col=col[1],lwd=2); 
  for(i in 2:length(res)){
    time_adj <- strsplit( substring(dimnames(res[[i]])[[1]],1,(length(dimnames(res[[i]])[[1]])-1)), ":")
    lapply(time_adj,function(x)x)
    
    lines( res[[i]]$RR, type="o", col=col[i])
  }  
  
  text_col_cond <- 1 + as.numeric(!is.na(res[[1]]$RR))
  text_col_cond <- text_col_cond + as.numeric(!is.na(res[[1]]$RR) & !is.na(res[[1]]$pval<=0.1))
  text_col_cond <- text_col_cond + as.numeric(!is.na(res[[1]]$RR) & !is.na(res[[1]]$pval<=0.05))
  legend("topright",legend= paste(res[[1]]$i,res[[1]]$all_cat), 
         text.col=c("gray65","black","blue","red")[text_col_cond], 
         cex=0.7, box.lty=0,bg="transparent")
  #legend("topright",legend= paste(res[[1]]$i,res[[1]]$all_cat), cex=0.7, box.lty=0,bg="transparent")
  legend("topleft",legend= names(res), cex=0.7, pch=1, lwd=1, col=col[1:length(res)], box.lty=0,bg="transparent")
}



###############################
#
#  functions to add results to report list and model list and save them.  ==>  delete ???
#
add_to_report_list <- function(x, name, list=report_list, add=T){ 
  if(!add) list <- list()
  else if(!is.list(list)) stop("'list' must be a list") 
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

###########
#  save report_list and model_list  ==>  ?? delete
#
save_results <- function(name, report_list, models_list, sep="" ){
  
  name <- gsub(":","",name)
  
  begin_report <- paste0(dap, sep, name, "_report")
  begin_report <- trimws( begin_report,"left","[ _:;,.]" )
  
  
  # copy report [and models] lists to lists with other names:
  assign(begin_report,  report_list )
  
  # save report [and models] list as .RData:
  save(list=begin_report, file = paste0(sdr, begin_report,".RData" ))
  
  if( !missing(models_list))
    if(length(models_list)>0 ){
      begin_models <- paste0(dap, sep, name, "_models")
      assign(begin_models,  models_list )
      save(list=begin_models, file = paste0(sdr_models, begin_models,".RData" ))
    }
  
  # print tables from the 'report' list in .txt file:
  sink(paste0(sdr, begin_report, ".txt" ))
  
  old_width = options (width=200, max.print=99999 )
  cat(paste0("\n\nNumber of rows in the dataset = ", nrow(scri_input),".\n\n\n"))
  
  for(i in 1:length(report_list)){
    cat(paste0("\n\n",names(report_list[i]),":"))
    for(j in 1:length(report_list[[i]])){
      cat(paste0("\n\n",names(report_list[[i]][j]),":\n\n"))
      print(lapply(report_list[[i]][[j]], format, justify="left", digits=3))
    }
  }  
  options(old_width)
  
  sink()
  
}

#
#####################


#################################################################
#################################################################
#################################################################
#
scri <- function(vax_def,
                 #strata_var = "all data", 
                 output_name, 
                 extra_name = "", 
                 formula="", time_seq=c(), time_seq_ref="most events",
                 event_time, event, event_date, event_info,
                 id,
                 rws ,
                 ref,
                 time_dep = NA,
                 combine_vars = c(),
                 start_obs, end_obs,
                 lab_orders = NA,
                 data,  strata_var="", strata_value, use_all_events=F,
                 data_event  = NA,
                 rw_observed_percentage=0,   # 100% - the whole interval must be observed; 0% - even one day is enough to include this 'id' in the current risk window
                 censored_vars=c(),            # The rule 'rw_observed_percentage' does not work for variables 'censored_vars'. 
                 #  (for example, "death_days" ==> 'id' is included in the corresponding risk window till death date.)
                 event_in_rw=T,                # if event in rw ==> this rw should not be deleted even if not completely observed
                 image_plots = T, image_file_separate=F, image_strata, image_tit="", image_brand=F, max_n_points=NA,
                 nvax,
                 delete_coef_no_events      = T,
                 delete_rows_start_with_no  = T,
                 delete_no_ref_cols         = T,
                 delete_median_cols         = T,
                 paral = T,  paral_vars = c(), n_cores=NA,               # parallel programming (using more cores)
                 lprint=T, 
                 global_plot_name, add_global_plot = T, CI = T,
                 save_data = F,
                 extra_plots=F, width=14, 
                 extra_parameters,
                 add_to="",
                 add_to_itself = F,
                 ndigits = 2,
                 sdr_tabs, sdr_models, 
                 cut_points_name = "",
                 lforest=T, forest_nrows=50 ,forest_cex=0.5, forest_cex_head=0.5,
                 ...
){   
  
  if(missing(data)) stop("Dataset 'data' is missing.")
  
  if(!missing(vax_def))
    if(class(vax_def)=="scri_parameters"){
      # vax variables:
      if(missing(nvax))          nvax          <- vax_def$nvax    # delete?
      
      # variable names:
      if(missing(id))            id            <- vax_def$data_parameters$id
      if(missing(start_obs))     start_obs     <- vax_def$data_parameters$start_obs
      if(missing(end_obs))       end_obs       <- vax_def$data_parameters$end_obs
      if(missing(censored_vars)) 
        if(any(names(vax_def$data_parameters)=="censored_vars")) 
          censored_vars <- vax_def$data_parameters$censored_vars
      
      # model parameters:
      if(missing(rws))              rws             <- vax_def$rws$rws_def
      if(missing(cut_points_name))  cut_points_name <- vax_def$rws$cut_points_name
      
      # lab order:
      if(missing(lab_orders))    lab_orders    <- vax_def$lab_orders
      
    }
  
  if(!missing(extra_parameters)){
    
    if( missing(image_plots        ) & "image_plots"         %in% names(extra_parameters) ) image_plots         <- extra_parameters[["image_plots"            ]]   
    if( missing(image_file_separate) & "image_file_separate" %in% names(extra_parameters) ) image_file_separate <- extra_parameters[["image_file_separate"    ]]   
    if( missing(image_strata       ) & "image_strata"        %in% names(extra_parameters) ) image_strata        <- extra_parameters[["image_strata"           ]]   
    if( missing(image_brand        ) & "image_brand"         %in% names(extra_parameters) ) image_brand         <- extra_parameters[["image_brand"            ]]   
    if( missing(max_n_points       ) & "max_n_points"        %in% names(extra_parameters) ) max_n_points        <- extra_parameters[["max_n_points"           ]]   
    
    
    if( missing(delete_coef_no_events    ) & "delete_coef_no_events"     %in% names(extra_parameters) ) delete_coef_no_events     <- extra_parameters[[ "delete_coef_no_events"     ]]   
    if( missing(delete_coef_no_events    ) & "delete_coef_no_events"     %in% names(extra_parameters) ) delete_coef_no_events     <- extra_parameters[[ "delete_coef_no_events"     ]]   
    if( missing(delete_rows_start_with_no) & "delete_rows_start_with_no" %in% names(extra_parameters) ) delete_rows_start_with_no <- extra_parameters[[ "delete_rows_start_with_no" ]]   
    if( missing(delete_no_ref_cols       ) & "delete_no_ref_cols"        %in% names(extra_parameters) ) delete_no_ref_cols        <- extra_parameters[[ "delete_no_ref_cols"        ]]   
    if( missing(delete_median_cols       ) & "delete_median_cols"        %in% names(extra_parameters) ) delete_median_cols        <- extra_parameters[[ "delete_median_cols"        ]]   
    
    
    if( missing(paral           ) & "paral"             %in% names(extra_parameters) ) paral            <- extra_parameters[["paral"            ]]   
    if( missing(n_cores         ) & "n_cores"           %in% names(extra_parameters) ) n_cores          <- extra_parameters[["n_cores"          ]]   
    if( missing(lprint          ) & "lprint"            %in% names(extra_parameters) ) lprint           <- extra_parameters[["lprint"           ]]   
    if( missing(global_plot_name) & "global_plot_name"  %in% names(extra_parameters) ) global_plot_name <- extra_parameters[["global_plot_name" ]]   
    if( missing(add_global_plot ) & "add_global_plot"   %in% names(extra_parameters) ) add_global_plot  <- extra_parameters[["add_global_plot"  ]]   
    if( missing(save_data       ) & "save_data"         %in% names(extra_parameters) ) save_data        <- extra_parameters[["save_data"        ]]   
    if( missing(extra_plots     ) & "extra_plots"       %in% names(extra_parameters) ) extra_plots      <- extra_parameters[["extra_plots"      ]]   
    if( missing(width           ) & "width"             %in% names(extra_parameters) ) width            <- extra_parameters[["width"            ]]
    if( missing(CI              ) & "CI"                %in% names(extra_parameters) ) CI               <- extra_parameters[["CI"               ]]
    if( missing(CI              ) & "CI_draw"           %in% names(extra_parameters) ) CI               <- extra_parameters[["CI_draw"          ]]
    
    if( missing(time_seq        ) & "time_seq"          %in% names(extra_parameters) ) time_seq         <- extra_parameters[["time_seq"         ]]   
    if( missing(sdr_tabs        ) & "sdr_tabs"          %in% names(extra_parameters) ) sdr_tabs         <- extra_parameters[["sdr_tabs"         ]]   
    if( missing(sdr_models      ) & "sdr_models"        %in% names(extra_parameters) ) sdr_models       <- extra_parameters[["sdr_models"       ]]   
    
  }
  
  if(!missing(event_info)){
    if(missing(event     )) event      <- event_info[["event"     ]]
    if(missing(event_time)) event_time <- event_info[["event_time"]]
    if(missing(event_date)) event_date <- event_info[["event_date"]]
  }
  
  
  
  
  #######################################################
  #
  #             output structure, content of output files, directories:
  #
  ################
  # directories:
  #
  #     directory 1: g_export / scri / { ?dap }_{ event }  /   ?  /{output_name}.txt; {output_name}.RData; {output_name}_{stratum ?or all strata? }.pdf
  #
  #     directory 2: g_?local? / scri / { event }  / ?    /{output_name}_models.RData
  #
  ###############
  # 'output_name'
  #
  #     output_name <- { dap }_{ event }_[ extra_name ]_[ nothing or covid_selection ]_{ no_split; brand  via vax_dep 'before' }_{ dist  via vax_dep 'after'}_{ nothing or stratum }
  #
  ###############
  # content of {output_name}.RData, {output_name}.txt, {output_name}_models.RData files; {output_name}.pdf
  #   
  #    without strata:      |    with strata:                                                                                                                                           
  #    _____________________|________________________________________________________________________                                                                           
  #                         |                                                                                                                                                                              
  #                         |                                                                                                                                                         
  #     - cut_points_28d    |      -- cut_points_28d                                                                
  #       -- $no_adj        |         -- $no_adj                    ( <- if only stratum events: use_all_events=F )                                                                                              
  #       -- $adj_30ds_1    |         -- $adj_30ds_1                          --  ||  --                                                                                                    
  #                         |         -- $adj_20ds_10                         --  ||  --                                                                                
  #                         |         -- ...                                                                                                                          
  #       -- $adj_20ds_10   |         -- $no_adj_all_events         ( <- all events from are used: use_all_events=T )                                                                                                                   
  #                         |         -- $adj_30ds_1_all_events     (    they are place in on separate category for risk windows )                                                                                      
  #                         |         -- $adj_20ds_10_all_events    (    but they are used for calendar time intervals           )                                                                                                        
  #       -- ...            |         -- ...                                  --  ||  --                                                                                                                      
  #                         |                                                                                                                                            
  #     - cut_points_7d     |      -- cut_points_7d                                                                                                                          
  #       -- ...                      -- ...                                                                                                                             
  #
  
  output_name <- ""
  if(any(ls()=="dap"))                            output_name <- c( output_name, dap                                                                  )
  if(any(ls()=="event"))                          output_name <- c( output_name, event                                                                )
  if(extra_name != "")                            output_name <- c( output_name, extra_name                                                           )
  if(any(ls()=="covid_selection"))                output_name <- c( output_name, "covid_selection"                                                    )
  if(any(names(vax_def$rws)=="vax_dep")){                                                                  
    if(any(names(vax_def$rws$vax_dep)=="before")) output_name <- c( output_name, paste0( gsub("_","",vax_def$rws$vax_dep["before"]), collapse = "_")  )
    if(any(names(vax_def$rws$vax_dep)=="after" )) output_name <- c( output_name, paste0( gsub("_","",vax_def$rws$vax_dep["after"] ), collapse = "_")  )         
  } else                                          output_name <- c( output_name, "nosplit"                                                            )
  if(strata_var!="")                              output_name <- c( output_name, strata_value                                                         )
  #if(strata_var!="")                              output_name <- c( output_name, paste0( strata_value, "_",  ifelse(use_all_events,"allevents",""))  )
  
  output_name <- paste0(output_name, collapse="_")
  output_name <- trimws(output_name, "left", "_")
  
  
  # stratum:
  # if only for subset ==> delete all other rows
  if(strata_var!="" & !use_all_events) {
    data <- data[ data[,strata_var]== strata_value, ]
    strata_var <- ""
  }
  
  
  if( length(time_seq)>1 & length(time_seq_ref)==1 ) time_seq_ref <- rep(time_seq_ref,length(time_seq))
  
  if(any(ls()=="res")) rm(res)
  
  # create image plots:
  if(image_plots){  
    
    if(any( !( names(list(...)) %in% names(formals(pdf)) ) )) 
      stop(paste0("Argument[s] '", paste(names(list(...))[ !( names(list(...)) %in% names(formals(pdf)) )], collapse="', '" ),
                  "' is not an argumnent of function 'pdf' from library 'qpdf'."))
    
    pdf(file=paste0(sdr_tabs,"image_tmp.pdf"), width=width,  ... )
    #pdf(file=paste0(sdr,"image_",gsub(" |:","",istr),"_tmp.pdf"), width=width,  ... )
    
    if(!missing(image_strata)){
      for(im_str in unique(data[ , image_strata ]) ){
        cond <- data[,image_strata ]==im_str
        #for(im_str in unique(data[ cond_stratum, image_strata ]) ){
        #cond <- cond_stratum & data[,image_strata ]==im_str
        if(image_brand)
          for(im_br in rev(names(sort(table(unlist(data[ cond, paste0("type_vax",1:nvax,"_short") ]))))) )
            brand_images( data[ cond, ], ae_event=event, brand=im_br, tit=paste0(image_tit,im_str), max_n_points=max_n_points )
        else 
          brand_images( data[ cond, ], ae_event=event,   tit=paste0(image_tit,im_str), max_n_points=max_n_points )
      }
    }
    else {
      if(image_brand)
        for(im_br in rev(names(sort(table(unlist(data[ , paste0("type_vax",1:nvax,"_short") ]))))) )
          brand_images( data, ae_event=event, brand=im_br, tit=image_tit, max_n_points=max_n_points )
      else 
        brand_images( data, ae_event=event, tit=image_tit, max_n_points=max_n_points  )
      
      #	    for(im_br in rev(names(sort(table(unlist(data[ cond_stratum, paste0("type_vax",1:nvax,"_short") ]))))) )
      #	      brand_images( data[cond_stratum,], ae_event=event, brand=im_br, tit=image_tit, max_n_points=max_n_points )
      #	  else 
      #	    brand_images( data[cond_stratum,], ae_event=event, tit=image_tit, max_n_points=max_n_points  )
    }
    
    dev.off()
    
    image_file_separate <- F
    if(!is.null(vax_def$vax_dep)) image_file_separate <- T
    if(image_file_separate) {
      
      if( add_global_plot &  file.exists(paste0(sdr_tabs,output_name,"_images.pdf")) ){
        
        file.copy( from=paste0(sdr_tabs,output_name,"_images.pdf"), to=paste0(sdr_tabs,"temp_all_images.pdf"), overwrite=T)   
        files_to_copy <- c( paste0(sdr_tabs,"temp_all_images.pdf"), paste0(sdr_tabs,"image_tmp.pdf"))
        #files_to_copy <- c( paste0(sdr,"_temp_all_images.pdf"), paste0(sdr,"image_",gsub(" |:","",istr),"_tmp.pdf"))
        qpdf::pdf_combine( files_to_copy , paste0(sdr_tabs,output_name,"_images.pdf")  )  
        if(file.exists(paste0(sdr_tabs,"temp_all_images.pdf")))     suppressWarnings( file.remove(paste0(sdr_tabs,"temp_all_images.pdf")) )
        
      } 
      else file.copy( from=paste0(sdr_tabs,"image_tmp.pdf"), to=paste0(sdr_tabs,output_name,"_images.pdf"), overwrite=T)
      #else file.copy( from=paste0(sdr,"image_",gsub(" |:","",istr),"_tmp.pdf"), to=paste0(sdr,dap, "_", gsub(" |:","",output_name),"_images.pdf"), overwrite=T)
    }
  }
  
  res <- vector("list", length=length(time_seq)+1)
  names(res) <- c("no_adj", names(time_seq) )
  
  if(strata_var!="" & use_all_events) names(res) <- paste0(names(res), "_all_events")
  
  res[[1]] <-  scri_fit(formula = formula,
                        vax_def=vax_def,
                        event_time = event_time, event = event, id=id,
                        rws        = rws, 
                        ref=ref,
                        time_dep   = time_dep,
                        combine_vars = combine_vars,
                        start_obs  = start_obs, end_obs = end_obs,
                        lab_orders = lab_orders,
                        data       = data, strata_var=strata_var, strata_value=strata_value, use_all_events=use_all_events,
                        data_event = data_event,
                        nvax       = nvax,
                        delete_coef_no_events      = delete_coef_no_events,     
                        delete_rows_start_with_no  = delete_rows_start_with_no, 
                        delete_no_ref_cols         = delete_no_ref_cols,        
                        delete_median_cols         = delete_median_cols,        
                        rw_observed_percentage     = rw_observed_percentage,          
                        censored_vars = censored_vars,           
                        event_in_rw = event_in_rw,               
                        lprint=lprint,
                        save_data = save_data)
  
  #############################
  #
  # parallel use of cores:
  #
  if(length(time_seq)>0 & paral){
    
    library(parallel)
    
    if(is.na(n_cores)) n_cores <- detectCores() - 2 
    n_cores <- min( length(time_seq), n_cores, na.rm=T )
    
    cl      <- makeCluster( n_cores )    # outfile = paste0(sdr,"log_parallel.txt")
    
    clusterEvalQ( cl, { library("survival") } )
    clusterExport(cl, c("scri_fit", "refresh_event_variable", "create_rws", "split_intervals", 
                        "summary_tab", "factor_ref", "combine_vars_func" ), 
                  envir=environment()  )
    for(ipar in names(as.list(match.call()))[-1])
      clusterExport(cl, ipar, envir=environment() )        
    if(!missing(paral_vars)) clusterExport(cl, paral_vars )
    
    res_paral <- parLapply(cl,
                           time_seq,  # list with different sets of intervals
                           function(i_time_seq) 
                             scri_fit(formula = ifelse(formula=="","", paste( formula, " +  cal_time_cat") ),
                                      vax_def=vax_def,
                                      event_time = event_time, event = event, id=id,
                                      rws = rws, 
                                      ref=ref,
                                      time_seq = i_time_seq,  split_seq_name = "cal_time_cat", #time_seq_ref="most events",time_seq_ref=time_seq_ref[i_time_seq],
                                      time_dep   = time_dep, 
                                      combine_vars = combine_vars,
                                      start_obs = start_obs, end_obs = end_obs,
                                      lab_orders = lab_orders,
                                      data = data, strata_var=strata_var, strata_value=strata_value, use_all_events=use_all_events,
                                      data_event  = data_event,
                                      nvax       = nvax,
                                      delete_coef_no_events      = delete_coef_no_events,    
                                      delete_rows_start_with_no  = delete_rows_start_with_no,
                                      delete_no_ref_cols         = delete_no_ref_cols,       
                                      delete_median_cols         = delete_median_cols,       
                                      rw_observed_percentage     = rw_observed_percentage,   
                                      censored_vars = censored_vars,           
                                      event_in_rw = event_in_rw,               
                                      lprint=lprint,
                                      save_data = save_data)
    )
    
    stopCluster(cl)
    
    names_overlap <- match(names(res),names(res_paral))
    res[ names(res_paral)[ names_overlap[!is.na(names_overlap)] ] ] <- res_paral[!is.na(match(names(res_paral),names(res)))]
    
  }  # end of parallel
  
  
  if(length(time_seq)>0 & !paral)
    for(i in 2:length(res)){
      res[[i]] <-  scri_fit(formula = ifelse( formula=="","", paste( formula, " +  cal_time_cat") ),
                            vax_def=vax_def,
                            event_time = event_time, event = event, id=id,
                            rws = rws, 
                            ref=ref,
                            time_seq = time_seq[[i-1]] , time_seq_ref=time_seq_ref[i-1], split_seq_name = "cal_time_cat", #time_seq_ref="most events", 
                            time_dep   = time_dep,
                            combine_vars = combine_vars,
                            start_obs = start_obs, end_obs = end_obs,
                            lab_orders = lab_orders,
                            data = data, strata_var=strata_var, strata_value=strata_value, use_all_events=use_all_events,
                            data_event   = data_event,
                            nvax       = nvax,
                            delete_coef_no_events      = delete_coef_no_events,    
                            delete_rows_start_with_no  = delete_rows_start_with_no,
                            delete_no_ref_cols         = delete_no_ref_cols,       
                            delete_median_cols         = delete_median_cols,       
                            rw_observed_percentage     = rw_observed_percentage,     
                            censored_vars = censored_vars,           
                            event_in_rw = event_in_rw,               
                            lprint=lprint,
                            save_data = save_data)#[[1]]
      
    }  
  
  
  # for output file with 'tabs'  
  tabs             <- lapply(res,function(x) x$tab )
  attributes(tabs) <- c( attributes(tabs), lapply(res,function(x)x[names(x)==c("tab_full")]) )
  class(tabs)      <- "scri_tabs"
  attributes(tabs) <- c( attributes(tabs), 
                         name      = output_name,
                         vax_def = list(vax_def),                  
                         event   = list( c(event=event, event_time=event_time, event_date=event_date) ) )
  
  res <- c (res, name      = output_name,
            vax_def = list(vax_def),                  
            event   = list( c( event=event, event_time=event_time, event_date=event_date))   )
  
  
  res  <- list(res) ;  if(cut_points_name!="") names( res) <- cut_points_name
  tabs <- list(tabs);  if(cut_points_name!="") names(tabs) <- cut_points_name
  
  
  if(add_to_itself) add_to <- output_name
  
  if(add_to!="") {
    
    load(paste0(sdr_tabs,   add_to,        ".RData"))
    load(paste0(sdr_models, add_to, "_models.RData"))
    
    tabs <- c(        get(add_to)           , tabs )
    res  <- c( get(paste0(add_to,"_models")), res  ) 
    
  }
  
  
  ##################  
  # save the results in .RData and .txt files:  
  
  assign(       output_name           , tabs)
  assign(paste0(output_name,"_models"), res )
  
  save( list =        output_name           , file = paste0(sdr_tabs  , output_name,       ".RData" ))
  save( list = paste0(output_name,"_models"), file = paste0(sdr_models, output_name,"_models.RData" ))
  
  # print tables from the 'report' list in .txt file:
  sink(paste0(sdr_tabs, output_name, ".txt" ))
  {
    old_width = options (width=300, max.print=99999 )
    #cat(paste0("\n\nNumber of ids in the dataset = ", length(data_vax[,?id]),".\n\n\n"))   #???
    #cat(paste0("\n\nNumber of rows in the dataset = ", nrow(scri_input),".\n\n\n"))   #???
    
    cat(paste("\n\n Name:\t",output_name,"\n"))
    
    if(length(tabs)>0)
      for(i1 in 1:length(tabs)){
        txt_i1 <- paste0("\n\n",names(tabs)[i1])
        if(length(dim(tabs[[i1]]))==2){
          cat(paste(txt_i1,":\n\n"))
          print(format(tabs[[i1]],              justify="left", digits=ndigits))
        }
        
        if(length(tabs[[i1]])>0)
          for(i2 in 1:length(tabs[[i1]])){
            txt_i2 <- paste0(txt_i1," $ ",names(tabs[[i1]])[i2])
            if(length(dim(tabs[[i1]][[i2]]))==2){
              cat(paste(txt_i2,":\n\n"))
              print(format(tabs[[i1]][[i2]],        justify="left", digits=ndigits, nsmall=ndigits))
            }
            if(length(tabs[[i1]][[i2]])>0)
              for(i3 in 1:length(tabs[[i1]][[i2]])){
                txt_i3 <- paste0(txt_i2," $ ",names(tabs[[i1]][[i2]])[i3])
                if(length(dim(tabs[[i1]][[i2]][[i3]]))==2){
                  cat(paste(txt_i3,":\n\n"))
                  print(format(tabs[[i1]][[i2]][[i3]], justify="left", digits=ndigits, nsmall=ndigits))
                }
              } # for i3
          } # for i2
      } # for i1  
    
    
    cat("\n\n\nSpecified parameters:\n\n")
    
    for(i1 in 1:length(tabs)){
      cat(paste(" ", names(tabs)[i1],":\n\n"))
      cat("   Event info:\t")
      cat( paste0( "c( ", paste0( paste0( names(attributes(tabs[[i1]])$event), ' = "', attributes(tabs[[i1]])$event, '"' ), collapse=', ' ), " )" ))
      
      cat("\n\n\n   Vaccine info:\n")
      print(attributes(tabs[[i1]])$vax_def)
      cat("\n\n")
    }
    
    options(old_width)
  }  
  sink()
  
  
  ret <- list( tabs   = tabs,
               models = res  )
  class(ret) <- "scri_output"
  
  
  if(add_global_plot){  
    # create plots with coefficients:
    if(extra_plots) plot_name <- paste0(sdr_tabs,output_name,".pdf")
    else            plot_name <- paste0(sdr_tabs,"temp_plot.pdf")
    
    pdf(file=plot_name, width=width,  ...)
    #for(istr in names(tabs_strata) )
    if(!is.null(tabs[[1]]))
      for(i1 in 1:length(tabs))
        #for(i2 in 1:length(tabs[[i1]]))
        plot_res(tabs[[i1]], main=paste( event, formula,"; ",image_tit,"\n",output_name,"; ", names(tabs)[i1]), col=col_list, CI=CI)
    
        if(lforest) forest_plots_tab( tabs[[i1]][[1]], nrows_forest_plot=forest_nrows,cex=forest_cex, cex_head =forest_cex_head, ltable=F )  
    
    #  for(istr in names(tabs_strata) )
    #    if(!is.null(tabs_strata[[istr]][[1]]))
    #      plot_res(tabs_strata[[istr]], main=paste( event, formula_text,"; ",image_tit,"\n",output_name), col=col_list, CI=CI)
    dev.off()
    
    # add plot[s] into a global file
    if( add_global_plot &  file.exists(paste0(sdr_tabs,dap, "_", gsub(" |:","",output_name),".pdf")) ){
      file.copy( from=paste0(sdr_tabs,dap, "_", gsub(" |:","",output_name),".pdf"), to=paste0(sdr_tabs,"temp_plot_2.pdf"), overwrite=T)   
      files_to_copy <- c( paste0(sdr_tabs,"temp_plot_2.pdf"), plot_name)
    } 
    else files_to_copy <- plot_name
    
    if(image_plots & !image_file_separate) files_to_copy <- c( paste0(sdr_tabs,"image_tmp.pdf"), files_to_copy )
    #if(image_plots & !image_file_separate) files_to_copy <- c( paste0(sdr,"image_",gsub(" |:","", strata),"_tmp.pdf"), files_to_copy )
    
    qpdf::pdf_combine( files_to_copy , paste0(sdr_tabs,output_name,".pdf")  )    
    
    if(!extra_plots & file.exists(plot_name))       suppressWarnings( file.remove(       plot_name              ) )
    if(file.exists(paste0(sdr_tabs,"temp_plot_2.pdf")))  suppressWarnings( file.remove(paste0(sdr_tabs, "temp_plot_2.pdf" )) )
    if(file.exists(paste0(sdr_tabs,"image_tmp.pdf")))    suppressWarnings( file.remove(paste0(sdr_tabs, "image_tmp.pdf"    )) )
    #if(file.exists(paste0(sdr,"image_",gsub(" |:","",strata),"_tmp.pdf")))   suppressWarnings( file.remove(paste0(sdr,"image_",gsub(" |:","",strata),"_tmp.pdf")) )
  }  
  
  
  ret
  
}  # the end of function 'scri'


#
#################################################################
#################################################################
#################################################################






#################################################################
#################################################################
#################################################################
#
#    functions for images and 3D plots:
# 
#######
#  data preparation for plots:
data_plot_prepair <- function(vax_def, plot_data, event, vax2, vax3, vax4){
  
  # delete rows with missing events or missing vax1:
  plot_data <- plot_data[!is.na(plot_data[,paste0(event,"_date")]) & !is.na(plot_data[,vax_def$data_parameters$vax_date]), ]
  #plot_data <- plot_data[!is.na(plot_data[,paste0(event,"_date")]) & !is.na(plot_data$date_vax1), ]
  
  #  if(missing(vax2)){  # TRUE if variable 'date_vax2' exists and is not empty.
  #    vax2 <- any(names(plot_data)=="date_vax2")
  #    if(vax2) vax2 <- any(!is.na(plot_data$date_vax2))
  #  }  
  #  if(missing(vax3)){  # TRUE if variable 'date_vax3' exists and is not empty.
  #    vax3 <- any(names(plot_data)=="date_vax3")
  #    if(vax3) vax3 <- any(!is.na(plot_data$date_vax3))
  #  }  
  #  if(missing(vax4)){  # TRUE if variable 'date_vax3' exists and is not empty.
  #    vax4 <- any(names(plot_data)=="date_vax4")
  #    if(vax4) vax4 <- any(!is.na(plot_data$date_vax4))
  #  }  
  
  
  plot_data$event_date  <-  as.Date(plot_data[,paste0(event,"_date")]) 
  plot_data$vax_date    <-  as.Date(plot_data[,paste0(vax_def$data_parameters$vax_date)]) 
  
  plot_data$event_days  <-  plot_data[, event_time                      ] 
  plot_data$vax_days    <-  plot_data[, vax_def$data_parameters$vax_time] 
  
  # vax_def:
  #
  #         $data_parameters$vax_name:  "vax_number"  ("vax 1 ",  "vax 2 " "vax 3 " vax 4)  or 'vax_name' ( "dose  1.1", "dose  1.2", "booster 1"  booster 2  booster 3     )
  #         $data_parameters$vax_date:  "date_vax"
  #         $data_parameters$vax_time:  "vax_days"
  #
  #         $rws$cond_var:    "vax_number"  or "vax_name"
  #         $rws$name:        "v0" "v1" "v2" "v3" "v4"
  #         $rws$cond_value: "vax 1 " "vax 1 " "vax 2 " "vax 3 " "vax 4 "   ==> values of variable vax_Def$data_parameters$vax_name
  #         
  #         # condition for vax with names "v0", "v1", "v2", ...
  #         for( ivaxname in vax_def$rws$name )
  #         ==> cond_vax_name_ivaxname <- data_plot[, vax_def$rws$cond_var ] == vax_def$rws$cond_value[ vax_def$rws$name==ivaxname ]
  
  #         # condition for vax with names "v0", "v1", "v2", ...
  #         for( ivaxname in vax_def$rws$name )
  #         ==> cond_vax_name_ivaxname <- data_plot[, vax_def$rws$cond_var ] == vax_def$rws$cond_value[ vax_def$rws$name==ivaxname ]
  
  
  # condition for one vax:
  # ways 1,2:
  for( ivaxname in vax_def$rws$name[-1] ){
    cond_vax_name_ivaxname   <-  data_plot[, vax_def$rws$cond_var ]             == vax_def$rws$cond_value[ vax_def$rws$name==ivaxname ]
    cond_vax_name_ivaxname   <-  data_plot[, vax_def$data_parameters$vax_name ] == vax_def$rws$cond_value[ vax_def$rws$name==ivaxname ]
  }
  # ways 3,4:
  for( ivaxname in unique( data_plot[, vax_def$data_parameters$vax_name ] ) ){
    cond_vax_name_ivaxname   <-  data_plot[, vax_def$rws$cond_var ]             == vax_def$rws$cond_value[ vax_def$rws$name==ivaxname ]
    cond_vax_name_ivaxname   <-  data_plot[, vax_def$data_parameters$vax_name ] == vax_def$rws$cond_value[ vax_def$rws$name==ivaxname ]
  }
  
  plot_data$event_days   <- as.numeric(difftime( plot_data$event_date, as.Date("2020-09-01"), units="days"))
  plot_data$vax_days     <- as.numeric(difftime( plot_data$vax_date,  as.Date("2020-09-01"), units="days"))
  if(vax2) 
    plot_data$vax2_num   <- as.numeric(difftime( plot_data$date_vax2,  as.Date("2020-09-01"), units="days"))
  if(vax3) 
    plot_data$vax3_num <- as.numeric(difftime( plot_data$date_vax3,  as.Date("2020-09-01"), units="days"))
  if(vax4) 
    plot_data$vax4_num <- as.numeric(difftime( plot_data$date_vax4,  as.Date("2020-09-01"), units="days"))
  
  plot_data$event_vax1_num    <- as.numeric(difftime( plot_data$event_date, plot_data$date_vax1,     units="days"))
  if(vax2) 
    plot_data$event_vax2_num    <- as.numeric(difftime( plot_data$event_date, plot_data$date_vax2,     units="days"))
  if(vax3) 
    plot_data$event_vax3_num  <- as.numeric(difftime( plot_data$event_date, plot_data$date_vax3,     units="days"))
  if(vax4) 
    plot_data$event_vax4_num  <- as.numeric(difftime( plot_data$event_date, plot_data$date_vax4,     units="days"))
  
  
  #####
  # create list with conditions for points before vax1, between vax1 and vax2, between vax2 and vax3, after vax3:
  plot_data$cond_before_vax1 <- plot_data$event_vax1_num<0
  plot_data$cond_after_vax1  <- plot_data$event_vax1_num>=0
  
  plot_data$cond_between_vax1_vax2       <- plot_data$event_vax1_num>=0 
  if(vax2){
    cond <- !is.na(plot_data$event_vax2_num)
    plot_data$cond_between_vax1_vax2[cond] <-  plot_data$event_vax2_num[cond]<0 & plot_data$cond_between_vax1_vax2[cond]
    
    plot_data$cond_after_vax2        = plot_data$event_vax2_num>=0 
    plot_data$cond_after_vax2[is.na(plot_data$cond_after_vax2)] = F 
    
    
    plot_data$cond_between_vax2_vax3       <-  plot_data$cond_after_vax2 
    if(vax3){
      cond <- !is.na(plot_data$event_vax3_num)
      plot_data$cond_between_vax2_vax3[cond] <- plot_data$event_vax3_num[cond]<0 & plot_data$cond_between_vax2_vax3[cond]
      
      plot_data$cond_after_vax3        <- plot_data$event_vax3_num>=0 
      plot_data$cond_after_vax3[is.na(plot_data$cond_after_vax3)] <- F
      
      plot_data$cond_between_vax3_vax4       <-  plot_data$cond_after_vax3 
      if(vax4){
        cond <- !is.na(plot_data$event_vax4_num)
        plot_data$cond_between_vax3_vax4[cond] <- plot_data$event_vax3_num[cond]<0 & plot_data$cond_between_vax3_vax4[cond]
        
        plot_data$cond_after_vax4        <- plot_data$event_vax4_num>=0 
        plot_data$cond_after_vax4[is.na(plot_data$cond_after_vax4)] <- F
      }  
    }
  }
  
  plot_data
}   # end of function 'data_plot_prepair'


#######
# plot with 3 images per vaccine :
images_plots_doses <- function(vax_def, data, event, 
                               tit="", 
                               plots=1:100, 
                               mfrow, mfcol, add_plots=F,
                               xmin, xmax, ymin, ymax, 
                               xlab_extra = "",
                               cex=0.8,
                               place = "bottomright"  # for legend
){
  
  data <- data_plot_prepair(data, event)
  if(nrow(data)==0)  return()
  
  if(!add_plots){
    if(missing(mfrow) & missing(mfrow)){
      if(any(names(data)=="cond_after_vax3"))
        par(mfrow=c(3,3))
      else
        par(mfrow=c(2,3))
    }  
    else{
      if(!missing(mfrow)) par(mfrow=mfrow)
      else par(mfcol=mfcol)
    }
  }  
  
  points_legend <- function(xvar, yvar, data, points=c(0,1,2,3), place, pre_lines=T, cex=cex, cex.legend=0.6, pt.cex.legend=1){
    
    ################
    # ablines:
    abline(v=c(0,7,14,21,28, 60)); abline(v=0,lwd=2)
    if(pre_lines) abline(v=c(-30,-90))
    abline(h=c(2021 + 0.25*((-1):10)), col="pink")
    abline(h=c(2021,2022), col="pink",lwd=2)
    
    ##############
    # points:
    if(any(points==0))
      points(data[ data$cond_before_vax1 & !is.na(data[,xvar]),xvar], 
             1970 + as.numeric(as.Date("2020-09-01"))/365.25 + data[ data$cond_before_vax1     & !is.na(data[,xvar]), yvar]/365.25, 
             cex=cex )
    if(any(points==1) & any(names(data)=="cond_between_vax1_vax2"))
      points(data[ data$cond_between_vax1_vax2 & !is.na(data[,xvar]), xvar], 
             1970 + as.numeric(as.Date("2020-09-01"))/365.25 + data[ data$cond_between_vax1_vax2 & !is.na(data[,xvar]), yvar]/365.25,
             col="red", cex=cex)
    if(any(points==2) & any(names(data)=="cond_between_vax2_vax3"))
      points(data[ data$cond_between_vax2_vax3 & !is.na(data[,xvar]),xvar], 
             1970 + as.numeric(as.Date("2020-09-01"))/365.25 + data[ data$cond_between_vax2_vax3 & !is.na(data[,xvar]), yvar]/365.25,
             col="magenta", cex=cex)
    if(any(points==3) & any(names(data)=="cond_after_vax3"))
      points(data[ data$cond_after_vax3 & !is.na(data[,xvar]),xvar], 
             1970 + as.numeric(as.Date("2020-09-01"))/365.25 + data[ data$cond_after_vax3        & !is.na(data[,xvar]), yvar]/365.25,
             col="purple3", cex=cex)
    
    ##############
    # legend:
    if(!missing(place)){
      legend <- c( "vertical lines: -90,-30,0,7,14,21,28,60 days" , "horiz.lines: every 3 months", "",
                   paste(event,c("before dose 1", "between dose 1 and dose 2","between dose 2 and dose 3", "after dose 3")) )
      pch    <- c(32,32,32, 1,1,1,1)
      lty    <- c(1,1,0,    0,0,0,0)
      col    <- c("black","pink","black",   "black","red","magenta","blue")
      if(F)
        if( all(names(data)!="cond_after_vax3") ) {
          legend <- legend[ -( length(legend) - c(0:1) ) ]
          pch    <- pch[    -( length(pch)    - c(0:1) ) ]
          lty    <- lty[    -( length(lty)    - c(0:1) ) ]
          col    <- col[    -( length(col)    - c(0:1) ) ]
        }  
      legend(place,legend=legend, pch=pch, lty=lty, col=col, bg="transparent", box.lty=0, cex=cex.legend, pt.cex=pt.cex.legend)
    }
    
  } # end of sub-function 'points_legend' 
  
  at_date     <- seq( 2015,  as.numeric(substring(Sys.Date(),1,4))+1, by=0.25 ) 
  at_date_lab <- month.abb[c(1,4,7,10)][1+4*at_date%%1] 
  at_date_lab[at_date_lab==month.abb[1]] <- paste0( at_date_lab, "\n", at_date%/%1 )[at_date_lab==month.abb[1]]
  
  
  #####
  #  vax 1
  ylim <- range(data$event_num, na.rm=T)
  xlim <- range(data$event_vax1_num, na.rm=T); xlim[2] <- max(28,xlim[2])
  
  if(!missing(xmin)) { xlim[1] <- xmin; bound_x <- T }
  if(!missing(xmax)) { xlim[2] <- xmax; bound_x <- T }
  if(!missing(ymin)) ylim[1] <- ymin
  if(!missing(ymax)) ylim[2] <- ymax
  
  # 1    relative to dose 1
  if(any(plots==1)){
    distr_2d <- kde2d(x=data$event_vax1_num, y=data$event_num,  h=20, n=100, lims=c(xlim, ylim))
    distr_2d$y <- 1970 + as.numeric(as.Date("2020-09-01"))/365.25 + distr_2d$y/365.25
    
    try({
      image(distr_2d, ylab=paste("date of", event), xlab=paste(event,"days from dose 1", xlab_extra), axes=F )
      axis(1); axis(2, at=at_date, labels=at_date_lab, las=1); box(col="yellow3")
      points_legend("event_vax1_num", "event_num", data, place=place, cex=cex )
      title(paste0(event,"; ",tit,"\nrelative to dose 1"))
    }, silent=T)
  }
  
  # 2    relative to dose 1
  if(any(plots==2)){
    distr_2d <- kde2d(x=data$event_vax1_num, y=data$vax1_num, h=20, n=100, lims=c(xlim, ylim))
    distr_2d$y <- 1970 + as.numeric(as.Date("2020-09-01"))/365.25 + distr_2d$y/365.25
    
    
    try({
      image(distr_2d, ylab="date of dose 1",  xlab=paste(event,"days from dose 1", xlab_extra), axes=F )
      axis(1); axis(2, at=at_date, labels=at_date_lab, las=1); box(col="yellow3")
      points_legend( "event_vax1_num", "vax1_num", data,  cex=cex)  # place="topright",
      title(paste0(event,"; ",tit,"\nrelative to dose 1"))
    }, silent=T)
  }
  
  # 3   relative to dose 1
  if(any(plots==3)){
    cond <- data$cond_before_vax1 | data$cond_between_vax1_vax2
    if(any(cond)){
      distr_2d <- kde2d(x=data$event_vax1_num[cond], y=data$vax1_num[cond], h=20, n=100, lims=c(xlim, ylim))
      distr_2d$y <- 1970 + as.numeric(as.Date("2020-09-01"))/365.25 + distr_2d$y/365.25
      
      
      try({
        image(distr_2d, ylab="date of dose 1",  xlab=paste(event,"days from dose 1", xlab_extra), axes=F  )
        axis(1); axis(2, at=at_date, labels=at_date_lab, las=1); box(col="yellow3")
        points_legend( "event_vax1_num", "vax1_num", data, points=c(0,1), cex=cex)
        title(paste0(event,"; ",tit,"\nrelative to dose 1, before dose 2"))
      }, silent=T)
    }
  }
  
  
  #####
  #  vax 2:
  
  if(any(names(data)=="cond_after_vax2")){
    
    xlim <- range(data$event_vax2_num, na.rm=T); xlim[2] <- max(28,xlim[2])
    
    if(!missing(xmin)) { xlim[1] <- xmin; bound_x <- T }
    if(!missing(xmax)) { xlim[2] <- xmax; bound_x <- T }
    
    # 4   relative to dose 2
    if(any(plots==4)){
      if(any(!is.na(data$event_vax2_num))){
        distr_2d <- kde2d(x=data$event_vax2_num[!is.na(data$event_vax2_num)], y=data$event_num[!is.na(data$event_vax2_num)],  h=20, n=100, lims=c(xlim, ylim))
        distr_2d$y <- 1970 + as.numeric(as.Date("2020-09-01"))/365.25 + distr_2d$y/365.25
        
        
        try({
          image(distr_2d, ylab=paste("date of", event),  xlab=paste(event,"days from dose 2", xlab_extra), axes=F  )
          axis(1); axis(2, at=at_date, labels=at_date_lab, las=1); box(col="yellow3")
          points_legend( "event_vax2_num", "event_num", data, pre_lines=F, cex=cex )
          title(paste0(event,"; ",tit," \nrelative to dose 2; only persons with dose 2!"))
        }, silent=T)
      }
    }
    
    # 5   relative to dose 2
    if(any(plots==5)){
      if(any(!is.na(data$event_vax2_num))){
        distr_2d <- kde2d(x=data$event_vax2_num[!is.na(data$event_vax2_num)], y=data$vax2_num[!is.na(data$event_vax2_num)], h=20, n=100, lims=c(xlim, ylim))
        distr_2d$y <- 1970 + as.numeric(as.Date("2020-09-01"))/365.25 + distr_2d$y/365.25
        
        
        try({
          image(distr_2d, ylab="date of dose 2",  xlab=paste(event,"days from dose 2", xlab_extra), axes=F  )
          axis(1); axis(2, at=at_date, labels=at_date_lab, las=1); box(col="yellow3")
          points_legend( "event_vax2_num", "vax2_num", data, pre_lines=F, cex=cex)
          title(paste0(event,"; ",tit,"\nrelative to dose 2;  only persons with dose 2!"))
        }, silent=T)
      }
    }
    
    # 6   relative to dose 2
    if(any(plots==6)){
      cond <- ( !data$cond_after_vax2 | data$cond_between_vax2_vax3 )  & !is.na(data$event_vax2_num)
      if(any(cond)){
        distr_2d <- kde2d(x=data$event_vax2_num[cond], y=data$vax2_num[cond], h=20, n=100, lims=c(xlim, ylim))
        distr_2d$y <- 1970 + as.numeric(as.Date("2020-09-01"))/365.25 + distr_2d$y/365.25
        
        
        try({
          image(distr_2d, ylab="date of dose 2",  xlab=paste(event,"days from dose 2", xlab_extra), axes=F  )
          axis(1); axis(2, at=at_date, labels=at_date_lab, las=1); box(col="yellow3")
          points_legend( "event_vax2_num", "vax2_num", data, points=c(0,1,2), pre_lines=F, cex=cex )
          title(paste0(event,"; ",tit,"\nrelative to dose 2, before dose 3; only persons with dose 2!"))
        }, silent=T)
      }
    }
  }
  
  #####
  #  vax 3:
  if(any(names(data)=="cond_after_vax3")){
    xlim <- range(data$event_vax3_num, na.rm=T); xlim[2] <- max(28,xlim[2])
    
    if(!missing(xmin)) { xlim[1] <- xmin; bound_x <- T }
    if(!missing(xmax)) { xlim[2] <- xmax; bound_x <- T }
    
    # 7   relative to dose 3
    if(any(plots==7)){
      if(any(!is.na(data$event_vax3_num))){
        distr_2d <- kde2d(x=data$event_vax3_num[!is.na(data$event_vax3_num)], y=data$event_num[!is.na(data$event_vax3_num)],  h=20, n=100, lims=c(xlim, ylim))
        distr_2d$y <- 1970 + as.numeric(as.Date("2020-09-01"))/365.25 + distr_2d$y/365.25
        
        
        try({
          image(distr_2d, ylab=paste("date of", event),  xlab=paste(event,"days from dose 3", xlab_extra), axes=F  )
          axis(1); axis(2, at=at_date, labels=at_date_lab, las=1); box(col="yellow3")
          points_legend( "event_vax3_num", "event_num", data, pre_lines=F, cex=cex )
          title(paste0(event,"; ",tit,"\nrelative to dose 3; only persons with dose 3!"))
        }, silent=T)
      }
    }
    
    # 8   relative to dose 3
    if(any(plots==8)){
      if(any(!is.na(data$event_vax3_num))){
        distr_2d <- kde2d(x=data$event_vax3_num[!is.na(data$event_vax3_num)], 
                          y=data$vax3_num[!is.na(data$event_vax3_num)], h=20, n=100, lims=c(xlim, ylim))
        distr_2d$y <- 1970 + as.numeric(as.Date("2020-09-01"))/365.25 + distr_2d$y/365.25
        
        
        try({
          image(distr_2d, ylab="date of dose 3",  xlab=paste(event,"days from dose 3", xlab_extra), axes=F  )
          axis(1); axis(2, at=at_date, labels=at_date_lab, las=1); box(col="yellow3")
          points_legend("event_vax3_num", "vax3_num", data, pre_lines=F, cex=cex)
          title(paste0(event,"; ",tit,"\nrelative to dose 3; only persons with dose 3!"))
        }, silent=T)
      }
    }  
    
    # 9   relative to dose 3
    if(any(plots==9)){
      if(any(names(data)=="cond_between_vax3_vax4")){
        cond <- ( !data$cond_after_vax3 | data$cond_between_vax3_vax4 ) & !is.na(data$event_vax3_num)
        if(any(cond)){
          distr_2d <- kde2d(x=data$event_vax3_num[cond], y=data$vax3_num[cond], h=20, n=100, lims=c(xlim, ylim))
          distr_2d$y <- 1970 + as.numeric(as.Date("2020-09-01"))/365.25 + distr_2d$y/365.25
          
          
          try({
            image(distr_2d, ylab="date of dose 3", xlab=paste(event,"days from dose 3", xlab_extra), axes=F  )
            axis(1); axis(2, at=at_date, labels=at_date_lab, las=1); box(col="yellow3")
            points_legend("event_vax3_num", "vax3_num", data, points=c(0,1,2,3), pre_lines=F, cex=cex )
            title(paste0(event,"; ",tit,"\nrelative to dose 3, before dose 4; only persons with dose 3!"))
          }, silent=T)
        }
        # else {
        #   plot(1,1,type="n", axes=F, xlab="", ylab="" );  text(1,1,"no data")
        #   title(paste0(event,"; ",tit,"relative to dose 2, before dose 3 \n only persons with dose 2!"))
        # }
      }
    } 
  }  
}  #  end of function for max 6 images 'images_plots_doses'





brand_images <- function(vax_def, plot_data, ae_event, brand="", tit="", max_n_points=NA){  
  
  cond <- plot_data[,ae_event]==1
  if(brand!=""){
    if(any(names(plot_data)=="type_vax1_short"))
      cond <- cond | ( plot_data$type_vax1_short==brand & !is.na(plot_data$type_vax1_short) & plot_data[,ae_event]==1  ) 
    if(any(names(plot_data)=="type_vax2_short"))
      cond <- cond | ( plot_data$type_vax2_short==brand & !is.na(plot_data$type_vax2_short) & plot_data[,ae_event]==1  ) 
    if(any(names(plot_data)=="type_vax3_short"))
      cond <- cond | ( plot_data$type_vax3_short==brand & !is.na(plot_data$type_vax3_short) & plot_data[,ae_event]==1  ) 
  }
  plot_data <- as.data.frame(plot_data[ cond, ])
  
  tit_points <- ""
  if(!is.na(max_n_points)) 
    if( max_n_points < nrow(plot_data) ) {
      tit_points <- paste0(max_n_points," points from ",nrow(plot_data)) 
      plot_data <- plot_data[ sample(1:nrow(plot_data),max_n_points), ]
    }
  
  
  ylim <- range( as.numeric(difftime( as.Date(plot_data[,paste0(ae_event,"_date")]), as.Date("2020-09-01"), units="days")), na.rm=T)
  ylim[1] <- min(0, ylim[1])
  ylim[2] <- max(as.numeric(difftime( as.Date("2022-01-01"), as.Date("2020-09-01"), units="days")), ylim[2])
  
  if(brand !="") brand_tit <- paste0(" is ",brand)
  else           brand_tit <- ""
  
  if(tit_points!="") brand_tit <- paste0(brand_tit, "\n", tit_points)
  
  if(tit!="") tit <- paste0(tit,"; ")
  
  # dose 1 is brand 'brand': 
  cond <- cond_event <- plot_data[,ae_event]==1
  if(brand!="") cond <- cond_event & plot_data$type_vax_short[]==brand
  if(sum(cond)>=3) images_plots_doses( vax_def, plot_data[cond,], ae_event, tit=paste(tit,"dose 1",brand_tit),  plots=c(1:3), mfrow=c(3,3),cex=1, ymin=ylim[1], ymax= ylim[2] )
  # dose 2 is brand 'brand': 
  if(brand!="" & any(names(plot_data)=="type_vax2_short")) 
    cond <- cond_event &  !is.na(plot_data$type_vax2_short) & plot_data$type_vax2_short==brand
  if(sum(cond)>=3) images_plots_doses( vax_def, plot_data[cond,], ae_event, tit=paste(tit,"dose 2",brand_tit), plots=c(4:6), add_plots = T, cex=1, ymin=ylim[1], ymax= ylim[2] )
  # dose 3 is brand 'brand': 
  if(brand!="" & any(names(plot_data)=="type_vax3_short")) 
    cond <- cond_event &  !is.na(plot_data$type_vax3_short) & plot_data$type_vax3_short==brand
  if(sum(cond)>=3) images_plots_doses( vax_def, plot_data[cond,], ae_event, tit=paste(tit,"dose 3",brand_tit), plots=c(7:9), add_plots = T, cex=1, ymin=ylim[1], ymax= ylim[2] )
  
  
  ###############    from -120 till 60 days: #################
  # dose 1 is brand 'brand': 
  xlab_extra <- "(between -120 and 60 days)"
  cond <- cond_event <- plot_data[,paste0(ae_event,"_date")]>=as.Date(plot_data$date_vax1)-90 & plot_data[,ae_event]==1 & !is.na(plot_data[,paste0(ae_event,"_date")]) & !is.na(as.Date(plot_data$date_vax1))
  if(brand!="") cond <- cond_event & plot_data$type_vax1_short==brand
  if(sum(cond)>=3) images_plots_doses( vax_def, plot_data[cond,], ae_event, tit=paste(tit,"dose 1",brand_tit), plots=c(1:3), mfrow=c(3,3),cex=1, ymin=ylim[1], ymax= ylim[2], xmin=-120, xmax=60, xlab_extra=xlab_extra, place="topleft" )
  # dose 2 is brand 'brand': 
  if(brand!="" & any(names(plot_data)=="type_vax2_short")) 
    cond <- cond_event &  !is.na(plot_data$type_vax2_short) & plot_data$type_vax2_short==brand
  if(sum(cond)>=3) images_plots_doses( vax_def, plot_data[cond,], ae_event, tit=paste(tit,"dose 2",brand_tit), plots=c(4:6), add_plots = T, cex=1, ymin=ylim[1], ymax= ylim[2], xmin=-120, xmax=60, xlab_extra=xlab_extra )
  # dose 3 is brand 'brand': 
  if(brand!="" & any(names(plot_data)=="type_vax3_short")) 
    cond <- cond_event &  !is.na(plot_data$type_vax3_short) & plot_data$type_vax3_short==brand
  if(sum(cond)>=3) images_plots_doses( vax_def, plot_data[cond,], ae_event, tit=paste(tit,"dose 3",brand_tit), plots=c(7:9), add_plots = T, cex=1, ymin=ylim[1], ymax= ylim[2], xmin=-120, xmax=60, xlab_extra=xlab_extra )
  
}  # the end of function 'brand_images'

# brand_images(plot_data, ae_event=ae_event, brand="Moderna", tit="")
# brand_images(plot_data, ae_event=ae_event,  tit="")



#
#   the end of functions :
#  
###############################


#################################################################
###############################    ####################
scri_data_parameters <- function(...){
  
  res   <- list()
  param <- list(...)
  if(any(names(param)=="data")) data <- param[["data"]]
  
  if(F){              
    # specify 'nvax':
    if(all(names(param)!="nvax")){
      if(any(names(param)=="data")) 
        nvax <- max(as.numeric(gsub("[^0-^9]+","",names(data)[substring(names(data),1,3)=="vax"])),na.rm=T)
      else nvax <- 3
    }
    else nvax <- param[["nvax"]]
    res$nvax <- nvax
  }        
  # parameters for dataset:
  if(any(names(param)=="data")){
    res$data_parameters <- list()
    if(F){        
      # specify vaccination time variable 'vax_time':
      tmp <- tmp0 <- c("vax%n_time",  "vax%n_t",  "days_vax%n","vax%n_days","time_vax%n","t_vax%n")
      if(any(names(param)=="vax_time")) tmp <- param[["vax_time"]]
      if(any( unlist(lapply(1:res$nvax,function(ivax) sum(tolower(names(data)) %in% gsub("%n",ivax,tmp))))!=1 )) 
        stop(paste0("Specify the time of vaccination variable 'vax_time' (default,'",paste0(tmp0,collapse="' or '"),"'). Use %n instead of the numbers in the vaccine's names."))
      else res$data_parameters$vax_time <- unlist(lapply(1:res$nvax,function(ivax) names(data)[tolower(names(data)) %in% gsub("%n",ivax,tmp)])) 
    }          
    
    res$data_parameters$vax_time <- "vax_time"
    res$data_parameters$vax_name <- "vax_name"
    res$data_parameters$vax_date <- "vax_date"
    
    # specify the id variable, 'id':
    tmp <- tmp0 <- c("id","person_id","pat","pid" )
    if(any(names(param)=="id")) tmp <- param[["id"]]
    if(sum(tolower(names(data)) %in% tmp)!=1) stop(paste0("Specify the 'id' variable (default,'",paste0(tmp0,collapse="' or '"),"')."))
    else res$data_parameters$id <- names(data)[tolower(names(data)) %in% tmp]
    
    # specify the vax_time variable, 'vax_time':
    tmp <- tmp0 <- c("vax_time","time_vax",  "vax_t","t_vax",   "vax_days","days_vax")
    if(any(names(param)=="vax_time")) tmp <- param[["vax_time"]]
    if(sum(tolower(names(data)) %in% tmp)!=1) stop(paste0("Specify the 'vax_time' variable (default,'",paste0(tmp0,collapse="' or '"),"')."))
    else res$data_parameters$vax_time <- names(data)[tolower(names(data)) %in% tmp]
    
    # specify the vax_date variable, 'vax_date':
    tmp <- tmp0 <- c("vax_date","date_vax", "vax_time","time_vax",   "vax_t","t_vax",   "vax_days","days_vax")
    if(any(names(param)=="vax_date")) tmp <- param[["vax_date"]]
    if(sum(tolower(names(data)) %in% tmp)!=1) stop(paste0("Specify the 'vax_date' variable (default,'",paste0(tmp0,collapse="' or '"),"')."))
    else res$data_parameters$vax_date <- names(data)[tolower(names(data)) %in% tmp]
    
    # specify the vax_name variable, 'vax_name':
    tmp <- tmp0 <- c("vax_name","name_vax")
    if(any(names(param)=="vax_name")) tmp <- param[["vax_name"]]
    if(sum(tolower(names(data)) %in% tmp)!=1) stop(paste0("Specify the 'vax_name' variable (default,'",paste0(tmp0,collapse="' or '"),"')."))
    else res$data_parameters$vax_name <- names(data)[tolower(names(data)) %in% tmp]
    
    # specify the start of observation 'start_obs':
    tmp <-  tmp0 <- c("start_obs","start_obs_time","t_start_obs","start_study","start_study_time","t_start_study","study_entry_days","study_entry_time","t_study_entry")
    if(any(names(param)=="start_obs")) tmp <- param[["start_obs"]]
    if(sum(tolower(names(data)) %in% tmp)!=1) stop(paste0("Specify the start of observation variable (default,'",paste0(tmp0,collapse="' or '"),"')."))
    else res$data_parameters$start_obs <- names(data)[tolower(names(data)) %in% tmp]
    
    # specify the end of observation 'end_obs':
    tmp <- tmp0 <- c("end_obs","end_obs_time","t_end_obs","end_study","end_study_time","t_end_study","study_exit_days","study_exit_time","t_study_exit")
    if(any(names(param)=="end_obs")) tmp <- param[["end_obs"]]
    if(sum(tolower(names(data)) %in% tmp)!=1) stop(paste0("Specify the end of observation variable (default,'",paste0(tmp0,collapse="' or '"),"')."))
    else res$data_parameters$end_obs <- names(data)[tolower(names(data)) %in% tmp]
    
    # specify censored variables, for example, death_days:
    tmp <- c("death_time","death_days","days_death")
    if(any(names(param)=="censored_vars")) tmp <- param[["censored_vars"]]
    if(sum(tolower(names(data)) %in% tmp)>0) res$data_parameters$censored_vars <- tmp[tmp %in% names(data)]
    else res$data_parameters$censored_vars <- c()
    
  }
  else{
    if(any(names(param)=="vax_time"))      res$data_parameters$vax_time      <- param[["vax_time"]]      else res$data_parameters$vax_time      <- paste0("vax",1:res$nvax,"_days") 
    if(any(names(param)=="id"))            res$data_parameters$id            <- param[["id"]]            else res$data_parameters$id            <- "id" 
    if(any(names(param)=="start_obs"))     res$data_parameters$start_obs     <- param[["start_obs"]]     else res$data_parameters$start_obs     <- "start_obs"  
    if(any(names(param)=="end_obs"))       res$data_parameters$end_obs       <- param[["end_obs"]]       else res$data_parameters$end_obs       <- "end_obs"  
    if(any(names(param)=="censored_vars")) res$data_parameters$censored_vars <- param[["censored_vars"]] else res$data_parameters$censored_vars <- "death_time"   
  }
  
  class(res) <- "scri_parameters"
  res
}  # end of function 'scri_data_parameters'

define_rws <- function( obj,
                        cond_vax_var, cond_vax_values, vax_names_short, 
                        before_vax_lab=c( "reference", "buffer" ), after_vax_lab,
                        #before_vax_lab=c( "pre-exposure", "buffer" ), after_vax_lab,
                        ref = "reference",
                        #ref = "pre-",
                        data,
                        lab_order = T,
                        ...                        #  for names as: 'begin_cut_points', 'after_overlap_priority', ...
){ 
  
  names_before_after <- c( "name","t0","cond","cond_var","cond_value","vax_dep","cut_points", "lab", "ref", "lab_add_interval", "no_last_interval", "overlap_priority_after" )
  names_rws_short    <- c("before","after", "rws_def")
  names_rws          <- c( names_rws_short, names_before_after, paste0("before_",names_before_after), paste0(names_before_after,"_before"), paste0("after_",names_before_after) , paste0(names_before_after,"_after")  )
  extra_pars         <- list(...)
  
  if(!missing(obj)){
    if(!is.list(obj)){ 
      if(mode(obj)=="character"){ if(substring(obj,1,4)=="list") obj<-list(rws=list(rws_def=obj)) }
      else stop("'obj' should be a list.")
    }  
    if(!("rws" %in% names(obj))) 
      if(any( names_rws_short %in% names(obj) )) {obj <- list(rws=obj); class(obj) <- "scri_parameters"}
    else if(any(names_rws %in% names(obj))) {}  # not ready yet
  }
  
  if(missing(cond_vax_var)){
    if(!is.null(obj$data_parameters$vax_name))  cond_vax_var <-  obj$data_parameters$vax_name
    else {
      if(missing(cond_vax_var) & missing(data)) stop("Define 'cond_vax_var' or/and 'data'.")
      # count vax  1,2,3,... !!!
      nvax <- 1
      ids <- data[,obj$data_parameters$id]
      while(T){
        ids <- ids[duplicated(ids)]
        if(length(ids)==0) break
        nvax <- nvax+1
        
      }
    }
  }
  
  
  if(missing(cond_vax_values))
    if(!missing(data)){
      if( !any(cond_vax_var %in% names(data))) stop(paste0("variable '",cond_vax_var,"' not found in dataset 'data'."))
      cond_vax_values_all <- data[,cond_vax_var]
      if(!is.null(obj$data_parameters$vax_time)) cond_vax_values_all <- cond_vax_values_all[order(data[,obj$data_parameters$vax_time])]
      cond_vax_values <- cond_vax_values_all[!duplicated(cond_vax_values_all)]
      cond_vax_values <- c( cond_vax_values[1], cond_vax_values )  # the first value for reference period ==> condition tov the start of the first vax
      
      if(missing(vax_names_short)){ 
        vax_names_short <- paste0(substring(cond_vax_values[1],1,1),"0") 
        for(ichar in  substring(cond_vax_values[-1],1,1)[!duplicated(substring(cond_vax_values[-1],1,1))] ) 
          vax_names_short <- c( vax_names_short, paste0( ichar, 1:sum(substring(cond_vax_values[-1],1,1)==ichar) )  )
      }
      else {
        if(mode(vax_names_short)=="character" & length(vax_names_short)==1){
          if( !any(vax_names_short %in% names(data))) stop(paste0("variable '",vax_names_short,"' not found in dataset 'data'."))
          vax_names_short <- data[ !duplicated(cond_vax_values_all), vax_names_short]
        }
      }      
      
      if(missing(after_vax_lab)) after_vax_lab <- cond_vax_values[-1]
      else {
        if(mode(after_vax_lab)=="character" & length(after_vax_lab)==1){
          if( !any(after_vax_lab %in% names(data))) stop(paste0("variable '",after_vax_lab,"' not found in dataset 'data'."))
          after_vax_lab <- data[ !duplicated(cond_vax_values_all), after_vax_lab]
        }
      }
      
    }
  
  if(length(vax_names_short)!=length(cond_vax_values)) stop(paste0("The length of 'vax_names_short' (",length(vax_names_short),") should be the same as the length of 'cond_vax_value' (",length(cond_vax_values),")"))
  if(length(vax_names_short)!=length(after_vax_lab)+1) stop(paste0("The length of 'vax_names_short' (",length(vax_names_short),") should be the same as the length of 'after_vax_lab' (",length(after_vax_lab),") + 1."))
  
  obj$rws$name       <- vax_names_short
  obj$rws$t0         <- obj$data_parameters$vax_time
  obj$rws$cond_var   <- cond_vax_var
  obj$rws$cond_value <- cond_vax_values
  
  # vaccine dependent variables. Adding "before"- or "after"-  risk windows 'lab'
  if("vax_dep" %in% names(extra_pars)) {
    obj$rws$vax_dep <- extra_pars$vax_dep
    extra_pars <- extra_pars[names(extra_pars)!="vax_dep"]  
    if(is.null(names(obj$rws$vax_dep))) names(obj$rws$vax_dep) <- rep("before",length(obj$rws$vax_dep))
  }
  
  # cut points name:
  if("cut_points_name" %in% names(extra_pars)) {
    obj$rws$cut_points_name <- paste0("cut_points_",extra_pars$cut_points_name)
    extra_pars <- extra_pars[names(extra_pars)!="cut_points_name"]  
  }
  else obj$rws$cut_points_name <- ""
  
  
  
  if(F){  
    if(!missing(obj)){
      if("rws" %in% names(extra_pars)){
        if("rws" %in% names(obj)){
          if(!is.list(rws)) stop("'rws' should be a list.")
          for(iname in names(rws)) obj[["rws"]][[iname]] <- rws[[iname]]
          rws <- obj$rws
        } 
        else obj$rws <- rws
      }
      else if("rws" %in% names(obj)) rws <- obj$rws
    }
    else{
      if("rws" %in% names(extra_pars)) if(!is.list(rws)) stop("'rws' should be a list.") 
      scri_obj <- list(rws=rws)  
    }
  }
  
  
  if( length(extra_pars)>0){
    for(ibefore_after in c("before","after")){
      if( any( (cond1<-substring(names(extra_pars),1,nchar(ibefore_after))==ibefore_after) | (cond2<-substring(names(extra_pars),nchar(names(extra_pars))-nchar(ibefore_after)+1,nchar(names(extra_pars)))==ibefore_after) )){
        
        if(any(cond1)) for(iname in names(extra_pars)[cond1]) {
          obj$rws[[ibefore_after]][[(tmp<-trimws(substring(iname,nchar(ibefore_after)+1),"left","[ _.$]"))]]  <- extra_pars[[iname]]
          if(!(tmp %in% names_before_after)) 
            stop("strange name[s] for '", paste0(tmp[tmp %in% names_before_after],collapse="','"), "'. (Possible names in this format: '", paste0( paste0(ibefore_after,"_",names_before_after),collapse="','"),"')" )
        }
        if(any(cond2)) for(iname in names(extra_pars)[cond2]) {
          obj$rws[[ibefore_after]][[(tmp<-trimws(substring(iname,1,nchar(iname)-nchar(ibefore_after)),"right","[ _.]"))]] <- extra_pars[[iname]] 
          if(!(tmp %in% names_before_after)) 
            stop("strange name[s] for '", paste0(tmp[tmp %in% names_before_after],collapse="','"), "'. (Possible names in this format: '", paste0( paste0("_",names_before_after,"_",ibefore_after),collapse="','"),"')" )
        }        
        extra_pars <- extra_pars[ !cond1 & !cond2  ]
      } 
    }
    if("rws_def" %in% names(extra_pars)) {
      obj$rws$rws_def <- rws_def
      extra_pars <- extra_pars[ names(extra_pars)!= "rws_def"  ]
    }
  }
  
  
  #### cut_points:
  
  ## before vax part:
  # cut_points for the part before vax: before$cut_points
  if("cut_points_before" %in% names(extra_pars)) obj$rws$before$cut_points <- extra_pars[[       "cut_points_before"]]
  if("before_cut_points" %in% names(extra_pars)) obj$rws$before$cut_points <- extra_pars[["before_cut_points"       ]]
  if(is.null(obj$rws$before$cut_points)) stop("'cut_points_before' must be defined." )
  if(is.list(obj$rws$before$cut_points) & length(obj$rws$before$cut_points)==1) obj$rws$before$cut_points <- obj$rws$before$cut_points[[1]] 
  
  obj$rws$before$lab <- before_vax_lab
  if(!missing(ref) | is.null(obj$rws$before$ref)) obj$rws$before$ref <- ref[1]
  
  # extra parameters for period before vax:
  if(is.null(obj$rws$before$lab_add_interval)){ if("lab_add_interval_before" %in% names(extra_pars)) obj$rws$before$lab_add_interval <- extra_pars[["lab_add_interval_before"]] else obj$rws$before$lab_add_interval <- T     } 
  if(is.null(obj$rws$before$no_last_interval)){ if("no_last_interval_before" %in% names(extra_pars)) obj$rws$before$no_last_interval <- extra_pars[["no_last_interval_before"]] else obj$rws$before$no_last_interval <- T     } 
  if(is.null(obj$rws$before$overlap_priority)){ if("overlap_priority_before" %in% names(extra_pars)) obj$rws$before$overlap_priority <- extra_pars[["overlap_priority_before"]] else obj$rws$before$overlap_priority <- "next"} 
  extra_pars <- extra_pars[!(names(extra_pars) %in% c("lab_add_interval_before","no_last_interval_before","overlap_priority_before"))]
  
  
  
  ## after vax part:
  # cut_points for the part after vax: after$cut_points
  if(!is.null(obj$rws$after$cut_points)) {
    if(!is.list(obj$rws$after$cut_points)) {
      obj$rws$after$cut_points <- list(obj$rws$after$cut_points)
      names(obj$rws$after$cut_points) <- vax_names_short[1]                        
    }
    if(length(vax_names_short)>2 & length(obj$rws$after$cut_points)==1) {          
      obj$rws$after$cut_points <- rep(obj$rws$after$cut_points,length(vax_names_short)-1)
      names(obj$rws$after$cut_points) <- vax_names_short[-1]
    }  
  }
  # after_vax_lab
  if(is.null(after_vax_lab)) after_vax_lab <- as.list(paste0("dose ",1:(length(vax_names_short)-1)," "))
  else{
    if( !is.list(after_vax_lab) ) {
      if(length(after_vax_lab)==length(vax_names_short)-1) after_vax_lab <- as.list(after_vax_lab)
      else after_vax_lab <- list(after_vax_lab)
    }
    if(length(vax_names_short)>2 & length(after_vax_lab)==1) after_vax_lab <- rep(after_vax_lab,length(vax_names_short)-1)
  }
  obj$rws$after$lab <- after_vax_lab
  names(obj$rws$after$lab) <- vax_names_short[-1]
  if(!missing(ref) | is.null(obj$rws$after$ref)){ 
    if(length(ref)>1) obj$rws$after$ref <- ref[2:length(vax_names_short)]
    else obj$rws$after$ref <- rep(ref,length(vax_names_short)-1)
  }
  
  # extra parameters for period after vax:
  if(is.null(obj$rws$after$lab_add_interval)){ if("lab_add_interval_after" %in% names(extra_pars)) obj$rws$after$lab_add_interval <- extra_pars[["lab_add_interval_after"]] else obj$rws$after$lab_add_interval <- T     } 
  if(is.null(obj$rws$after$no_last_interval)){ if("lab_add_interval_after" %in% names(extra_pars)) obj$rws$after$no_last_interval <- extra_pars[["no_last_interval_after"]] else obj$rws$after$no_last_interval <- F     } 
  if(is.null(obj$rws$after$overlap_priority)){ if("lab_add_interval_after" %in% names(extra_pars)) obj$rws$after$overlap_priority <- extra_pars[["overlap_priority_after"]] else obj$rws$after$overlap_priority <- "next"} 
  extra_pars <- extra_pars[!(names(extra_pars) %in% c("lab_add_interval_after","no_last_interval_after","overlap_priority_after"))]
  
  if(length(vax_names_short)>2){
    if(length(obj$rws$after$lab_add_interval) ==1) obj$rws$after$lab_add_interval <- rep(obj$rws$after$lab_add_interval,length(vax_names_short)-1)
    if(length(obj$rws$after$no_last_interval) ==1) obj$rws$after$no_last_interval <- rep(obj$rws$after$no_last_interval,length(vax_names_short)-1)
    if(length(obj$rws$after$overlap_priority) ==1) obj$rws$after$overlap_priority <- rep(obj$rws$after$overlap_priority,length(vax_names_short)-1)
  }  
  
  
  # before the first vaccine part:
  if(obj$rws$cut_points_name=="")
    rws_text <- paste0(                                                                               ' list( '    ) 
  else rws_text <- paste0(                             obj$rws$cut_points_name,                          ' = list( '  )  
  rws_text <- paste0(rws_text, '\n\t',                 obj$rws$name[1],                                  ' = list( '  )
  rws_text <- paste0(rws_text, 'name="',               obj$rws$name[1],                                  '"'          )
  rws_text <- paste0(rws_text, ', t0="',               obj$rws$t0,                                       '"'          )
  rws_text <- paste0(rws_text, ', cond_var="',         obj$rws$cond_var,                                 '"'          )
  rws_text <- paste0(rws_text, ', cond_value="',       obj$rws$cond_value[1],                            '"'          )
  
  if(!is.null(obj$rws$vax_dep))
    rws_text <- paste0(rws_text, ', vax_dep=c(', paste0(paste0(names(obj$rws$vax_dep),'="',obj$rws$vax_dep,'"'), collapse=','),  ')' )
  
  rws_text <- paste0(rws_text, ', cut_points=c(',      paste0(obj$rws$before$cut_points,collapse=', '),  ')'          )
  rws_text <- paste0(rws_text, ', lab=c("',            paste0(obj$rws$before$lab,collapse='","'),        '")'         )
  rws_text <- paste0(rws_text, ', ref="',              obj$rws$before$ref,                               '"'          )
  rws_text <- paste0(rws_text, ', lab_add_interval=',  obj$rws$before$lab_add_interval                                )
  rws_text <- paste0(rws_text, ', no_last_interval=',  obj$rws$before$no_last_interval                                )
  rws_text <- paste0(rws_text, ', overlap_priority="', obj$rws$before$overlap_priority,                  '" )'        )
  
  # after each dose parts:
  for(ivax in 2:length(obj$rws$name)) {
    rws_text <- paste0(rws_text, ', \n\t',               obj$rws$name[ivax],                                       ' = list( '  )
    rws_text <- paste0(rws_text, 'name="',               obj$rws$name[ivax],                                       '"'          )
    rws_text <- paste0(rws_text, ', t0="',               obj$rws$t0,                                               '"'          )
    rws_text <- paste0(rws_text, ', cond_var="',         obj$rws$cond_var,                                         '"'          )
    rws_text <- paste0(rws_text, ', cond_value="',       obj$rws$cond_value[ivax],                                 '"'          )
    
    if(!is.null(obj$rws$vax_dep))
      rws_text <- paste0(rws_text, ', vax_dep=c(', paste0(paste0(names(obj$rws$vax_dep),'="',obj$rws$vax_dep,'"'), collapse=','),  ')'         )
    
    rws_text <- paste0(rws_text, ', cut_points=c(',      paste0(obj$rws$after$cut_points[[ivax-1]],collapse=', '),  ')'         )
    rws_text <- paste0(rws_text, ', lab=c("',            paste0(obj$rws$after$lab[[ivax-1]],collapse='","'),        '")'        )
    rws_text <- paste0(rws_text, ', ref="',              obj$rws$after$ref[ivax-1],                                 '"'         )
    rws_text <- paste0(rws_text, ', lab_add_interval=',  obj$rws$after$lab_add_interval[ivax-1]                                 )
    rws_text <- paste0(rws_text, ', no_last_interval=',  obj$rws$after$no_last_interval[ivax-1]                                 )
    rws_text <- paste0(rws_text, ', overlap_priority="', obj$rws$after$overlap_priority[ivax-1],                     '" )'      )
  }
  
  rws_text <- paste0(rws_text, ' )')
  
  obj$rws$rws_def <- rws_text
  
  # add lab order
  if(lab_order) obj <- create_lab_orders(obj, data=data)
  
  obj
}  # end of function 'define_rws'


#define_rws(vax_def,  cut_points_before = c(-91,-29), cut_points_after = c(0,1,29,62), cond_vax_var="vax_name", data=data_vax)
#define_rws(vax_def,  cut_points_before = c(-91,-29), cut_points_after = c(0,1,29,62), cond_vax_var="vax_name", data=data_vax, vax_dep = c( before="type_vax_short", after="dist_gt_60" ))


create_lab_orders <- function(obj, 
                              data, 
                              vax_dep_order = c("pfizer","pfize","moderna","moder","astrazeneca","astra","az","j&j","jj","janssen","janss")
){
  
  #if(!missing(obj) & any(is.na(rws))) if(!is.null(obj$rws)) 
  rws <- eval(parse(text=obj$rws$rws_def) )
  
  lab_orders <- list(  
    #c("F", "M" ),
    c("sex0","sex:0","sexF","sex:F", "sex1", "sex:1" , "sexM", "sex:M" ),
    paste0("age",c("(-1,30]","(30,40]","(30,50]","(30,60]","(30,Inf]",">=30",">30","(40,50]","(50,60]","(50,65]","(50,Inf]",">=50",">50",">=60","(60,Inf]", ">65" )),
    #c("(-1,30]","(30,40]","(30,50]","(30,60]","(30,Inf]",">=30",">30","(40,50]","(50,60]","(50,65]","(50,Inf]",">=50",">50",">=60","(60,Inf]", ">65" ),
    paste0("d",1:nvax,":")
  )
  
  if(any(!is.na(rws))){
    
    if(!is.list(rws)) rws <- eval(parse(text=rws))
    rws <- rws[sapply(rws,length)>1]
    
    refs_list_name <- unique(sapply(rws, function(x) x$ref))
    #refs_list_name <- unique(rws$after$ref) #unique(sapply(rws, function(x) x$ref))
    refs_list_name <- sapply(rws,function(x) ifelse( any(grepl(refs_list_name, x$lab)), x$name, NA) ) 
    refs_list_name <- refs_list_name[!is.na(refs_list_name)]
    
    
    vax_dep_before <- unique(unlist(lapply(rws,function(x)x$vax_dep[names(x$vax_dep)=="before"])))
    vax_dep_after  <- unique(unlist(lapply(rws,function(x)x$vax_dep[names(x$vax_dep)=="after"])))
    
    if(any( sapply(rws,function(x)!is.null(x$vax_dep)) )){  
      
      rws <- lapply(rws,function(x){ if(is.null(names(x$vax_dep))) names(x$vax_dep)<-"before"; x} )
      #rws <- unique(rws$after$ref)[1]   #lapply(rws,function(x){ if(is.null(names(x$vax_dep))) names(x$vax_dep)<-"before"; x} )
      
      
      if(!missing(data)){
        if(!is.list(vax_dep_order)) vax_dep_order <- list(vax_dep_order)
        for(iorder in  1:length(vax_dep_order) )
          for(ivar in  vax_dep_before  ){
            vax_dep_values <- unique(data[,ivar])
            tmp <-  match(gsub(" ","",tolower(vax_dep_order[[iorder]])),gsub(" ","",tolower(vax_dep_values)))
            vax_dep_values <- unique(c( vax_dep_values[tmp[!is.na(tmp)]], vax_dep_values) )
            vax_dep_values <- vax_dep_values[!is.na(vax_dep_values)]
            lab_orders <- c(lab_orders,  list(  c(rbind( vax_dep_values, gsub(" ","",vax_dep_values),
                                                         paste0("no ",vax_dep_values), paste0("no ",gsub(" ","",vax_dep_values))
            ), "no ","no_"   )) )
          }
      }
    }
    
    if(any( sapply(rws,function(x)!is.null(x$cond_value)) )){  
      dose_names <- unlist(lapply(rws,function(x)x$cond_value))
      lab_orders <- c(lab_orders,  list(  dose_names[!duplicated(dose_names)]   ))
    }
    
    if(any( sapply(rws,function(x)!is.null(x$lab)) )){ 
      labs <- unlist(lapply(rws[sapply(rws,function(x) !(x$name %in% refs_list_name) )],function(x)x$lab))
      names(labs) <- NULL
      lab_orders <- c(lab_orders,  list(  labs[!duplicated(labs)]   ))
    }
    
    if(any( sapply(rws,function(x)!is.null(x$name)) )){  
      names <- unlist(lapply(rws,function(x)x$name))
      names(names) <- NULL
      lab_orders <- c(lab_orders,  list(  names[!duplicated(names)]   ))
    }
    
  }
  
  lab_orders <- c( lab_orders, list(
    #c("Pfi", "Mod", "Ast",  "JJ","J&J" ),
    #c("Pfizer", "Moderna", "AstraZeneca", "JJ","J&J" ),
    substring( unlist( ifelse(any(ls()=="brands"), list( c(rbind( brands, paste0("no ",brands), paste0("no_",brands)), "no" ) ), "" )) , 1, 5) ,
    unlist( ifelse(any(ls()=="brands"), list( c(rbind( brands, paste0("no ",brands), paste0("no_",brands)), "no" ) ),  "" )) ,
    c("reference","buf", "dose", "vax", "boos", "booster"),
    c("ref",      "buf", "dose", "vax", "boos", "booster"),
    #c("pre-","buf", "dose", "vax", "boos", "booster"),
    paste0( rep(paste0("dose ",1:nvax),each=12), rep(c("ref"," ref","buf"," buf","<"," <","("," (","["," [",">"," >"),nvax) ),
    #paste0( rep(paste0("dose ",1:nvax),each=12), rep(c("pre-"," pre-","buf"," buf","<"," <","("," (","["," [",">"," >"),nvax) ),
    c("ref","buf", paste0("dose ",1:nvax), "only_for_time_adj" ),
    #c("pre-","buf", paste0("dose ",1:nvax), "only_for_time_adj" ),
    c("[-91;-29]","[-90;-30]","[-29;-1]","[-28;-1]","[0;0]","[1;7]","[1;14]","[1;21]","[1;28]","[8;14]","[15;21]","[15;28]","[22;28]","[22;42]",">28",
      "[29;60]","[29;61]","[29;63]",">42",">60","[61;180]","[61;182]",">61","[63;180]","[63;182]",">63",">180",">182")   ))
  
  
  
  
  if(any(!is.na(rws)))
    if(length(vax_dep_after)>0)
      if(!missing(data)){
        if(!is.list(vax_dep_order)) vax_dep_order <- list(vax_dep_order)
        for(iorder in  1:length(vax_dep_order) )
          for(ivar in  vax_dep_after ){
            vax_dep_values <- unique(data[,ivar])
            tmp <-  match(gsub(" ","",tolower(vax_dep_order[[iorder]])),gsub(" ","",tolower(vax_dep_values)))
            vax_dep_values <- unique(c( vax_dep_values[tmp[!is.na(tmp)]], vax_dep_values) )
            vax_dep_values <- vax_dep_values[!is.na(vax_dep_values)]
            lab_orders <- c(lab_orders,  list(  vax_dep_values ))
          }
      }
  
  lab_orders <- c(lab_orders, list(
    paste0(":",c("(-1,30]","(30,60]","(60,Inf]","(60, Inf]")),
    paste0(":",c("(-Inf,-1]","(-1,70]","(70,Inf]")),
    paste0(":",c("(-\U221E,-1]","(-Inf,-1]","(-1,21]","(-1,30]","(21,35]","(30,60]","(35,56]","(56,84]","(60,Inf]","(60, Inf]","(84, Inf]","(84, \U221E]")),
    paste0(":",c("(-\U221E,-1]","(-Inf,-1]","(-1,175]","(175,Inf]","(175, \U221E]"))
  ))
  
  obj$lab_orders <- lab_orders
  
  obj
}  # end of function 'create_lab_orders'  


print.scri_parameters <- function(x, show_order=F){  
  
  cat("\n*****  SCRI parameters  *****\n")
  
  print_scri_help_format <- function( x, i, text, width=40){ 
    if(is.null(names(x[[i]]))) {
      main_part <- paste0(x[i],collapse="','")
      if(length(x[[i]])==1 & is.character(x[[i]])) main_part <- paste0('"', main_part, '"')
    }
    else  main_part <- paste0('c( ', paste0( paste0(names(x[[i]]), '="',x[[i]],'"'), collapse=','), ' )' )
    cat(paste0(format( text, width=width ),
               format(names(x)[i],width=max(nchar(names(x))), justify = "right")," = ",
               main_part, "\n"))
  }
  print_scri_help_noformat <- function( x, i, text, width=40){
    cat(paste0(format( text, width=width ),
               names(x)[i]," = ",
               ifelse(length(x[[i]])==1 & is.character(x[[i]]), "'",'') ,
               paste0(x[i],collapse="','"),
               ifelse(length(x[[i]])==1 & is.character(x[[i]]), "'", '' ) , "\n"))
  }
  
  xx <- unclass(x)
  # nvax
  if(any(names(xx)=="nvax"))
    cat(paste0("\nNumber of doses: nvax = ",xx$nvax,"\n"))
  
  # dataset parameters
  if(any(names(xx)=="data_parameters")){
    cat("\nThe data parameters:\n")
    for(ipar in 1:length(xx$data_parameters))
      print_scri_help_format(xx$data_parameters, ipar, text=paste0(" - the ",names(xx$data_parameters)[ipar]," variable: ") )
    cat("\n")
  }
  # risk intervals parameters
  if(any(names(xx)=="rws")){
    cat("The risk intervals parameters:\n\n")
    for(irw in 1:length(xx$rws)){
      if(names(xx$rws)[irw]=="rws_def"){
        cat("\n 1. The risk window parameters: \n\n" )
        print_scri_help_noformat(xx$rws, irw, text=paste0("   a) the risk window definition:\n\n      ") )
      }
      
      # 'before'  and 'after' parameters
      if(names(xx$rws)[irw] %in% c("before")){
        cat("\n   b) windows before the first dose:\n\t")
        if(xx$rws$before$no_last_interval)
          cat(paste0(' risk windows = { ' , paste0( paste0('"',xx$rws$before$lab,'" = [',xx$rws$before$cut_points,";",c(xx$rws$before$cut_points[-1],0)-1,"]"), collapse=", ")," }\n\t "))
        other_before_par <- list(lapply(xx$rws$before[!(names(xx$rws$before) %in% c("cut_points","lab"))],function(x,i)x[[i]],1))
        print_scri_help_noformat(other_before_par, 1, text="other_param ",width=0 )
        cat("\n")
      }
      if(names(xx$rws)[irw] %in% "after"){
        cat("   c) windows after vaccination:\n")
        for(ivax in 1:length(xx$rws$after$cut_points)){
          cat("       after dose ",names(xx$rws$after$cut_points)[ivax],":\t")
          if(xx$rws$after$no_last_interval[ivax])
            cat(paste0("risk windows = { [" , paste0( paste0(xx$rws$after$cut_points[[ivax]][-length(xx$rws$after$cut_points[[ivax]])],";",
                                                             c(xx$rws$after$cut_points[[ivax]][-1]-1)), collapse="], ["),"] }; "))
          else 
            cat(paste0("risk windows = { [" , paste0( paste0(xx$rws$after$cut_points[[ivax]],";",c(xx$rws$after$cut_points[[ivax]][-1]-1,"max")), collapse="], ["),"] }; "))
          other_after_par <- list(sapply(xx$rws$after[names(xx$rws$after)!="cut_points"],function(x,i)x[[i]],ivax))
          print_scri_help_noformat(other_after_par, 1, text="other_param",width=11 )
        }
        cat("\n")
      }
      
      #if(names(xx)[irw] %in% "lab_orders" & show_order)
      #  for(i1 in 1:length(xx$rws$lab_orders)){
      #    print_scri_help_format(xx$rws$lab_orders, i1, text=paste0("      the ",names(xx$rws$lab_orders)[i1]," variable: ") )
      #  }  
      
      # other risk windows parameters
      if( !( names(xx$rws)[irw] %in% c("rws_def","before","after") )) 
        print_scri_help_format(xx$rws, irw, text=paste0("   - the ",names(xx$rws)[irw]," variable: ") )
    }
    cat("\n")
  }
  
  # lab_orders
  if("lab_orders" %in% names(xx) & show_order ){
    cat("\n\nParameter 'lab_orders:'\n")
    for(i1 in 1:length(xx$lab_orders)){
      print_scri_help_noformat(xx$lab_orders, i1, text=paste0("   ",names(xx$lab_orders)[i1]), width=0  )
    }  
  }
  
  # other parameters
  if(any(!(names(xx) %in% c("data_parameters","rws","lab_orders"))))
    for(ipar in 1:length(tmp<-xx[ !(names(xx) %in% c("data_parameters","rws","lab_orders")) ]) )
      print_scri_help_format(tmp, ipar, text=paste0("The ",names(tmp)[ipar]," variable: ") )
  invisible(x)
}   # end of function 'print.scri_input'


print.scri_tabs <- function(x,digits){  
  attributes(x)[names(attributes(x))!="names"] <- NULL
  if(missing(digits)) print(x)
  else lapply(1:length(x),function(i){ print(names(x)[i]);print(format(x[[i]],digits=digits, nsmall=digits)) })
  NULL
}  


###################################

forest_plots_tab <- function(tt0, ndigits=2, nrows_forest_plot=55, cex=0.8, cex_head=0.8, lplot=T, ltable=T){
  row.names(tt0) <- NULL
  tt <- tt0[, !(names(tt0) %in%  c("event","i"))]
  #print(apply(tt,2,mode))
  
  tt <- as.data.frame(lapply(tt,function(x,digits){if(mode(x)=="numeric")x<-round(x,digits=digits);x},digits=ndigits))
  #print(apply(tt,2,mode))
  tt$group_start <- 1:nrow(tt)
  tt$group_start[!grepl("reference",tt$all_cat) & c(F,substring(tt$all_cat[-1],1,3) == substring(tt$all_cat[-nrow(tt)],1,3))] <- NA
  #tt$group_start[!grepl("pre-",tt$all_cat) & c(F,substring(tt$all_cat[-1],1,3) == substring(tt$all_cat[-nrow(tt)],1,3))] <- NA
  #tt$group_start[c(F,substring(tt$all_cat[-1],1,3) == substring(tt$all_cat[-nrow(tt)],1,3))] <- NA
  tt$group_start[ !c( T, tt$group_start[-1] - tt$group_start[-nrow(tt)] >1 ) ] <- NA
  
  tt$group <- 1
  tt$group <- rep( tt$group_start[!is.na(tt$group_start)], c(tt$group_start[!is.na(tt$group_start)][-1],nrow(tt)+1) - tt$group_start[!is.na(tt$group_start)] )
  
  no_RR <- F
  if(!any("lci" %in% names(tt))) no_RR <- T
  
  if(no_RR)var_RR   <- "RR_data"
  
  else {
    var_RR   <- "RR"  
    tt$RR_CI <- paste0(format(tt$RR,nsmall=ndigits)," (",format(tt$lci,nsmall=ndigits),";",format(tt$uci,nsmall=ndigits),")")
    tt$RR_CI[is.na(tt$RR)] <- ""
  }
  
  
  if(lplot){
    groups <- tt$group[!duplicated(tt$group)]
    
    add_text <- function(){  
      # headings:
      op <- par(cex=cex_head, font=2)
      text(-(  (4:1)*10/(40/max(tt[,var_RR],na.rm=T)) )[c(1,4)],  2, c("               # event in","# observed days per person in"), adj=0.5)
      text(-(  (4:1)*10/(40/max(tt[,var_RR],na.rm=T)) ),          1, c("risk wind", "ref.wind", "risk wind", "ref.wind"))
      par(font=4)  # bold italic font
      
      ### add text for the subgroups
      #text(-16, tt$group_start, tt$all_cat[!is.na(tt$group_start)], pos=4 )
      par(op) 
    }
    
    start_y <- 1
    if(no_RR) print("CI's are not correct!!! There are no estimates from Poisson regression!!!")
    for(igr in 1:length(groups)){
      if(sum(tt$group == groups[igr])==0)next
      if(all(is.na(tt[tt$group == groups[igr],var_RR])))next
      if(start_y>nrows_forest_plot | start_y + sum(!is.na(tt[tt$group == groups[igr],var_RR]))>nrows_forest_plot){ start_y <- 1; par(new=F) }
      IRR_tit <- format("IRR (CI)",width=15)
      if(no_RR){
        tt$lci <- pmax(0, tt[,var_RR] - 0.1 ) 
        tt$uci <- tt[,var_RR] + 0.1
        IRR_tit <- "IRR from data"
      }
      RR_max <- max(tt[,var_RR],na.rm=T)
      with(tt[tt$group == groups[igr] & !is.na(tt[,var_RR]),], { 
        forest.default(slab=all_cat, x=get(var_RR), ci.lb=lci, ci.ub=uci,
                       refline=1, cex=cex,
                       header = c("Intervals",IRR_tit),
                       xlim=c(-(max(nchar(tt$all_cat))/40*max(1,RR_max)+max(1,RR_max)), max(1,RR_max)*1.6), ylim=c(2, -nrows_forest_plot),
                       alim=c(  0, max(1,RR_max)*1.1),
                       ilab=cbind(events_rw,events_ref, days_per_id_rw, days_per_id_ref), 
                       ilab.xpos= -(  (4:1)*10/(40/max(1,RR_max)) ), rows= -start_y  
        )} )
      if(start_y==1) add_text()
      start_y <- start_y + sum(!is.na(tt[tt$group == groups[igr],var_RR])) + 1
      par(new=T)
    }  
    par(new=F) 
  }
  
  if(ltable){
    tmp <- tt[!is.na(tt$group_start),]
    tmp$group <- tmp$group - 0.5
    tmp[,names(tmp)!="group"] <- ""
    tt_print <- rbind.data.frame(tt,tmp)
    tt_print <- tt_print[order(tt_print$group),]
    row.names(tt_print) <- NULL
    
    #tt_print$days_per_id_rw <- format(tt_print$days_per_id_rw,digits=2,nsmall=2)
    #tt_print$pval <- sprintf(".2%s",tt_print$pval )
    #tt <- as.data.frame(lapply(tt,function(x,digits){if(mode(x)=="numeric")x<-format(x,digits=ndigits);x},digits=ndigits))
    
    
    # columns description:
    cat('Column names:\n')
    cat('   - #ev.risk"      - the number of events in the risk window                                      \n'  )
    cat('   - #ev.ref."      - the number of events in the corresponding reference window                   \n'  )
    cat('   - #days_id_risk" - the number of observed days in the risk window per person                    \n'  )
    cat('   - #days_id_ref." - the number of observed days in the corresponding reference window per person \n\n')
    
    # print table:
    if(!no_RR)
      knitr::kable( tt_print[,c("all_cat", "events_rw","events_ref", "days_per_id_rw", "days_per_id_ref", "RR_CI", "pval" )],
                    col.names = c("Intervals", "#ev.risk","#ev.ref.","#days_id_rw", "#days_id_ref", "IRR(CI)", "pval" ),
                    align="lrrrrrr"  )
    else
      knitr::kable( tt_print[,c("all_cat", "events_rw","events_ref", "days_per_id_rw", "days_per_id_ref", "RR_data")],
                    col.names = c("Intervals", "#ev.risk","#ev.ref.","#days_id_rw", "#days_id_ref", "IRR_from_data" ),
                    align="lrrrrrr"  )
  }
}


###############################
###############################


# 
# Function:     table1
# Description:  create a table with counts and percentages for one categorical variable. Similar to 'tabyl'.
#

table1 <- function(x, title="", digits=2, sep=" & ", print=c(T,T,T,T) ){
  if(!is.null(dim(x)) & length(dim(x))==2){
    for(icol in 2:ncol(x)) x[,1] <- paste(x[,1],x[,icol],sep=sep)
    x <- x[,1]
  }  
  else x <- as.factor(x)
  if(any(print) & title!="") cat(paste(title,"\n"))
  cbind( 
    n=(tb<-table( x, useNA="ifany" )), 
    cum_n=cumsum(tb), 
    percent=round(100*tb/sum(tb),digits), 
    cum_percent=round(100*cumsum( tb/sum(tb) ),digits), 
    percent2=c(round(100*(tb2<-table(x))/sum(tb2),digits),rep(NA,length(tb)-length(tb2))),
    cum_percent2 = c(round(100*cumsum( (tb2<-table(x))/sum(tb2) ),digits),rep(NA,length(tb)-length(tb2))) 
  )[,c( print, any(is.na(x)) & print[3] , any(is.na(x)) & print[4] ), drop=F]
}
#
#table1 <- function(x, digits=2){
#  x <- as.factor(x)
#  cbind( n=(tb<-table(x,useNA="ifany")), 
#         percent=round(100*tb/sum(tb),digits), 
#         percent2=c(round(100*(tb2<-table(x))/sum(tb2),digits),rep(NA,length(tb)-length(tb2))) )[,c(T,T,any(is.na(x)))]
#}





