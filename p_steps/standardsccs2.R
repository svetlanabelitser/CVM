
# Program Information  ----------------------------------------------------

# Program:      standardsccs2.R 
# Author:       adopted and changed from function 'standardsccs' (library SCCS version of sept 2021) by Svetlana Belitser
#
#  more user-friendly parameter names.
#
# in input parameters 'adrugnames' is added. The length of 'adrugnames' should correspond to the number of intervals in 'adrug'. 
#


standardsccs2 <- function(formula, indiv, astart, aend, aevent, adrug, aedrug, adrugnames, expogrp = list(), washout = list(), 
                         sameexpopar = list(), agegrp = NULL, seasongrp=NULL, dob=NULL, dataformat="stack", data, save_data=F) {
  
  if (dataformat!="multi" & dataformat!="stack"){
    
    stop("Please input dataformat as multi or stack")
  }

  #if (!is.null(seasongrp) & is.null(dob)){
  #  stop("Please input date of birth (dob)")
  #}
  
  yon <- deparse(substitute(adrug)) 
  yon1 <- as.formula(paste("z", "~", yon))
  adrugcolnames <- all.vars(yon1, functions = FALSE, unique = TRUE)[-1]
  # colname  <- deparse(substitute(adrug))
  adrug  <- eval(substitute(adrug), data, parent.frame())

  # Changing adrug to a list if given as cbind(adrug1, adrug2,...) or adrug not as a list
  
  if ((dataformat=="multi" & !is.null(ncol(adrug)))) {
    adrug <- data.frame(adrug)
    adrug <- list(adrug) 
  } else if (dataformat=="stack" & !is.null(ncol(adrug))){
    adrug <- data.frame(adrug)
    adrug1 <- list()
    for (i in 1:ncol(adrug)){
      adrug1[[i]] <- adrug[,i]
    }
    adrug <- adrug1
  } else if (length(adrugcolnames)==1 & length(adrug)!=1) {
    adrug <- list(adrug)
    
  } else {
    adrug <- adrug
  }
  
  
  for (i in 1:length(adrug)){
    adrug[[i]] <- data.frame(adrug[[i]])
    if(length(unlist(adrugcolnames))==1)
      names(adrug[[i]]) <- unlist(adrugcolnames)
  }
  
  ncoladrug <- unlist(lapply(adrug,ncol))  #Sv
  #Sv ncoladrug <- NULL
  #Sv for (i in 1:length(adrug)){
  #Sv   ncoladrug[i] <- ncol(adrug[[i]]) 
  #Sv }
  
  #Sv CAN GIVE ERROR NAMES OF COLUMNS!!!! Because all non-missing colnames are shifted to left!!! 
  #Sv for (i in 1:length(adrug)) {
  #Sv   colnames(adrug[[i]]) <- adrugcolnames[c(1, cumsum(ncoladrug)+1)[-(length(ncoladrug)+1)][i]:cumsum(ncoladrug)[i]]
  #Sv }
  
  colname  <- adrugcolnames
  
  indiv  <- eval(substitute(indiv), data, parent.frame())
  astart <- eval(substitute(astart), data, parent.frame())
  aend   <- eval(substitute(aend), data, parent.frame())
  aevent <- eval(substitute(aevent), data, parent.frame())
  # adrug  <- eval(substitute(adrug), data, parent.frame())
  aedrug <- eval(substitute(aedrug), data, parent.frame())
  dob <- eval(substitute(dob), data, parent.frame())
  
  
  # Changing aedrug to a list if given as cbind(aedrug1, aedrug2,...) or aedrug not as a list
  
  if ((dataformat=="multi" & !is.null(ncol(aedrug)))) {
    aedrug <- data.frame(aedrug)
    aedrug <- list(aedrug) 
  } else if (dataformat=="stack" & !is.null(ncol(aedrug))){
    aedrug <- data.frame(aedrug)
    aedrug1 <- list()
    for (i in 1:ncol(aedrug)){
      aedrug1[[i]] <- aedrug[,i]
    }
    aedrug <- aedrug1
  } else if (length(adrugcolnames)==1 & length(aedrug)!=1) {
    aedrug <- list(aedrug)
    
  } else {
    aedrug <- aedrug
  }
  

  # ------------------Getting the fixed covariates from the formula ------------#
  
  qq <- all.vars(as.formula(formula))[-c(which(all.vars(as.formula(formula))=="age"), which(all.vars(as.formula(formula))=="season"), which(all.vars(as.formula(formula))=="event"))]
  
  if (length(qq)==0) {
    cov <- cbind()
  }   else {
    cova <- qq[is.na(match(qq, colname))]
    cov <- data.frame(data[, cova])
    colnames(cov) <- cova
  }
  if(length(adrug)>1){
    if( length(sameexpopar)==1 ) sameexpopar <- rep(sameexpopar,length(adrug))
    if( !is.list(expogrp) ) expogrp <- rep( list(expogrp), length(adrug) )
  }
   
  chopdat <- formatdata2(indiv=indiv, astart=astart, aend=aend, aevent=aevent, adrug=adrug, aedrug=aedrug, expogrp = expogrp, washout = washout , 
                        sameexpopar = sameexpopar, agegrp = agegrp, seasongrp=seasongrp, dob=dob, cov=cov, dataformat=dataformat, data=NULL)
  
  adruga <- qq[!(qq %in% cova)]
  
 
  events_sum     <- as.data.frame.table(tapply(chopdat$event,    as.list(chopdat)[c(cova, adruga )], sum))
  atrisk_days    <- as.data.frame.table(tapply(chopdat$interval, as.list(chopdat)[c(cova, adruga )], sum))
  atrisk_persons <- as.data.frame.table(tapply(chopdat$indiv,    as.list(chopdat)[c(cova, adruga )], function(x)length(unique(x)) ))
  
  events_sum <- cbind.data.frame(events_sum, atrisk_days[,ncol(atrisk_days)], atrisk_persons[,ncol(atrisk_persons)])
  names(events_sum)[ncol(events_sum)+(-2:0)] <- c("n_events","atrisk_days","atrisk_persons")
  events_sum$event_rate    <- events_sum$n_events / events_sum$atrisk_days    * 100
  events_sum$event_percent <- events_sum$n_events / events_sum$atrisk_persons * 100
  
  events_sum$all_cat <- rep("",nrow(events_sum))
  events_sum <- events_sum[,c(ncol(events_sum), 1:(ncol(events_sum)-1) )]
  for(i in 2:(ncol(events_sum)-5)){
    #if(i==ncol(events_sum)-3) events_sumall_cat2 <- events_sumall_cat
    events_sum$all_cat <- paste0( events_sum$all_cat, ifelse(i==2,"",":"), names(events_sum)[i], events_sum[,i] )
  }

  fmla <- paste(formula, "+", "strata(indivL)", "+", "offset(log(interval))")
  fmla1 <- as.formula(paste("event~", fmla[3]))
  mod <- try( clogit(formula = fmla1, data = chopdat) )
  
  if(class(mod)[[1]]== "try-error"){ 
    warning(paste("The SCCS model could not be run. Try to repeate for groups with enough cases.\n"))
    sccs_res <- F
  }  
  else sccs_res <- T

  if(sccs_res){
    res_tab <-    cbind.data.frame( exp( cbind( IRR=mod$coef, confint(mod)) ),  summary(mod)$coefficients[,c("Pr(>|z|)", "coef", "se(coef)" )] )
    dimnames(res_tab)[[1]] <- gsub(" ", "", dimnames(res_tab)[[1]], fixed = TRUE)
    #names(res_tab) <- c("R?irr","lci","uci","pval","coef","se(coef)")
  
    events_sum[, names(res_tab) ] <- NA 
    matched_rows <- match( rownames(res_tab[!is.na(res_tab[,1]),]), events_sum$all_cat )
    events_sum[ matched_rows, ncol(events_sum) - (ncol(res_tab)-1):0 ] <- res_tab[!is.na(res_tab[,1]),][!is.na(matched_rows),]
    if(sum(!is.na(matched_rows)) != sum(!is.na(res_tab[,1])))
      warning(paste(
        'Not all coefficients from the analysis are matched to frequency table. Not matched:"',
        paste0( rownames(res_tab[!is.na(res_tab[,1]),])[ is.na(matched_rows) ] ,collapse='", "'), '".'  ))
    
    chopdat$adrugnames <- ""
    res_tab$rownames    <- rownames(res_tab)
    events_sum$all_cat2 <- events_sum$all_cat
    
    #Sv:
    if(!missing(adrugnames)){
      if(length(adrugnames)==sum(unlist(lapply(adrug,ncol)))){
        all_drugnames <- tapply ( adrugnames,  cut(1:length(adrugnames), cumsum(c(0,unlist(lapply(adrug,ncol)))) ), c, simplify=F )
        if(sum(unlist(lapply(expogrp,length)))>0){
          if(!is.list(expogrp)) expogrp <- list(expogrp)
          expogrpnames <- expogrp
          for(idr in 1:length(all_drugnames)){
            expogrpnames[[idr]] <- c( paste0("[",expogrp[[idr]][-length(expogrp[[idr]])],";",expogrp[[idr]][-1]-1,"]"), paste0(">",expogrp[[i]][length(expogrp[[i]])]) )
            all_drugnames[[idr]] <- paste0((tmp<-expand.grid(expogrpnames[[idr]], all_drugnames[[idr]] ))[,2]," & ", tmp[,1])
            for(irw in length(all_drugnames[[idr]]):1){
              pattern     <- paste0(names(adrug[[idr]])[1],irw)
              new_pattern <- all_drugnames[[idr]][irw]
              dimnames(res_tab)[[1]] <- gsub(pattern, new_pattern, dimnames(res_tab)[[1]], fixed = TRUE)
              events_sum$all_cat <- gsub(  pattern, new_pattern, events_sum$all_cat )
              chopdat$adrugnames[paste0( names(adrug[[idr]])[1],chopdat[,names(adrug[[idr]])[1]]) == pattern] <- new_pattern
            }  
          }  
        } else {
          for(idr in 1:length(all_drugnames))
            for(irw in length(all_drugnames[[idr]]):1){
              pattern     <- paste0(names(adrug[[idr]])[1],irw)
              new_pattern <- all_drugnames[[idr]][irw]
              dimnames(res_tab)[[1]] <- gsub(pattern, new_pattern, dimnames(res_tab)[[1]], fixed = TRUE)
              events_sum$all_cat <- gsub(  pattern, new_pattern, events_sum$all_cat )
              chopdat$adrugnames[paste0( names(adrug[[idr]])[1],chopdat[,names(adrug[[idr]])[1]]) == pattern] <- new_pattern
            }  
        }
        
        #? if(length(adrug_names)==nrow(res_tab)) rownames(res_tab) <- adrug_names
      }    
      else warning(paste("'adrugnames' is not used. Its length should be", sum(unlist(lapply(adrug,ncol))) ) )
      if(!missing(expogrp)){
        print(1)
      }
    }  
    
    if(length(agegrp)>0){
      agegrp_names <- c(min(astart),agegrp,max(aend)) # the same length as 'age_groups'  + 1
      #age_groups       <- seq(from = min(scri_input$start)+60, to = max(scri_input$end)-90, by = 30)
      #age_groups_names <- c(min(scri_input$start),age_groups,max(scri_input$end)) # the same length as 'age_groups'  + 1
      agegrp_names <- paste0("[",agegrp_names[-length(agegrp_names)],";",agegrp_names[-1]-c(rep(1,length(agegrp_names)-2),0),"]")
      for(iage in length(agegrp_names):1){
        dimnames(res_tab)[[1]] <- gsub(paste0("relevel\\(age,ref=.*)",iage), paste0("t",agegrp_names[iage]), dimnames(res_tab)[[1]])
        dimnames(res_tab)[[1]] <- gsub(paste0("age",iage), paste0("t",agegrp_names[iage]), dimnames(res_tab)[[1]])
      }  
    }

    dimnames(res_tab)[[1]] <- gsub("factor(cov)", "cov=", dimnames(res_tab)[[1]], fixed = TRUE)
  }  
  

  ret <- list( tab          = events_sum)
  if(sccs_res) ret <- c( ret, tab_analysis = list(res_tab) )
  ret <- c( ret, 
            call         = list( match.call()),
            adrugnames   = list( adrugnames), 
            expogrp      = list( expogrp), 
            washout      = list( washout) )
  
  if(sccs_res) ret <- c( ret, standardSCCS = list( summary(mod)) )
  else ret <- c( ret, standardSCCS = list( mod) )

  if(save_data) ret <- c( ret, chopdat=list(chopdat) )
  
  ret
}

