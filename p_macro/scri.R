
# Program Information  ----------------------------------------------------

# Program:      scri.R 
# Author:       Svetlana Belitser (started with function 'standardsccs' from library SCCS (sept 2021) )
#               nov 2021
#
#  * clear output with names of intervals (instead of unclear numbers)
#  * factor and character variables can be used for 'stratified' analysis (and interactions) 
#
#
#  * in input parameters 'adrugnames' is added. The length of 'adrugnames' should correspond to the number of intervals in 'adrug'. 
#
#  * rw_ref = 1    # number of risk window reference category
#  * sameexpopar also works 
#  * expogrp works
#  * covariate interaction of splitting works
#  * dataformat: 'multi' works. "stack": did not try yet


scri <- function(formula, indiv, astart, aend, aevent, adrug, aedrug, adrugnames, rw_ref=1, expogrp = list(), washout = list(), 
                         sameexpopar = list(), agegrp = NULL, seasongrp=NULL, dob=NULL, dataformat="stack", data, save_data=F) {
  
  if(nrow(data)==0) return("no data") 
  
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
  if(is.character(substitute(aevent)))  aevent <- data[, aevent ]
  # adrug  <- eval(substitute(adrug), data, parent.frame())
  aedrug <- eval(substitute(aedrug), data, parent.frame())
  dob <- eval(substitute(dob), data, parent.frame())
  
  #data[,deparse1(substitute(aevent))] 
  
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
                        sameexpopar = F,# Sv sameexpopar, 
                        agegrp = agegrp, seasongrp=seasongrp, dob=dob, cov=cov, dataformat=dataformat, data=NULL)
   
  
  # chopdat[1:10,]

  adruga <- qq[!(qq %in% cova)]
  
  
  # SCRI ==> delete 'baseline' risk windows 0
  for(idr in adruga){
    chopdat <- chopdat[chopdat[,idr] != 0,]  # delete rows with risk intervals 0
    if(length(expogrp)>0) 
      chopdat[chopdat[,idr] %in% ( (rw_ref-1)*length(expogrp) +1:length(expogrp)), idr] <- (rw_ref-1)*length(expogrp) + 1
    
    if(sameexpopar)
      if(length(expogrp)>0){
        for(iexpo in 1:length(expogrp)){
          npar <- sort(unique(as.character(chopdat[chopdat[,idr]!=rw_ref, idr])))
          chopdat[ chopdat[,idr] %in% npar[seq(iexpo,length(npar),by=length(expogrp))] , idr] <- npar[iexpo]
        }    
      }    
      else {
        chopdat[ chopdat[,idr]!=rw_ref , idr] <- 2
        if(length(adrugnames)!=2){
          adrugnames[-rw_ref] <- paste( adrugnames[-rw_ref], collapse=" & ")
          adrugnames <- adrugnames[!duplicated(adrugnames)]
        }  
      }  
    
    chopdat[,idr] <- as.factor(as.character(chopdat[,idr]))
    lv <- levels(chopdat[,idr])
    contrasts(chopdat[,idr]) <- contr.treatment(length(lv),(1:length(lv))[lv==as.character(rw_ref)])
  }  
  

  events_sum     <- as.data.frame.table(tapply(chopdat$event,    as.list(chopdat)[c(cova, adruga )], sum))
  atrisk_days    <- as.data.frame.table(tapply(chopdat$interval, as.list(chopdat)[c(cova, adruga )], sum))
  atrisk_persons <- as.data.frame.table(tapply(chopdat$indiv,    as.list(chopdat)[c(cova, adruga )], function(x)length(unique(x)) ))
  
  events_sum <- cbind.data.frame(events_sum, atrisk_days[,ncol(atrisk_days)], atrisk_persons[,ncol(atrisk_persons)])
  names(events_sum)[ncol(events_sum)+(-2:0)] <- c("n_events","atrisk_days","atrisk_persons")
  events_sum$relative_rate <- events_sum$n_events / events_sum$atrisk_days    * 100
  events_sum$event_percent <- events_sum$n_events / events_sum$atrisk_persons * 100
  
  events_sum$all_cat <- rep("",nrow(events_sum))
  events_sum <- events_sum[,c(ncol(events_sum), 1:(ncol(events_sum)-1) )]
  for(i in 2:(ncol(events_sum)-5)){
    #if(i==ncol(events_sum)-3) events_sumall_cat2 <- events_sumall_cat
    events_sum$all_cat <- paste0( events_sum$all_cat, ifelse(i==2,"",":"), names(events_sum)[i], events_sum[,i] )
  }

  
  #events_sum0 <- events_sum
  
  #covsmall   <- events_sum$type_vax1[ is.na(events_sum$n_events) | events_sum$n_events <=6 ]
  #adrugsmall <- events_sum$vd[        is.na(events_sum$n_events) | events_sum$n_events <=6 ]
  #
  #for(i in 1:length(covsmall)){
  #  chopdat <- chopdat[ !( chopdat[,"type_vax1"]==covsmall[i] & chopdat[,"vd"]==adrugsmall[i] ) ,]
  #}
  
 # for(idr in adruga){
 #   chopdat <- chopdat[chopdat[,idr] != 0,]  # delete rows with risk intervals 0
 #   
 #   chopdat[,idr] <- as.factor(as.character(chopdat[,idr]))
 #   lv <- levels(chopdat[,idr])
 #   contrasts(chopdat[,idr]) <- contr.treatment(length(lv),(1:length(lv))[lv==as.character(cov_ref)])
 # }  
  
  
  # clogit(event ~ type_vax1/vd + strata(indivL) + offset(log(interval)), data=chopdat)
  # coxph(formula = Surv(rep(1, 1190L), event) ~ I(ovd %in% 2:3) + I(ovd %in% 3:4) + eventday + strata(indivL) + offset(log(interval)), data = chopdat)
  fmla <- paste(formula, "+", "strata(indivL)", "+", "offset(log(interval))")
  fmla1 <- as.formula(paste("event~", fmla[3]))
  mod <- try( clogit(formula = fmla1, data = chopdat) )
  
  if(class(mod)[[1]]== "try-error"){ 
    warning(paste("There are small numbers in at least one strata, check the SCCS model specification.\n"))
    sccs_res <- F
  }  
  else sccs_res <- T

  if(sccs_res){
    res_tab <-    cbind.data.frame( exp( cbind( RR=mod$coef, confint(mod)) ),  summary(mod)$coefficients[,c("Pr(>|z|)", "coef", "se(coef)" ), drop=F] )
    dimnames(res_tab)[[2]] <- gsub(" ", "", dimnames(res_tab)[[2]] )
    # shift names:
    shifted_names <-  paste0(adruga,levels(events_sum[,adruga]))
    for(i in length(shifted_names):1)
      dimnames(res_tab)[[1]] <- gsub(paste0(adruga,i), shifted_names[i], dimnames(res_tab)[[1]] )
    
    events_sum[, names(res_tab) ] <- NA 
    matched_rows <- match( rownames(res_tab[!is.na(res_tab[,1]),]), events_sum$all_cat )
    events_sum[ matched_rows[!is.na(matched_rows)], ncol(events_sum) - (ncol(res_tab)-1):0 ] <- res_tab[!is.na(res_tab[,1]),][!is.na(matched_rows),]
    if(sum(!is.na(matched_rows)) != sum(!is.na(res_tab[,1])))
      warning(paste(
        'Not all coefficients from the analysis are matched to frequency table. Not matched:"',
        paste0( rownames(res_tab[!is.na(res_tab[,1]),])[ is.na(matched_rows) ] ,collapse='", "'), '".'  ))
    
    chopdat$adrugnames <- ""
    res_tab$rownames    <- rownames(res_tab)
    events_sum$all_cat2 <- events_sum$all_cat

    #Sv:
    if(!missing(adrugnames)){
      #if(length(adrugnames)==nrow(events_sum) ){
      if(length(adrugnames)==sum(unlist(lapply(adrug,ncol)))){
        all_drugnames <- tapply ( adrugnames,  cut(1:length(adrugnames), cumsum(c(0,unlist(lapply(adrug,ncol)))) ), c, simplify=F )
        if(sum(unlist(lapply(expogrp,length)))>0){
          if(!is.list(expogrp)) expogrp <- list(expogrp)
          expogrpnames <- expogrp
          for(idr in 1:length(all_drugnames)){
            expogrpnames[[idr]] <- c( paste0("[",expogrp[[idr]][-length(expogrp[[idr]])],";",expogrp[[idr]][-1]-1,"]"), paste0(">",expogrp[[idr]][length(expogrp[[idr]])]) )
            names_rw <- all_drugnames[[idr]]
            all_drugnames[[idr]] <- paste0((tmp<-expand.grid(expogrpnames[[idr]], all_drugnames[[idr]] ))[,2]," & ", tmp[,1])
            all_drugnames[[idr]][ (rw_ref-1)*length(expogrpnames[[idr]]) + 1 ] <- names_rw[rw_ref]
            #all_drugnames[[idr]] <- paste0((tmp<-expand.grid(expogrpnames[[idr]], all_drugnames[[idr]] ))[,2]," & ", tmp[,1])
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
  

  ret <- list( tab          = events_sum )
  if(sccs_res) ret <- c( ret, tab_analysis = list( res_tab ))
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

