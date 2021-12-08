
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


scri <- function( formula, indiv, astart, aend, aevent, adrug, aedrug, 
                  adrugnames, rw_ref=1, 
                  expogrp = list(), washout = list(), 
                  sameexpopar = list(), 
                  agegrp = NULL,     #  first element in the vector is the start of the second age group. The first age group starts at the minimum of astart.
                  age_ref = "most_events",
                  seasongrp=NULL, dob=NULL, dataformat="stack", 
                  data, 
                  save_data=F) {
  
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
  if(is.character(substitute(aevent)))  aevent <- data[, aevent ]   #Sv
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
  adruga <- qq[!(qq %in% cova)]  

  #Sv: ???
  if(length(adrug)>1){
    if( length(rw_ref)==1 )      rw_ref <- rep(rw_ref,length(adrug))
    if( length(sameexpopar)==1 ) sameexpopar <- rep(sameexpopar,length(adrug))
    if( !is.list(expogrp) )      expogrp <- rep( list(expogrp), length(adrug) )
  }
  if(!missing(agegrp))     agegrp     <- agegrp[agegrp>min(astart) & agegrp<max(aend)] 
  
  if(!is.list(adrugnames)) adrugnames <- list(adrugnames)
  names(adrugnames) <- adruga
  if( length(unlist(adrugnames))==sum(unlist(lapply(adrug,ncol))) ) 
    adrugnames <- lapply(adrugnames, function(x)c("none",x))
  
  
  chopdat <- formatdata2(indiv=indiv, astart=astart, aend=aend, 
                         aevent=aevent, adrug=adrug, aedrug=aedrug, expogrp = expogrp, 
                         washout = washout , 
                         sameexpopar =  sameexpopar, 
                         agegrp = agegrp, 
                         seasongrp=seasongrp, dob=dob, cov=cov, dataformat=dataformat, 
                         data=NULL)
   
  # chopdat[1:10,]
  
  # function:
  create_events_sum <- function(chopdat,adruga, with_cov=T, rename_col=F){
    if(with_cov) drs <- c(cova, adruga )
    else         drs <- adruga
    events_sum     <- as.data.frame.table(tapply(chopdat$event,    as.list(chopdat)[drs], sum))
    atrisk_days    <- as.data.frame.table(tapply(chopdat$interval, as.list(chopdat)[drs], sum))
    atrisk_persons <- as.data.frame.table(tapply(chopdat$indiv,    as.list(chopdat)[drs], function(x)length(unique(x)) ))
   
    events_sum <- cbind.data.frame(events_sum, atrisk_days[,ncol(atrisk_days)], atrisk_persons[,ncol(atrisk_persons)])
    names(events_sum)[ncol(events_sum)+(-2:0)] <- c("n_events","atrisk_days","atrisk_persons")
    events_sum$relative_rate <- events_sum$n_events / events_sum$atrisk_days    * 100
    events_sum$event_percent <- events_sum$n_events / events_sum$atrisk_persons * 100
    
    for(i in 1:( match("n_events", colnames(events_sum))-1 ))
      events_sum$all_cat <- paste0( events_sum$all_cat, ifelse(i==1,"",":"), names(events_sum)[i], events_sum[,i] )
    if(rename_col){
      events_sum <- cbind.data.frame(drug=adruga, events_sum)
      colnames(events_sum)[colnames(events_sum)==adruga] <- "period" 
    }
    if(!with_cov) events_sum[,cova] <- NA
    events_sum
  } # end of function
  

  for(idr in adruga){
    if(any((tb<-table(chopdat[,idr]))==0)){
      adrugnames[[adruga==idr]] <- adrugnames[[adruga==idr]][tb>0]
      chopdat[,idr] <- as.factor(as.character(chopdat[,idr]))
    }
    contrasts(chopdat[,idr]) <- contr.treatment(length( levels(chopdat[,idr]) ), match( as.character(rw_ref[adruga==idr]), levels(chopdat[,idr]) ))
    colnames(contrasts(chopdat[,idr])) <- rownames(  contrasts(chopdat[,idr])    )[as.numeric(colnames(  contrasts(chopdat[,idr])   ))]
  }
   
  for(idr in 1:length(adruga)){
    events_sum_idr <- create_events_sum(chopdat, adruga[idr], with_cov=T, rename_col=T)
    #events_sum_idr <- cbind.data.frame(drug=adruga[idr], events_sum_idr)
    #colnames(events_sum_idr)[colnames(events_sum_idr)==adruga[idr]] <- "period" 
    if(idr==1) events_sum <- list(tab=events_sum_idr)
    else       events_sum$tab <- rbind.data.frame( events_sum$tab, events_sum_idr)
  }  
  events_sum <- c(events_sum, list( all_combi=create_events_sum(chopdat, adruga, with_cov=T, rename_col=F)) )
 
  
  # for 'age':
  if(any(all.vars(as.formula(formula))=="age")){
    age_tab <- create_events_sum(chopdat,"age", with_cov=F, rename_col=T)
    events_sum$tab <- rbind.data.frame( events_sum$tab, age_tab[,match(colnames(events_sum$tab),colnames(age_tab))])
    # contrasts:
    if(age_ref=="most_events") 
      age_ref_real <- match(age_tab$period[  age_tab$n_events == max(age_tab$n_events,na.rm=T) ][1], levels(age_tab$period))
    if(is.numeric(age_ref)){
      if(!is.null(names(age_ref)) & substring(names(age_ref),1,5)=="close"){
        difs <- age_ref - c(min(astart),agegrp,max(aend))
        age_ref_real <- c(min(astart),agegrp,max(aend))[abs(difs)==min(abs(difs))][1]
        age_ref_real <- ifelse( age_ref - age_ref_real >=0, 
                               match( age_ref_real, c( min(astart),agegrp             )),
                               match( age_ref_real, c(             agegrp,max(aend)   ))  )
      } 
      else age_ref_real <- age_ref
    }  
    contrasts(chopdat$age) <- contr.treatment( length(levels(chopdat$age)), age_ref_real )
    colnames(contrasts(chopdat$age)) <- rownames(  contrasts(chopdat$age)    )[as.numeric(colnames(  contrasts(chopdat$age)   ))]
  }
  

  # clogit(event ~ type_vax1/vd + strata(indivL) + offset(log(interval)), data=chopdat)
  # coxph(formula = Surv(rep(1, 1190L), event) ~ I(ovd %in% 2:3) + I(ovd %in% 3:4) + eventday + strata(indivL) + offset(log(interval)), data = chopdat)
  fmla <- paste(formula, "+", "strata(indivL)", "+", "offset(log(interval))")
  fmla1 <- as.formula(paste("event~", fmla[3]))
  mod <- try( clogit(formula = fmla1, data = chopdat, control=coxph.control(iter.max=1000)) )
  
  if(class(mod)[[1]]== "try-error"){ 
    warning(paste("There are small numbers in at least one strata, check the SCCS model specification.\n"))
    sccs_res <- F
  }  
  else sccs_res <- T

  if(sccs_res){
    res_tab <-    cbind.data.frame( exp( cbind( RR=mod$coef, confint(mod)) ),  summary(mod)$coefficients[,c("Pr(>|z|)", "coef", "se(coef)" ), drop=F] )
    dimnames(res_tab)[[2]] <- gsub(" ", "", dimnames(res_tab)[[2]] )

    events_sum$tab[, names(res_tab) ] <- NA 
    matched_rows <- match( rownames(res_tab[!is.na(res_tab[,1]),]), events_sum$tab$all_cat )
    events_sum$tab[ matched_rows[!is.na(matched_rows)], ncol(events_sum$tab) - (ncol(res_tab)-1):0 ] <- res_tab[!is.na(res_tab[,1]),][!is.na(matched_rows),]
    if(sum(!is.na(matched_rows)) != sum(!is.na(res_tab[,1])))
      warning(paste(
        'Not all coefficients from the analysis are matched to frequency table. Not matched:"',
        paste0( rownames(res_tab[!is.na(res_tab[,1]),])[ is.na(matched_rows) ] ,collapse='", "'), '".'  ))
    
    if(save_data) chopdat[,paste0(adruga,"_names")] <- ""
    res_tab$rownames              <- rownames(res_tab)
    events_sum$tab$all_cat2       <- events_sum$tab$all_cat
    events_sum$all_combi$all_cat2 <- events_sum$all_combi$all_cat

    #Sv:
    if(!missing(adrugnames)){
      #if(length(unlist(adrugnames)) %in%  (c(0,length(adruga)) + sum(unlist(lapply(adrug,ncol)))) ){
      #  if(sum(unlist(lapply(expogrp,length)))>0){  ##### NOT YET!!!
      #    if(!is.list(expogrp)) expogrp <- list(expogrp)
      #    expogrpnames <- expogrp
      #    for(idr in 1:length(all_drugnames)){
      #      expogrpnames[[idr]] <- c( paste0("[",expogrp[[idr]][-length(expogrp[[idr]])],";",expogrp[[idr]][-1]-1,"]"), paste0(">",expogrp[[idr]][length(expogrp[[idr]])]) )
      #      names_rw <- all_drugnames[[idr]]
      #      all_drugnames[[idr]] <- paste0((tmp<-expand.grid(expogrpnames[[idr]], all_drugnames[[idr]] ))[,2]," & ", tmp[,1])
      #      all_drugnames[[idr]][ (rw_ref-1)*length(expogrpnames[[idr]]) + 1 ] <- names_rw[rw_ref]
      #      for(irw in length(all_drugnames[[idr]]):1){
      #        pattern     <- paste0(names(adrug[[idr]])[1],irw)
      #        new_pattern <- all_drugnames[[idr]][irw]
      #        dimnames(res_tab)[[1]] <- gsub(pattern, new_pattern, dimnames(res_tab)[[1]], fixed = TRUE)
      #        events_sum$all_cat <- gsub(  pattern, new_pattern, events_sum$all_cat )
      #        if(save_data) chopdat[paste0( names(adrug[[idr]])[1],chopdat[,names(adrug[[idr]])[1]]) == pattern, paste0(adruga[idr],"_names")  ] <- new_pattern
      #      }  
      #    }  
      #  } else {
          for(idr in 1:length(adruga))
            for(irw in length(adrugnames[[idr]]):1){
              pattern     <- paste0(adruga[[idr]],levels(chopdat[,adruga[idr]])[irw]) 
              new_pattern <- adrugnames[[idr]][irw]
              dimnames(res_tab)[[1]]       <- gsub( pattern, new_pattern, dimnames(res_tab)[[1]], fixed = TRUE)
              events_sum$tab$all_cat       <- gsub( pattern, new_pattern, events_sum$tab$all_cat )
              events_sum$all_combi$all_cat <- gsub( pattern, new_pattern, events_sum$all_combi$all_cat )
              if(save_data) chopdat[paste0( names(adrug[[idr]])[1],chopdat[,names(adrug[[idr]])[1]]) == pattern, paste0(adruga[idr],"_names")  ] <- new_pattern
            }  
      #  }
      #}    
      #else warning(paste("'adrugnames' is not used. Its total length should be", sum(unlist(lapply(adrug,ncol))) ) )
      #if(!missing(expogrp)){
      #  print(1)
      #}
    }  

    #  'age' names:
    if(any(all.vars(as.formula(formula))=="age")){
      agegrp_names <- c(min(astart),agegrp,max(aend)) 
      agegrp_names <- paste0("[",agegrp_names[-length(agegrp_names)],";",agegrp_names[-1]-c(rep(1,length(agegrp_names)-2),0),"]")
      if(save_data) chopdat$age_names <- ""
      for(iage in length(agegrp_names):1){
        dimnames(res_tab)[[1]] <- gsub(paste0("relevel\\(age,ref=.*)",iage), paste0("t",agegrp_names[iage]), dimnames(res_tab)[[1]])
        dimnames(res_tab)[[1]] <- gsub(paste0("age",iage), paste0("t",agegrp_names[iage]), dimnames(res_tab)[[1]])
        events_sum$tab$all_cat <- gsub(paste0("age",iage), paste0("t",agegrp_names[iage]), events_sum$tab$all_cat )
        if(save_data) chopdat$age_names[ chopdat$age == iage  ] <- paste0("t",agegrp_names[iage])
      }  
    }

  }  
  

  ret <- events_sum
  if(sccs_res) ret <- c( ret, tab_analysis = list( res_tab ))
  
  # contrasts:
  contrasts_dr <-  list()
  for(idr in adruga)
    contrasts_dr <- c( contrasts_dr, list(contrasts( chopdat[,idr] )) ) 
  names(contrasts_dr) <- adruga
  if(any(all.vars(as.formula(formula))=="age"))
    contrasts_dr <- c( contrasts_dr, age=list(contrasts( chopdat$age )) ) 
  
  ret <- c( ret, 
            call         = list( match.call()),
            list( parameters   = list(
                      adrugnames   = list( adrugnames   ), 
                      rw_ref       = list( rw_ref       ),
                      contrasts    = list( contrasts_dr ),
                      expogrp      = list( expogrp      ), 
                      sameexpopar  = list( sameexpopar  ),
                      washout      = list( washout      ),
                      agegrp       = list( agegrp       ) 
              ))
         )
  
  if(sccs_res) ret <- c( ret, standardSCCS = list( summary(mod)) )
  else ret <- c( ret, standardSCCS = list( mod) )

  if(save_data) ret <- c( ret, chopdat=list(chopdat) )
  
  ret
}

