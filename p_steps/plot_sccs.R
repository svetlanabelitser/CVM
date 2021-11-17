
  # for dates:
  myoperi_date <- scri_input$myopericarditis_date
  peri_date    <- scri_input$pericarditis_date
  myo_date     <- scri_input$myocarditis_date
  
  vax1_date <- scri_input$date_vax1
  vax2_date <- scri_input$date_vax2
  
  # for days:
  myoperi_days <- scri_input$myopericarditis_days
  peri_days    <- scri_input$pericarditis_days
  myo_days     <- scri_input$myocarditis_days
  
  vax1_days <- scri_input$days_vax1
  vax2_days <- scri_input$days_vax2
  
  
  ##########
  #    calculate frequencies for histograms:
  # from 01-01-2019 to last myo/pericarditis:
  breaks <- seq(min(myoperi_date,na.rm=T)-1,max(myoperi_date,na.rm=T)+6,by=7) 
  # from 01-01-2020 to last myo/pericarditis:
  breaks2020 <- seq(as.Date("2020-01-01")-1,max(myoperi_date,na.rm=T)+6,by=7) 
  # from 01-01-2020 to last myo/pericarditis:
  # breaks2020_1day <- seq(as.Date("2020-01-01")-1,max(myoperi_date,na.rm=T)+6,by=1) 

  # counts or frequency:
  freq <- T  # or F
  hist_var <- ifelse(freq,"counts","density")
    
  hist_mp <- hist( myoperi_date, breaks, plot=F ) #, main="#myoperi per week (except the last interval)" )
  hist_m  <- hist( myo_date,     breaks, plot=F ) 
  hist_p  <- hist( peri_date,    breaks, plot=F )
 


  cond_mp <- myoperi_date > min(breaks2020)
  hist_mp_before_vax1 <- hist(myoperi_date[cond_mp & myoperi_date < vax1_date], breaks2020, freq=freq, plot=F )
  hist_mp_after_vax1  <- hist(myoperi_date[cond_mp &myoperi_date >= vax1_date], breaks2020, freq=freq, plot=F )
  hist_mp_after_vax2  <- hist(myoperi_date[cond_mp &myoperi_date >= vax2_date], breaks2020, freq=freq, plot=F )

  cond_m <- myo_date > min(breaks2020)
  hist_m_before_vax1  <- hist(myo_date[cond_m & myo_date <  vax1_date], breaks2020, freq=freq, plot=F )
  hist_m_after_vax1   <- hist(myo_date[cond_m & myo_date >= vax1_date], breaks2020, freq=freq, plot=F )
  hist_m_after_vax2   <- hist(myo_date[cond_m & myo_date >= vax2_date], breaks2020, freq=freq, plot=F )
  
  cond_p <- peri_date > min(breaks2020)
  hist_p_before_vax1  <- hist(peri_date[cond_p & peri_date <  vax1_date], breaks2020, freq=freq, plot=F )
  hist_p_after_vax1   <- hist(peri_date[cond_p & peri_date >= vax1_date], breaks2020, freq=freq, plot=F )
  hist_p_after_vax2   <- hist(peri_date[cond_p & peri_date >= vax2_date], breaks2020, freq=freq, plot=F )
  
  hist_mp2020 <- hist( myoperi_date[cond_mp], breaks2020, plot=F ) #, main="#myoperi per week (except the last interval)" )
  hist_m2020  <- hist( myo_date[cond_m],      breaks2020, plot=F ) 
  hist_p2020  <- hist( peri_date[cond_p],     breaks2020, plot=F )
  
  

  ##############
  # histograms for #myo/peri, #myo, # peri
  par(mfrow=c(1,1))
  hist_mp <- hist( myoperi_date, breaks,  freq=freq, plot=T, 
                 col=rgb(0,0,1,1/4), main="#myo-peri per week (except the last interval)" )
  grid()
  plot( hist_p, freq=freq, col=rgb(0,1,0,1/2), add=T)  
  plot( hist_m, freq=freq,  col=rgb(1,0,0,1/2), add=T)  
  legend("top",fill=c(rgb(0,0,1,1/4), rgb(0,1,0,1/2), rgb(1,0,0,1/2)), 
         legend=c("#myo/peri", "#peri", "#myo"), horiz=T,  )
  ################
 
  ##############
  # histograms for #myo/peri before and after vax1, and vax2:
  par(mfrow=c(2,1))
  hist_mp <- hist( myoperi_date, breaks,  freq=freq, plot=T, 
                   col=rgb(0.8,0.8,0.8,1/4), main="#myo-peri per week (except the last interval)" )
  hist_mp <- hist( myoperi_date[cond_mp], breaks2020,  freq=freq, plot=T, 
                   ylim=range(c(hist_mp2020[[hist_var]],
                                hist_mp_before_vax1[[hist_var]],
                                hist_mp_after_vax1[[hist_var]],
                                hist_mp_after_vax2[[hist_var]])), 
                   col=rgb(0.8,0.8,0.8,1/4), main="#myo-peri per week (except the last interval)" )
  grid()
  plot( hist_mp_before_vax1, freq=freq, col=rgb(0,1,0,0.8), add=T)  
  plot( hist_mp_after_vax1,  freq=freq, col=rgb(0.1,0.1,1,1/2), add=T)  
  plot( hist_mp_after_vax2,  freq=freq, col=rgb(1,0,0,1/3), add=T)  
  legend("top",fill=c(rgb(0.8,0.8,0.8,1/4), rgb(0,1,0,0.8), rgb(0.1,0.1,1,1/2), rgb(1,0,0,1/2) ), 
         legend=c("#myo/peri all", "before vax1", "after vax1", "after vax2"), horiz=T )
  ################
  
  
  
  ##############
  # histograms for #peri before and after vax1, and vax2:
  par(mfrow=c(2,1))
  hist_p <- hist( peri_date, breaks,  freq=freq, plot=T, 
                   col=rgb(0.8,0.8,0.8,1/4), main="#peri per week (except the last interval)" )
  hist_p <- hist( peri_date[cond_p], breaks2020,  freq=freq, plot=T, 
                   col=rgb(0.8,0.8,0.8,1/4), main="#peri per week (except the last interval)" )
  grid()
  plot( hist_p_before_vax1, freq=freq, col=rgb(0,1,0,0.8), add=T)  
  plot( hist_p_after_vax1,  freq=freq, col=rgb(0.1,0.1,1,1/2), add=T)  
  plot( hist_p_after_vax2,  freq=freq, col=rgb(1,0,0,1/3), add=T)  
  legend("topleft",fill=c(rgb(0.8,0.8,0.8,1/4), rgb(0,1,0,0.8), rgb(0.1,0.1,1,1/2), rgb(1,0,0,1/2) ), 
         legend=c("#peri all", "before vax1", "after vax1", "after vax2"), horiz=T,  )
  ################
  
  
  
  ##############
  # histograms for #myo before and after vax1, and vax2:
  par(mfrow=c(2,1))
  hist_m <- hist( myo_date, breaks,  freq=freq, plot=T, 
                   col=rgb(0.8,0.8,0.8,1/4), main="#myo per week (except the last interval)" )
  hist_m <- hist( myo_date[cond_m], breaks2020,  freq=freq, plot=T, 
                   col=rgb(0.8,0.8,0.8,1/4), main="#myo per week (except the last interval)" )
  grid()
  plot( hist_m_before_vax1, freq=freq, col=rgb(0,1,0,0.8), add=T)  
  plot( hist_m_after_vax1, freq=freq,  col=rgb(0.1,0.1,1,1/2), add=T)  
  plot( hist_m_after_vax2, freq=freq,  col=rgb(1,0,0,1/3), add=T)  
  legend("top",fill=c(rgb(0.8,0.8,0.8,1/4), rgb(0,1,0,0.8), rgb(0.1,0.1,1,1/2), rgb(1,0,0,1/2) ), 
         legend=c("#myo all", "before vax1", "after vax1", "after vax2"), horiz=T,  )
  ################
  
  
  
  ####################################################################
  #
  #   vax1: time==0
  #
  ##############
  
  myoperi_minus_vax1 <- myoperi_days - vax1_days
  peri_minus_vax1 <- peri_days - vax1_days
  myo_minus_vax1 <- myo_days - vax1_days
  
  myoperi_minus_vax2 <- myoperi_days - vax2_days
  peri_minus_vax2 <- peri_days - vax2_days
  myo_minus_vax2 <- myo_days - vax2_days
  
  # breaks parameters:
  interval_len <- 1  # 1 day  or 7 days
  #days_from <- -365
  #days_from <- -180
  days_from <- -92
  days_to <- max(c(myoperi_minus_vax1, myoperi_minus_vax2),na.rm=T)
  
  # breaks:
  # all available time
  breaks_before_vax1_all    <- seq( min(myoperi_minus_vax1)-1, -1, interval_len )
  breaks_after_vax1_all     <- seq( -1, max(c(myoperi_minus_vax1, myoperi_minus_vax2)+interval_len-1,na.rm=T), interval_len )
  breaks_vax1_all          <- c(breaks_before_vax1_all,breaks_after_vax1_all[-1])
  # from 'days_from'  to 'days_to'
  breaks_before_vax1_from   <- rev(seq( -1, days_from-interval_len+1, -interval_len ))
  breaks_after_vax1_from    <- seq( -1, days_to+interval_len-1, interval_len )
  breaks_vax1_from          <- c(breaks_before_vax1_from,breaks_after_vax1_from[-1])
  breaks_after_vax2_from    <- seq( -1,  days_to+interval_len-1, interval_len )       # vax2
  
  # conditions for: from 'days_from'  to 'days_to':
  cond_mp_before_vax1_from <- myoperi_minus_vax1 >= days_from & myoperi_minus_vax1 < 0
  cond_p_before_vax1_from  <- peri_minus_vax1    >= days_from & peri_minus_vax1    < 0
  cond_m_before_vax1_from  <- myo_minus_vax1     >= days_from & myo_minus_vax1     < 0
  
  cond_mp_after_vax1_from <- myoperi_minus_vax1 >= 0 & myoperi_minus_vax1 <= days_to 
  cond_p_after_vax1_from  <- peri_minus_vax1    >= 0 & peri_minus_vax1    <= days_to 
  cond_m_after_vax1_from  <- myo_minus_vax1     >= 0 & myo_minus_vax1     <= days_to
  
  cond_mp_vax1_from <- cond_mp_before_vax1_from | cond_mp_after_vax1_from
  cond_p_vax1_from  <-  cond_p_before_vax1_from |  cond_p_after_vax1_from
  cond_m_vax1_from  <-  cond_m_before_vax1_from |  cond_m_after_vax1_from
  
  cond_mp_after_vax2_from <- myoperi_minus_vax2 >= 0 & myoperi_minus_vax2 <= days_to 
  cond_p_after_vax2_from  <- peri_minus_vax2    >= 0 & peri_minus_vax2    <= days_to 
  cond_m_after_vax2_from  <- myo_minus_vax2     >= 0 & myo_minus_vax2     <= days_to

  # hist for all avalilable data  
  hist_mp_vax1 <- hist( myoperi_minus_vax1, breaks_vax1_all, plot=F )
  hist_p_vax1  <- hist( peri_minus_vax1,    breaks_vax1_all, plot=F )
  hist_m_vax1  <- hist( myo_minus_vax1,     breaks_vax1_all, plot=F)
  
  # hist for: from 'days_from'  to 'days_to':
  hist_mp_vax1_from <- hist( myoperi_minus_vax1[cond_mp_vax1_from], breaks_vax1_from, plot=F )
  hist_p_vax1_from  <- hist( peri_minus_vax1[   cond_mp_vax1_from], breaks_vax1_from, plot=F )
  hist_m_vax1_from  <- hist( myo_minus_vax1[    cond_m_vax1_from],  breaks_vax1_from, plot=F)
  
  hist_mp_before_vax1_from <- hist( myoperi_minus_vax1[cond_mp_before_vax1_from], breaks_before_vax1_from, plot=F )
  hist_p_before_vax1_from  <- hist( peri_minus_vax1[    cond_p_before_vax1_from], breaks_before_vax1_from, plot=F )
  hist_m_before_vax1_from  <- hist( myo_minus_vax1[     cond_m_before_vax1_from], breaks_before_vax1_from, plot=F)
  
  hist_mp_after_vax1_from <- hist( myoperi_minus_vax1[cond_mp_after_vax1_from], breaks_after_vax1_from, plot=F )
  hist_p_after_vax1_from  <- hist( peri_minus_vax1[    cond_p_after_vax1_from], breaks_after_vax1_from, plot=F )
  hist_m_after_vax1_from  <- hist( myo_minus_vax1[     cond_m_after_vax1_from], breaks_after_vax1_from, plot=F)
  
  hist_mp_after_vax2_from <- hist( myoperi_minus_vax2[cond_mp_after_vax2_from], breaks_after_vax2_from, plot=F )
  hist_p_after_vax2_from  <- hist( peri_minus_vax2[    cond_p_after_vax2_from], breaks_after_vax2_from, plot=F )
  hist_m_after_vax2_from  <- hist( myo_minus_vax2[     cond_m_after_vax2_from], breaks_after_vax2_from, plot=F)

  ##################
  # histograms with vax-time 1: for #myo/peri, #peri, #myo
  #   all available days:
  par(mfrow=c(2,1))
  hist_mp_vax1 <- hist( myoperi_minus_vax1, breaks_vax1_all,  freq=freq, plot=T, 
                        col=rgb(0.8,0.8,0.8,1/4), 
                        main=paste0("#myo/peri per ",interval_len," day[s] (except the last interval)\n vax1 (orange line)") )
  abline(v=0,col="orange", lwd=3)
  # for the chosen window:
  hist_mp_vax1_from <- hist( myoperi_minus_vax1[cond_mp_before_vax1_from | cond_mp_after_vax1_from], 
                             c(breaks_before_vax1_from,breaks_after_vax1_from[-1]), freq=freq, plot=T, 
                             col=rgb(0.8,0.8,0.8,1/4), 
                             main=paste0("#myo/peri per ",interval_len," day[s] (except the last interval)\n vax1 (orange line)") )
  abline(v=0,col="orange", lwd=3)
  grid()
  #plot( hist_mp_vax1, col=rgb(0,1,0,0.8), add=T)  
  plot( hist_p_vax1_from, freq=freq,  col=rgb(0.1,0.1,1,1/2), add=T)  
  plot( hist_m_vax1_from, freq=freq,  col=rgb(1,0,0,1/3), add=T)  
  legend("topright",fill=c(rgb(0.8,0.8,0.8,1/4), rgb(0.1,0.1,1,1/2), rgb(1,0,0,1/3) ), 
         legend=c("#myo/peri ", "#peri", "#myo"), horiz=T, bty="n" )
  ################
  
  ##################
  # histograms for #myo/peri with vax-time 1 and vax-time 2:
  #   all available days:
  par(mfrow=c(2,1))
  hist_mp_vax1 <- hist( myoperi_minus_vax1, breaks_vax1_all,  freq=freq, plot=T, 
                        col=rgb(0.8,0.8,0.8,1/4), 
                        main=paste0("#myo/peri per ",interval_len," day[s] (except the last interval)\n vax1 (orange line)") )
  abline(v=0,col="orange", lwd=3)
  # for the chosen window:
  hist_mp_before_vax1_from <- hist( myoperi_minus_vax1[cond_mp_before_vax1_from], breaks_before_vax1_from,  freq=freq, plot=T, 
                                    col=rgb(0,1,0,0.8), xlim=range(breaks_vax1_from),
                                    main=paste0("#myo/peri per ",interval_len," day[s] (except the last interval)\n vax1 (orange line)") )
  abline(v=0,col="orange", lwd=3)
  grid()
  plot( hist_mp_after_vax1_from, freq=freq,  col=rgb(0.1,0.1,1,1/2), add=T)  
  plot( hist_mp_after_vax2_from, freq=freq,  col=rgb(1,0,0,1/2), add=T)  
  legend("topright",fill=c(rgb(0,1,0,0.8),  rgb(0.1,0.1,1,1/2), rgb(1,0,0,1/2) ), 
         legend=c("#myo/peri before vax1",  "after vax1", "after vax2"), horiz=T, bty="n" )
  ################
  
  ##################
  # histograms for #peri with vax-time 1 and vax-time 2:
  #   all available days:
  par(mfrow=c(2,1))
  hist_p_vax1 <- hist( peri_minus_vax1, breaks_vax1_all,  freq=freq,  plot=T, 
                        col=rgb(0.8,0.8,0.8,1/4), 
                        main=paste0("#peri per ",interval_len," day[s] (except the last interval)\n vax1 (orange line)") )
  abline(v=0,col="orange", lwd=3)
  # for the chosen window:
  hist_p_before_vax1_from <- hist( peri_minus_vax1[cond_p_before_vax1_from], breaks_before_vax1_from,  freq=freq, plot=T, 
                                    col=rgb(0,1,0,0.8), xlim=range(breaks_vax1_from),
                                    main=paste0("#peri per ",interval_len," day[s] (except the last interval)\n vax1 (orange line)") )
  abline(v=0,col="orange", lwd=3)
  grid()
  plot( hist_p_after_vax1_from,  freq=freq, col=rgb(0.1,0.1,1,1/2), add=T)  
  plot( hist_p_after_vax2_from,  freq=freq, col=rgb(1,0,0,1/2), add=T)  
  legend("topright",fill=c(rgb(0,1,0,0.8),  rgb(0.1,0.1,1,1/2), rgb(1,0,0,1/2) ), 
         legend=c("#peri before vax1",  "after vax1", "after vax2"), horiz=T, bty="n" )
  ################
  
  ##################
  # histograms for #myo with vax-time 1 and vax-time 2:
  #   all available days:
  par(mfrow=c(2,1))
  hist_m_vax1 <- hist( myo_minus_vax1, breaks_vax1_all,  freq=freq, plot=T, 
                        col=rgb(0.8,0.8,0.8,1/4), 
                        main=paste0("#myo per ",interval_len," day[s] (except the last interval)\n vax1 (orange line)") )
  abline(v=0,col="orange", lwd=3)
  # for the chosen window:
  hist_m_before_vax1_from <- hist( myo_minus_vax1[cond_m_before_vax1_from], breaks_before_vax1_from,  freq=freq, plot=T, 
                                    col=rgb(0,1,0,0.8), xlim=range(breaks_vax1_from),
                                    main=paste0("#myo per ",interval_len," day[s] (except the last interval)\n vax1 (orange line)") )
  abline(v=0,col="orange", lwd=3)
  grid()
  plot( hist_m_after_vax1_from, freq=freq,  col=rgb(0.1,0.1,1,1/2), add=T)  
  plot( hist_m_after_vax2_from, freq=freq,  col=rgb(1,0,0,1/2), add=T)  
  legend("topright",fill=c(rgb(0,1,0,0.8),  rgb(0.1,0.1,1,1/2), rgb(1,0,0,1/2) ), 
         legend=c("#myo before vax1",  "after vax1", "after vax2"), horiz=T, bty="n" )
  ################
  
  



