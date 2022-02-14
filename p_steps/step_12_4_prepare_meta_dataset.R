#######################
#
#
#  12.4 preparation for meta analysis
#


prevax_per_start <- -90
prevax_per_end   <- -30
vax1_end         <- 28
vax2_end         <- 28
original_rw_names <- c( paste0( "pre-exposure [",prevax_per_start,";",prevax_per_end,"]" ), 
                              paste0( "buffer" ),  
                              paste0( "dose 1 day 0" ),  
                              paste0( "dose 1 [1;",vax1_end,"]" ),  
                              paste0( "between" ), 
                              paste0( "dose 2 day 0" ), 
                              paste0( "dose 2 [1;",vax2_end,"]" )
)
                              
period_names_Sophie <- c( "control window",
                          "buffer period",
                          "dose 1 day 0",
                          "dose 1 risk window",
                          "in between doses",
                          "dose 2 day 0",
                          "dose 2 risk window" )
# compare period names:
print("CHECK: please compare risk windows names:")
print(cbind(original_rw_names=original_rw_names, period_names_Sophie))

if(length(original_rw_names)!= length(period_names_Sophie))
  stop("Different length of 'original_rw_names' and 'period_names_Sophie'!")


#######################################################
#
# function to build a table for meta-analysis for one model:
#
model_to_tab <- function(dap, subgroup, between_model_name = "_scri_models_A", 
                         model_names_include = "buf_betw",
                         select_models,
                         model_rw_names = original_rw_names,
                         report_labels = period_names_Sophie, 
                         var_name = "vd"){
  
  cat(paste0("Models from file: '",dap,between_model_name,subgroup,"'\n"))
  models <- get(paste0(dap,between_model_name,subgroup))
  
    
  
  if(!missing(select_models)) {
    models_selected <- names(models)[names(models) %in% select_models]
    if(length(models_selected)!=length(select_models)){
      print("Not all 'select_models' were found.")
      print( list( found     = names(models)[names(models) %in% select_models],
                   not_found = names(models)[ !( names(models) %in% select_models) ] ))
    }
  } else {
    models_selected <- names(models)[ grep(model_names_include, names(models)) ]
    # delete later!!! Because a wrong name in 12_2: forgot to add 'buf_betw':
    if( model_names_include == "buf_betw" & substring(subgroup,1,8)=="_sex_age")
      models_selected <- c( "all_brands_sex_age", models_selected)
  }
  cat(paste0("\nSelected model names:\n\n'" , paste0(models_selected,collapse = "',\n'"),"\n"))
  cat(paste0("\nNOT Selected model names:\n\n'" , paste0(names(models)[ !( names(models) %in% models_selected) ],collapse = "',\n'"),"'.\n\n\n"))
  
  if(length(models_selected)==0) return("No model is founded.")
  
  all_tabs <- vector(mode="list",length=length(models_selected))
  names(all_tabs) <- models_selected

  for(im in models_selected){
    model <- models[[im]]
    
    if( is.null(unlist(model[[1]])[1])   ) next   
    if( unlist(model[[1]])[1]=="no data" ) next

    tab   <- model$tab
    
    #####
    # change names of risk windows:
    # replace period names:
    if(length(model_rw_names)!= length(report_labels))
      stop("Different length of 'model_rw_names' and 'report_labels'!")
    tab$label <- tab$all_cat
    for(ilab in 1:length(model_rw_names))
      tab$label <- gsub(model_rw_names[ilab], report_labels[ilab],tab$label, fixed=T)
    
    # change colnames:
    names(tab)[match( 
      c("RR","2.5%","97.5%","Pr(>|z|)","coef","se(coef)"),names(tab))] <- 
      c("irr","lci",  "uci",    "pval",  "yi",     "sei")
  
    # create extra variables:
    tab$dap       <- dap
    tab$analysis  <- "unadjusted"
    tab$eventtype <- "myopericarditis"
    tab$subgroup  <- subgroup
    tab$vacctype  <-  switch( substring(tolower(im),1,3), 
                              "all"="All brands",
                              "ast"="AstraZeneca",
                              "pfi"="Pfizer",
                              "mod"="Moderna",
                              "j&j"="Jonson & Jonson",
                              "j"  ="Jonson & Jonson",
                              substring(im,1,10)
    )
    if("sex" %in% names(tab)) tab$sex <- as.character(tab$sex)
    if(grepl("sex",tolower(im))){
      if(grepl("sexf|sex0",tolower(im))) tab$sex <- "women"
      if(grepl("sexm|sex1",tolower(im))) tab$sex <- "men"
    }  
    if(any(names(tab)=="sex")){
      tab$sex[substring(tolower(as.character(tab$sex)),1,1) %in% c("f","0","w","v")] <- "women"
      tab$sex[substring(tolower(as.character(tab$sex)),1,1) %in% c("m","1")]         <- "men"
    }
    if("age_cat" %in% names(tab)) tab$age_cat <- as.character(tab$age_cat)
    if(grepl("age",tolower(im))){
      if(grepl("age(-1,30]", tolower(im),fixed=T)) tab$age_cat <- "under30"
      if(grepl("age(30,120]",tolower(im),fixed=T)) tab$age_cat <- "30up"
    }  
    if(any(names(tab)=="age_cat")){
      tab$age_cat[tolower(as.character(tab$age_cat)) %in% c("(-1,30]" )] <- "under30"
      tab$age_cat[tolower(as.character(tab$age_cat)) %in% c("(30,120]")] <- "30up"
    }
    
    # assign 0 and 1 if 'control window'
    tab[ grep("control window",tab$label), c("irr","lci",  "uci")] <- 1
    tab[ grep("control window",tab$label), c("yi", "sei"        )] <- 0
    
    tab <- tab[, !( names(tab) %in% c("all_cat2")  )]
   
    # to get the same order of all column names:
    cols_betw_drug_period <- ((1:ncol(tab))[names(tab)=="drug"]+1) : ((1:ncol(tab))[names(tab)=="period"]-1) 
    if( length(cols_betw_drug_period)==1 | cols_betw_drug_period[1] <=cols_betw_drug_period[2] ) 
      tab <- cbind.data.frame(  tab[, !( (1:ncol(tab)) %in% cols_betw_drug_period ) ],
                                tab[,    (1:ncol(tab)) %in% cols_betw_drug_period, drop=F   ]
                             )
    if( "sex" %in% names(tab) & "age_cat" %in% names(tab) )
      tab <- tab[, c( names(tab)[!(names(tab) %in% c("sex","age_cat"))], "sex", "age_cat" )]
    
    all_tabs[[im]] <- tab[, !( names(tab) %in% c("all_cat2")  )]
  }

  # number of columns should be the same!
  if( any( unlist(lapply(all_tabs,ncol))!=unlist(lapply(all_tabs,ncol))[1]) ){
    print(unlist(lapply(all_tabs,ncol)))
    res <- readline(prompt="Stop?")
    if(res!="n") stop("Different number of columns!")
  }
  tab1 <- do.call( "rbind.data.frame", all_tabs ); rownames(tab1)<-NULL
  
  list(one_table = tab1,
       multiple_tables = all_tabs)
}   # the end of function 'model_to_tab'

# examples:
# tabs <- model_to_tab( dap="CPRD", subgroup="all" ,         report_labels=period_names_Sophie )
#
#  CPRD_sex_age_all_data <- model_to_tab( dap="CPRD", subgroup="sex_age_all" , report_labels=period_names_Sophie ) # "CPRD_scri_models_A_sex_age_all"
#  CPRD_sex_age_all_data$one_table


daps <- c("CPRD")  # c("CPRD","ARS", ...) 
subgroups <- c( paste0( c("","_age","_sex_age"), "_all"                         ),
                paste0( c("","_age","_sex_age"), "_noCovid_before_myoperi"      ),
                paste0( c("","_age","_sex_age"), "_noCovid_before_myoperi_14d"  ),
                paste0( c("","_age","_sex_age"), "_noCovid_before_myoperi_28d"  ),
                paste0( c("","_age","_sex_age"), "_noCovid_in_study_period"     )
              )

new_tables <- c()
for(idap in daps)
  for(igr in subgroups){
    res <- model_to_tab( dap=idap, 
                  subgroup=igr, 
                  between_model_name = "_scri_models_A", 
                  model_names_include = "buf_betw",
                  model_rw_names = original_rw_names,
                  report_labels = period_names_Sophie, 
                  var_name = "vd")
    assign(paste0(idap,igr), res )
    new_tables <- c(new_tables, paste0(idap,igr))
  }

# names of created tables:
new_tables

# tables for meta analysis:
CPRD_all$one_table                              
CPRD_age_all$one_table                           
CPRD_sex_age_all$one_table 

CPRD_noCovid_before_myoperi$one_table            
CPRD_age_noCovid_before_myoperi$one_table       
CPRD_sex_age_noCovid_before_myoperi$one_table 

CPRD_noCovid_before_myoperi_14d$one_table        
CPRD_age_noCovid_before_myoperi_14d$one_table    
CPRD_sex_age_noCovid_before_myoperi_14d$one_table

CPRD_noCovid_before_myoperi_28d$one_table        
CPRD_age_noCovid_before_myoperi_28d$one_table    
CPRD_sex_age_noCovid_before_myoperi_28d$one_table

CPRD_noCovid_in_study_period$one_table           
CPRD_age_noCovid_in_study_period$one_table       
CPRD_sex_age_noCovid_in_study_period$one_table   

