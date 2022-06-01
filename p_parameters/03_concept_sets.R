vaccine__conceptssets <- c("Covid_vaccine")

concept_set_domains<- vector(mode="list")
concept_set_domains[["Covid_vaccine"]] = "VaccineATC"

OUTCOME_events <- list()

OUTCOME_events <- c("HF","CAD","MYOCARD","COVID","ARDS")

CONTROL_events <-list()
CONTROL_events <-c()

OUTCOMES_conceptssets <- c("HF_narrow","HF_possible","CAD_narrow","CAD_possible","MYOCARD_narrow","MYOCARD_possible","COVID_narrow","COVID_possible","ARDS_narrow","ARDS_possible")

COV_conceptssets <- c("COVCANCER","COVCOPD","COVHIV","COVCKD","COVDIAB","COVOBES","COVSICKLE","CONTRDIVERTIC","CONTRHYPERT")

DRUGS_conceptssets <- c("DP_COVCANCER","DP_COVDIAB","DP_CVD","DP_COVHIV","DP_COVCKD","DP_COVCOPD","DP_COVOBES","DP_COVSICKLE","IMMUNOSUPPR","DP_CONTRHYPERT")

SEVERCOVID_conceptsets <- c("MechanicalVent","ITA_HOSP_ARDS_COVID")

PROC_conceptsets <- c("ICU_VENTILATION")

RESULTS_conceptsets <- c("COVID_test_coded")


FREE_TEXT_conceptsets <- c()

concept_sets_of_our_study <- unique(c(OUTCOMES_conceptssets, COV_conceptssets, DRUGS_conceptssets, SEVERCOVID_conceptsets, FREE_TEXT_conceptsets,RESULTS_conceptsets,PROC_conceptsets))

for (concept in c(OUTCOMES_conceptssets, COV_conceptssets, SEVERCOVID_conceptsets, FREE_TEXT_conceptsets)) {
  concept_set_domains[[concept]] = "Diagnosis"
}
for (concept in c(DRUGS_conceptssets)) {
  concept_set_domains[[concept]] = "Medicines"
}

for (concept in c(PROC_conceptsets)) {
  concept_set_domains[[concept]] = "Procedures"
}

for (concept in c(RESULTS_conceptsets)) {
  concept_set_domains[[concept]] = "Results"
}

concept_set_codes_our_study_pre <- vector(mode="list")
concept_set_codes_our_study_pre_excl <- vector(mode="list")

list_outcomes_MIS <- c()

# source(paste0(thisdir,"/p_parameters/archive_parameters/parameters_raw.R"))

source(paste0(thisdir,"/p_parameters/archive_parameters/parameters_including_listcodescsv.R"))

mapconcept <- vector(mode="list")


mapconcept[["COVID_narrow"]] <- "COVID19DX_AESI_narrow"
mapconcept[["COVID_possible"]] <- "COVID19DX_AESI_possible"
mapconcept[["ARDS_narrow"]] <- "ARDS_AESI_narrow"
mapconcept[["ARDS_possible"]] <- "ARDS_AESI_possible"
mapconcept[["MYOCARD_narrow"]] <- "MYOPERICARD_AESI_narrow"
mapconcept[["MYOCARD_possible"]] <- "MYOPERICARD_AESI_possible"
mapconcept[["HF_narrow"]] <- "HF_AESI_narrow"
mapconcept[["HF_possible"]] <- "HF_AESI_possible"
mapconcept[["CAD_narrow"]] <- "CAD_AESI_narrow"
mapconcept[["CAD_possible"]] <- "CAD_AESI_possible"
mapconcept[["COVCANCER"]] <- c("ANYMALIGNANCY_COV", "METASTATICSOLIDTUMOR_COV")
mapconcept[["COVCOPD"]] <- "CHRONICPULMONARYDISEASE_CH"
mapconcept[["COVHIV"]] <- c("AIDS_CH_narrow", "AIDS_CH_possible", "HIVNOAIDS_CH_narrow", "HIVNOAIDS_CH_possible")
mapconcept[["COVCKD"]] <- "KDCHRONIC_COV"
mapconcept[["COVDIAB"]] <- "DM12_COV"
mapconcept[["COVOBES"]] <- "OBESITY_COV"
mapconcept[["COVSICKLE"]] <- "SICKLECELL_COV"

mapconceptDP <- vector(mode="list")

mapconceptDP[["DP_COVCANCER"]] <- "DP_COVCANCER_Covariate"
mapconceptDP[["DP_COVDIAB"]]  <- "DP_COVDIAB_Covariate"
mapconceptDP[["DP_CVD"]]  <- "DP_COVCARDIOCEREBROVAS_Covariate"
mapconceptDP[["DP_COVHIV"]]  <- "DP_COVHIV_Covariate"
# mapconcept[["DP_COVCKD"]]  (not available)
mapconceptDP[["DP_COVCOPD"]]  <- "DP_COVRESPCHRONIC_Covariate"
mapconceptDP[["DP_COVOBES"]]  <- "DP_COVOBESITY_Covariate"
mapconceptDP[["DP_COVSICKLE"]]  <- "DP_COVSICKLE_Covariate"
mapconceptDP[["IMMUNOSUPPR"]]  <- "DP_IMMUNOSUPPR_Covariate"
mapconceptDP[["DP_CONTRHYPERT"]]  <- "DP_CONTRHYPERT_Covariate"


list_dia_concepts_to_be_mapped <- names(mapconcept)
list_drug_concepts_to_be_mapped <- names(mapconceptDP)

for (concept in list_dia_concepts_to_be_mapped){
  for (mapcon in mapconcept[[concept]]){
    for (codesystem in names(codelist_diagnosis[[mapcon]]) ){  
      concept_set_codes_our_study_pre[[concept]][[codesystem]] <- append(concept_set_codes_our_study_pre[[concept]][[codesystem]], codelist_diagnosis[[mapcon]][[codesystem]] )
     }
  }
} 

for (concept in list_drug_concepts_to_be_mapped){
  for (mapcon in mapconceptDP[[concept]]){
    for (codesystem in names(codelist_diagnosis[[mapcon]]) ){  
      concept_set_codes_our_study_pre[[concept]][[codesystem]] <- append(concept_set_codes_our_study_pre[[concept]][[codesystem]], codelist_diagnosis[[mapcon]][[codesystem]] )
    }
  }
} 



# Down syndrome <- DOWN
# Mental health disorders <- DEPRESSION, DEMENTIA, SCHIZOPHRENIA
# Chronic liver disease <- LIVERCIRRHOSIS, NONALCOHOLICLIVER, HEPATITISAUTOIMMUNE, ALCOHOLICLIVER
# Cardio/Cerebrovascular disease diagnosis <- STROKEHEMO, STROKEISCH, TIA, ANEURYSMVASCMALF, CAD, HF, CARDIOMYOPATHY
# 
# 
# New DPs
# DP_COVMENTHEALTH

