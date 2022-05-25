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

concept_sets_of_our_study <- c(OUTCOMES_conceptssets, COV_conceptssets, DRUGS_conceptssets, SEVERCOVID_conceptsets, FREE_TEXT_conceptsets,RESULTS_conceptsets)

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

source(paste0(thisdir,"/p_parameters/archive_parameters/parameters_raw.R"))


