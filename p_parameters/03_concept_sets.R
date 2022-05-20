vaccine__conceptssets <- c("Covid_vaccine")

concept_set_domains<- vector(mode="list")
concept_set_domains[["Covid_vaccine"]] = "VaccineATC"

OUTCOME_events <- list()
# OUTCOME_events <- c("GBS","ADEM","NARCOLEPSY","ACUASEARTHRITIS","DM1","MICROANGIO","HF","STRCARD","CAD","ARR","MYOCARD","PERICARD","SOCV","ALI","AKI","GENCONV","MENINGOENC","ARD","ERYTH","CHILBLAIN","ANOSMIA","ANAPHYL","KD","MISCC","MIS","SUDDENDEAT","TRANSMYELITIS","DIC","Hemostroke","Ischstroke","Sinusthrom","VTE","TP","TMA","COVID","Myocardalone","BP")

OUTCOME_events <- c("HF","CAD","MYOCARD","COVID","ARD")

CONTROL_events <-list()
CONTROL_events <-c("CONTRDIVERTIC","CONTRHYPERT")
CONTROL_events <-c()

# OUTCOMES_conceptssets <- c("GBS_narrow","GBS_possible","ADEM_narrow","ADEM_possible","NARCOLEPSY_narrow","NARCOLEPSY_possible","ACUASEARTHRITIS_narrow","ACUASEARTHRITIS_possible","DM1_narrow","DM1_possible","MICROANGIO_narrow","MICROANGIO_possible","HF_narrow","HF_possible","STRCARD_narrow","STRCARD_possible","CAD_narrow","CAD_possible","ARR_narrow","ARR_possible","MYOCARD_narrow","MYOCARD_possible","SOCV_narrow","SOCV_possible","ALI_narrow","ALI_possible","AKI_narrow","AKI_possible","GENCONV_narrow","GENCONV_possible","MENINGOENC_narrow","MENINGOENC_possible","ARD_narrow","ARD_possible","ERYTH_narrow","ERYTH_possible","CHILBLAIN_narrow","CHILBLAIN_possible","ANOSMIA_narrow","ANOSMIA_possible","ANAPHYL_narrow","ANAPHYL_possible","KD_narrow","KD_possible","MISCC_narrow","MISCC_possible","MIS_narrow","MIS_possible","SUDDENDEAT_narrow","SUDDENDEAT_possible","TRANSMYELITIS_narrow","TRANSMYELITIS_possible","DIC_narrow","DIC_possible","Hemostroke_narrow","Hemostroke_possible","Ischstroke_narrow","Ischstroke_possible","Sinusthrom_narrow","Sinusthrom_possible","VTE_narrow","VTE_possible","TP_narrow","TP_possible","TMA_narrow","TMA_possible","COVID_narrow","COVID_possible","Myocardalone_narrow","Myocardalone_possible","BP_narrow","BP_possible","PERICARD_narrow","PERICARD_possible")
OUTCOMES_conceptssets <- c("HF_narrow","HF_possible","CAD_narrow","CAD_possible","MYOCARD_narrow","MYOCARD_possible","COVID_narrow","COVID_possible","ARD_narrow","ARD_possible")

COV_conceptssets <- c("COVCANCER","COVCOPD","COVHIV","COVCKD","COVDIAB","COVOBES","COVSICKLE","CONTRDIVERTIC","CONTRHYPERT")

DRUGS_conceptssets <- c("DP_COVCANCER","DP_COVDIAB","DP_CVD","DP_COVHIV","DP_COVCKD","DP_COVCOPD","DP_COVOBES","DP_COVSICKLE","IMMUNOSUPPR","DP_CONTRHYPERT")

SEVERCOVID_conceptsets <- c("COVIDSYMPTOM","MechanicalVent")
SEVERCOVID_conceptsets <- c("MechanicalVent","ITA_HOSP_ARDS_COVID")

PROC_conceptsets <- c("ICU_VENTILATION")

RESULTS_conceptsets <- c("COVID_test_coded")


FREE_TEXT_conceptsets <- c("MYOCARD_narrow_free_text", "MYOCARD_possible_free_text")
FREE_TEXT_conceptsets <- c()

<<<<<<< HEAD
concept_sets_of_our_study <- c(OUTCOMES_conceptssets, COV_conceptssets, DRUGS_conceptssets, SEVERCOVID_conceptsets, FREE_TEXT_conceptsets,RESULTS_conceptsets)
=======
concept_sets_of_our_study <- c(OUTCOMES_conceptssets, COV_conceptssets, DRUGS_conceptssets,
                               SEVERCOVID_conceptsets, FREE_TEXT_conceptsets)
>>>>>>> 6c7f08e448bd85bed625ecb3e9536352991ee935

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

concept_set_codes_our_study_pre[["Covid_vaccine"]][["ATC"]] <- c("J07BX03")

list_outcomes_MIS <- c()

source(paste0(thisdir,"/p_parameters/archive_parameters/parameters_raw.R"))


