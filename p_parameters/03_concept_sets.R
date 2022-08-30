# the output of this step are several lists and lists of lists to be used by CreateConceptsetDatasets

# concept_set_domains
# level 1 name of the conceptset (including in the name whether it is narrow or possible)
# level 2 domain

# concept_set_codes_our_study,
# level 1 name of the conceptset (including in the name whether it is narrow or possible)
# level 2 coding system
# level 3 list of codes incuded in that conceptset for that coding system

# concept_set_codes_our_study_excl
# level 1 name of the conceptset (including in the name whether it is narrow or possible)
# level 2 coding system
# level 3 list of codes to be excluded from that conceptset for that coding system

# the lists of all conceptsets 
# concept_sets_of_our_study

# input: the VAC4EU spreadsheets, restricted to the conceptsets associated with this study

OUT_codelist <- readxl::read_excel(paste0(thisdir,"/p_parameters/archive_parameters/20220727_V2_ROC20_full_codelist.xlsx"))
OUT_codelist <- as.data.table(OUT_codelist)
OUT_codelist <- OUT_codelist[, .(coding_system, code,
                                 concept_name = paste(system, event_abbreviation, type, sep = "_", tags))]

DRUG_codelist <- readxl::read_excel(paste0(thisdir,"/p_parameters/archive_parameters/Variables_ALG_DP_ROC20_July22.xlsx"),
                                    sheet = "DrugProxies")
DRUG_codelist <- as.data.table(DRUG_codelist)

VAR_codelist <- readxl::read_excel(paste0(thisdir,"/p_parameters/archive_parameters/Variables_ALG_DP_ROC20_July22.xlsx"),
                                   sheet = "Variables")
VAR_codelist <- as.data.table(VAR_codelist)

ALGO_codelist <- readxl::read_excel(paste0(thisdir,"/p_parameters/archive_parameters/Variables_ALG_DP_ROC20_July22.xlsx"),
                                    sheet = "ALG")
ALGO_codelist <- as.data.table(ALGO_codelist)

varnames <- list() # list Varname
# isAESI[var] <- # true or false
# iscov[var] 
# isNEG[var]
# isalgorithm[var] <- # true or false
# isalgorithminput[var] <- # true or false

vaccine__conceptssets <- c("DP_VACCINES")

concept_set_domains<- vector(mode="list")
concept_set_domains[["DP_VACCINES"]] = "Vaccines"

OUTCOME_variables <- VAR_codelist[(AESI), Varname]
CONTROL_variables <- VAR_codelist[(NEG), Varname]
COV_variables <- VAR_codelist[(COV), Varname]

# OUTCOMES_conceptssets <- c("HF_narrow","HF_possible","CAD_narrow","CAD_possible","MYOCARD_narrow","MYOCARD_possible","COVID_narrow","COVID_possible","ARDS_narrow","ARDS_possible") 

VAR_conceptssets <- vector(mode="list")

test <- VAR_codelist[!(Algorithm), ]
a <- paste0(test[!(Algorithm) & !(COV), Varname], "_narrow")
names(a) <- paste0(test[!(Algorithm) & !(COV), Varname])

VAR_conceptssets <- c(VAR_conceptssets, a)

test <- VAR_codelist[!(Algorithm), ]
a <- paste0(test[!(Algorithm) & (COV), Varname], c("_narrow", "_possible"))
names(a) <- paste0(test[!(Algorithm) & (COV), Varname])

VAR_conceptssets <- c(VAR_conceptssets, a)

OUTCOMES_conceptssets <- c()
OUTCOME_algorithm <- vector(mode="list")

test <- VAR_codelist[!(Algorithm), ]
OUTCOME_algorithm <- paste0(test[!(Algorithm), Varname], "_narrow")
names(OUTCOME_algorithm) <- paste0(test[!(Algorithm), Varname])

test <- VAR_codelist[(Algorithm), .(Varname)]
test1 <- merge(test, ALGO_codelist, by.x = "Varname", by.y = "Algorithm", all.x = T)
OUTCOME_algorithm <- paste0(test[!(Algorithm), Varname], "_narrow")
names(OUTCOME_algorithm) <- paste0(test[!(Algorithm), Varname])


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

mapconceptDP[["DP_COVCANCER"]] <- "DP_COVCANCER_codesheet"
mapconceptDP[["DP_COVDIAB"]]  <- "DP_COVDIAB_codesheet"
mapconceptDP[["DP_CVD"]]  <- "DP_COVCARDIOCEREBROVAS_codesheet"
mapconceptDP[["DP_COVHIV"]]  <- "DP_COVHIV_codesheet"
# mapconcept[["DP_COVCKD"]]  (not available)
mapconceptDP[["DP_COVCOPD"]]  <- "DP_COVRESPCHRONIC_codesheet"
mapconceptDP[["DP_COVOBES"]]  <- "DP_COVOBESITY_codesheet"
mapconceptDP[["DP_COVSICKLE"]]  <- "DP_COVSICKLE_codesheet"
mapconceptDP[["IMMUNOSUPPR"]]  <- "DP_IMMUNOSUPPR_codesheet"
mapconceptDP[["DP_CONTRHYPERT"]]  <- "DP_CONTRHYPERT_codesheet"


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
    for (codesystem in names(codelist_drug_proxies[[mapcon]]) ){  
      concept_set_codes_our_study_pre[[concept]][[codesystem]] <- append(concept_set_codes_our_study_pre[[concept]][[codesystem]], codelist_drug_proxies[[mapcon]][[codesystem]] )
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

