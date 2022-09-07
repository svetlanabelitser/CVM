# create few lists, each containing variables of interest for different D3s

VAR_codelist <- readxl::read_excel(File_variables_ALG_DP_ROC20_July22, sheet = "Variables")
VAR_codelist <- as.data.table(VAR_codelist)

concept_set_domains<- vector(mode="list")
concept_set_domains[["DP_VACCINES"]] = "Vaccines"

NoAlgo <- VAR_codelist[!(Algorithm), ]
OUTCOME_variables <- setNames(paste0(NoAlgo[(AESI), Varname], "_narrow"), NoAlgo[(AESI), Varname])
CONTROL_variables <- setNames(paste0(NoAlgo[(NEG), Varname], "_narrow"), NoAlgo[(NEG), Varname])
COV_variables <- setNames(paste0(rep(NoAlgo[(COV), Varname], 2), c("_narrow", "_possible")),
                          rep(NoAlgo[(COV), Varname], 2))

for (concept in c(OUTCOME_variables, CONTROL_variables, COV_variables)) {
  concept_set_domains[[concept]] = "Diagnosis"
}

NoAlgo <- DRUG_codelist[!(Algorithm), ]
OUTCOME_variables_DRUG <- NoAlgo[(AESI), Drug_proxie]
OUTCOME_variables_DRUG <- setNames(OUTCOME_variables_DRUG, OUTCOME_variables_DRUG)
OUTCOME_variables <- c(OUTCOME_variables, OUTCOME_variables_DRUG)
COV_variables_DRUG <- NoAlgo[(COV), Drug_proxie]
COV_variables_DRUG <- setNames(COV_variables_DRUG, COV_variables_DRUG)
COV_variables <- c(COV_variables, COV_variables_DRUG)

for (concept in c(OUTCOME_variables_DRUG, COV_variables_DRUG[COV_variables_DRUG != "DP_VACCINES"])) {
  concept_set_domains[[concept]] = "Medicines"
}

concept_sets_of_our_study <- c(OUTCOME_variables, CONTROL_variables, COV_variables)
