# create few lists, each containing variables of interest for different D3s

VAR_codelist <- readxl::read_excel(File_variables_ALG_DP_ROC20_July22, sheet = "Variables")
VAR_codelist <- as.data.table(VAR_codelist)

concept_set_domains<- vector(mode="list")
concept_set_domains[["DP_VACCINES"]] = "Vaccines"

NoAlgo <- VAR_codelist[!(Algorithm), ]
OUTCOME_variables <- sapply(NoAlgo[(AESI), Varname], paste0, "_narrow")
CONTROL_variables <- sapply(NoAlgo[(NEG), Varname], paste0, "_narrow")
COV_variables <- lapply(Map(identity, NoAlgo[(COV), Varname]), paste0, c("_narrow", "_possible"))

for (concept in unlist(c(OUTCOME_variables, CONTROL_variables, COV_variables), use.names = F)) {
  concept_set_domains[[concept]] = "Diagnosis"
}

NoAlgo <- DRUG_codelist[!(Algorithm), ]
OUTCOME_variables_DRUG <- sapply(NoAlgo[(AESI), Drug_proxie], identity)
OUTCOME_variables <- c(OUTCOME_variables, OUTCOME_variables_DRUG)
COV_variables_DRUG <- sapply(NoAlgo[(COV), Drug_proxie], identity)
COV_variables <- c(COV_variables, COV_variables_DRUG)

for (concept in c(OUTCOME_variables_DRUG, COV_variables_DRUG[COV_variables_DRUG != "DP_VACCINES"])) {
  concept_set_domains[[concept]] = "Medicines"
}

concept_sets_of_our_study <- c(OUTCOME_variables, CONTROL_variables, COV_variables)
variables_of_our_study <- concept_sets_of_our_study
