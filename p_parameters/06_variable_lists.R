# create few lists, each containing variables of interest for different D3s

VAR_codelist <- readxl::read_excel(File_variables_ALG_DP_ROC20_July22, sheet = "Variables")
VAR_codelist <- as.data.table(VAR_codelist)

# TODO remove before release?
VAR_codelist <- VAR_codelist[Varname == "D_Diverticulitis_AESI", Varname := "D_DIVERTICULITIS_AESI"]

OUTCOME_variables <- c(VAR_codelist[(AESI), Varname], DRUG_codelist[(AESI), Drug_proxie])

# TODO remove before release?
OUTCOME_variables <- OUTCOME_variables[OUTCOME_variables %not in% c("B_COAGDIS_AESI")]

CONTROL_variables <- VAR_codelist[(NEG), Varname]

# TODO remove before release?
CONTROL_variables <- CONTROL_variables[CONTROL_variables %not in% c("SO_CONJUNCTIVITIS_COV")]

COV_variables <- c(VAR_codelist[(COV), Varname], DRUG_codelist[(COV), Drug_proxie])
VACCINES_variable <- "COVID_VACCINES"

variables_of_our_study <- c(VAR_codelist[, Varname], DRUG_codelist[, Drug_proxie])
auxiliary_variables <- c(VAR_codelist[(Algorithm_input) & !((COV) | (NEG) | (AESI)), Varname],
                         DRUG_codelist[(Algorithm_input) & !((COV) | (AESI)), Drug_proxie])
