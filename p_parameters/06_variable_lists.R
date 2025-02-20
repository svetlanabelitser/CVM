# create few lists, each containing variables of interest for different D3s
VAR_codelist <- readxl::read_excel(File_variables_ALG_DP_ROC20, sheet = "Variables")
VAR_codelist <- as.data.table(VAR_codelist)
rm(File_variables_ALG_DP_ROC20)

# TODO remove before release?
VAR_codelist <- VAR_codelist[Varname == "D_Diverticulitis_AESI", Varname := "D_DIVERTICULITIS_AESI"]

# list of variable names of OUTCOMES
OUTCOME_variables <- c(VAR_codelist[(AESI), Varname], DRUG_codelist[(AESI), Drug_proxie])

# list of variable names of CONTROLS
CONTROL_variables <- VAR_codelist[(NEG), Varname]

# list of variable names of COVARIATES
COV_variables_raw <- c(VAR_codelist[(COV), Varname], DRUG_codelist[(COV), Drug_proxie])

# Variable for COVID_VACCINES
VACCINES_variable <- "COVID_VACCINES"

# TODO check recurrent events
recurrent_OUTCOME_variables <- c("Im_ANAPHYLAXIS_AESI")

# Creating DP_variables from COV manually
# list of variable names of DRUG_PROXIES
DP_variables <- COV_variables_raw[grepl("^DP_", COV_variables_raw)]

# list of variable names of COVARIATES without DRUG_PROXIES
COV_variables <- setdiff(COV_variables_raw, DP_variables) 

# TODO test if needed
# variables_of_our_study <- c(VAR_codelist[, Varname], DRUG_codelist[, Drug_proxie])
# auxiliary_variables <- c(VAR_codelist[(Algorithm_input) & !((COV) | (NEG) | (AESI)), Varname],
#                          DRUG_codelist[(Algorithm_input) & !((COV) | (AESI)), Drug_proxie])
