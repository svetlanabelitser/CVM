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

OUT_codelist <- fread(paste0(thisdir,"/p_parameters/archive_parameters/20220905_V2_ROC20_full_codelist.csv"))
OUT_codelist <- OUT_codelist[, .(coding_system, code, type, tags, event_abbreviation, system)]
OUT_codelist <- OUT_codelist[type != "PrA"]

concept_set_codes_our_study <- df_to_list_of_list(OUT_codelist, codying_system_recode = "auto", type_col = "type")

test <- unique(OUT_codelist[, .(concept_name = paste(system, event_abbreviation, type, tags, sep = "_"))])

File_variables_ALG_DP_ROC20_July22 <- paste0(thisdir,"/p_parameters/archive_parameters/Variables_ALG_DP_ROC20_July22.xlsx")
DRUG_codelist <- as.data.table(readxl::read_excel(File_variables_ALG_DP_ROC20_July22, sheet = "DrugProxies",
                                                  .name_repair = ~ vctrs::vec_as_names(..., repair = "universal", quiet = TRUE)))

test <- unique(DRUG_codelist[, Drug_proxie])

DRUG_codelist <- DRUG_codelist[Drug_proxie != "DP_VACCINES", ][, ATC.codes := strsplit(ATC.codes, ",")]
DRUG_codelist_list <- df_to_list_of_list(DRUG_codelist, code_col = "ATC.codes", concepts_col = "Drug_proxie",
                                    codying_system_col = F, codying_system_recode = "auto")

concept_set_codes_our_study <- c(concept_set_codes_our_study, DRUG_codelist_list)

vaccine__conceptssets <- c("DP_VACCINES")
