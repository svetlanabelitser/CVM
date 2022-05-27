#--------------------------
# COVID_vaccine

concept_set_codes_our_study_pre[["Covid_vaccine"]][["ATC"]] <- c("J07BX03")


#--------------------------
# procedure for mechanical ventilation

concept_set_codes_our_study_pre[["ICU_VENTILATION"]][["ICD9PROC"]] <- c("96.70","96.71","96.72")


#--------------------------
# results from covid test recorded with a code

concept_set_codes_our_study_pre[["COVID_test_coded"]][["Veneto_lab_coding_system"]] <- c("91.12.1_0")


#--------------------------
# load conceptsets from csv files

csvdiagnosis <- "20220525_ROC20_full_codelist"
csvdrugproxies <- "20220525_ROC20_drug_proxies_codelist"


codelist_diagnosis <- fread(paste0(thisdir,"/p_parameters/archive_parameters/",csvdiagnosis,".csv"))

codelist_diagnosis <- df_to_list_of_list(codelist_diagnosis, codying_system_recode = "auto", imputed_tags = F, type_col = "type")

codelist_drug_proxies <- fread(paste0(thisdir,"/p_parameters/archive_parameters/", csvdrugproxies,".csv"))
codelist_drug_proxies <- df_to_list_of_list(codelist_drug_proxies, code_col = "atc_codes", concepts_col = "drug_proxy",
                                    codying_system_col = F, codying_system_recode = "auto", type_col = "type_drug_proxy")
# save(codelist_test, file = paste(thisdir, "20220525_ROC20_drug_proxies_codelist.RData", sep = "\\"))


