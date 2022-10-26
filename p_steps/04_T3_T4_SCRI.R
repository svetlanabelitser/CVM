#----------------------------------------
# temporary: the next few lines must be removed as soon as this step can be linked to the true data pipeline

if (!require("rstudioapi")) install.packages("rstudioapi")
stepdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
stepdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

inputdir <- paste0(stepdir,'/../i_input_synthetic/')

# end of the lines to be removed

# assign to the parameter inputdir the true directory where the dataset D3_study_population_SCRI is stored
# inputdir <-
#----------------------------------------

#----------------------------------------

load(paste0(inputdir,"D3_study_population_SCRI.RData"))