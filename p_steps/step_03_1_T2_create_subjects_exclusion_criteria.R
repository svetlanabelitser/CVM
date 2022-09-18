# -----------------------------------------------------
# CREATE EXCLUSION CRITERIA for persons/spells

# input: D3_PERSONS, OBSERVATION_PERIODS, output_spells_category
# output: D3_selection_criteria

print('CREATE EXCLUSION CRITERIA')

load(paste0(dirtemp,"D3_PERSONS.RData"))
D3_PERSONS <- D3_PERSONS[, .(person_id, sex_at_instance_creation, birth_date, death_date)]

#CHANGE COLUMN NAMES
setnames(D3_PERSONS, c("birth_date", "death_date"), c("date_of_birth", "date_of_death"))

#CONVERT SEX to BINARY 0/1
D3_PERSONS[, sex := fifelse(sex_at_instance_creation == "M", 1, 0)] #1:M 0:F

D3_PERSONS[, sex_or_date_of_birth_is_not_defined := fifelse(is.na(sex) | is.na(date_of_birth), 1, 0)]
D3_PERSONS[, birth_date_absurd := fifelse(year(date_of_birth) < 1900 | year(date_of_birth) > year(study_end), 1, 0)]

# find if a person has died before the start of the study
D3_PERSONS[, death_before_study_entry := fifelse(!is.na(date_of_death) & date_of_death < study_start, 1, 0)]


# OBSERVATION PERIODS -----------------------------------------------------
#new
for (subpop in subpopulations_non_empty){
  print(subpop)
  
  load(paste0(dirtemp,"D3_clean_spells.RData"))
  if (this_datasource_has_subpopulations == T){
    D3_clean_spells <- D3_clean_spells[[subpop]]
  }
  D3_clean_spells <- D3_clean_spells[, .(person_id, entry_spell_category, exit_spell_category, starts_after_ending, starts_at_birth)]
  
  # find if all spells of a person have start date before end date and remove spells that start after they end
  D3_spells_ex <- copy(D3_clean_spells)[, Min_ex := min(starts_after_ending), by = "person_id"]
  D3_spells_ex <- D3_spells_ex[Min_ex == starts_after_ending, ][, Min_ex := NULL]
  setnames(D3_spells_ex, "starts_after_ending", "all_spells_start_after_ending")
  
  # find if all spells of a person do not overlap the study period and remove spells outside of study period
  D3_spells_ex[, no_spell_overlapping_the_study_period := fifelse(exit_spell_category < study_start, 1, 0, na = 1)]
  D3_spells_ex[, Min_ex := min(no_spell_overlapping_the_study_period), by = "person_id"]
  D3_spells_ex <- D3_spells_ex[Min_ex == no_spell_overlapping_the_study_period, ][, Min_ex := NULL]
  
  # find if all spells of a person are shorter than 365 days
  D3_spells_ex[, distance := correct_difftime(exit_spell_category, entry_spell_category)]
  D3_spells_ex[, no_spell_longer_than_365_days := fifelse(distance < 365 & starts_at_birth == 0, 1, 0, na = 1)]
  D3_spells_ex[, no_spell_longer_than_365_days := min(no_spell_longer_than_365_days), by = "person_id"]

  D3_spells_ex <- D3_spells_ex[, .(person_id, all_spells_start_after_ending, no_spell_overlapping_the_study_period,
                                   no_spell_longer_than_365_days)]
  D3_spells_ex <- unique(D3_spells_ex)
  
  PERSONS_spells <- merge(D3_PERSONS, D3_clean_spells, all.x = T, by = "person_id")
  
  # find if a person has no spell and remove spells without entry_spell_category
  PERSONS_spells[, no_observation_period := fifelse(is.na(entry_spell_category), 1, 0)]
  PERSONS_spells[, Minop_start_date := min(no_observation_period), by = "person_id"]
  PERSONS_spells <- PERSONS_spells[Minop_start_date == no_observation_period, ][, Minop_start_date:= NULL]
  
  load(paste0(dirtemp, "selected_doses.RData"))
  selected_doses <- selected_doses[, .(vx_dose, max_dose = max(vx_dose), count_doses = .N), by = person_id]
  selected_doses[, there_is_no_first_dose_but_there_are_higher_doses := fifelse(max_dose != count_doses, 1, 0)]
  selected_doses <- selected_doses[, .(vx_dose, there_is_no_first_dose_but_there_are_higher_doses)]
  
  PERSONS_spells_doses <- merge(PERSONS_spells, selected_doses, all.x = T, by = "person_id")
  PERSONS_spells_doses <- 
  
  
  
  
  D3_exclusion_no_observation_period <- unique(PERSONS_in_OP[,.(person_id, sex_or_date_of_birth_is_not_defined, birth_date_absurd, no_observation_period)])
  
  ## KEEP ONLY NEEDED VARs
  D3_inclusion_from_PERSONS <- D3_PERSONS[,.(person_id,sex,date_of_birth,date_of_death)]
  
  
  
  
  
  
  
  
  na_date = lubridate::ymd(99991231)
  
  output_spells_category_enriched <- merge(D3_inclusion_from_PERSONS, output_spells_category, all.x = T, by="person_id")
  output_spells_category_enriched <- output_spells_category_enriched[entry_spell_category < date_of_birth + 60, entry_spell_category := date_of_birth]
  output_spells_category_enriched <- output_spells_category_enriched[, death_before_study_entry := fifelse(!is.na(date_of_death) & date_of_death < study_start, 1, 0)]
  output_spells_category_enriched <- output_spells_category_enriched[!is.na(entry_spell_category) & !is.na(exit_spell_category), no_observation_period_including_study_start := fifelse(study_start %between% list(entry_spell_category,exit_spell_category) & entry_spell_category < exit_spell_category, 0, 1)]
  output_spells_category_enriched <- output_spells_category_enriched[is.na(entry_spell_category) | is.na(exit_spell_category), no_observation_period_including_study_start := 1]
  output_spells_category_enriched <- output_spells_category_enriched[, insufficient_run_in := fifelse(entry_spell_category >= date_of_birth & entry_spell_category <= study_start - 365, 0, 1)]
  
  output_spells_category_enriched <- output_spells_category_enriched[, insufficient_run_in := min(fifelse(entry_spell_category == date_of_birth & year(date_of_birth) == 2018, 0 , insufficient_run_in)), by="person_id"]
  output_spells_category_enriched <- output_spells_category_enriched[, study_entry_date := study_start][, start_follow_up := start_lookback][, study_exit_date:= fifelse(no_observation_period_including_study_start == 0, pmin(date_of_death, exit_spell_category, study_end, na.rm = T), na_date)]
  
  output_spells_category_enriched <- output_spells_category_enriched[, no_observation_period_including_study_start := min(no_observation_period_including_study_start, na.rm = T), by="person_id"][, study_exit_date := min(study_exit_date, na.rm = T), by="person_id"]
  
  D3_exclusion_observation_periods_not_overlapping <- unique(output_spells_category_enriched[,.(person_id, study_entry_date, start_follow_up, study_exit_date, death_before_study_entry, insufficient_run_in, no_observation_period_including_study_start, age_over18)])
  
  PERSONS_OP <- merge(D3_inclusion_from_PERSONS,
                      D3_exclusion_no_observation_period,
                      by="person_id",
                      all.x = T)
  PERSONS_OP2 <- merge(PERSONS_OP,
                       D3_exclusion_observation_periods_not_overlapping,
                       by="person_id",
                       all.x = T)
  
  coords<-c("sex_or_date_of_birth_is_not_defined", "birth_date_absurd", "no_observation_period", "insufficient_run_in","no_observation_period_including_study_start", "death_before_study_entry", "age_over18")
  selection_criteria <- PERSONS_OP2[, (coords) := replace(.SD, is.na(.SD), 0), .SDcols = coords]
  selection_criteria <- selection_criteria[!is.na(study_entry_date), ]
  
  tempname<-paste0("D3_selection_criteria",suffix[[subpop]])
  assign(tempname,selection_criteria)
  save(list=paste0("D3_selection_criteria",suffix[[subpop]]),file=paste0(dirtemp,"D3_selection_criteria",suffix[[subpop]],".RData"))
  rm(list=paste0("D3_selection_criteria",suffix[[subpop]]))
  
}

rm(output_spells_category_enriched,D3_inclusion_from_PERSONS,D3_exclusion_observation_periods_not_overlapping)
rm(PERSONS_OP, PERSONS_OP2, na_date, coords)
rm(D3_PERSONS, PERSONS_in_OP, output_spells_category,OBSERVATION_PERIODS, D3_exclusion_no_observation_period,selection_criteria)

