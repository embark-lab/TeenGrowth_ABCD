# =============================================================================
# ABCD 6.0 - Eating Disorder Data Processing
# Updated from 5.0 script to accommodate 6.0 data structure changes
# 
# Key differences from 5.0:
#   - TSV files instead of CSV
#   - participant_id (was src_subject_id), session_id (was eventname)
#   - session_id format: ses-00A, ses-01A, ... ses-06A (was baseline_year_1_arm_1, etc.)
#   - New timepoints: ses-05A (year 5) and ses-06A (year 6)
#   - No separate _ss (symptom scores) file — symptoms are in the _ed file
#   - No raw items — only derived symptom/diagnosis variables
#   - Weight control subtypes collapsed: vomit vs. other (no individual breakdown)
#   - No binge_frequency continuous variable (replaced by binary wkly3mo)
#   - No age of onset (edsymptoms_aoo) variables
# =============================================================================

library(dplyr)
library(tidyr)
library(TeenGrowth)
library(zscorer)

dir.create("data/6.0", recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# 0. LOAD AND CLEAN ANTHROPOMETRIC DATA
# =============================================================================

ant <- read.delim('ABCD_6.0/rawdata/phenotype/ph_y_anthr.tsv', header = TRUE)

# Load demographic data
demos <- read.delim('ABCD_6.0/rawdata/phenotype/ab_g_stc.tsv', header = TRUE)

sex <- demos |> 
  select(participant_id, ab_g_stc__cohort_sex) |> 
  filter(!is.na(ab_g_stc__cohort_sex))

# Add sex to ant data
ant <- full_join(ant, sex, by = c("participant_id"))

# Derive needed variables for z-scoring
ant <- ant |> 
  mutate(BMI = (703 * as.numeric(ph_y_anthr__weight_mean)) / as.numeric(ph_y_anthr__height_mean)^2)

ant <- ant |> 
  mutate(age_days = as.numeric(ph_y_anthr_age) * 365.25) |> 
  mutate(ht = as.numeric(ph_y_anthr__height_mean) * 2.54) |> 
  mutate(wt = as.numeric(ph_y_anthr__weight_mean) * 0.453592) |> 
  mutate(sex = case_when(
    ab_g_stc__cohort_sex == 1 ~ 1, 
    ab_g_stc__cohort_sex == 2 ~ 2,
    TRUE ~ NA_real_))

ant <- TeenGrowth::add_bmiz(data = ant, 
                            wt = 'wt', 
                            ht = 'ht', 
                            age = 'age_days', 
                            sex = 'sex')

save(ant, file = 'data/6.0/ant.RData')

# =============================================================================
# 1. LOAD ED DATA
# =============================================================================

# In 6.0, everything is in one file per reporter (no separate _ss file)
ed_youth <- read.delim('ABCD_6.0/rawdata/phenotype/mh_y_ksads__ed.tsv', header = TRUE)
ed_parents <- read.delim('ABCD_6.0/rawdata/phenotype/mh_p_ksads__ed.tsv', header = TRUE)

# =============================================================================
# 2. TIMEPOINT MAPPING
# =============================================================================

timepoint_map <- c(
  "ses-00A" = 0, "ses-01A" = 1, "ses-02A" = 2,
  "ses-03A" = 3, "ses-04A" = 4, "ses-05A" = 5, "ses-06A" = 6
)

# =============================================================================
# 3. DEFINE VARIABLE MAPPINGS
# =============================================================================

# --- Symptom variables (equivalent to the old _ss file ksads_13_XX variables) ---

# Youth symptom variable names in 6.0
ed_s_vars_y <- c(
  "mh_y_ksads__ed__fear__obese__pres_sx",       # fear of obese - current (was ksads_13_66_t)
  "mh_y_ksads__ed__fear__obese__past_sx",       # fear of obese - past (was ksads_13_67_t)
  "mh_y_ksads__ed__emac__pres_sx",              # emaciation - current (was ksads_13_68_t)
  "mh_y_ksads__ed__emac__past_sx",              # emaciation - past (was ksads_13_69_t)
  "mh_y_ksads__ed__wghtcntrl__vom__pres_sx",    # vomiting - current (was ksads_13_70_t)
  "mh_y_ksads__ed__wghtcntrl__vom__past_sx",    # vomiting - past (was ksads_13_71_t)
  "mh_y_ksads__ed__wghtcntrl__oth__pres_sx",    # lax/diet/exercise - current (was ksads_13_72_t)
  "mh_y_ksads__ed__wghtcntrl__oth__past_sx",    # lax/diet/exercise - past (was ksads_13_73_t)
  "mh_y_ksads__ed__binge__pres_sx",             # binge eating - current (was ksads_13_74_t)
  "mh_y_ksads__ed__binge__past_sx"              # binge eating - past (was ksads_13_75_t)
)

# Parent symptom variable names in 6.0
ed_s_vars_p <- c(
  "mh_p_ksads__ed__fear__obese__pres_sx",
  "mh_p_ksads__ed__fear__obese__past_sx",
  "mh_p_ksads__ed__emac__pres_sx",
  "mh_p_ksads__ed__emac__past_sx",
  "mh_p_ksads__ed__wghtcntrl__vom__pres_sx",
  "mh_p_ksads__ed__wghtcntrl__vom__past_sx",
  "mh_p_ksads__ed__wghtcntrl__oth__pres_sx",
  "mh_p_ksads__ed__wghtcntrl__oth__past_sx",
  "mh_p_ksads__ed__binge__pres_sx",
  "mh_p_ksads__ed__binge__past_sx"
)

# Standardized short names (for downstream compatibility with 5.0 analysis code)
symptom_short_names <- c(
  "fowg_current_scored", "fowg_past_scored",
  "lowwt_current_scored", "lowwt_past_scored",
  "wtcontrol_vomit_current_scored", "wtcontrol_vomit_past_scored",
  "wtcontrol_other_current_scored", "wtcontrol_other_past_scored",
  "binge_current_scored", "binge_past_scored"
)

# =============================================================================
# 4. EXTRACT & CLEAN SYMPTOM SCORES
# =============================================================================

# Youth symptoms
ed_s_youth <- ed_youth %>%
  select(participant_id, session_id, all_of(ed_s_vars_y))

colnames(ed_s_youth) <- c("participant_id", "session_id", symptom_short_names)

ed_s_youth <- ed_s_youth %>%
  filter(rowSums(is.na(pick(3:12))) < 10)

ed_s_youth$reporter <- "youth"

# Parent symptoms
ed_s_parents <- ed_parents %>%
  select(participant_id, session_id, all_of(ed_s_vars_p))

colnames(ed_s_parents) <- c("participant_id", "session_id", symptom_short_names)

ed_s_parents <- ed_s_parents %>%
  filter(rowSums(is.na(pick(3:12))) < 10)

ed_s_parents$reporter <- "parent"

# Combine
ed_symptoms_scored <- rbind(ed_s_youth, ed_s_parents)
rm(ed_s_youth, ed_s_parents)

# =============================================================================
# 5. EXTRACT & RENAME FULL ED VARIABLES (SYMPTOMS + DIAGNOSES)
# =============================================================================

# --- Youth rename mapping: new_name = old_name ---
youth_rename <- c(
  "participant_id" = "participant_id",
  "session_id" = "session_id",
  # Binge symptoms
  "binge_characteristics_current" = "mh_y_ksads__ed__binge__charac__pres_sx",
  "binge_characteristics_past"    = "mh_y_ksads__ed__binge__charac__past_sx",
  "binge_distress_current"        = "mh_y_ksads__ed__binge__distrs__pres_sx",
  "binge_distress_past"           = "mh_y_ksads__ed__binge__distrs__past_sx",
  "binge_current"                 = "mh_y_ksads__ed__binge__pres_sx",
  "binge_past"                    = "mh_y_ksads__ed__binge__past_sx",
  "binge_wkly3mo_current"         = "mh_y_ksads__ed__binge__wkly3mo__pres_sx",
  "binge_wkly3mo_past"            = "mh_y_ksads__ed__binge__wkly3mo__past_sx",
  # Compensatory behaviors
  "compbehav_current"             = "mh_y_ksads__ed__compbehav__pres_sx",
  "compbehav_past"                = "mh_y_ksads__ed__compbehav__past_sx",
  "compbehav_wkly3mo_current"     = "mh_y_ksads__ed__compbehav__wkly3mo__pres_sx",
  "compbehav_wkly3mo_past"        = "mh_y_ksads__ed__compbehav__wkly3mo__past_sx",
  # Core AN symptoms
  "emaciation_current"            = "mh_y_ksads__ed__emac__pres_sx",
  "emaciation_past"               = "mh_y_ksads__ed__emac__past_sx",
  "fowg_current"                  = "mh_y_ksads__ed__fear__obese__pres_sx",
  "fowg_past"                     = "mh_y_ksads__ed__fear__obese__past_sx",
  # Self-worth
  "selfworth_wt_current"          = "mh_y_ksads__ed__slfwrth__pres_sx",
  "selfworth_wt_past"             = "mh_y_ksads__ed__slfwrth__past_sx",
  # Weight control
  "wtcontrol_other_current"       = "mh_y_ksads__ed__wghtcntrl__oth__pres_sx",
  "wtcontrol_other_past"          = "mh_y_ksads__ed__wghtcntrl__oth__past_sx",
  "wtcontrol_vomit_current"       = "mh_y_ksads__ed__wghtcntrl__vom__pres_sx",
  "wtcontrol_vomit_past"          = "mh_y_ksads__ed__wghtcntrl__vom__past_sx",
  # AN diagnoses
  "dx_an_restrict_current"        = "mh_y_ksads__ed__an__restrict__pres_dx",
  "dx_an_restrict_past"           = "mh_y_ksads__ed__an__restrict__past_dx",
  "dx_an_restrict_partrem"        = "mh_y_ksads__ed__an__restrict__partrem_dx",
  "dx_an_bingepurge_current"      = "mh_y_ksads__ed__an__bingeprg__pres_dx",
  "dx_an_bingepurge_past"         = "mh_y_ksads__ed__an__bingeprg__past_dx",
  "dx_an_bingepurge_partrem"      = "mh_y_ksads__ed__an__bingeprg__partrem_dx",
  "dx_atypical_an_current"        = "mh_y_ksads__ed__an__oth__pres_dx",
  "dx_atypical_an_past"           = "mh_y_ksads__ed__an__oth__past_dx",
  # BN diagnoses
  "dx_bn_current"                 = "mh_y_ksads__ed__bulim__pres_dx",
  "dx_bn_past"                    = "mh_y_ksads__ed__bulim__past_dx",
  "dx_bn_partrem"                 = "mh_y_ksads__ed__bulim__partrem_dx",
  "dx_bn_lowfreq_current"         = "mh_y_ksads__ed__bulim__oth__mindur__pres_dx",
  "dx_bn_lowfreq_past"            = "mh_y_ksads__ed__bulim__oth__mindur__past_dx",
  # BED diagnoses
  "dx_bed_current"                = "mh_y_ksads__ed__binge__pres_dx",
  "dx_bed_past"                   = "mh_y_ksads__ed__binge__past_dx",
  "dx_bed_partrem"                = "mh_y_ksads__ed__binge__partrem_dx",
  "dx_bed_lowfreq_current"        = "mh_y_ksads__ed__binge__oth__mindur__pres_dx",
  "dx_bed_lowfreq_past"           = "mh_y_ksads__ed__binge__oth__mindur__past_dx"
)

# --- Parent rename mapping (swap mh_y_ for mh_p_ in old names i.e. values) ---
parent_rename <- youth_rename
parent_rename[] <- gsub("mh_y_ksads", "mh_p_ksads", parent_rename)

# Apply renames: select old names (values), then rename to new names
ed_youth_renamed <- ed_youth %>%
  select(all_of(unname(youth_rename))) %>%
  rename(!!!youth_rename)

ed_parents_renamed <- ed_parents %>%
  select(all_of(unname(parent_rename))) %>%
  rename(!!!parent_rename)

# Add reporter
ed_youth_renamed$reporter <- "youth"
ed_parents_renamed$reporter <- "parent"

# Combine
ed_all <- rbind(ed_youth_renamed, ed_parents_renamed)
rm(ed_youth_renamed, ed_parents_renamed, ed_youth, ed_parents)

# =============================================================================
# 6. MERGE SYMPTOMS WITH DIAGNOSES
# =============================================================================

ed_all <- full_join(ed_all, ed_symptoms_scored,
                    by = c("participant_id", "session_id", "reporter"))
rm(ed_symptoms_scored)

# NOTE: Age of onset (edsymptoms_aoo) variables are NOT available in 6.0.

# Save
save(ed_all, file = "data/6.0/ed_all.RData")

# =============================================================================
# 7. BINGE EATING VARIABLES
# =============================================================================

rm(list = ls())
load("data/6.0/ed_all.RData")

# Recode special values to NA across all numeric columns
# 555 = not assessed, 888 = don't know
ed_all <- ed_all %>%
  mutate(across(where(is.numeric), ~ ifelse(.x %in% c(555, 888), NA, .x)))

# Reload timepoint map
timepoint_map <- c(
  "ses-00A" = 0, "ses-01A" = 1, "ses-02A" = 2,
  "ses-03A" = 3, "ses-04A" = 4, "ses-05A" = 5, "ses-06A" = 6
)

binge <- ed_all %>%
  select(participant_id, session_id, reporter, starts_with("binge"))

# Lifetime: if current == 1, then lifetime = 1; otherwise use past
binge <- binge %>%
  mutate(binge_lifetime = ifelse(binge_current_scored == 1, 1, binge_past_scored))

# Key variables
# NOTE: binge_frequency is NOT available in 6.0.
# Closest equivalent is binge_wkly3mo_current (binary: meets weekly/3mo threshold)
binge_keyvars <- binge %>%
  select(participant_id, session_id, reporter,
         binge_current_scored, binge_past_scored, binge_lifetime,
         binge_wkly3mo_current, binge_wkly3mo_past,
         binge_characteristics_current, binge_distress_current)

# Rename for consistency
binge_keyvars <- binge_keyvars %>%
  rename(binge_current = binge_current_scored)

# Timepoint
binge_keyvars$timepoint_num <- timepoint_map[binge_keyvars$session_id]

# Rolling binge variable
binge_keyvars <- binge_keyvars %>%
  arrange(participant_id, timepoint_num) %>%
  group_by(participant_id, reporter) %>%
  mutate(binge_rolling = if_else(cumsum(ifelse(is.na(binge_lifetime), 0, binge_lifetime)) > 0, 1, 0)) %>%
  ungroup()

# Check
table(binge_keyvars$reporter, binge_keyvars$timepoint_num, binge_keyvars$binge_rolling)

# Save
save(binge_keyvars, file = "data/6.0/binge_keyvars.RData")

# =============================================================================
# 8. WEIGHT CONTROL VARIABLES
# =============================================================================

# In 6.0, weight control is only available as:
#   - vomit (present/past)
#   - other (laxatives, exercise, dieting, pills — combined) (present/past)
# Individual subtypes (restrict, exercise, pills, laxative) are NOT available.

wt_control <- ed_all %>%
  select(participant_id, session_id, reporter, starts_with("wtcontrol"))

# Lifetime variables (using the _scored versions from symptom merge)
wt_control <- wt_control %>%
  mutate(
    vomit_lifetime = ifelse(wtcontrol_vomit_current_scored == 1, 1, wtcontrol_vomit_past_scored),
    other_lifetime = ifelse(wtcontrol_other_current_scored == 1, 1, wtcontrol_other_past_scored)
  )

# Timepoint
wt_control$timepoint_num <- timepoint_map[wt_control$session_id]

# Rolling variables
wt_control <- wt_control %>%
  arrange(participant_id, timepoint_num) %>%
  group_by(participant_id, reporter) %>%
  mutate(
    vomit_rolling = if_else(cumsum(ifelse(is.na(vomit_lifetime), 0, vomit_lifetime)) > 0, 1, 0),
    other_rolling = if_else(cumsum(ifelse(is.na(other_lifetime), 0, other_lifetime)) > 0, 1, 0)
  ) %>%
  ungroup()

# Select key variables
wt_control <- wt_control %>%
  select(participant_id, session_id, timepoint_num, reporter,
         wtcontrol_vomit_current_scored, wtcontrol_vomit_past_scored, vomit_lifetime, vomit_rolling,
         wtcontrol_other_current_scored, wtcontrol_other_past_scored, other_lifetime, other_rolling) %>%
  rename(
    vomit_current = wtcontrol_vomit_current_scored,
    vomit_past    = wtcontrol_vomit_past_scored,
    other_current = wtcontrol_other_current_scored,
    other_past    = wtcontrol_other_past_scored
  )

# Save
save(wt_control, file = "data/6.0/wt_control.RData")

# =============================================================================
# 9. FEAR OF WEIGHT GAIN (FOWG)
# =============================================================================

fowg <- ed_all %>%
  select(participant_id, session_id, reporter, starts_with("fowg"))

# Lifetime
fowg$fowg_lifetime <- ifelse(fowg$fowg_current_scored == 1, 1, fowg$fowg_past_scored)

# Timepoint
fowg$timepoint_num <- timepoint_map[fowg$session_id]

# Rolling
fowg <- fowg %>%
  arrange(participant_id, timepoint_num) %>%
  group_by(participant_id, reporter) %>%
  mutate(fowg_rolling = if_else(cumsum(ifelse(is.na(fowg_lifetime), 0, fowg_lifetime)) > 0, 1, 0)) %>%
  ungroup()

# NOTE: fowg_current_half (based on raw fowg frequency item) is NOT available in 6.0.

# Select and rename
fowg <- fowg %>%
  select(participant_id, session_id, reporter, timepoint_num,
         fowg_current_scored, fowg_past_scored, fowg_lifetime, fowg_rolling) %>%
  rename(fowg_current = fowg_current_scored,
         fowg_past = fowg_past_scored)

# Save
save(fowg, file = "data/6.0/fowg.RData")

# =============================================================================
# 10. PUBERTY (PDS)
# =============================================================================
# In 6.0, PDS mean and category are pre-computed (no scoring function needed).
# Categories are numeric 1-5: 1=Pre, 2=Early, 3=Mid, 4=Late, 5=Post
# Sex-specific: separate columns for female (__f_) and male (__m_)

pub <- read.delim('ABCD_6.0/rawdata/phenotype/ph_y_pds.tsv', header = TRUE)

# Combine sex-specific mean and category into single columns
pds_scored <- pub %>%
  mutate(
    # Coalesce female and male means
    pds_mean = coalesce(
      as.numeric(na_if(ph_y_pds__f_mean, "n/a")),
      as.numeric(na_if(ph_y_pds__m_mean, "n/a"))
    ),
    # Coalesce female and male categories, map to labels
    pds_categ_num = coalesce(
      as.numeric(na_if(ph_y_pds__f_categ, "n/a")),
      as.numeric(na_if(ph_y_pds__m_categ, "n/a"))
    ),
    pds_category = case_when(
      pds_categ_num == 1 ~ "Pre",
      pds_categ_num == 2 ~ "Early",
      pds_categ_num == 3 ~ "Mid",
      pds_categ_num == 4 ~ "Late",
      pds_categ_num == 5 ~ "Post",
      TRUE ~ NA_character_
    )
  ) %>%
  select(participant_id, session_id, pds_mean, pds_category) %>%
  mutate(
    timepoint = recode(session_id, !!!timepoint_map),
    timepoint = factor(timepoint, levels = 0:6,
                       labels = c("Baseline", "Year 1", "Year 2", "Year 3", 
                                  "Year 4", "Year 5", "Year 6"))
  ) %>%
  select(-session_id) %>%
  rename(id = participant_id)

save(pds_scored, file = "data/6.0/pds_scored.RData")

# =============================================================================
# NOTES ON 5.0 → 6.0 DIFFERENCES
# =============================================================================
#
# ID/structure changes:
#   - src_subject_id → participant_id
#   - eventname → session_id (ses-00A through ses-06A)
#   - CSV → TSV
#   - Two new timepoints: ses-05A (year 5), ses-06A (year 6)
#
# Variables LOST in 6.0 (no raw items):
#   - Individual weight control subtypes: restrict, exercise, dietpills,
#     waterpills, laxative (all collapsed into wghtcntrl__oth)
#   - Binge frequency (continuous/ordinal) — replaced by binary wkly3mo
#   - Age of onset (edsymptoms_aoo_*)
#   - fowg_current_half (based on raw frequency item)
#   - All individual raw questionnaire items (ksads_eatingdis_raw_*)
#   - wtcontrol_past_duration, wtcontrol_past_date, wtcontrol_other_frequency,
#     vomit_frequency
#
# Variables GAINED in 6.0:
#   - binge_characteristics (3+ associated symptoms)
#   - binge_distress
#   - binge_wkly3mo (weekly for 3 months threshold)
#   - compbehav (compensatory behaviors)
#   - compbehav_wkly3mo
#   - selfworth_wt (self-worth tied to weight)
#   - Partial remission diagnoses (partrem_dx)
#   - OSFED low frequency/limited duration subtypes for BN and BED
#   - Atypical AN diagnosis
