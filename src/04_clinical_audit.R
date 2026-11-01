library(tidyverse)

clin    <- readRDS("data/raw/clinical_indexed.rds")
pat_raw <- readRDS("data/raw/clinical_patient_biotab.rds")

grep("gender|sex", names(clin), value = TRUE)

pat <- pat_raw |>
  slice(-(1:2)) |>
  type_convert()

nrow(pat)
n_distinct(pat$bcr_patient_barcode)
n_distinct(clin$submitter_id)

table(pat$er_status_by_ihc, useNA = "ifany")
table(pat$menopause_status, useNA = "ifany")
table(clin$ajcc_pathologic_stage, useNA = "ifany")
summary(clin$age_at_diagnosis)




pat_clean <- pat |>
  transmute(
    patient   = bcr_patient_barcode,
    er_ihc    = if_else(er_status_by_ihc %in% c("Positive", "Negative"),
                        er_status_by_ihc, NA_character_),
    menopause = case_when(
      str_starts(menopause_status, "Pre")  ~ "Pre",
      str_starts(menopause_status, "Peri") ~ "Peri",
      str_starts(menopause_status, "Post") ~ "Post",
      TRUE ~ NA_character_),
    age_years_bt = age_at_diagnosis
  )

clin_clean <- clin |>
  transmute(
    patient      = submitter_id,
    age_years    = age_at_diagnosis / 365.25,
    sex          = sex_at_birth,
    stage_detail = ajcc_pathologic_stage,
    stage = case_when(
      str_detect(stage_detail, "^Stage IV")  ~ "IV",
      str_detect(stage_detail, "^Stage III") ~ "III",
      str_detect(stage_detail, "^Stage II")  ~ "II",
      str_detect(stage_detail, "^Stage I")   ~ "I",
      TRUE ~ NA_character_)
  )

table(pat_clean$er_ihc, useNA = "ifany")
table(pat_clean$menopause, useNA = "ifany")
table(clin_clean$stage, useNA = "ifany")
table(clin_clean$sex, useNA = "ifany")

sum(clin_clean$age_years >= 89.9, na.rm = TRUE)

age_check <- inner_join(clin_clean, pat_clean, by = "patient") |>
  mutate(age_diff = abs(age_years - age_years_bt))
summary(age_check$age_diff)
sum(age_check$age_diff > 1, na.rm = TRUE)




clinical <- full_join(clin_clean, pat_clean, by = "patient")
nrow(clinical)
saveRDS(clinical, "data/processed/clinical_clean.rds")