library(tidyverse)

analytic <- readRDS("data/processed/analytic.rds") |>
  mutate(
    stage  = factor(stage, levels = c("I", "II", "III", "IV"), ordered = TRUE),
    er_ihc = factor(er_ihc, levels = c("Negative", "Positive")),
    complete = if_else(!is.na(er_ihc) & !is.na(age_years) & !is.na(stage),
                       "Complete", "Incomplete")
  )

table(analytic$complete)

miss_pattern <- analytic |>
  count(er_missing    = is.na(er_ihc),
        age_missing   = is.na(age_years),
        stage_missing = is.na(stage))
miss_pattern

comp_cc <- analytic |>
  group_by(complete) |>
  summarise(
    n                = n(),
    age_median       = median(age_years, na.rm = TRUE),
    esr1_median      = median(ESR1_log2),
    mki67_median     = median(MKI67_log2),
    pct_er_positive  = 100 * mean(er_ihc == "Positive", na.rm = TRUE),
    pct_stage_III_IV = 100 * mean(stage >= "III", na.rm = TRUE)
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 1)))
comp_cc

write_csv(miss_pattern, "reports/phase1/table_missing_pattern.csv")
write_csv(comp_cc,      "reports/phase1/table_complete_vs_incomplete.csv")