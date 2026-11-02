library(tidyverse)

expr     <- readRDS("data/processed/expr_patient.rds")
clinical <- readRDS("data/processed/clinical_clean.rds")

p_rna  <- unique(expr$patient)
p_clin <- unique(clinical$patient)

length(p_rna)
length(p_clin)
length(intersect(p_rna, p_clin))
setdiff(p_rna, p_clin)
length(setdiff(p_clin, p_rna))

merged <- inner_join(clinical, expr, by = "patient")
nrow(merged)

analytic <- merged |> filter(sex == "female")
nrow(analytic)

analytic_cc <- analytic |>
  filter(!is.na(er_ihc), !is.na(age_years), !is.na(stage))
nrow(analytic_cc)

saveRDS(analytic, "data/processed/analytic.rds")