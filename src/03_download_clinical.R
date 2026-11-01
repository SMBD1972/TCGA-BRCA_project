library(TCGAbiolinks)
library(tidyverse)

clin <- GDCquery_clinic("TCGA-BRCA", type = "clinical")
dim(clin)
saveRDS(clin, "data/raw/clinical_indexed.rds")

q_bt <- GDCquery(
  project       = "TCGA-BRCA",
  data.category = "Clinical",
  data.format   = "bcr biotab"
)
GDCdownload(q_bt)
bt_files <- list.files("GDCdata", pattern = "clinical_.*brca.*\\.txt$",
                       recursive = TRUE, full.names = TRUE)
basename(bt_files)

pat_file <- bt_files[grepl("clinical_patient_brca", bt_files)]
pat_raw <- read_tsv(pat_file, show_col_types = FALSE)

head(pat_raw[, 1:4], 4)

saveRDS(pat_raw, "data/raw/clinical_patient_biotab.rds")
dim(pat_raw)

c("age_at_diagnosis", "ajcc_pathologic_stage", "gender") %in% names(clin)
c("er_status_by_ihc", "menopause_status") %in% names(pat_raw)
