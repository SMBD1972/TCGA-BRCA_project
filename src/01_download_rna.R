library(TCGAbiolinks)

q_rna <- GDCquery(
  project       = "TCGA-BRCA",
  data.category = "Transcriptome Profiling",
  data.type     = "Gene Expression Quantification",
  workflow.type = "STAR - Counts",
  sample.type   = "Primary Tumor"
)

res <- getResults(q_rna)

nrow(res)
length(unique(substr(res$cases, 1, 12)))

library(SummarizedExperiment)

GDCdownload(q_rna, method = "api", files.per.chunk = 50)

rna <- GDCprepare(q_rna)

saveRDS(rna, "data/raw/rna_se.rds")

dim(rna)
