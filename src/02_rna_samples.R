library(SummarizedExperiment)
library(tidyverse)

rna <- readRDS("data/raw/rna_se.rds")

samples <- as.data.frame(colData(rna)) |>
  select(barcode) |>
  mutate(patient = substr(barcode, 1, 12))

table(table(samples$patient))



dup <- samples |>
  group_by(patient) |>
  filter(n() > 1) |>
  arrange(patient, barcode)

print(dup, n = 30)

samples_one <- samples |>
  arrange(patient, barcode) |>
  distinct(patient, .keep_all = TRUE)

nrow(samples_one)

samples_one |>
  filter(patient %in% dup$patient)



rna_one <- rna[, samples_one$barcode]
dim(rna_one)

genes <- c("ESR1", "GATA3", "FOXA1", "MKI67")

gene_info <- as.data.frame(rowData(rna_one))
idx <- match(genes, gene_info$gene_name)
gene_info[idx, c("gene_id", "gene_name", "gene_type")]

tpm <- assay(rna_one, "tpm_unstrand")[idx, ]
rownames(tpm) <- genes

expr <- as.data.frame(t(tpm)) |>
  rownames_to_column("barcode") |>
  mutate(patient = substr(barcode, 1, 12)) |>
  mutate(across(all_of(genes), ~ log2(.x + 1), .names = "{.col}_log2"))

summary(expr)

dir.create("data/processed", showWarnings = FALSE)
saveRDS(expr, "data/processed/expr_patient.rds")


expr_long <- expr |>
  select(patient, all_of(genes)) |>
  pivot_longer(-patient, names_to = "gene", values_to = "tpm")

p_raw <- ggplot(expr_long, aes(x = tpm)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  facet_wrap(~ gene, scales = "free") +
  labs(title = "Distribution of gene expression (raw TPM)",
       x = "Expression (TPM)",
       y = "Number of patients")

p_log <- ggplot(expr_long, aes(x = log2(tpm + 1))) +
  geom_histogram(bins = 40, fill = "darkorange", color = "white") +
  facet_wrap(~ gene, scales = "free") +
  labs(title = "Distribution of gene expression (log2 scale)",
       x = "Expression, log2(TPM + 1)",
       y = "Number of patients")

p_raw
p_log

ggsave("figures/fig1_gene_expr_raw.png", p_raw, width = 8, height = 6, dpi = 300)
ggsave("figures/fig2_gene_expr_log2.png", p_log, width = 8, height = 6, dpi = 300)