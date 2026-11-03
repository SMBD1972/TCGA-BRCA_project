library(tidyverse)
library(janitor)

analytic <- readRDS("data/processed/analytic.rds") |>
  mutate(
    stage     = factor(stage, levels = c("I", "II", "III", "IV"), ordered = TRUE),
    menopause = factor(menopause, levels = c("Pre", "Peri", "Post"), ordered = TRUE),
    er_ihc    = factor(er_ihc, levels = c("Negative", "Positive"))
  )

nrow(analytic)



tab_er    <- analytic |> tabyl(er_ihc)    |> adorn_pct_formatting()
tab_stage <- analytic |> tabyl(stage)     |> adorn_pct_formatting()
tab_meno  <- analytic |> tabyl(menopause) |> adorn_pct_formatting()

tab_er
tab_stage
tab_meno

write_csv(tab_er,    "reports/phase1/table_freq_er.csv")
write_csv(tab_stage, "reports/phase1/table_freq_stage.csv")
write_csv(tab_meno,  "reports/phase1/table_freq_menopause.csv")




num_vars <- c("age_years", "ESR1_log2", "GATA3_log2", "FOXA1_log2", "MKI67_log2")

desc <- analytic |>
  select(all_of(num_vars)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "value") |>
  group_by(variable) |>
  summarise(
    n       = sum(!is.na(value)),
    missing = sum(is.na(value)),
    mean    = mean(value, na.rm = TRUE),
    sd      = sd(value, na.rm = TRUE),
    median  = median(value, na.rm = TRUE),
    q1      = quantile(value, 0.25, na.rm = TRUE),
    q3      = quantile(value, 0.75, na.rm = TRUE),
    iqr     = IQR(value, na.rm = TRUE),
    min     = min(value, na.rm = TRUE),
    max     = max(value, na.rm = TRUE)
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 2)))

desc
write_csv(desc, "reports/phase1/table_numeric_summary.csv")




comp <- analytic |>
  filter(!is.na(er_ihc)) |>
  group_by(er_ihc) |>
  summarise(
    n      = n(),
    median = median(ESR1_log2),
    q1     = quantile(ESR1_log2, 0.25),
    q3     = quantile(ESR1_log2, 0.75),
    mean   = mean(ESR1_log2),
    sd     = sd(ESR1_log2)
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 2)))

comp
write_csv(comp, "reports/phase1/table_esr1_by_er.csv")




p_stage <- analytic |>
  filter(!is.na(stage)) |>
  ggplot(aes(x = stage)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Pathologic stage at diagnosis",
       x = "AJCC pathologic stage",
       y = "Number of patients")

p_age <- analytic |>
  filter(!is.na(age_years)) |>
  ggplot(aes(x = age_years)) +
  geom_histogram(binwidth = 5, boundary = 0, fill = "steelblue", color = "white") +
  labs(title = "Age at diagnosis",
       x = "Age (years)",
       y = "Number of patients")

p_box <- analytic |>
  filter(!is.na(er_ihc)) |>
  ggplot(aes(x = er_ihc, y = ESR1_log2)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 0.8) +
  labs(title = "ESR1 expression by clinical ER status",
       x = "ER status (IHC)",
       y = "ESR1 expression, log2(TPM + 1)")

p_stage
p_age
p_box

ggsave("figures/fig3_stage_bar.png",      p_stage, width = 6, height = 4, dpi = 300)
ggsave("figures/fig4_age_hist.png",       p_age,   width = 6, height = 4, dpi = 300)
ggsave("figures/fig5_esr1_by_er_box.png", p_box,   width = 6, height = 5, dpi = 300)


analytic |>
  filter(!is.na(er_ihc)) |>
  mutate(discordant = (er_ihc == "Negative" & ESR1_log2 > 5) |
           (er_ihc == "Positive" & ESR1_log2 < 3)) |>
  count(er_ihc, discordant)
