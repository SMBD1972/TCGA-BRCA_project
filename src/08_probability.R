library(tidyverse)

analytic <- readRDS("data/processed/analytic.rds") |>
  mutate(
    stage  = factor(stage, levels = c("I", "II", "III", "IV"), ordered = TRUE),
    er_ihc = factor(er_ihc, levels = c("Negative", "Positive"))
  )

nrow(analytic)




thr <- median(analytic$ESR1_log2)
thr

analytic <- analytic |>
  mutate(
    A = case_when(er_ihc == "Positive" ~ TRUE,
                  er_ihc == "Negative" ~ FALSE,
                  TRUE ~ NA),
    B = case_when(stage >= "III" ~ TRUE,
                  stage <  "III" ~ FALSE,
                  TRUE ~ NA),
    H = ESR1_log2 > thr
  )

summary(analytic[, c("A", "B", "H")])



d <- analytic |> filter(!is.na(A), !is.na(B))
n <- nrow(d)
n

tab_AB <- table(A = d$A, B = d$B)
tab_AB

p_A  <- mean(d$A)
p_B  <- mean(d$B)
p_AB <- mean(d$A & d$B)

c(P_A = p_A, P_B = p_B, P_A_and_B = p_AB)



p_B_given_A    <- mean(d$B[d$A])
p_B_given_notA <- mean(d$B[!d$A])

p_A_given_B    <- mean(d$A[d$B])

c(P_B_given_A = p_B_given_A,
  P_B_given_notA = p_B_given_notA,
  P_A_given_B = p_A_given_B)

p_AB / p_A


c(P_AB_observed = p_AB,
  P_A_times_P_B = p_A * p_B)

c(P_B = p_B, P_B_given_A = p_B_given_A)

expected <- outer(rowSums(tab_AB), colSums(tab_AB)) / n
round(expected, 1)
tab_AB