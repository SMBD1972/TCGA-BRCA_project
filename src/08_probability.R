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



d2 <- analytic |> filter(!is.na(A))
n2 <- nrow(d2)
n2

tab_AH <- table(A = d2$A, H = d2$H)
tab_AH

p_A2  <- mean(d2$A)
p_H2  <- mean(d2$H)
p_AH  <- mean(d2$A & d2$H)

c(P_A = p_A2, P_H = p_H2, P_A_and_H = p_AH)

p_A_given_H    <- mean(d2$A[d2$H])
p_A_given_notH <- mean(d2$A[!d2$H])
p_H_given_A    <- mean(d2$H[d2$A])
p_H_given_notA <- mean(d2$H[!d2$A])

c(P_A_given_H = p_A_given_H,
  P_A_given_notH = p_A_given_notH,
  P_H_given_A = p_H_given_A,
  P_H_given_notA = p_H_given_notA)

c(P_AH_observed = p_AH, P_A_times_P_H = p_A2 * p_H2)