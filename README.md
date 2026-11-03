# ESR1 Expression and Clinical ER Status in Breast Cancer (TCGA-BRCA)

Statistics course project – Fall 1405, Sharif University of Technology
Path B: clinical + RNA-seq

## Question

Does ESR1 expression differ between ER-positive and ER-negative primary breast tumors,
and is it associated with ER status and GATA3, FOXA1, MKI67 expression after accounting
for age and stage?

## Data

TCGA-BRCA open-access files from NCI GDC (Data Release 46.0, downloaded 2026-11-01).
Details and rebuild steps: [`data/README.md`](data/README.md). Data files are not in this repo.

## Structure

- `src/` – R scripts, run in order `01` → `07`
- `figures/` – figures
- `reports/phase1.pdf` – Phase 1 report
- `reports/phase1/` – Phase 1 tables
- `PROJECT_CARD.md` – project card
- `sessionInfo.txt` – R and package versions

## Reproduce

1. Install R 4.6.1, then:
   `install.packages(c("BiocManager", "tidyverse", "janitor"))`
   `BiocManager::install(c("TCGAbiolinks", "SummarizedExperiment"))`
2. Open `TCGA-BRCA_project.Rproj` and run the scripts in `src/` in order.

## Status

- Phase 1: complete (tag `phase-1`)
- Phases 2–5: upcoming