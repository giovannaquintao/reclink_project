library(dplyr)
library(stringr)
library(stringdist)

rm(list=ls())
gc()

source("c:/projects/reclink_project/R/parse_names.R")
source("c:/projects/reclink_project/R/link_names.R")

# ── 1. Carregar nomes ─────────────────────────────────────────────────────────
pe_de_meia <- data.table::fread(
  "C:/Users/giova/OneDrive/raw_data/inep/pe_de_meia/2025_new/202501_PeDeMeia.csv",
  encoding = "Latin-1", select = "NOME BENEFICIÁRIO",nrows = 10000)

bolsa_familia <- data.table::fread(
  "C:/Users/giova/OneDrive/raw_data/bolsa_familia/202101_BolsaFamilia_Pagamentos.csv",
  encoding = "Latin-1", select = "NOME FAVORECIDO",nrows = 10000)

nomes_pdm <- unique(pe_de_meia[["NOME BENEFICIÁRIO"]])
nomes_bf  <- unique(bolsa_familia[["NOME FAVORECIDO"]])
nomes_pdm <- nomes_pdm[!is.na(nomes_pdm) & nchar(trimws(nomes_pdm)) > 0]
nomes_bf  <- nomes_bf[!is.na(nomes_bf)   & nchar(trimws(nomes_bf))  > 0]

# ── 2. Linkar ─────────────────────────────────────────────────────────────────

resultado <- link_names(nomes_pdm, nomes_bf, link_by = "first_name")

table(resultado$classificacao)
