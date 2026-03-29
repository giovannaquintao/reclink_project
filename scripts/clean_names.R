library(dplyr)
library(stringr)
library(stringdist)

rm(list=ls())
gc()

source("c:/projects/reclink_project/R/parse_names.R")
source("c:/projects/reclink_project/R/link_names.R")

# ── 1. Carregar dados de teste ────────────────────────────────────────────────
dataset_a <- read.csv("c:/projects/reclink_project/data/test/dataset_a.csv",
                      stringsAsFactors = FALSE)
dataset_b <- read.csv("c:/projects/reclink_project/data/test/dataset_b.csv",
                      stringsAsFactors = FALSE)

# ── 2. Linkar apenas por nome ─────────────────────────────────────────────────
resultado <- link_names(
  nomes_a = dataset_a$nome,
  nomes_b = dataset_b$nome,
  link_by = "first_letters",
  n_letters = 1
)

table(resultado$classificacao)
View(resultado)

# ── 3. Linkar por nome + data de nascimento + sexo ───────────────────────────
resultado_completo <- link_names(
  nomes_a      = dataset_a$nome,
  nomes_b      = dataset_b$nome,
  link_by      = c("first_letters", "date_birth", "sex"),
  n_letters    = 1,
  date_birth_a = dataset_a$data_nascimento,
  date_birth_b = dataset_b$data_nascimento,
  sex_a        = dataset_a$sexo,
  sex_b        = dataset_b$sexo
)

table(resultado_completo$classificacao)
