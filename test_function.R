library(dplyr)
library(stringr)
library(stringdist)

rm(list = ls())
gc()

source("c:/projects/reclink_project/R/parse_names.R")
source("c:/projects/reclink_project/R/link_names.R")
source("c:/projects/reclink_project/R/link_surnames.R")

# 1. Load test data
dataset_a <- read.csv("c:/projects/reclink_project/data/test/dataset_a.csv",
                      stringsAsFactors = FALSE)
dataset_b <- read.csv("c:/projects/reclink_project/data/test/dataset_b.csv",
                      stringsAsFactors = FALSE)

# 2. Link by name only
result <- link_names(
  names_a   = dataset_a$nome,
  names_b   = dataset_b$nome,
  id_a      = dataset_a$id,
  id_b      = dataset_b$id,
  link_by   = "first_letters",
  n_letters = 1
)

table(result$classification)
View(result)

# 3. Link by name + date of birth + sex
result_full <- link_names(
  names_a      = dataset_a$nome,
  names_b      = dataset_b$nome,
  id_a         = dataset_a$id,
  id_b         = dataset_b$id,
  link_by      = c("first_letters", "date_birth", "sex"),
  n_letters    = 1,
  date_birth_a = dataset_a$data_nascimento,
  date_birth_b = dataset_b$data_nascimento,
  sex_a        = dataset_a$sexo,
  sex_b        = dataset_b$sexo
)

table(result_full$classification)

# 4. Link by surname only
sur_a <- read.csv("c:/projects/reclink_project/data/test/dataset_surnames_a.csv",
                  stringsAsFactors = FALSE)
sur_b <- read.csv("c:/projects/reclink_project/data/test/dataset_surnames_b.csv",
                  stringsAsFactors = FALSE)

result_sur <- link_surnames(
  surnames_a = sur_a$sobrenome,
  surnames_b = sur_b$sobrenome,
  id_a       = sur_a$id,
  id_b       = sur_b$id,
  link_by    = "first_letters",
  n_letters  = 1
)

table(result_sur$classification)
View(result_sur)
