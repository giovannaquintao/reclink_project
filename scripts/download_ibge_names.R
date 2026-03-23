library(data.table)
library(basedosdados)
library(httr)
library(dplyr)

dest <- "C:/Users/giova/OneDrive/raw_data/ibge/names"

# ── 1. Prenomes 2010 via BD+ ──────────────────────────────────────────────────
set_billing_id("humancapitalindex")

ibge_nomes <- read_sql("
  SELECT
    nome,
    SUM(quantidade_nascimentos_ate_2010) AS frequencia
  FROM `basedosdados.br_ibge_nomes_brasil.quantidade_municipio_nome_2010`
  GROUP BY nome
  ORDER BY frequencia DESC
")

cat("Total de prenomes unicos:", nrow(ibge_nomes), "\n")
print(head(ibge_nomes, 20))

write.csv(ibge_nomes, file.path(dest, "ibge_nomes_2010.csv"), row.names = FALSE)
cat("Salvo em:", file.path(dest, "ibge_nomes_2010.csv"), "\n")

# sobrenomes: ver download_ibge_sobrenomes.R
