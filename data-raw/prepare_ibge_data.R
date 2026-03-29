# Rodar este script UMA VEZ para gerar os dados internos do pacote.
# Requer que os CSVs do IBGE estejam disponíveis localmente.
#
# Após rodar, os arquivos em data/ são commitados no repositório
# e os usuários do pacote não precisam baixar nada.

library(data.table)

ibge_nomes_path      <- "C:/Users/giova/OneDrive/raw_data/ibge/names/ibge_nomes_2010.csv"
ibge_sobrenomes_path <- "C:/Users/giova/OneDrive/raw_data/ibge/names/ibge_sobrenomes_2022.csv"

# ── Prenomes 2010 ─────────────────────────────────────────────────────────────
ibge_nomes_raw <- fread(ibge_nomes_path)
ibge_nomes <- ibge_nomes_raw[, .(
  nome_upper = toupper(nome),
  frequencia = frequencia
)]

# ── Sobrenomes 2022 ───────────────────────────────────────────────────────────
ibge_sobrenomes_raw <- fread(ibge_sobrenomes_path)
ibge_sobrenomes <- ibge_sobrenomes_raw[, .(
  nome_upper = toupper(items.nome),
  frequencia = items.frequencia
)]

# ── Salvar como dados do pacote ───────────────────────────────────────────────
save(ibge_nomes,     file = "data/ibge_nomes.rda",     compress = "xz")
save(ibge_sobrenomes, file = "data/ibge_sobrenomes.rda", compress = "xz")

