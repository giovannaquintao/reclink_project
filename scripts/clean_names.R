library(data.table)
library(stringi)
library(stringr)
library(dplyr)

# ── Partículas portuguesas ────────────────────────────────────────────────────
PARTICLES <- c("DE", "DA", "DO", "DOS", "DAS", "E", "DI", "DU", "VAN", "VON")
PARTICLE_PATTERN <- paste0("\\b(", paste(PARTICLES, collapse = "|"), ")\\b")

# Frequência mínima para considerar um token como prenome (evita sobrenomes raros)
FIRST_NAME_FREQ_THRESHOLD <- 500

# ── Carregar lista de prenomes do IBGE ────────────────────────────────────────
ibge_path <- "C:/Users/giova/OneDrive/raw_data/ibge/names/ibge_nomes_2010.csv"
ibge_nomes <- fread(ibge_path)
ibge_nomes[, nome_upper := toupper(nome)]

first_names_set <- ibge_nomes[frequencia >= FIRST_NAME_FREQ_THRESHOLD, nome_upper]

cat("Prenomes IBGE com freq >=", FIRST_NAME_FREQ_THRESHOLD, ":", length(first_names_set), "\n")

# ── Funções de limpeza ────────────────────────────────────────────────────────
normalize <- function(x) {
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    toupper() |>
    str_squish()
}

remove_particles <- function(x) {
  x |>
    str_remove_all(PARTICLE_PATTERN) |>
    str_squish()
}

is_first_name <- function(token) {
  token %in% first_names_set
}

# ── Parser principal ──────────────────────────────────────────────────────────
parse_name <- function(nome_original) {
  nome_limpo         <- normalize(nome_original)
  nome_sem_particulas <- remove_particles(nome_limpo)

  tokens <- str_split(nome_sem_particulas, " ")[[1]]
  tokens <- tokens[nchar(tokens) > 0]

  if (length(tokens) == 0) {
    return(list(
      nome_original       = nome_original,
      nome_limpo          = nome_limpo,
      nome_sem_particulas = nome_sem_particulas,
      primeiro_nome       = NA_character_,
      sobrenomes          = NA_character_,
      qtd_sobrenomes      = 0L
    ))
  }

  # Detectar nome composto: token 2 presente na lista IBGE de prenomes
  if (length(tokens) >= 2 && is_first_name(tokens[2])) {
    primeiro_nome  <- paste(tokens[1], tokens[2])
    sobr_tokens    <- tokens[-(1:2)]
  } else {
    primeiro_nome  <- tokens[1]
    sobr_tokens    <- tokens[-1]
  }

  sobrenomes     <- if (length(sobr_tokens) == 0) NA_character_
                   else paste(sobr_tokens, collapse = " ")
  qtd_sobrenomes <- length(sobr_tokens)

  list(
    nome_original       = nome_original,
    nome_limpo          = nome_limpo,
    nome_sem_particulas = nome_sem_particulas,
    primeiro_nome       = primeiro_nome,
    sobrenomes          = sobrenomes,
    qtd_sobrenomes      = qtd_sobrenomes
  )
}

# ── Carregar dados ────────────────────────────────────────────────────────────
pe_de_meia <- fread(
  "C:/Users/giova/OneDrive/raw_data/inep/pe_de_meia/2025_new/202501_PeDeMeia.csv",
  nrows = 500,
  encoding = "Latin-1",
  select = "NOME BENEFICIÁRIO"
)

bolsa_familia <- fread(
  "C:/Users/giova/OneDrive/raw_data/bolsa_familia/202101_BolsaFamilia_Pagamentos.csv",
  nrows = 500,
  encoding = "Latin-1",
  select = "NOME FAVORECIDO"
)

nomes <- unique(c(
  pe_de_meia[["NOME BENEFICIÁRIO"]],
  bolsa_familia[["NOME FAVORECIDO"]]
))
nomes <- nomes[!is.na(nomes) & nchar(trimws(nomes)) > 0]
cat("Total de nomes únicos para processar:", length(nomes), "\n")

# ── Processar ─────────────────────────────────────────────────────────────────
results <- bind_rows(lapply(nomes, parse_name))

# ── Preview ───────────────────────────────────────────────────────────────────
cat("\nResultado:\n")
print(head(results, 30))

cat("\nNomes compostos detectados:\n")
print(results[str_detect(results$primeiro_nome, " "), ])
