library(data.table)
library(stringi)
library(stringr)

# ── Constantes ────────────────────────────────────────────────────────────────
PARTICLES <- c("DE", "DA", "DO", "DOS", "DAS", "E", "DI", "DU", "VAN", "VON")
PARTICLE_PATTERN <- paste0("\\b(", paste(PARTICLES, collapse = "|"), ")\\b")

FIRST_NAME_FREQ_THRESHOLD <- 500
SURNAME_COMPOUND_THRESHOLD <- 200

# ── Carregar dados IBGE ───────────────────────────────────────────────────────
ibge_nomes <- fread("C:/Users/giova/OneDrive/raw_data/ibge/names/ibge_nomes_2010.csv")
ibge_nomes[, nome_upper := toupper(nome)]
first_names_set <- ibge_nomes[frequencia >= FIRST_NAME_FREQ_THRESHOLD, nome_upper]

ibge_sobrenomes <- fread("C:/Users/giova/OneDrive/raw_data/ibge/names/ibge_sobrenomes_2022.csv")
ibge_sobrenomes <- ibge_sobrenomes[, .(nome_upper = toupper(items.nome), frequencia = items.frequencia)]

# ── Funções ───────────────────────────────────────────────────────────────────
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

# token 2 é parte do nome composto se freq como sobrenome < SURNAME_COMPOUND_THRESHOLD
is_compound_token <- function(token) {
  freq <- ibge_sobrenomes$frequencia[ibge_sobrenomes$nome_upper == token]
  length(freq) == 0 || freq < SURNAME_COMPOUND_THRESHOLD
}

parse_name <- function(nome_original) {
  nome_limpo          <- normalize(nome_original)
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

  if (length(tokens) >= 2 && is_compound_token(tokens[2])) {
    primeiro_nome <- paste(tokens[1], tokens[2])
    sobr_tokens   <- tokens[-(1:2)]
  } else {
    primeiro_nome <- tokens[1]
    sobr_tokens   <- tokens[-1]
  }

  list(
    nome_original       = nome_original,
    nome_limpo          = nome_limpo,
    nome_sem_particulas = nome_sem_particulas,
    primeiro_nome       = primeiro_nome,
    sobrenomes          = if (length(sobr_tokens) == 0) NA_character_
                         else paste(sobr_tokens, collapse = " "),
    qtd_sobrenomes      = length(sobr_tokens)
  )
}
