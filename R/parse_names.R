library(data.table)
library(stringi)
library(stringr)

# ── Constantes ────────────────────────────────────────────────────────────────
PARTICLES <- c("DE", "DA", "DO", "DOS", "DAS", "E", "DI", "DU", "VAN", "VON")
PARTICLE_PATTERN <- paste0("\\b(", paste(PARTICLES, collapse = "|"), ")\\b")

FIRST_NAME_FREQ_THRESHOLD <- 200
SURNAME_COMPOUND_THRESHOLD <- 200

# ── Carregar dados IBGE ───────────────────────────────────────────────────────
load(system.file("../data/ibge_nomes.rda",      package = "reclink", mustWork = TRUE))
load(system.file("../data/ibge_sobrenomes.rda", package = "reclink", mustWork = TRUE))

first_names_set <- ibge_nomes[frequencia >= FIRST_NAME_FREQ_THRESHOLD, nome_upper]

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

# token 2 é parte do nome composto se:
#   - é sobrenome raro (freq < SURNAME_COMPOUND_THRESHOLD), OU
#   - é primeiro nome comum (freq >= FIRST_NAME_FREQ_THRESHOLD)
is_compound_token <- function(token) {
  freq_sob <- ibge_sobrenomes$frequencia[ibge_sobrenomes$nome_upper == token]
  is_rare_surname  <- length(freq_sob) == 0 || freq_sob < SURNAME_COMPOUND_THRESHOLD
  is_common_fname  <- token %in% first_names_set
  is_rare_surname || is_common_fname
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
