library(data.table)
library(stringi)
library(stringr)

utils::globalVariables(c(
  "ibge_nomes", "ibge_sobrenomes", "frequencia", "freq_relativa", "nome_upper"
))

# ── Constantes ────────────────────────────────────────────────────────────────
PARTICLES <- c("DE", "DA", "DO", "DOS", "DAS", "E", "DI", "DU", "VAN", "VON")
PARTICLE_PATTERN <- paste0("\\b(", paste(PARTICLES, collapse = "|"), ")\\b")

# Thresholds em frequência relativa (proporção da população)
# Equivale a ~1 pessoa em 1 milhão — separa nomes raríssimos de incomuns
FIRST_NAME_FREQ_THRESHOLD  <- 1e-6
SURNAME_COMPOUND_THRESHOLD <- 1e-6

# ── Carregar dados IBGE ───────────────────────────────────────────────────────
.pkg_data_dir <- function() {
  # Pacote instalado: system.file retorna o caminho correto
  path <- system.file("data", package = "reclink")
  if (nchar(path) > 0) return(path)
  # Desenvolvimento (source): sobe a árvore até encontrar DESCRIPTION
  wd <- normalizePath(".")
  while (nchar(wd) > 3) {
    if (file.exists(file.path(wd, "DESCRIPTION"))) return(file.path(wd, "data"))
    wd <- dirname(wd)
  }
  stop("Não foi possível localizar a pasta data/ do pacote reclink.")
}

load(file.path(.pkg_data_dir(), "ibge_nomes.rda"))
load(file.path(.pkg_data_dir(), "ibge_sobrenomes.rda"))

ibge_nomes[, freq_relativa := frequencia / sum(frequencia)]
ibge_sobrenomes[, freq_relativa := frequencia / sum(frequencia)]

first_names_set <- ibge_nomes[freq_relativa >= FIRST_NAME_FREQ_THRESHOLD, nome_upper]

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
  freq_sob <- ibge_sobrenomes$freq_relativa[ibge_sobrenomes$nome_upper == token]
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
