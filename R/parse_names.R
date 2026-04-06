utils::globalVariables(c(
  "ibge_nomes", "ibge_sobrenomes", "frequencia", "freq_pct", "name_upper",
  "rare_surname_threshold"
))

PARTICLES <- c("DE", "DA", "DO", "DOS", "DAS", "E", "DI", "DU", "VAN", "VON")
PARTICLE_PATTERN <- paste0("\\b(", paste(PARTICLES, collapse = "|"), ")\\b")

compound_threshold <- 0.5  # min % of population to treat token as a first name

.pkg_env <- new.env(parent = emptyenv())

.data_dir <- function() {
  path <- system.file("data", package = "reclink")
  if (nchar(path) > 0) return(path)
  wd <- normalizePath(".")
  while (nchar(wd) > 3) {
    if (file.exists(file.path(wd, "DESCRIPTION"))) return(file.path(wd, "data"))
    wd <- dirname(wd)
  }
  stop("Cannot find data/ directory for package reclink.")
}

.onLoad <- function(libname, pkgname) {
  load(file.path(.data_dir(), "ibge_nomes.rda"),      envir = .pkg_env)
  load(file.path(.data_dir(), "ibge_sobrenomes.rda"), envir = .pkg_env)
  if ("nome_upper" %in% names(.pkg_env$ibge_nomes))
    data.table::setnames(.pkg_env$ibge_nomes,      "nome_upper", "name_upper")
  if ("nome_upper" %in% names(.pkg_env$ibge_sobrenomes))
    data.table::setnames(.pkg_env$ibge_sobrenomes, "nome_upper", "name_upper")
  total <- sum(.pkg_env$ibge_nomes[["frequencia"]])
  .pkg_env$ibge_nomes[["freq_pct"]] <-
    100 * .pkg_env$ibge_nomes[["frequencia"]] / total
}

normalize <- function(x) {
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    toupper() |>
    str_squish()
}

strip_particles <- function(x) {
  x |>
    str_remove_all(PARTICLE_PATTERN) |>
    str_squish()
}

is_compound_token <- function(token) {
  pct <- .pkg_env$ibge_nomes$freq_pct[.pkg_env$ibge_nomes$name_upper == token]
  length(pct) > 0 && pct >= compound_threshold
}

# % of population — first names above this are "common"
common_firstname_threshold <- 1.0
# % of population — surnames below this are "rare"
rare_surname_threshold     <- 0.1

is_common_firstname <- function(token) {
  pct <- .pkg_env$ibge_nomes$freq_pct[.pkg_env$ibge_nomes$name_upper == token]
  length(pct) > 0 && pct >= common_firstname_threshold
}

is_common_surname <- function(token) {
  freq  <- .pkg_env$ibge_sobrenomes$frequencia[
    .pkg_env$ibge_sobrenomes$name_upper == token
  ]
  total <- sum(.pkg_env$ibge_sobrenomes$frequencia)
  length(freq) > 0 && (100 * freq / total) >= rare_surname_threshold
}

is_known_surname <- function(token) {
  token %in% .pkg_env$ibge_sobrenomes$name_upper
}

# Returns TRUE if any near-match token pair are both recognized surnames
# AND their JW distance is above strict_threshold.
# If JW <= strict_threshold, treat as typo even if both are known surnames
# (e.g. SOUSA/SOUZA, RODRIGUES/RODRIGES).
any_known_surname_pair <- function(tok_a, tok_b,
                                   threshold        = 0.15,
                                   strict_threshold = 0.05) {
  for (ta in tok_a) {
    dists   <- stringdist::stringdist(ta, tok_b, method = "jw", p = 0.1)
    min_d   <- min(dists)
    closest <- tok_b[which.min(dists)]
    if (min_d > strict_threshold && min_d <= threshold &&
        is_known_surname(ta) && is_known_surname(closest))
      return(TRUE)
  }
  FALSE
}

has_rare_surname <- function(tokens) {
  if (length(tokens) == 0) return(FALSE)
  freqs <- .pkg_env$ibge_sobrenomes$frequencia[
    .pkg_env$ibge_sobrenomes$name_upper %in% tokens
  ]
  total <- sum(.pkg_env$ibge_sobrenomes$frequencia)
  pcts  <- 100 * freqs / total
  any(pcts < rare_surname_threshold) || length(freqs) == 0
}

parse_name <- function(name) {
  clean       <- normalize(name)
  no_part     <- strip_particles(clean)
  tokens      <- str_split(no_part, " ")[[1]]
  tokens      <- tokens[nchar(tokens) > 0]

  if (length(tokens) == 0) {
    return(list(
      original   = name,
      clean      = clean,
      first_name = NA_character_,
      surnames   = NA_character_,
      n_surnames = 0L
    ))
  }

  if (length(tokens) >= 2 && is_compound_token(tokens[2])) {
    first_name     <- paste(tokens[1], tokens[2])
    surname_tokens <- tokens[-(1:2)]
  } else {
    first_name     <- tokens[1]
    surname_tokens <- tokens[-1]
  }

  fn_word_count <- length(str_split(first_name, " ")[[1]])
  clean_tokens  <- str_split(clean, " ")[[1]]
  if (fn_word_count >= length(clean_tokens)) {
    clean_surnames <- NA_character_
  } else {
    clean_surnames <- paste(
      clean_tokens[(fn_word_count + 1):length(clean_tokens)],
      collapse = " "
    )
  }

  list(
    original       = name,
    clean          = clean,
    first_name     = first_name,
    surnames       = if (length(surname_tokens) == 0) NA_character_
      else paste(surname_tokens, collapse = " "),
    clean_surnames = clean_surnames,
    n_surnames     = length(surname_tokens)
  )
}
