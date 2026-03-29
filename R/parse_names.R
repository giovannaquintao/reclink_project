utils::globalVariables(c(
  "ibge_nomes", "ibge_sobrenomes", "frequencia", "freq_pct", "name_upper"
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
  .pkg_env$ibge_nomes[["freq_pct"]] <-
    100 * .pkg_env$ibge_nomes[["frequencia"]] / sum(.pkg_env$ibge_nomes[["frequencia"]])
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
    first_name    <- paste(tokens[1], tokens[2])
    surname_tokens <- tokens[-(1:2)]
  } else {
    first_name    <- tokens[1]
    surname_tokens <- tokens[-1]
  }

  list(
    original   = name,
    clean      = clean,
    first_name = first_name,
    surnames   = if (length(surname_tokens) == 0) NA_character_
    else paste(surname_tokens, collapse = " "),
    n_surnames = length(surname_tokens)
  )
}
