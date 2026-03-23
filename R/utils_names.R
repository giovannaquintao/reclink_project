# Utilities for name cleaning, normalization, and splitting

# Portuguese particles to remove before matching
PARTICLES <- c("de", "da", "do", "dos", "das", "e", "di", "du", "van", "von")

#' Normalize a name string
#'
#' Converts to uppercase, removes accents, trims whitespace, and collapses
#' multiple spaces.
#'
#' @param name character vector
#' @return normalized character vector
normalize_name <- function(name) {
  name |>
    stringi::stri_trans_general("Latin-ASCII") |>
    toupper() |>
    stringr::str_squish()
}

#' Remove Portuguese name particles
#'
#' @param name character vector (already normalized / uppercase)
#' @return character vector with particles removed
remove_particles <- function(name) {
  pattern <- paste0("\\b(", paste(toupper(PARTICLES), collapse = "|"), ")\\b")
  name |>
    stringr::str_remove_all(pattern) |>
    stringr::str_squish()
}

#' Split a full name into first name and surnames
#'
#' The first token (after normalization and particle removal) is treated as the
#' first name; all remaining tokens are the surnames.
#'
#' @param name character vector of full names
#' @return data.frame with columns: first_name, surname, n_parts
split_name <- function(name) {
  clean <- name |> normalize_name() |> remove_particles()

  parts     <- stringr::str_split(clean, " ")
  first     <- vapply(parts, function(x) x[1],            character(1))
  surname   <- vapply(parts, function(x) paste(x[-1], collapse = " "), character(1))
  n_parts   <- vapply(parts, length,                       integer(1))

  data.frame(
    original   = name,
    clean      = clean,
    first_name = first,
    surname    = surname,
    n_parts    = n_parts,
    stringsAsFactors = FALSE
  )
}

#' Jaro-Winkler similarity between two character vectors (element-wise)
#'
#' Wraps \code{stringdist::stringsim} with method = "jw".
#'
#' @param a character vector
#' @param b character vector (same length as \code{a})
#' @param p prefix weight (default 0.1, standard JW)
#' @return numeric vector of similarities in [0, 1]
jw_sim <- function(a, b, p = 0.1) {
  if (!requireNamespace("stringdist", quietly = TRUE)) {
    stop("Package 'stringdist' is required. Install it with install.packages('stringdist').")
  }
  stringdist::stringsim(a, b, method = "jw", p = p)
}
