# IBGE name frequency data: download and loading

IBGE_NAMES_URL <- "https://servicodados.ibge.gov.br/api/v2/censos/nomes/ranking"
IBGE_LOCAL_PATH <- system.file("extdata", package = "reclink")

#' Download IBGE name frequency data and save locally
#'
#' Fetches the full name frequency ranking from the IBGE API and saves it as
#' a CSV in \code{inst/extdata/ibge_names.csv} inside the package source tree.
#'
#' @param dest_path path where the CSV will be saved.
#'   Defaults to \code{inst/extdata/ibge_names.csv} relative to the working
#'   directory (package source root).
#' @return invisibly returns the data.frame of downloaded names
#' @export
download_ibge_names <- function(dest_path = "inst/extdata/ibge_names.csv") {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required. Install it with install.packages('httr').")
  }

  message("Downloading IBGE name frequency data...")

  resp <- httr::GET(IBGE_NAMES_URL)
  httr::stop_for_status(resp)

  raw <- httr::content(resp, as = "parsed", simplifyVector = TRUE)

  # API returns a list with $nome (name) and $frequencia (frequency)
  df <- data.frame(
    nome       = toupper(raw$nome),
    frequencia = as.integer(raw$frequencia),
    ranking    = as.integer(raw$ranking),
    stringsAsFactors = FALSE
  )

  dir.create(dirname(dest_path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(df, dest_path, row.names = FALSE)

  message(sprintf("Saved %d names to '%s'.", nrow(df), dest_path))
  invisible(df)
}

#' Load IBGE name frequency data
#'
#' Reads the locally saved CSV and returns a named numeric vector of
#' frequencies, suitable for quick lookup.
#'
#' @param path path to the CSV file saved by \code{\link{download_ibge_names}}.
#' @return named numeric vector: names are uppercase name strings, values are
#'   frequencies.
#' @export
load_ibge_names <- function(path = "inst/extdata/ibge_names.csv") {
  if (!file.exists(path)) {
    stop(
      "IBGE names file not found at '", path, "'.\n",
      "Run download_ibge_names() first to fetch and save the data."
    )
  }

  df <- utils::read.csv(path, stringsAsFactors = FALSE)
  freq <- stats::setNames(df$frequencia, df$nome)
  freq
}

#' Look up name frequency (internal helper)
#'
#' Returns the IBGE frequency for a name token, or 0 if not found.
#'
#' @param token uppercase character string (single name token)
#' @param freq_table named numeric vector from \code{\link{load_ibge_names}}
#' @return integer frequency
.name_freq <- function(token, freq_table) {
  f <- freq_table[token]
  if (is.na(f)) 0L else as.integer(f)
}

#' Compute a commonness score for a full name
#'
#' Average frequency of all surname tokens present in the IBGE table,
#' normalized to [0, 1] using the maximum observed frequency.
#'
#' @param surname uppercase surname string (may contain multiple tokens)
#' @param freq_table named numeric vector from \code{\link{load_ibge_names}}
#' @return numeric in [0, 1]; higher = more common
.surname_commonness <- function(surname, freq_table) {
  if (is.na(surname) || nchar(stringr::str_trim(surname)) == 0) return(0)
  tokens <- stringr::str_split(surname, " ")[[1]]
  tokens <- tokens[nchar(tokens) > 0]
  if (length(tokens) == 0) return(0)

  freqs <- vapply(tokens, .name_freq, integer(1), freq_table = freq_table)
  max_f <- max(freq_table, na.rm = TRUE)
  mean(freqs) / max_f
}
