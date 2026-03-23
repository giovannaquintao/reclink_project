library(httr)
library(dplyr)

fetch_ranking <- function(sexo = NULL) {
  url <- "https://servicodados.ibge.gov.br/api/v2/censos/nomes/ranking"
  query <- if (!is.null(sexo)) list(sexo = sexo) else list()

  resp <- httr::GET(url, query = query)
  httr::stop_for_status(resp)

  # do NOT simplify — res is a nested list of {nome, frequencia, ranking}
  raw <- httr::content(resp, as = "parsed", simplifyVector = FALSE)

  # raw[[1]]$res is the list of name records
  records <- raw[[1]]$res
  df <- dplyr::bind_rows(lapply(records, as.data.frame))
  df$sexo <- raw[[1]]$sexo %||% NA_character_
  df
}

# helper: NULL coalesce
`%||%` <- function(a, b) if (is.null(a)) b else a

names_all    <- fetch_ranking()
names_male   <- fetch_ranking(sexo = "M")
names_female <- fetch_ranking(sexo = "F")

cat("=== STRUCTURE (all) ===\n")
print(str(names_all))

cat("\n--- All (first 30) ---\n")
print(head(names_all, 30))

cat("\n--- Male (first 30) ---\n")
print(head(names_male, 30))

cat("\n--- Female (first 30) ---\n")
print(head(names_female, 30))
