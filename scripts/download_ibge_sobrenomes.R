library(httr)
library(dplyr)

dest     <- "C:/Users/giova/OneDrive/raw_data/ibge/names"
csv_path <- file.path(dest, "ibge_sobrenomes_2022.csv")

# retomar do CSV existente
if (file.exists(csv_path)) {
  sobrenomes_df <- read.csv(csv_path, stringsAsFactors = FALSE)
  cat("Retomando: já temos", nrow(sobrenomes_df), "sobrenomes salvos.\n")
} else {
  sobrenomes_df <- data.frame()
}

page <- 1  # alterar para retomar de onde parou

repeat {
  url  <- paste0("https://servicodados.ibge.gov.br/api/v3/nomes/2022/localidade/0/ranking/sobrenome?page=", page)

  resp <- tryCatch(
    httr::GET(url, httr::timeout(30)),
    error = function(e) {
      cat(sprintf("  TIMEOUT/ERRO de conexão na page %d: %s\n", page, e$message))
      cat("  Salvando progresso atual...\n")
      write.csv(sobrenomes_df, csv_path, row.names = FALSE)
      cat("  Salvo:", nrow(sobrenomes_df), "sobrenomes em", csv_path, "\n")
      NULL
    }
  )

  if (is.null(resp)) break

  cat(sprintf("page %d — status %d\n", page, httr::status_code(resp)))

  if (httr::status_code(resp) != 200) {
    write.csv(sobrenomes_df, csv_path, row.names = FALSE)
    cat("Salvo:", nrow(sobrenomes_df), "sobrenomes em", csv_path, "\n")
    break
  }

  raw <- httr::content(resp, as = "parsed", simplifyVector = TRUE)
  if (length(raw) == 0 || is.null(raw)) {
    write.csv(sobrenomes_df, csv_path, row.names = FALSE)
    cat("Fim da base. Salvo:", nrow(sobrenomes_df), "sobrenomes em", csv_path, "\n")
    break
  }

  df <- tryCatch(
    as.data.frame(raw),
    error = function(e) {
      cat(sprintf("  ERRO ao parsear page %d: %s\n", page, e$message))
      NULL
    }
  )

  if (!is.null(df) && nrow(df) > 0) {
    sobrenomes_df <- dplyr::bind_rows(sobrenomes_df, df)
    write.csv(sobrenomes_df, csv_path, row.names = FALSE)  # salva a cada página
  }

  page <- page + 1
  Sys.sleep(0.2)
}

cat("Total final:", nrow(sobrenomes_df), "sobrenomes\n")
