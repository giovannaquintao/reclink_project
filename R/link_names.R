library(dplyr)
library(stringr)
library(stringdist)

# ── Helpers de comparação ─────────────────────────────────────────────────────

surname_tokens <- function(sob) {
  if (is.na(sob)) character(0) else str_split(sob, " ")[[1]]
}

jw_first <- function(a, b) {
  stringdist(a, b, method = "jw", p = 0.1)
}

jw_surnames <- function(sob_a, sob_b) {
  tok_a <- surname_tokens(sob_a)
  tok_b <- surname_tokens(sob_b)
  if (length(tok_a) == 0 || length(tok_b) == 0) return(NA_real_)
  scores <- sapply(tok_a, function(ta) min(stringdist(ta, tok_b, method = "jw", p = 0.1)))
  mean(scores)
}

classify_match <- function(pn_a, sob_a, pn_b, sob_b, jw_pn, jw_sob) {
  tok_a <- sort(surname_tokens(sob_a))
  tok_b <- sort(surname_tokens(sob_b))

  idêntico_pn  <- pn_a == pn_b
  idêntico_sob <- identical(tok_a, tok_b)
  reordenado   <- setequal(tok_a, tok_b) && !idêntico_sob
  faltando     <- length(tok_a) != length(tok_b) &&
                  (all(tok_a %in% tok_b) || all(tok_b %in% tok_a))

  dplyr::case_when(
    idêntico_pn & idêntico_sob                   ~ "identico",
    idêntico_pn & reordenado                      ~ "sobrenomes_reordenados",
    idêntico_pn & faltando                        ~ "sobrenome_faltando",
    jw_pn <= 0.1 & jw_sob <= 0.1 & reordenado    ~ "typo_e_reordenado",
    jw_pn <= 0.1 & jw_sob <= 0.1                 ~ "typo",
    TRUE                                          ~ "no_match"
  )
}

# ── Função principal ──────────────────────────────────────────────────────────

#' Link two vectors of names
#'
#' @param nomes_a   Character vector — nomes originais (base de origem)
#' @param nomes_b   Character vector — nomes candidatos a match
#' @param link_by   Variável de ligação para reduzir a matriz:
#'                  "first_letters" (padrão), "first_name"
#' @param n_letters Número de letras iniciais usadas quando link_by = "first_letters" (padrão: 1)
#'
#' @return data.frame com uma linha por nome em nomes_a (melhor match):
#'         nome_original, nome_melhor_match, classificacao,
#'         jw_primeiro_nome, jw_sobrenomes
link_names <- function(nomes_a, nomes_b,
                       link_by   = c("first_letters", "first_name"),
                       n_letters = 1) {

  link_by <- match.arg(link_by)

  # 1. Parsear nomes
  parsed_a <- bind_rows(lapply(nomes_a, parse_name))
  parsed_b <- bind_rows(lapply(nomes_b, parse_name))

  # 2. Chave de ligação
  make_key <- function(df) {
    if (link_by == "first_letters") {
      df |> mutate(link_key = substr(primeiro_nome, 1, n_letters))
    } else {
      df |> mutate(link_key = primeiro_nome)
    }
  }

  parsed_a <- make_key(parsed_a)
  parsed_b <- make_key(parsed_b)

  # 3. Matriz cruzada filtrada
  pares <- inner_join(parsed_a, parsed_b, by = "link_key", suffix = c("_a", "_b"))

  # 4. Similaridade e classificação
  scored <- pares |>
    rowwise() |>
    mutate(
      jw_primeiro_nome = jw_first(primeiro_nome_a, primeiro_nome_b),
      jw_sobrenomes    = jw_surnames(sobrenomes_a, sobrenomes_b),
      classificacao    = classify_match(
        primeiro_nome_a, sobrenomes_a,
        primeiro_nome_b, sobrenomes_b,
        jw_primeiro_nome, jw_sobrenomes
      )
    ) |>
    ungroup()

  # 5. Melhor match por nome original
  classificacao_ordem <- c(
    "identico", "sobrenomes_reordenados", "sobrenome_faltando",
    "typo", "typo_e_reordenado"
  )

  scored |>
    filter(.data$classificacao != "no_match") |>
    mutate(ordem_class = match(.data$classificacao, classificacao_ordem)) |>
    arrange(.data$ordem_class, .data$jw_primeiro_nome, .data$jw_sobrenomes) |>
    group_by(.data$nome_original_a) |>
    slice(1) |>
    ungroup() |>
    select(
      nome_original     = .data$nome_original_a,
      nome_melhor_match = .data$nome_original_b,
      classificacao     = .data$classificacao,
      jw_primeiro_nome  = .data$jw_primeiro_nome,
      jw_sobrenomes     = .data$jw_sobrenomes
    )
}


