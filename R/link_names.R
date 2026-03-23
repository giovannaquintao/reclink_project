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
  # ── flags de sobrenome ───────────────────────────────────────────────────────
  tok_a <- sort(surname_tokens(sob_a))
  tok_b <- sort(surname_tokens(sob_b))

  sob_identico   <- identical(tok_a, tok_b)
  sob_reordenado <- setequal(tok_a, tok_b) && !sob_identico
  sob_faltando   <- length(tok_a) != length(tok_b) &&
                    (all(tok_a %in% tok_b) || all(tok_b %in% tok_a))
  sob_typo       <- !sob_identico && !sob_reordenado && !sob_faltando &&
                    !is.na(jw_sob) && jw_sob <= 0.15

  # ── flags de primeiro nome ───────────────────────────────────────────────────
  pn_tok_a <- str_split(trimws(pn_a), " ")[[1]]
  pn_tok_b <- str_split(trimws(pn_b), " ")[[1]]

  pn_identico <- pn_a == pn_b
  pn_faltando <- !pn_identico &&
                 length(pn_tok_a) != length(pn_tok_b) &&
                 (all(pn_tok_a %in% pn_tok_b) || all(pn_tok_b %in% pn_tok_a))
  pn_typo     <- !pn_identico && !pn_faltando && !is.na(jw_pn) && jw_pn <= 0.1

  # ── todas as combinações (3 pn × 4 sob) ─────────────────────────────────────
  dplyr::case_when(
    pn_identico & sob_identico   ~ "pn_identico_sob_identico",
    pn_identico & sob_reordenado ~ "pn_identico_sob_reordenado",
    pn_identico & sob_faltando   ~ "pn_identico_sob_faltando",
    pn_identico & sob_typo       ~ "pn_identico_sob_typo",
    pn_faltando & sob_identico   ~ "pn_faltando_sob_identico",
    pn_faltando & sob_reordenado ~ "pn_faltando_sob_reordenado",
    pn_faltando & sob_faltando   ~ "pn_faltando_sob_faltando",
    pn_faltando & sob_typo       ~ "pn_faltando_sob_typo",
    pn_typo     & sob_identico   ~ "pn_typo_sob_identico",
    pn_typo     & sob_reordenado ~ "pn_typo_sob_reordenado",
    pn_typo     & sob_faltando   ~ "pn_typo_sob_faltando",
    pn_typo     & sob_typo       ~ "pn_typo_sob_typo",
    TRUE                         ~ "no_match"
  )
}

# ── Função principal ──────────────────────────────────────────────────────────

#' Link two vectors of names
#'
#' @param nomes_a      Character vector — nomes originais (base de origem)
#' @param nomes_b      Character vector — nomes candidatos a match
#' @param link_by      Vetor de variáveis de ligação para reduzir a matriz.
#'                     Valores possíveis (podem ser combinados):
#'                     "first_letters", "first_name", "date_birth", "sex"
#' @param n_letters    Nº de letras iniciais quando "first_letters" está em link_by (padrão: 1)
#' @param date_birth_a Vetor de datas de nascimento para nomes_a (obrigatório se "date_birth" em link_by)
#' @param date_birth_b Vetor de datas de nascimento para nomes_b (obrigatório se "date_birth" em link_by)
#' @param sex_a        Vetor de sexo para nomes_a (obrigatório se "sex" em link_by)
#' @param sex_b        Vetor de sexo para nomes_b (obrigatório se "sex" em link_by)
#'
#' @return data.frame com uma linha por nome em nomes_a (melhor match)
link_names <- function(nomes_a, nomes_b,
                       link_by      = "first_letters",
                       n_letters    = 1,
                       date_birth_a = NULL,
                       date_birth_b = NULL,
                       sex_a        = NULL,
                       sex_b        = NULL) {

  valid_link_by <- c("first_letters", "first_name", "date_birth", "sex")
  unknown <- setdiff(link_by, valid_link_by)
  if (length(unknown) > 0)
    stop("link_by inválido: ", paste(unknown, collapse = ", "),
         ". Opções: ", paste(valid_link_by, collapse = ", "))

  if ("date_birth" %in% link_by && (is.null(date_birth_a) || is.null(date_birth_b)))
    stop("date_birth_a e date_birth_b são obrigatórios quando link_by inclui 'date_birth'")

  if ("sex" %in% link_by && (is.null(sex_a) || is.null(sex_b)))
    stop("sex_a e sex_b são obrigatórios quando link_by inclui 'sex'")

  # 1. Parsear nomes
  parsed_a <- bind_rows(lapply(nomes_a, parse_name))
  parsed_b <- bind_rows(lapply(nomes_b, parse_name))

  # 2. Adicionar variáveis externas e construir chaves de ligação
  join_keys <- character(0)

  if ("first_letters" %in% link_by) {
    parsed_a$name_key <- substr(parsed_a$primeiro_nome, 1, n_letters)
    parsed_b$name_key <- substr(parsed_b$primeiro_nome, 1, n_letters)
    join_keys <- c(join_keys, "name_key")
  } else if ("first_name" %in% link_by) {
    parsed_a$name_key <- parsed_a$primeiro_nome
    parsed_b$name_key <- parsed_b$primeiro_nome
    join_keys <- c(join_keys, "name_key")
  }

  if ("date_birth" %in% link_by) {
    parsed_a$date_birth <- date_birth_a
    parsed_b$date_birth <- date_birth_b
    join_keys <- c(join_keys, "date_birth")
  }

  if ("sex" %in% link_by) {
    parsed_a$sex <- sex_a
    parsed_b$sex <- sex_b
    join_keys <- c(join_keys, "sex")
  }

  # 3. Matriz cruzada filtrada
  pares <- inner_join(parsed_a, parsed_b, by = join_keys, suffix = c("_a", "_b"),
                      relationship = "many-to-many")

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
    "pn_identico_sob_identico",    # 1 — match perfeito
    "pn_identico_sob_reordenado",  # 2
    "pn_identico_sob_faltando",    # 3
    "pn_identico_sob_typo",        # 4
    "pn_faltando_sob_identico",    # 5
    "pn_typo_sob_identico",        # 6
    "pn_faltando_sob_reordenado",  # 7
    "pn_typo_sob_reordenado",      # 8
    "pn_faltando_sob_faltando",    # 9
    "pn_typo_sob_faltando",        # 10
    "pn_faltando_sob_typo",        # 11
    "pn_typo_sob_typo"             # 12 — mais incerto
  )

  scored |>
    filter(!.data$classificacao %in% c("no_match", "pn_typo_sob_typo")) |>
    mutate(ordem_class = match(.data$classificacao, classificacao_ordem)) |>
    arrange(.data$ordem_class, .data$jw_primeiro_nome, .data$jw_sobrenomes) |>
    group_by(.data$nome_original_a) |>
    slice(1) |>
    ungroup() |>
    select(
      nome_original          = "nome_original_a",
      nome_melhor_match      = "nome_original_b",
      primeiro_nome_original = "primeiro_nome_a",
      primeiro_nome_match    = "primeiro_nome_b",
      sobrenomes_original    = "sobrenomes_a",
      sobrenomes_match       = "sobrenomes_b",
      classificacao          = "classificacao",
      jw_primeiro_nome       = "jw_primeiro_nome",
      jw_sobrenomes          = "jw_sobrenomes"
    )
}


