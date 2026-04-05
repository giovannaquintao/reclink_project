library(dplyr)
library(stringr)
library(stringdist)

utils::globalVariables(c(
  "first_name_a", "first_name_b", "surnames_a", "surnames_b",
  "clean_surnames_a", "clean_surnames_b", "clean_a", "clean_b",
  "jw_first_name"
))

split_surnames <- function(surnames) {
  if (is.na(surnames)) character(0) else str_split(surnames, " ")[[1]]
}

jw_first <- function(a, b) {
  stringdist(a, b, method = "jw", p = 0.1)
}

jw_mean_surnames <- function(sur_a, sur_b) {
  tok_a <- split_surnames(sur_a)
  tok_b <- split_surnames(sur_b)
  if (length(tok_a) == 0 || length(tok_b) == 0) return(NA_real_)
  scores <- sapply(tok_a, function(ta) {
    min(stringdist(ta, tok_b, method = "jw", p = 0.1))
  })
  mean(scores)
}

jw_complete_surnames <- function(clean_sur_a, clean_sur_b) {
  if (is.na(clean_sur_a) || is.na(clean_sur_b)) return(NA_real_)
  stringdist(clean_sur_a, clean_sur_b, method = "jw", p = 0.1)
}

jw_complete <- function(clean_a, clean_b) {
  if (is.na(clean_a) || is.na(clean_b)) return(NA_real_)
  stringdist(clean_a, clean_b, method = "jw", p = 0.1)
}

classify_match <- function(fn_a, sur_a, fn_b, sur_b, jw_fn, jw_sur) {
  tok_a_raw <- split_surnames(sur_a)
  tok_b_raw <- split_surnames(sur_b)
  tok_a     <- sort(tok_a_raw)
  tok_b     <- sort(tok_b_raw)

  sur_identical <- identical(tok_a_raw, tok_b_raw)
  sur_reordered <- identical(tok_a, tok_b) && !sur_identical
  sur_missing   <- length(tok_a) != length(tok_b) &&
    (all(tok_a %in% tok_b) || all(tok_b %in% tok_a))
  sur_typo      <- !sur_identical && !sur_reordered && !sur_missing &&
    !is.na(jw_sur) && jw_sur <= 0.15

  fn_tok_a     <- str_split(trimws(fn_a), " ")[[1]]
  fn_tok_b     <- str_split(trimws(fn_b), " ")[[1]]
  fn_identical <- fn_a == fn_b
  fn_missing   <- !fn_identical &&
    length(fn_tok_a) != length(fn_tok_b) &&
    (all(fn_tok_a %in% fn_tok_b) || all(fn_tok_b %in% fn_tok_a))
  fn_typo      <- !fn_identical && !fn_missing &&
    !is.na(jw_fn) && jw_fn <= 0.1

  dplyr::case_when(
    fn_identical & sur_identical  ~ "fn_identical_sur_identical",
    fn_identical & sur_reordered  ~ "fn_identical_sur_reordered",
    fn_identical & sur_missing    ~ "fn_identical_sur_missing",
    fn_identical & sur_typo       ~ "fn_identical_sur_typo",
    fn_missing   & sur_identical  ~ "fn_missing_sur_identical",
    fn_missing   & sur_reordered  ~ "fn_missing_sur_reordered",
    fn_missing   & sur_missing    ~ "fn_missing_sur_missing",
    fn_missing   & sur_typo       ~ "fn_missing_sur_typo",
    fn_typo      & sur_identical  ~ "fn_typo_sur_identical",
    fn_typo      & sur_reordered  ~ "fn_typo_sur_reordered",
    fn_typo      & sur_missing    ~ "fn_typo_sur_missing",
    fn_typo      & sur_typo       ~ "fn_typo_sur_typo",
    TRUE                          ~ "no_match"
  )
}

#' Link two vectors of names
#'
#' @param names_a      Character vector of names (source dataset)
#' @param names_b      Character vector of names (target dataset)
#' @param link_by      Blocking variables. Options: "first_letters",
#'                     "first_name", "date_birth", "sex"
#' @param n_letters    Number of leading letters for "first_letters" blocking
#' @param date_birth_a Date of birth vector for names_a
#' @param date_birth_b Date of birth vector for names_b
#' @param sex_a        Sex vector for names_a
#' @param sex_b        Sex vector for names_b
#'
#' @return data.frame with one row per name in names_a (best match)
link_names <- function(names_a, names_b,
                       link_by      = "first_letters",
                       n_letters    = 1,
                       date_birth_a = NULL,
                       date_birth_b = NULL,
                       sex_a        = NULL,
                       sex_b        = NULL) {

  valid_link_by <- c("first_letters", "first_name", "date_birth", "sex")
  unknown <- setdiff(link_by, valid_link_by)
  if (length(unknown) > 0)
    stop("Invalid link_by: ", paste(unknown, collapse = ", "),
         ". Options: ", paste(valid_link_by, collapse = ", "))

  if ("date_birth" %in% link_by &&
        (is.null(date_birth_a) || is.null(date_birth_b)))
    stop("date_birth_a and date_birth_b required when ",
         "link_by includes 'date_birth'")

  if ("sex" %in% link_by && (is.null(sex_a) || is.null(sex_b)))
    stop("sex_a and sex_b required when link_by includes 'sex'")

  # --- format checks --------------------------------------------------------
  if ("date_birth" %in% link_by) {
    cls_a <- class(date_birth_a)
    cls_b <- class(date_birth_b)
    if (!identical(cls_a, cls_b))
      stop("date_birth_a and date_birth_b have different classes: ",
           paste(cls_a, collapse = "/"), " vs ", paste(cls_b, collapse = "/"),
           ". Convert both to the same type before linking.")

    if (inherits(date_birth_a, "character")) {
      sample_a <- na.omit(date_birth_a)[1]
      sample_b <- na.omit(date_birth_b)[1]
      fmt_a <- if (grepl("^\\d{4}-\\d{2}-\\d{2}$", sample_a)) "YYYY-MM-DD"
        else if (grepl("^\\d{2}/\\d{2}/\\d{4}$", sample_a)) "DD/MM/YYYY"
        else "unknown"
      fmt_b <- if (grepl("^\\d{4}-\\d{2}-\\d{2}$", sample_b)) "YYYY-MM-DD"
        else if (grepl("^\\d{2}/\\d{2}/\\d{4}$", sample_b)) "DD/MM/YYYY"
        else "unknown"
      if (fmt_a != fmt_b)
        stop("date_birth_a and date_birth_b appear to use different formats: ",
             fmt_a, " vs ", fmt_b, ". Standardise both before linking.")
    }
  }

  if ("sex" %in% link_by) {
    vals_a <- sort(unique(na.omit(sex_a)))
    vals_b <- sort(unique(na.omit(sex_b)))
    if (!identical(vals_a, vals_b))
      warning("sex_a and sex_b have different value sets: ",
              "(", paste(vals_a, collapse = ", "), ")",
              " vs ",
              "(", paste(vals_b, collapse = ", "), ")",
              ". No pairs will match across mismatched categories.")
  }
  # --------------------------------------------------------------------------

  parsed_a <- bind_rows(lapply(names_a, parse_name))
  parsed_b <- bind_rows(lapply(names_b, parse_name))

  join_keys <- character(0)

  if ("first_letters" %in% link_by) {
    parsed_a$name_key <- substr(parsed_a$first_name, 1, n_letters)
    parsed_b$name_key <- substr(parsed_b$first_name, 1, n_letters)
    join_keys <- c(join_keys, "name_key")
  } else if ("first_name" %in% link_by) {
    parsed_a$name_key <- parsed_a$first_name
    parsed_b$name_key <- parsed_b$first_name
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

  pairs <- inner_join(
    parsed_a, parsed_b,
    by           = join_keys,
    suffix       = c("_a", "_b"),
    relationship = "many-to-many"
  )

  scored <- pairs |>
    rowwise() |>
    mutate(
      jw_first_name        = jw_first(first_name_a, first_name_b),
      jw_mean_surnames     = jw_mean_surnames(surnames_a, surnames_b),
      jw_complete_surnames = jw_complete_surnames(clean_surnames_a, clean_surnames_b),
      jw_complete          = jw_complete(clean_a, clean_b),
      classification       = classify_match(
        first_name_a, surnames_a,
        first_name_b, surnames_b,
        jw_first_name, jw_mean_surnames
      )
    ) |>
    ungroup()

  class_order <- c(
    "fn_identical_sur_identical",
    "fn_identical_sur_reordered",
    "fn_identical_sur_missing",
    "fn_identical_sur_typo",
    "fn_missing_sur_identical",
    "fn_typo_sur_identical",
    "fn_missing_sur_reordered",
    "fn_typo_sur_reordered",
    "fn_missing_sur_missing",
    "fn_typo_sur_missing",
    "fn_missing_sur_typo",
    "fn_typo_sur_typo"
  )

  scored |>
    filter(!.data$classification %in% c("no_match", "fn_typo_sur_typo")) |>
    mutate(class_rank = match(.data$classification, class_order)) |>
    arrange(.data$class_rank, .data$jw_first_name, .data$jw_mean_surnames) |>
    group_by(.data$original_a) |>
    slice(1) |>
    ungroup() |>
    select(
      original             = "original_a",
      best_match           = "original_b",
      first_name           = "first_name_a",
      first_name_match     = "first_name_b",
      surnames             = "surnames_a",
      surnames_match       = "surnames_b",
      classification       = "classification",
      jw_first_name        = "jw_first_name",
      jw_mean_surnames     = "jw_mean_surnames",
      jw_complete_surnames = "jw_complete_surnames",
      jw_complete          = "jw_complete"
    )
}
