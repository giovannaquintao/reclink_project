utils::globalVariables(c(
  "surnames_a", "surnames_b", "clean_sur_a", "clean_sur_b"
))

parse_surname_only <- function(surname) {
  clean  <- normalize(surname)
  no_par <- strip_particles(clean)
  tokens <- str_split(no_par, " ")[[1]]
  tokens <- tokens[nchar(tokens) > 0]

  list(
    original       = surname,
    clean          = clean,
    surnames       = if (length(tokens) == 0) NA_character_
      else paste(tokens, collapse = " "),
    clean_surnames = if (nchar(clean) == 0) NA_character_ else clean
  )
}

classify_surname_match <- function(sur_a, sur_b, jw_sur) {
  tok_a_raw <- split_surnames(sur_a)
  tok_b_raw <- split_surnames(sur_b)
  tok_a     <- sort(tok_a_raw)
  tok_b     <- sort(tok_b_raw)

  toks_match           <- identical(tok_a_raw, tok_b_raw)
  sur_identical        <- toks_match && has_rare_surname(tok_a_raw)
  sur_identical_common <- toks_match && !sur_identical
  sur_reordered        <- identical(tok_a, tok_b) && !toks_match
  shared        <- intersect(tok_a, tok_b)
  extra         <- setdiff(
    if (length(tok_a) > length(tok_b)) tok_a else tok_b,
    if (length(tok_a) > length(tok_b)) tok_b else tok_a
  )
  sur_missing   <- length(tok_a) != length(tok_b) &&
    (all(tok_a %in% tok_b) || all(tok_b %in% tok_a)) &&
    (has_rare_surname(shared) || length(extra) == 1)
  sur_typo      <- !sur_identical && !sur_reordered && !sur_missing &&
    !is.na(jw_sur) && jw_sur <= 0.15 &&
    !any_known_surname_pair(tok_a, tok_b)
  sur_different <- !sur_identical && !sur_reordered && !sur_missing &&
    !sur_typo && length(tok_a) > 0 && length(tok_b) > 0

  dplyr::case_when(
    sur_identical        ~ "sur_identical",
    sur_reordered        ~ "sur_reordered",
    sur_missing          ~ "sur_missing",
    sur_typo             ~ "sur_typo",
    sur_identical_common ~ "sur_identical_common",
    sur_different        ~ "sur_different",
    TRUE                 ~ "no_match"
  )
}

#' Link two vectors of surnames
#'
#' @param surnames_a   Character vector of surnames (source dataset)
#' @param surnames_b   Character vector of surnames (target dataset)
#' @param id_a         Identifier vector for surnames_a
#' @param id_b         Identifier vector for surnames_b
#' @param link_by      Blocking variables. Options: "first_letters",
#'                     "date_birth", "sex"
#' @param n_letters    Number of leading letters for "first_letters" blocking
#' @param date_birth_a Date of birth vector for surnames_a
#' @param date_birth_b Date of birth vector for surnames_b
#' @param sex_a        Sex vector for surnames_a
#' @param sex_b        Sex vector for surnames_b
#'
#' @return data.frame with one row per surname in surnames_a (best match)
link_surnames <- function(surnames_a, surnames_b,
                          id_a         = NULL,
                          id_b         = NULL,
                          link_by      = "first_letters",
                          n_letters    = 1,
                          date_birth_a = NULL,
                          date_birth_b = NULL,
                          sex_a        = NULL,
                          sex_b        = NULL) {

  valid_link_by <- c("first_letters", "date_birth", "sex")
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
           paste(cls_a, collapse = "/"), " vs ", paste(cls_b, collapse = "/"))

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
        stop("date_birth formats differ: ", fmt_a, " vs ", fmt_b)
    }
  }

  if ("sex" %in% link_by) {
    vals_a <- sort(unique(na.omit(sex_a)))
    vals_b <- sort(unique(na.omit(sex_b)))
    if (!identical(vals_a, vals_b))
      warning("sex_a and sex_b have different value sets: ",
              "(", paste(vals_a, collapse = ", "), ")",
              " vs ",
              "(", paste(vals_b, collapse = ", "), ")")
  }
  # --------------------------------------------------------------------------

  parsed_a <- bind_rows(lapply(surnames_a, parse_surname_only))
  parsed_b <- bind_rows(lapply(surnames_b, parse_surname_only))

  if (!is.null(id_a)) parsed_a$id <- id_a
  if (!is.null(id_b)) parsed_b$id <- id_b

  join_keys <- character(0)

  if ("first_letters" %in% link_by) {
    parsed_a$name_key <- substr(parsed_a$surnames, 1, n_letters)
    parsed_b$name_key <- substr(parsed_b$surnames, 1, n_letters)
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
      jw_mean_surnames     = jw_mean_surnames(surnames_a, surnames_b),
      jw_complete_surnames = jw_complete_surnames(clean_sur_a, clean_sur_b),
      classification       = classify_surname_match(
        surnames_a, surnames_b, jw_mean_surnames
      )
    ) |>
    ungroup()

  class_order <- c(
    "sur_identical",
    "sur_reordered",
    "sur_missing",
    "sur_typo",
    "sur_identical_common",
    "sur_different"
  )

  scored |>
    filter(.data$classification != "no_match") |>
    mutate(class_rank = match(.data$classification, class_order)) |>
    arrange(.data$class_rank, .data$jw_mean_surnames) |>
    group_by(.data$original_a) |>
    slice(1) |>
    ungroup() |>
    mutate(score = dplyr::case_when(
      .data$classification == "sur_identical"        ~ 10,
      .data$classification == "sur_reordered"        ~  7,
      .data$classification == "sur_identical_common" ~  6,
      .data$classification == "sur_missing"          ~  5,
      .data$classification == "sur_typo"             ~  3,
      .data$classification == "sur_different"        ~  0
    )) |>
    select(
      any_of(c(id_a = "id_a", id_b = "id_b")),
      original             = "original_a",
      best_match           = "original_b",
      surnames             = "surnames_a",
      surnames_match       = "surnames_b",
      classification       = "classification",
      score                = "score",
      jw_mean_surnames     = "jw_mean_surnames",
      jw_complete_surnames = "jw_complete_surnames"
    )
}
