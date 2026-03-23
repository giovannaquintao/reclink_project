rm(list=ls())
gc()

compare_names <- function(name1, name2,
                             method    = "jw",
                             threshold = 0.15) {
  
  
  # Portuguese/Spanish name particles to ignore
  particles <- c("de", "da", "do", "dos", "das")
  
  clean <- function(x) {
    tokens <- x |> trimws() |> tolower() |>
      iconv(to = "ASCII//TRANSLIT") |>
      gsub("[^a-z ]", "", x = _) |>
      strsplit("\\s+") 
    
    # Remove particles
    tokens[[1]][!tokens[[1]] %in% particles]
  }
  
  tokens1 <- clean(name1)
  tokens2 <- clean(name2)
  
  dist_mat <- outer(tokens1, tokens2,
                    Vectorize(function(a, b) stringdist(a, b, method = method)))
  rownames(dist_mat) <- tokens1
  colnames(dist_mat) <- tokens2
  
  # Greedy best-match
  matched1 <- rep(FALSE, length(tokens1))
  matched2 <- rep(FALSE, length(tokens2))
  pairs    <- list()
  
  cells <- which(!is.na(dist_mat), arr.ind = TRUE)
  cells <- cells[order(dist_mat[cells]), , drop = FALSE]
  
  for (k in seq_len(nrow(cells))) {
    i <- cells[k, 1]; j <- cells[k, 2]
    if (!matched1[i] && !matched2[j]) {
      pairs[[length(pairs) + 1]] <- list(
        i = i, j = j,
        token1 = tokens1[i],
        token2 = tokens2[j],
        dist   = dist_mat[i, j]
      )
      matched1[i] <- TRUE
      matched2[j] <- TRUE
    }
  }
  
  # ── NEW: detect reordering ──────────────────────────────────────────────────
  # A pair is "reordered" if its relative position differs from the other pairs
  matched_pairs <- pairs[sapply(pairs, function(p) p$dist <= threshold)]
  is_reordered  <- FALSE
  
  if (length(matched_pairs) >= 2) {
    i_order <- sapply(matched_pairs, `[[`, "i")   # positions in name1
    j_order <- sapply(matched_pairs, `[[`, "j")   # positions in name2
    # If rank orders don't agree → some tokens swapped
    is_reordered <- !identical(rank(i_order), rank(j_order))
  }
  
  # Classify
  classify <- function(d) {
    if (d == 0)         return("exact_match")
    if (d <= threshold)  return("misspelled")
    return("different")
  }
  
  rows <- lapply(pairs, function(p) {
    base_status <- classify(p$dist)
    # Tag as reordered only for matched (non-different) tokens when swap detected
    status <- if (is_reordered && base_status != "different") {
      paste0(base_status, "+reordered")   # e.g. "exact_match+reordered"
    } else {
      base_status
    }
    data.frame(token_name1 = p$token1, token_name2 = p$token2,
               pos_name1 = p$i, pos_name2 = p$j,
               distance = round(p$dist, 4), status = status,
               stringsAsFactors = FALSE)
  })
  
  unmatched1 <- tokens1[!matched1]
  if (length(unmatched1) > 0)
    rows <- c(rows, lapply(unmatched1, function(t)
      data.frame(token_name1 = t, token_name2 = NA_character_,
                 pos_name1 = NA_integer_, pos_name2 = NA_integer_,
                 distance = NA_real_, status = "missing_in_name2",
                 stringsAsFactors = FALSE)))
  
  unmatched2 <- tokens2[!matched2]
  if (length(unmatched2) > 0)
    rows <- c(rows, lapply(unmatched2, function(t)
      data.frame(token_name1 = NA_character_, token_name2 = t,
                 pos_name1 = NA_integer_, pos_name2 = NA_integer_,
                 distance = NA_real_, status = "missing_in_name1",
                 stringsAsFactors = FALSE)))
  
  detail <- do.call(rbind, rows)
  
  # Verdict
  statuses <- detail$status
  verdict <- dplyr::case_when(
    any(statuses == "different")                                        ~ "different",
    all(statuses == "exact_match")                                      ~ "identical",
    all(statuses == "exact_match+reordered")                           ~ "reordered",
    any(statuses %in% c("missing_in_name1", "missing_in_name2")) &
      is_reordered                                                       ~ "missing_token+reordered",
    any(statuses %in% c("missing_in_name1", "missing_in_name2"))       ~ "missing_token",
    any(grepl("reordered", statuses)) & any(grepl("misspelled", statuses)) ~ "misspelled+reordered",
    any(grepl("reordered", statuses))                                   ~ "reordered",
    
    any(grepl("misspelled", statuses))                                  ~ "misspelled",
    TRUE                                                                 ~ "identical"
  )
  
  list(verdict = verdict, detail = detail, is_reordered = is_reordered,
       name1 = name1, name2 = name2)
}

library(tidyverse)
library(stringdist)



address <- ifelse(Sys.info()[["user"]] == "giova", "C:/GPQDrive/servidor/", "")

df<-read_csv(paste0(address,"database/matrix_to_reclink.csv"))

df2<-df %>% 
  #slice_sample(n = 1000) %>% 
  rowwise() %>%
  mutate(
    comparison  = list(compare_names(student_name,name_student)),
    verdict     = comparison$verdict,
    is_reordered = comparison$is_reordered,
    detail      = list(comparison$detail)
  ) %>% 
  ungroup() %>% 
  filter(verdict != "different") 


write_csv(df2,paste0(address,"results/matrix_to_reclink_comparison.csv"))  



