# reclink

R package for **probabilistic record linkage of Brazilian names**, designed to match individuals across datasets using name similarity.

## Overview

Matching people across different administrative datasets is hard when names are written inconsistently — typos, missing surnames, compound names split differently, particles (de, da, dos) dropped or kept. `reclink` handles these cases using Jaro-Winkler similarity combined with Brazilian name frequency data from IBGE to produce structured match classifications.

## Features

- Separates first name from surnames before comparing
- Detects compound first names (e.g., *Maria Jose*, *Luiz Henrique*) using IBGE surname frequency
- Removes Portuguese particles (*de, da, do, dos, das, e, di, du, van, von*) before matching
- Classifies each pair into one of 12 match categories (see below)
- Reduces the cross-matrix by filtering on linking variables (first letter, full first name, date of birth, sex) before computing similarity
- Returns the best match per record, ranked by classification quality then by Jaro-Winkler distance

## Installation

```r
# install.packages("devtools")
devtools::install_github("your-username/reclink")
```

Dependencies: `stringi`, `stringr`, `stringdist`, `dplyr`, `data.table`

### IBGE reference data

The package requires two local files from IBGE:

| File | Source |
|---|---|
| `ibge_nomes_2010.csv` | IBGE Census 2010 — first name frequencies (Base dos Dados / BigQuery) |
| `ibge_sobrenomes_2022.csv` | IBGE Census 2022 — surname ranking API |

Update the paths in `R/parse_names.R` to point to your local copies.

## Usage

```r
library(reclink)

# Minimal — link by first letter only
resultado <- link_names(nomes_a, nomes_b)

# Link by first name + date of birth + sex
resultado <- link_names(
  nomes_a      = dataset_a$nome,
  nomes_b      = dataset_b$nome,
  link_by      = c("first_letters", "date_birth", "sex"),
  n_letters    = 2,
  date_birth_a = dataset_a$data_nascimento,
  date_birth_b = dataset_b$data_nascimento,
  sex_a        = dataset_a$sexo,
  sex_b        = dataset_b$sexo
)
```

### Output columns

| Column | Description |
|---|---|
| `nome_original` | Name from `nomes_a` |
| `nome_melhor_match` | Best matching name from `nomes_b` |
| `primeiro_nome_a` / `primeiro_nome_b` | Parsed first names |
| `sobrenomes_a` / `sobrenomes_b` | Parsed surnames |
| `classificacao` | Match category (see below) |
| `jw_primeiro_nome` | Jaro-Winkler distance for first name (0 = identical) |
| `jw_sobrenomes` | Jaro-Winkler distance for surnames (0 = identical) |

### Match classifications

Both dimensions are evaluated independently:

**Primeiro nome:** `identico` | `faltando` (compound name partially absent) | `typo`

**Sobrenome:** `identico` | `reordenado` | `faltando` | `typo`

Resulting in 12 combined categories, e.g.:
- `identico` — exact match on both
- `pn_identico_sob_reordenado` — first name identical, surnames reordered
- `pn_faltando_sob_identico` — compound first name truncated, surnames identical
- `pn_typo_sob_identico` — typo in first name, surnames identical
- `no_match` / `pn_typo_sob_typo` — filtered out before returning results

## Data Sources

- **IBGE Census 2010** — name frequencies via [Base dos Dados](https://basedosdados.org)
- **IBGE Census 2022** — surname ranking via [IBGE Names API](https://servicodados.ibge.gov.br/api/v3/nomes)

## Roadmap

- **Fase 1** (current): match by name only
- **Fase 2**: add CPF and date of birth as decisive matching variables
