# reclink

R package for **probabilistic record linkage of Brazilian names**, designed to match individuals across datasets using name similarity.

## Overview

Matching people across different administrative datasets is hard when names are written inconsistently — typos, missing surnames, compound names split differently, particles (de, da, dos) dropped or kept. `reclink` handles these cases using Jaro-Winkler similarity combined with Brazilian name frequency data from IBGE to produce structured match classifications.

## Package structure

```
R/
  parse_names.R     # parse_name(): split full name into first name + surnames
  link_names.R      # link_names(): main record linkage function
  utils_names.R     # normalize_name(), remove_particles(), split_name()
  ibge_freq.R       # frequency helpers for IBGE name data

data/
  ibge_nomes.rda        # IBGE Census 2010 — first name frequencies
  ibge_sobrenomes.rda   # IBGE Census 2022 — surname frequencies
  test/
    dataset_a.csv       # synthetic test data
    dataset_b.csv       # synthetic test data

data-raw/
  prepare_ibge_data.R   # script to regenerate .rda from source CSVs

test_function.R     # quick test script using data/test/
```

## Installation

```r
# install.packages("devtools")
devtools::install_github("giovannaquintao/reclink_project")
```

Dependencies: `stringi`, `stringr`, `stringdist`, `dplyr`, `data.table`

## Usage

```r
library(reclink)

# Link by first letter only (default)
result <- link_names(names_a, names_b)

# Link by first letters + date of birth + sex
result <- link_names(
  names_a      = dataset_a$nome,
  names_b      = dataset_b$nome,
  link_by      = c("first_letters", "date_birth", "sex"),
  n_letters    = 1,
  date_birth_a = dataset_a$data_nascimento,
  date_birth_b = dataset_b$data_nascimento,
  sex_a        = dataset_a$sexo,
  sex_b        = dataset_b$sexo
)
```

### `link_by` options (can be combined)

| Value | Description |
|---|---|
| `"first_letters"` | First N letters of first name (default, use with `n_letters`) |
| `"first_name"` | Exact first name match |
| `"date_birth"` | Date of birth (requires `date_birth_a`, `date_birth_b`) |
| `"sex"` | Sex (requires `sex_a`, `sex_b`) |

### Output columns

| Column | Description |
|---|---|
| `original` | Name from `names_a` |
| `best_match` | Best matching name from `names_b` |
| `first_name` / `first_name_match` | Parsed first names |
| `surnames` / `surnames_match` | Parsed surnames |
| `classification` | Match category (see below) |
| `jw_first_name` | Jaro-Winkler distance for first name (0 = identical) |
| `jw_surnames` | Jaro-Winkler distance for surnames (0 = identical) |

### Match classifications

First name and surname are evaluated independently:

**First name:** `identical` | `missing` (compound name partially absent) | `typo`

**Surname:** `identical` | `reordered` | `missing` | `typo`

Combined into 12 categories ranked by quality:

| Classification | Meaning |
|---|---|
| `fn_identical_sur_identical` | Exact match on both dimensions |
| `fn_identical_sur_reordered` | First name identical, surnames in different order |
| `fn_identical_sur_missing` | First name identical, one side has fewer surnames |
| `fn_identical_sur_typo` | First name identical, small surname difference |
| `fn_missing_sur_identical` | Compound first name truncated, surnames identical |
| `fn_typo_sur_identical` | Typo in first name, surnames identical |
| `fn_missing_sur_reordered` | ... |
| `fn_typo_sur_reordered` | ... |
| `fn_missing_sur_missing` | ... |
| `fn_typo_sur_missing` | ... |
| `fn_missing_sur_typo` | ... |
| `fn_typo_sur_typo` | Excluded from output |

Pairs classified as `no_match` or `fn_typo_sur_typo` are excluded from the output.

## Data sources

- **IBGE Census 2010** — first name frequencies via [Base dos Dados](https://basedosdados.org)
- **IBGE Census 2022** — surname ranking via [IBGE Names API](https://servicodados.ibge.gov.br/api/v3/nomes)

To regenerate the package data from source:
```r
source("data-raw/prepare_ibge_data.R")
```

## Roadmap

- **Phase 1** (current): match by name only
- **Phase 2**: add CPF and date of birth as decisive matching variables
