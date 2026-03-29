# reclink

R package for **probabilistic record linkage of Brazilian names**, designed to match individuals across datasets using name similarity.

## Overview

Matching people across different administrative datasets is hard when names are written inconsistently — typos, missing surnames, compound names split differently, particles (de, da, dos) dropped or kept. `reclink` handles these cases using Jaro-Winkler similarity combined with Brazilian name frequency data from IBGE to produce structured match classifications.

## Package structure

```
R/
  parse_names.R     # parse_name(): split full name into first name + surnames
  link_names.R      # link_names(): main record linkage function
  utils_names.R     # normalize_name(), remove_particles(), split_name(), jw_sim()
  ibge_freq.R       # download_ibge_names(), load_ibge_names(), frequency helpers

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
devtools::install_github("your-username/reclink")
```

Dependencies: `stringi`, `stringr`, `stringdist`, `dplyr`, `data.table`

## Usage

```r
library(reclink)

# Link by first letter only
resultado <- link_names(nomes_a, nomes_b)

# Link by first letters + date of birth + sex
resultado <- link_names(
  nomes_a      = dataset_a$nome,
  nomes_b      = dataset_b$nome,
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

Combined into 12 categories, e.g.:
- `identico` — exact match on both dimensions
- `pn_identico_sob_reordenado` — first name identical, surnames reordered
- `pn_faltando_sob_identico` — compound first name truncated, surnames identical
- `pn_typo_sob_identico` — typo in first name, surnames identical
- `no_match` / `pn_typo_sob_typo` — excluded from output

## Data sources

- **IBGE Census 2010** — first name frequencies via [Base dos Dados](https://basedosdados.org)
- **IBGE Census 2022** — surname ranking via [IBGE Names API](https://servicodados.ibge.gov.br/api/v3/nomes)

To regenerate the package data from source:
```r
source("data-raw/prepare_ibge_data.R")
```

## Roadmap

- **Fase 1** (current): match by name only
- **Fase 2**: add CPF and date of birth as decisive matching variables
