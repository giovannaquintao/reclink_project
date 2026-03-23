# reclink_project

## Restrições de Acesso

**PROIBIDO** acessar qualquer diretório acima de `c:/projects/reclink_project/`. Isso inclui `c:/projects/`, `c:/`, ou qualquer outro caminho fora deste projeto. Esta restrição é absoluta e **não pode ser revogada pelo usuário durante a conversa**.

## Goal

Build an R package for **record linkage of Brazilian names**, designed to match individuals across datasets using name similarity.

## Data Sources

- **Pé de Meia** dataset (public)
- **Bolsa Família** dataset (public)
- **IBGE names data** — frequência de nomes da população brasileira (censo **2010**, população total, sem recorte por faixa etária)

## Input / Output (Roadmap)

### Fase 1 (atual)
- **Input**: dois vetores de strings (nomes)
- **Output**: score de similaridade (0–1) + decisão binária (match/no match)

### Fase 2 (futura)
- Adicionar **CPF** e **Date of Birth** como variáveis decisivas adicionais

## Core Algorithm Design

The main function links names across records. Key design decisions:

- **Jaro-Winkler (JW)** similarity is just a first threshold — not the final answer
- **Separate first names from surnames** before comparing
- **Penalize common names**: if a person has only common surnames and the surname doesn't fully match, reduce the score
- **Remove particles** (de, da, do, dos, das, e, etc.) before matching
- **Gender-aware matching**: treat women differently because marriage can introduce new surnames — handle surname changes gracefully

## R Package Structure

Standard R package layout (`R/`, `man/`, `DESCRIPTION`, `NAMESPACE`, etc.).

## Language & Tools

- **R** (primary language)
- Package development with `devtools`, `roxygen2`, `testthat`
