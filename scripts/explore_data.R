library(data.table)
library(basedosdados)

# ── helpers ──────────────────────────────────────────────────────────────────
peek <- function(label, dt) {
  cat("\n", strrep("=", 60), "\n")
  cat(">>> ", label, "\n")
  cat(strrep("=", 60), "\n")
  cat("Dimensoes: ", nrow(dt), "linhas x", ncol(dt), "colunas\n\n")
  cat("Colunas:\n")
  print(names(dt))
  cat("\nEstrutura:\n")
  print(str(dt))
  cat("\nPrimeiras 5 linhas:\n")
  print(head(dt, 5))
}

# ── Pe de Meia ────────────────────────────────────────────────────────────────
pe_de_meia <- fread(
  "C:/Users/giova/OneDrive/raw_data/inep/pe_de_meia/2025_new/202501_PeDeMeia.csv",
  nrows = 1000,   # lê só 1000 linhas para explorar
  encoding = "Latin-1"
)
peek("Pe de Meia", pe_de_meia)

# ── Bolsa Familia ─────────────────────────────────────────────────────────────
bolsa_familia <- fread(
  "C:/Users/giova/OneDrive/raw_data/bolsa_familia/202101_BolsaFamilia_Pagamentos.csv",
  nrows = 1000,
  encoding = "Latin-1"
)
peek("Bolsa Familia", bolsa_familia)

# ── TSE ───────────────────────────────────────────────────────────────────────
cat("\n", strrep("=", 60), "\n")
cat(">>> TSE — arquivos na pasta\n")
cat(strrep("=", 60), "\n")
tse_files <- list.files(
  "C:/Users/giova/OneDrive/raw_data/TSE",
  recursive = TRUE,
  full.names = TRUE
)
print(tse_files)

sob <- read.csv("C:/Users/giova/OneDrive/raw_data/ibge/names/ibge_sobrenomes_2022.csv")

table(sob$page)
cat("Linhas:", nrow(sob), "\n")
cat("Colunas:", names(sob), "\n")
print(head(sob, 20))
