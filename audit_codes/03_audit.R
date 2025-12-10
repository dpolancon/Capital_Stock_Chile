############################################################
## 03_audit_Ig.R — Auditor especializado para Ig (2003 CLP)
## Stock–flow consistent capital stock reconstruction (Chile)
############################################################

library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(here)

dir_int <- here("data","interim")

Ig_file      <- file.path(dir_int, "Ig_2003.rds")
add_file     <- file.path(dir_int, "Ig_2003_additivity_residuals.rds")

raw_AD_file  <- file.path(dir_int, "raw_AD.rds")
shares_file  <- file.path(dir_int, "raw_hofman_Ig.rds")

############################################################
## Helper: detect jumps > k * median(abs(diff))
############################################################
detect_jumps <- function(x, k = 20) {
  dx <- diff(x)
  thr <- median(abs(dx), na.rm = TRUE) * k
  which(abs(dx) > thr)
}

############################################################
## AUDIT
############################################################

cat("\n==============================\n")
cat("      AUDIT 03: Ig_2003\n")
cat("==============================\n\n")

if (!file.exists(Ig_file)) stop("Ig_2003.rds no encontrado.")
if (!file.exists(add_file)) stop("Ig_2003_additivity_residuals.rds no encontrado.")

Ig <- readRDS(Ig_file)
res <- readRDS(add_file)
raw_AD <- readRDS(raw_AD_file)
raw_hof <- readRDS(shares_file)

############################################################
## 1. Estructura básica
############################################################
cat("Rows:", nrow(Ig), "\n")
cat("Years:", min(Ig$year), "→", max(Ig$year), "\n")

assets <- unique(Ig$asset)
cat("Assets:", paste(assets, collapse=", "), "\n\n")

############################################################
## 2. Check NA values
############################################################
na_n <- sum(is.na(Ig$value))
cat("NAs in Ig:", na_n, "\n")
if (na_n > 0) cat("⚠ WARNING: Ig contiene NAs.\n\n")

############################################################
## 3. Check additivity (res already computed)
############################################################
cat("\n--- RESULTADOS ADITIVIDAD (NR, C, T) ---\n")

summary_res <- res %>%
  summarise(
    max_abs_NR = max(abs(res_NR), na.rm=TRUE),
    max_abs_C  = max(abs(res_C),  na.rm=TRUE),
    max_abs_T  = max(abs(res_T),  na.rm=TRUE)
  )

print(summary_res)

if (any(summary_res > 1e-6)) {
  cat("⚠ WARNING: residuos de aditividad no triviales.\n")
} else {
  cat("✔ Aditividad perfecta dentro de tolerancia numérica.\n")
}

############################################################
## 4. Check consistency with AD benchmark
############################################################
cat("\n--- CONSISTENCIA CON AD (FBKF maquinaria y construcción) ---\n")

# Variables AD originales
ad_var_Ig_ME <- "FBKF en maquinaria"
ad_var_Ig_C  <- "FBKF en construcción"

AD_panel <- raw_AD %>%
  filter(var %in% c(ad_var_Ig_ME, ad_var_Ig_C)) %>%
  select(year, var, value) %>%
  pivot_wider(names_from = var, values_from = value)

Ig_check <- Ig %>%
  filter(asset %in% c("ME","C")) %>%
  select(year, asset, value) %>%
  pivot_wider(names_from = asset, values_from = value)

merged <- AD_panel %>%
  left_join(Ig_check, by = "year")

merged <- merged %>%
  mutate(
    diff_ME = `FBKF en maquinaria` - ME,
    diff_C  = `FBKF en construcción` - C
  )

cat("Max |diff_ME|:", max(abs(merged$diff_ME), na.rm=TRUE), "\n")
cat("Max |diff_C| :", max(abs(merged$diff_C),  na.rm=TRUE), "\n")

if (max(abs(merged$diff_ME), na.rm=TRUE) < 1e-6 &&
    max(abs(merged$diff_C), na.rm=TRUE)  < 1e-6) {
  cat("✔ Ig_ME y Ig_C replican EXACTAMENTE los valores AD.\n")
} else {
  cat("⚠ WARNING: Ig_ME o Ig_C NO coinciden con AD.\n")
}

############################################################
## 5. Check Hofman shares consistency
############################################################
cat("\n--- SHARES HOFMAN (s_NRC, s_RC) ---\n")

shares <- raw_hof %>%
  filter(asset %in% c("NRC","RC")) %>%
  select(year, asset, value) %>%
  pivot_wider(names_from = asset, values_from = value) %>%
  mutate(
    total = NRC + RC,
    s_NRC = NRC / total,
    s_RC  = RC / total
  )

cat("Shares disponibles años:", min(shares$year), "→", max(shares$year), "\n")

if (any(shares$total <= 0, na.rm=TRUE)) {
  cat("⚠ WARNING: shares Hofman con total <= 0.\n")
} else {
  cat("✔ Shares Hofman con totales positivos.\n")
}

############################################################
## 6. Check jumps / discontinuities in Ig
############################################################
cat("\n--- DETECCIÓN DE SALTOS EN Ig (por activo) ---\n")

jumps <- Ig %>%
  group_by(asset) %>%
  arrange(year) %>%
  summarise(
    pos = list(detect_jumps(value))
  )

print(jumps)

has_jumps <- jumps %>% filter(lengths(pos) > 0)

if (nrow(has_jumps) == 0) {
  cat("✔ No se detectan saltos estructurales sospechosos.\n")
} else {
  cat("⚠ WARNING: se detectan saltos > 20×mediana(abs(ΔIg)). Revisar años indicados.\n")
}

############################################################
## 7. Year consistency check
############################################################
cat("\n--- CONSISTENCIA DE RANGO DE AÑOS ---\n")

if (setequal(unique(Ig$year), unique(raw_AD$year))) {
  cat("✔ Años Ig coinciden EXACTAMENTE con años AD.\n")
} else {
  cat("⚠ WARNING: rango de años Ig NO coincide con AD.\n")
  cat("Años en AD pero no en Ig:\n")
  print(setdiff(unique(raw_AD$year), unique(Ig$year)))
  cat("\nAños en Ig pero no en AD:\n")
  print(setdiff(unique(Ig$year), unique(raw_AD$year)))
}

cat("\n\n>>> AUDITORÍA 03 COMPLETADA <<<\n")
