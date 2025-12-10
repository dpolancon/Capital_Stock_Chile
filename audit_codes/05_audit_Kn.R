############################################################
## 05_audit_Kn.R — Auditor del stock neto Kn_2003
## Proyecto: Reconstrucción SFC del acervo de capital (Chile)
############################################################

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(here)

dir_int <- here("data","interim")

Kn_file  <- file.path(dir_int, "Kn_2003.rds")
add_file <- file.path(dir_int, "Kn_2003_additivity_residuals.rds")

############################################################
## Helper: detectar saltos grandes (20× mediana abs(diff))
############################################################
detect_jumps <- function(x, k = 20) {
  dx  <- diff(x)
  thr <- median(abs(dx), na.rm = TRUE) * k
  which(abs(dx) > thr)
}

############################################################
## 1. Cargar archivos
############################################################

if (!file.exists(Kn_file)) stop("Kn_2003.rds no encontrado.")
if (!file.exists(add_file)) stop("Kn_2003_additivity_residuals.rds no encontrado.")

Kn      <- readRDS(Kn_file)
Kn_add  <- readRDS(add_file)

cat("\n==============================\n")
cat("      AUDITORÍA 05: Kn_2003\n")
cat("==============================\n\n")

############################################################
## 2. Estructura básica
############################################################

cat("Filas:", nrow(Kn), "\n")
cat("Años :", min(Kn$year), "→", max(Kn$year), "\n")

assets <- sort(unique(Kn$asset))
cat("Activos:", paste(assets, collapse=", "), "\n\n")

############################################################
## 3. Columnas requeridas
############################################################

required <- c("year","asset","var","value","price_base","source")
missing  <- setdiff(required, names(Kn))

if (length(missing) == 0) {
  cat("✔ Todas las columnas requeridas están presentes.\n\n")
} else {
  cat("❌ Faltan columnas:", paste(missing, collapse=", "), "\n\n")
}

############################################################
## 4. NA y numéricos
############################################################

na_count <- sum(is.na(Kn$value))
cat("NAs en value:", na_count, "\n")
if (na_count > 0) cat("⚠ WARNING: existen valores NA en Kn.\n")

if (!is.numeric(Kn$value)) {
  cat("❌ ERROR: value no es numérico.\n\n")
} else {
  cat("✔ value es numérico.\n\n")
}

############################################################
## 5. Panel rectangular
############################################################

panel_test <- Kn %>%
  count(year, asset) %>%
  tidyr::pivot_wider(names_from = asset, values_from = n, values_fill = 0)

incomplete_years <- panel_test %>%
  filter(dplyr::if_any(-year, ~ .x == 0)) %>%
  pull(year)

if (length(incomplete_years) == 0) {
  cat("✔ Panel rectangular completo (todos los activos presentes en todos los años).\n\n")
} else {
  cat("⚠ WARNING: años con activos faltantes:\n")
  print(incomplete_years)
  cat("\n")
}

############################################################
## 6. Aditividad NR, C, T
############################################################

cat("--- Resumen aditividad ---\n")
print(Kn_add)

res_vals <- Kn_add %>%
  summarise(
    max_NR = max(abs(res_NR), na.rm=TRUE),
    max_C  = max(abs(res_C),  na.rm=TRUE),
    max_T  = max(abs(res_T),  na.rm=TRUE)
  )

print(res_vals)

if (all(abs(res_vals) < 1e-6)) {
  cat("✔ Aditividad perfecta dentro de tolerancia numérica.\n\n")
} else {
  cat("⚠ WARNING: hay violaciones de aditividad significativas.\n\n")
}

############################################################
## 7. Saltos grandes por activo
############################################################

cat("--- Saltos temporales (ΔKn > 20× mediana abs(diff)) ---\n\n")

jumps <- Kn %>%
  group_by(asset) %>%
  arrange(year) %>%
  summarise(jumps = list(detect_jumps(value)), .groups = "drop")

print(jumps)

jumped <- jumps %>% filter(lengths(jumps) > 0)

if (nrow(jumped) == 0) {
  cat("✔ No se detectan saltos anómalos.\n\n")
} else {
  cat("⚠ WARNING: activos con cambios abruptos:\n")
  print(jumped)
  cat("\n")
}

############################################################
## 8. Final
############################################################

cat("\n>>> AUDITORÍA 05 COMPLETADA <<<\n")
############################################################
