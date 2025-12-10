############################################################
## 04_audit_Kg.R — Auditor del stock bruto Kg_2003
## Proyecto: Reconstrucción SFC del acervo de capital (Chile)
############################################################

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(here)

dir_int <- here("data","interim")

Kg_file  <- file.path(dir_int, "Kg_2003.rds")
add_file <- file.path(dir_int, "Kg_2003_additivity_residuals.rds")

anchor_file <- file.path(dir_int, "anchor_1950.rds")
TD_file     <- file.path(dir_int, "raw_TD_indices.rds")


############################################################
## Helper: detectar saltos grandes (20× mediana abs(diff))
############################################################
detect_jumps <- function(x, k = 20) {
  dx <- diff(x)
  thr <- median(abs(dx), na.rm = TRUE) * k
  which(abs(dx) > thr)
}


############################################################
## 1. Cargar archivos
############################################################

if (!file.exists(Kg_file)) stop("Kg_2003.rds no encontrado.")
if (!file.exists(add_file)) stop("Kg_2003_additivity_residuals.rds no encontrado.")

Kg <- readRDS(Kg_file)
Kg_add <- readRDS(add_file)

cat("\n==============================\n")
cat("      AUDITORÍA 04: Kg_2003\n")
cat("==============================\n\n")


############################################################
## 2. Estructura básica
############################################################

cat("Filas:", nrow(Kg), "\n")
cat("Años :", min(Kg$year), "→", max(Kg$year), "\n")

assets <- sort(unique(Kg$asset))
cat("Activos:", paste(assets, collapse=", "), "\n\n")


############################################################
## 3. Revisar columnas requeridas
############################################################

required <- c("year","asset","var","value","price_base","source")

missing <- setdiff(required, names(Kg))

if (length(missing) == 0) {
  cat("✔ Todas las columnas requeridas están presentes.\n\n")
} else {
  cat("❌ Faltan columnas:", paste(missing, collapse=", "), "\n\n")
}


############################################################
## 4. NA y estructura numérica
############################################################

na_count <- sum(is.na(Kg$value))
cat("NAs en value:", na_count, "\n")

if (na_count > 0) {
  cat("⚠ WARNING: existen valores NA en Kg.\n")
}

if (!is.numeric(Kg$value)) {
  cat("❌ ERROR: value no es numérico.\n")
} else {
  cat("✔ value es numérico.\n")
}

cat("\n")


############################################################
## 5. Panel rectangular (todos los activos en todos los años)
############################################################

panel_test <- Kg %>%
  count(year, asset) %>%
  spread(asset, n, fill = 0)

incomplete_years <- panel_test %>%
  filter(if_any(-year, ~ .x == 0)) %>%
  pull(year)

if (length(incomplete_years) == 0) {
  cat("✔ Panel rectangular completo (todos los activos presentes en todos los años).\n\n")
} else {
  cat("⚠ WARNING: años con activos faltantes:\n")
  print(incomplete_years)
  cat("\n")
}


############################################################
## 6. Chequeo de aditividad NR = ME + NRC ; C = NRC + RC ; T = ME + NRC + RC
############################################################

cat("--- Resumen aditividad ---\n")
print(Kg_add)

res_vals <- Kg_add %>% summarise(
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
## 7. Saltos grandes por activo (continuidad temporal)
############################################################

cat("--- Saltos temporales (ΔKg > 20× mediana abs(diff)) ---\n\n")

jumps <- Kg %>%
  group_by(asset) %>%
  arrange(year) %>%
  summarise(jumps = list(detect_jumps(value))) %>%
  ungroup()

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
## 8. Comparar valores 1950 ME & NRC con ancla Hofman
############################################################

anchor <- readRDS(anchor_file)

anchor_1950_vals <- anchor %>%
  select(prices, starts_with("Kg_")) %>%
  filter(prices == 2003) %>%
  pivot_longer(starts_with("Kg_"), names_to="asset_raw", values_to="Kg") %>%
  mutate(asset = sub("^Kg_", "", asset_raw)) %>%
  filter(asset %in% c("ME","NRC"))

Kg_1950_est <- Kg %>%
  filter(year == 1950, asset %in% c("ME","NRC")) %>%
  select(asset, value)

cat("--- Consistencia con ancla 1950 ---\n")
joined <- left_join(anchor_1950_vals, Kg_1950_est, by="asset")

names(joined)[names(joined) == "value"] <- "Kg_est_2003"

joined$diff <- joined$Kg_est_2003 - joined$Kg

print(joined)

if (all(abs(joined$diff) < 1e-6, na.rm=TRUE)) {
  cat("✔ Coherente con niveles ancla 1950.\n")
} else {
  cat("⚠ WARNING: diferencias en la ancla histórica.\n")
}

############################################################
## 9. Final
############################################################

cat("\n>>> AUDITORÍA 04 COMPLETADA <<<\n")
############################################################
