############################################################
## 02_harmonize_units.R — TD–SFC COMPLIANT VERSION (FINAL)
##
## Objetivo:
##   - Construir Ig_2003_pattern.rds  (único nivel = Pérez-E)
##   - Construir Kg_pattern.rds       (Tafunell–Ducoing solo shares)
##   - Construir Kn_2003_levels.rds   (Clio-Lab, con corrección de escala)
##
## Este script NO produce series finales.
## Solo produce patrones (Ig, Kg) y niveles (Kn).
############################################################

library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(here)

dir_int <- here("data","interim")
dir_out <- here("data","interim")

canonical <- c("ME","NRC","RC","C","T","NR")

############################################################
## 1. Cargar raw datasets
############################################################

AD       <- readRDS(file.path(dir_int, "raw_AD.rds"))
hof_Ig   <- readRDS(file.path(dir_int, "raw_hofman_Ig.rds"))
hof_K    <- readRDS(file.path(dir_int, "raw_hofman_K.rds"))
clio_Kn  <- readRDS(file.path(dir_int, "raw_clio_Kn.rds"))
taf_Ig   <- readRDS(file.path(dir_int, "raw_tafunell_Ig.rds"))
td_Kg    <- readRDS(file.path(dir_int, "raw_TD_indices.rds"))

############################################################
## 2. Extract Pérez–Eyzaguirre Ig (único benchmark absoluto)
############################################################

Ig_perez <- AD %>%
  filter(var %in% c("FBKF en maquinaria",
                    "FBKF en construcción")) %>%
  mutate(asset = case_when(
    var == "FBKF en maquinaria"   ~ "ME",
    var == "FBKF en construcción" ~ "C"
  )) %>%
  group_by(year) %>%
  summarise(Ig_2003 = sum(value), .groups="drop")

############################################################
## 3. Build Ig patterns (Hofman + Tafunell 2013)
##    SOLO SHARES — nunca niveles
############################################################

pattern_hofman <- hof_Ig %>%
  select(year, asset, value) %>%
  group_by(year) %>%
  mutate(share = value / sum(value)) %>%
  ungroup() %>%
  mutate(src = "hofman")

pattern_taf <- taf_Ig %>%
  select(year, asset, value) %>%
  group_by(year) %>%
  mutate(share = value / sum(value)) %>%
  ungroup() %>%
  mutate(src = "tafunell2013")

pattern_Ig <- bind_rows(pattern_hofman, pattern_taf) %>%
  group_by(year, asset) %>%
  summarise(share = mean(share, na.rm = TRUE), .groups="drop")

############################################################
## 4. Construct Ig_2003_pattern (NO es Ig final)
############################################################

Ig_2003_pattern <- pattern_Ig %>%
  left_join(Ig_perez, by="year") %>%
  mutate(
    value = share * Ig_2003,
    var = "Ig_2003_pattern",
    price_base = "2003_CLP",
    source = "PerezEyzaguirre + Hofman + Tafunell2013"
  ) %>%
  select(year, asset, var, value, price_base, source)

saveRDS(Ig_2003_pattern, file.path(dir_out, "Ig_2003_pattern.rds"))

############################################################
## 5. Kg patterns from Tafunell–Ducoing 2016
##    SOLO SHARES — nunca niveles
############################################################

Kg_pattern <- td_Kg %>%
  select(year, asset, value) %>%
  group_by(year) %>%
  mutate(share = value / sum(value)) %>%
  ungroup() %>%
  mutate(
    var = "Kg_pattern",
    price_base = "index_2019_100",
    source = "TafunellDucoing2016"
  )

saveRDS(Kg_pattern, file.path(dir_out, "Kg_pattern.rds"))

############################################################
## 6. Kn levels from Clio-Lab
##    CORRECCIÓN DE ESCALA: Clio suele estar en miles de millones.
############################################################

detect_scale <- function(x) {
  # Si la mediana es > 500 → casi seguro miles de millones
  if (median(x, na.rm = TRUE) > 500) return(1000)
  return(1)
}

scale_factor <- detect_scale(clio_Kn$value)

Kn_2003_levels <- clio_Kn %>%
  mutate(
    value = value * scale_factor,     # ← FIX CRÍTICO
    var = "Kn_2003_level",
    price_base = "2003_CLP",
    source = paste0("ClioLab_scaled_x", scale_factor)
  ) %>%
  select(year, asset, var, value, price_base, source)

saveRDS(Kn_2003_levels, file.path(dir_out, "Kn_2003_levels.rds"))

############################################################
## END — Script 02 TD–SFC compliant
message(paste0(
  "02_harmonize_units.R completed. Clio scale factor = x", scale_factor, "."
))
############################################################
