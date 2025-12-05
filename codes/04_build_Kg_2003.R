############################################################
## 04_build_Kg_2003.R
## Project: Stock–flow consistent capital stock reconstruction (Chile)
##
## Tasks:
##  - Build gross capital stocks Kg in 2003 CLP
##  - For ME & NRC: use Tafunell & Ducoing Kg indices as paths,
##    anchored on Hofman 1950 Kg (1980 CLP → 2003 CLP via phi_g)
##  - Construct Kg_NR = Kg_ME + Kg_NRC
##  - For RC, C, T: use Hofman Kg (1950–1994) converted to 2003 CLP
##  - Combine all into a single Kg panel in 2003 CLP
##  - Check additivity (NR = ME + NRC; C = NRC + RC; T = ME + NRC + RC)
##  - Save Kg_2003.rds in data/interim/
############################################################

## -------------------------
## 0. Libraries & setup
## -------------------------
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(ggplot2)
library(purrr)
library(stringr)

## Load global directories, assets, helpers
source("00_setup.R")

## -------------------------
## 1. Load inputs
## -------------------------
raw_TD_indices <- readRDS(file.path(dir_data_interim, "raw_TD_indices.rds"))
hofman_2003    <- readRDS(file.path(dir_data_interim, "hofman_2003.rds"))
anchor_1950    <- readRDS(file.path(dir_data_interim, "anchor_1950.rds"))

## anchor_1950 has: asset, Kg_1980, Kg_2003, Kn_1980, Kn_2003, phi_g, phi_n

## -------------------------
## 2. Build Kg paths for ME & NRC from TD indices + 1950 anchor
## -------------------------
## raw_TD_indices structure (from 01_load_raw.R):
##   year, asset, var = "Kg_index", value, price_base = "index_1929_100", source
##
## We restrict to:
##   - var == "Kg_index"
##   - asset in {ME, NRC}
## Then anchor at 1950:
##   Kg_1980_{a,t} = Kg_1980_{a,1950} * (index_{a,t} / index_{a,1950})
##   Kg_2003_{a,t} = phi_g_a * Kg_1980_{a,t}

assets_TD_ME_NRC <- c("ME", "NRC")

TD_indices_ME_NRC <- raw_TD_indices %>%
  filter(var == "Kg_index", asset %in% assets_TD_ME_NRC)

## Extract 1950 index per asset
TD_index_1950 <- TD_indices_ME_NRC %>%
  filter(year == 1950) %>%
  select(asset, index_1950 = value)

if (nrow(TD_index_1950) != length(assets_TD_ME_NRC)) {
  stop("Tafunell & Ducoing Kg indices must contain year 1950 for ME and NRC.")
}

## Get 1950 Kg_1980 and phi_g from anchor
anchor_gross_ME_NRC <- anchor_1950 %>%
  filter(asset %in% assets_TD_ME_NRC) %>%
  select(asset, Kg_1980_anchor = Kg_1980, phi_g)

Kg_ME_NRC_2003 <- TD_indices_ME_NRC %>%
  left_join(TD_index_1950, by = "asset") %>%
  left_join(anchor_gross_ME_NRC, by = "asset") %>%
  mutate(
    ## Kg in 1980 CLP along TD path, anchored at Hofman 1950 level
    Kg_1980_t = Kg_1980_anchor * (value / index_1950),
    ## Convert to 2003 CLP using phi_g from the anchor
    Kg_2003_t = phi_g * Kg_1980_t
  ) %>%
  transmute(
    year,
    asset,
    var        = "Kg",
    value      = Kg_2003_t,
    price_base = "2003_CLP",
    source     = "TD_index_anchored_Hofman1950"
  ) %>%
  arrange(year, asset)

## -------------------------
## 3. Construct Kg_NR as ME + NRC (2003 CLP)
## -------------------------
Kg_NR_2003 <- Kg_ME_NRC_2003 %>%
  select(year, asset, value) %>%
  pivot_wider(
    names_from  = asset,
    values_from = value
  ) %>%
  mutate(
    NR = ME + NRC
  ) %>%
  select(year, NR) %>%
  pivot_longer(
    cols      = -year,
    names_to  = "asset",
    values_to = "value"
  ) %>%
  mutate(
    var        = "Kg",
    price_base = "2003_CLP",
    source     = "Derived_NR_ME_plus_NRC_TDpath"
  ) %>%
  arrange(year, asset)

## -------------------------
## 4. Use Hofman Kg (2003 CLP) for RC, C, T (1950–1994)
## -------------------------
## hofman_2003 structure:
##   year, asset, var, value, price_base = "2003_CLP", source (Hofman2000_K_2003, etc.)
##
## We restrict to:
##   - var == "Kg"
##   - asset in {RC, C, T}
##   - year in 1950–1994
assets_Hofman_RCCT <- c("RC", "C", "T")

Kg_RCCT_2003 <- hofman_2003 %>%
  filter(
    var == "Kg",
    asset %in% assets_Hofman_RCCT,
    year >= 1950,
    year <= 1994
  ) %>%
  arrange(year, asset)

## -------------------------
## 5. Combine all Kg series into one panel
## -------------------------
Kg_2003 <- bind_rows(
  Kg_ME_NRC_2003,
  Kg_NR_2003,
  Kg_RCCT_2003
) %>%
  arrange(year, asset)

saveRDS(Kg_2003, file = file.path(dir_data_interim, "Kg_2003.rds"))

## -------------------------
## 6. Additivity check for Kg
## -------------------------
Kg_2003_additivity <- check_additivity(Kg_2003, "Kg")

saveRDS(
  Kg_2003_additivity,
  file = file.path(dir_data_interim, "Kg_2003_additivity_residuals.rds")
)

############################################################
## End of 04_build_Kg_2003.R
############################################################
