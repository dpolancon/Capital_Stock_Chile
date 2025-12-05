############################################################
## 05_build_Kn_2003.R
## Project: Stock–flow consistent capital stock reconstruction (Chile)
##
## Tasks:
##  - Use Clio-Lab net capital stocks (Kn) as backbone for ME, C, T
##  - Use Hofman Kn for NRC, RC (1950–1994) in 2003 CLP
##  - Outside 1950–1994, decompose Clio-Lab Kn_C into NRC & RC
##    using Hofman stock shares (θ_NRC, θ_RC)
##  - Define Kn_NR = Kn_ME + Kn_NRC
##  - Combine all Kn series into a single 2003 CLP panel
##  - Check additivity (NR = ME + NRC; C = NRC + RC; T = ME + NRC + RC)
##  - Save Kn_2003.rds and additivity residuals in data/interim/
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
clio_Kn_2003 <- readRDS(file.path(dir_data_interim, "clio_Kn_2003.rds"))
hofman_2003  <- readRDS(file.path(dir_data_interim, "hofman_2003.rds"))
anchor_1950  <- readRDS(file.path(dir_data_interim, "anchor_1950.rds"))  # kept for reference

## clio_Kn_2003 structure (from 02_harmonize_units.R):
##   year, asset, var = "Kn", value, price_base = "2003_CLP",
##   source = "ClioLab_Kn_anchored1950" or "ClioLab_Kn"

## hofman_2003 structure:
##   year, asset, var (Ig, Kg, Kn), value, price_base = "2003_CLP", source

## -------------------------
## 2. Clio-Lab backbone for ME, C, T (Kn in 2003 CLP)
## -------------------------
backbone_assets <- c("ME", "C", "T")

Kn_backbone <- clio_Kn_2003 %>%
  filter(var == "Kn", asset %in% backbone_assets) %>%
  arrange(year, asset)

## -------------------------
## 3. Hofman Kn shares for construction (NRC, RC) relative to C
## -------------------------
## Use Hofman Kn (1950–1994) to compute:
##   θ_NRC_t = Kn_NRC_H / Kn_C_H
##   θ_RC_t  = 1 - θ_NRC_t
## Shares are computed in 2003 CLP, but same as in 1980 CLP.

hof_Kn_for_shares <- hofman_2003 %>%
  filter(
    var == "Kn",
    asset %in% c("NRC", "RC", "C"),
    year >= 1950,
    year <= 1994
  ) %>%
  select(year, asset, value) %>%
  pivot_wider(
    names_from  = asset,
    values_from = value
  ) %>%
  mutate(
    total_C   = C,
    theta_NRC = dplyr::if_else(total_C > 0, NRC / total_C, NA_real_),
    theta_RC  = dplyr::if_else(total_C > 0, RC  / total_C, NA_real_)
  ) %>%
  select(year, theta_NRC, theta_RC)

## -------------------------
## 4. Extend shares θ_NRC, θ_RC to full Clio-Lab C year range
## -------------------------
years_clio_C <- clio_Kn_2003 %>%
  filter(var == "Kn", asset == "C") %>%
  distinct(year) %>%
  arrange(year)

shares_full <- years_clio_C %>%
  left_join(hof_Kn_for_shares, by = "year") %>%
  arrange(year) %>%
  tidyr::fill(theta_NRC, theta_RC, .direction = "downup")

## -------------------------
## 5. Decompose Clio-Lab Kn_C into NRC & RC (2003 CLP)
## -------------------------
Kn_C <- clio_Kn_2003 %>%
  filter(var == "Kn", asset == "C") %>%
  select(year, Kn_C_2003 = value)

Kn_NRC_RC_from_C <- Kn_C %>%
  left_join(shares_full, by = "year") %>%
  mutate(
    Kn_NRC_2003 = theta_NRC * Kn_C_2003,
    Kn_RC_2003  = theta_RC  * Kn_C_2003
  ) %>%
  select(year, Kn_NRC_2003, Kn_RC_2003)

## Wide table with decomposition-based NRC, RC
Kn_NRC_RC_decomp_wide <- Kn_NRC_RC_from_C %>%
  transmute(
    year,
    NRC_dec = Kn_NRC_2003,
    RC_dec  = Kn_RC_2003
  )

## -------------------------
## 6. Hofman Kn for NRC & RC (1950–1994, 2003 CLP)
## -------------------------
Kn_NRC_RC_hof_wide <- hofman_2003 %>%
  filter(
    var == "Kn",
    asset %in% c("NRC", "RC"),
    year >= 1950,
    year <= 1994
  ) %>%
  select(year, asset, value) %>%
  pivot_wider(
    names_from  = asset,
    values_from = value
  ) %>%
  transmute(
    year,
    NRC_hof = NRC,
    RC_hof  = RC
  )

## -------------------------
## 7. Combine Hofman and decomposition for NRC & RC
## -------------------------
## Rule:
##  - If Hofman Kn exists (1950–1994), use it
##  - Otherwise, use decomposition of Clio-Lab C
Kn_NRC_RC_final_wide <- years_clio_C %>%
  left_join(Kn_NRC_RC_decomp_wide, by = "year") %>%
  left_join(Kn_NRC_RC_hof_wide,   by = "year") %>%
  mutate(
    Kn_NRC_2003 = dplyr::if_else(!is.na(NRC_hof), NRC_hof, NRC_dec),
    Kn_RC_2003  = dplyr::if_else(!is.na(RC_hof),  RC_hof,  RC_dec),
    source_NRC  = dplyr::if_else(!is.na(NRC_hof),
                                 "Hofman2000_Kn_2003",
                                 "ClioLab_C_decomp_shares"),
    source_RC   = dplyr::if_else(!is.na(RC_hof),
                                 "Hofman2000_Kn_2003",
                                 "ClioLab_C_decomp_shares")
  )

## Long-format NRC & RC panel
Kn_NRC_long <- Kn_NRC_RC_final_wide %>%
  transmute(
    year,
    asset      = "NRC",
    var        = "Kn",
    value      = Kn_NRC_2003,
    price_base = "2003_CLP",
    source     = source_NRC
  )

Kn_RC_long <- Kn_NRC_RC_final_wide %>%
  transmute(
    year,
    asset      = "RC",
    var        = "Kn",
    value      = Kn_RC_2003,
    price_base = "2003_CLP",
    source     = source_RC
  )

Kn_NRC_RC_panel <- bind_rows(Kn_NRC_long, Kn_RC_long) %>%
  arrange(year, asset)

## -------------------------
## 8. Construct Kn_NR = Kn_ME + Kn_NRC (2003 CLP)
## -------------------------
Kn_ME <- Kn_backbone %>%
  filter(asset == "ME") %>%
  select(year, Kn_ME_2003 = value)

Kn_NRC <- Kn_NRC_RC_panel %>%
  filter(asset == "NRC") %>%
  select(year, Kn_NRC_2003 = value)

Kn_NR_panel <- full_join(Kn_ME, Kn_NRC, by = "year") %>%
  mutate(
    value      = Kn_ME_2003 + Kn_NRC_2003,
    asset      = "NR",
    var        = "Kn",
    price_base = "2003_CLP",
    source     = "Derived_NR_ME_plus_NRC"
  ) %>%
  select(year, asset, var, value, price_base, source) %>%
  arrange(year)

## -------------------------
## 9. Combine all Kn series into a single panel
## -------------------------
Kn_2003 <- bind_rows(
  Kn_backbone,      # ME, C, T from Clio-Lab (anchored)
  Kn_NRC_RC_panel,  # NRC, RC combined (Hofman inside window, Clio decomposed outside)
  Kn_NR_panel       # NR derived from ME + NRC
) %>%
  arrange(year, asset)

saveRDS(Kn_2003, file = file.path(dir_data_interim, "Kn_2003.rds"))

## -------------------------
## 10. Additivity check for Kn
## -------------------------
Kn_2003_additivity <- check_additivity(Kn_2003, "Kn")

saveRDS(
  Kn_2003_additivity,
  file = file.path(dir_data_interim, "Kn_2003_additivity_residuals.rds")
)

############################################################
## End of 05_build_Kn_2003.R
############################################################
