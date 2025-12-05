############################################################
## 03_build_Ig_2003.R
## Project: Stock–flow consistent capital stock reconstruction (Chile)
##
## Tasks:
##  - Build AD-consistent Ig in 2003 CLP by asset
##  - Use Hofman NRC/RC shares in construction to decompose AD construction
##  - Define Ig_ME, Ig_NRC, Ig_RC, Ig_C, Ig_NR, Ig_T (all 2003 CLP)
##  - Check additivity (NR = ME + NRC; C = NRC + RC; T = ME + NRC + RC)
##  - Save Ig_2003.rds in data/interim/
##
## NOTE:
##  - This script assumes Pérez–Eyzaguirre AD file has variables named:
##      "FBKF_me"           = gross fixed capital formation in machinery & equipment
##      "FBKF_construction" = gross fixed capital formation in construction
##    If your variable names differ, change the constants ad_var_Ig_ME and
##    ad_var_Ig_C below.
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
## 1. Load intermediate raw objects
## -------------------------
raw_AD        <- readRDS(file.path(dir_data_interim, "raw_AD.rds"))
raw_hofman_Ig <- readRDS(file.path(dir_data_interim, "raw_hofman_Ig.rds"))

## -------------------------
## 2. Identify AD variables for Ig (ME and Construction)
## -------------------------
## Adjust these names if your AD dataset uses different labels.
ad_var_Ig_ME <- "FBKF_me"
ad_var_Ig_C  <- "FBKF_construction"

## Basic check to avoid silent mismatch
vars_AD <- unique(raw_AD$var)
if (!all(c(ad_var_Ig_ME, ad_var_Ig_C) %in% vars_AD)) {
  stop(
    "Variables specified in ad_var_Ig_ME / ad_var_Ig_C were not found in raw_AD$var. ",
    "Available vars are: ",
    paste(sort(vars_AD), collapse = ", ")
  )
}

## -------------------------
## 3. Compute Hofman NRC/RC shares in construction
## -------------------------
## Using Hofman 2000 Ig (1980 CLP) to get shares:
##   s_NRC_H_t = Ig_NRC_H / (Ig_NRC_H + Ig_RC_H)
##   s_RC_H_t  = 1 - s_NRC_H_t
## These shares are independent of the price base, so we do not
## convert to 2003 CLP here.

hofman_shares <- raw_hofman_Ig %>%
  filter(asset %in% c("NRC", "RC")) %>%
  select(year, asset, value) %>%
  pivot_wider(
    names_from  = asset,
    values_from = value
  ) %>%
  mutate(
    total_C = NRC + RC,
    s_NRC   = dplyr::if_else(total_C > 0, NRC / total_C, NA_real_),
    s_RC    = dplyr::if_else(total_C > 0, RC  / total_C, NA_real_)
  ) %>%
  select(year, s_NRC, s_RC)

## Extend shares to full AD year range by carrying forward and backward
## the nearest available year. This implements the rule:
## "For years without shares, use constant shares from the nearest period."
years_AD <- raw_AD %>%
  distinct(year) %>%
  arrange(year)

hofman_shares_full <- years_AD %>%
  left_join(hofman_shares, by = "year") %>%
  arrange(year) %>%
  tidyr::fill(s_NRC, s_RC, .direction = "downup")

## -------------------------
## 4. Extract ME and Construction Ig from AD (2003 CLP)
## -------------------------
AD_Ig_components <- raw_AD %>%
  filter(var %in% c(ad_var_Ig_ME, ad_var_Ig_C)) %>%
  select(year, var, value) %>%
  pivot_wider(
    names_from  = var,
    values_from = value
  ) %>%
  ## Rename to clean internal labels
  rename(
    Ig_ME_AD = !!ad_var_Ig_ME,
    Ig_C_AD  = !!ad_var_Ig_C
  ) %>%
  arrange(year)

## -------------------------
## 5. Decompose AD construction into NRC and RC using Hofman shares
## -------------------------
Ig_decomp <- AD_Ig_components %>%
  left_join(hofman_shares_full, by = "year") %>%
  mutate(
    ## Core assumption: Hofman NRC/RC shares apply to AD construction
    ## (both in 2003 CLP).
    Ig_NRC_2003 = s_NRC * Ig_C_AD,
    Ig_RC_2003  = s_RC  * Ig_C_AD,
    ## AD machinery Ig is taken as-is for ME:
    Ig_ME_2003  = Ig_ME_AD,
    ## Construction, NR, and total:
    Ig_C_2003   = Ig_NRC_2003 + Ig_RC_2003,
    Ig_NR_2003  = Ig_ME_2003  + Ig_NRC_2003,
    Ig_T_2003   = Ig_ME_2003  + Ig_C_2003
  )

## -------------------------
## 6. Build long-format Ig panel (2003 CLP)
## -------------------------
Ig_2003 <- Ig_decomp %>%
  transmute(
    year,
    ME = Ig_ME_2003,
    NRC = Ig_NRC_2003,
    RC = Ig_RC_2003,
    C  = Ig_C_2003,
    NR = Ig_NR_2003,
    T  = Ig_T_2003
  ) %>%
  pivot_longer(
    cols      = -year,
    names_to  = "asset",
    values_to = "value"
  ) %>%
  mutate(
    var        = "Ig",
    price_base = "2003_CLP",
    source     = "AD_plus_Hofman_shares"
  ) %>%
  select(year, asset, var, value, price_base, source) %>%
  arrange(year, asset)

saveRDS(Ig_2003, file = file.path(dir_data_interim, "Ig_2003.rds"))

## -------------------------
## 7. Additivity check for Ig
## -------------------------
## This checks:
##   NR ?= ME + NRC
##   C  ?= NRC + RC
##   T  ?= ME + NRC + RC
Ig_2003_additivity <- check_additivity(Ig_2003, "Ig")

saveRDS(
  Ig_2003_additivity,
  file = file.path(dir_data_interim, "Ig_2003_additivity_residuals.rds")
)

############################################################
## End of 03_build_Ig_2003.R
############################################################
