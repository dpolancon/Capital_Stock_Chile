############################################################
## 02_harmonize_units.R
## Project: Stock–flow consistent capital stock reconstruction (Chile)
##
## Tasks:
##  - Load Hofman (Ig, Kg, Kn), Clio-Lab, and 1950 anchor
##  - Convert Hofman Ig, Kg, Kn from 1980 CLP to 2003 CLP
##    using phi_g (gross) and phi_n (net) from the 1950 anchor
##  - Align Clio-Lab Kn to Hofman 1950 anchor for ME, C, T
##  - Save:
##      * data/interim/hofman_2003.rds
##      * data/interim/clio_Kn_2003.rds
##      * data/interim/stocks_2003_level0.rds (optional combined stocks)
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

## Explicitly reload anchor_1950 from RDS (as specified in the plan)
anchor_1950 <- readRDS(file.path(dir_data_interim, "anchor_1950.rds"))

## -------------------------
## 1. Load intermediate raw objects
## -------------------------
raw_hofman_Ig <- readRDS(file.path(dir_data_interim, "raw_hofman_Ig.rds"))
raw_hofman_K  <- readRDS(file.path(dir_data_interim, "raw_hofman_K.rds"))
raw_cliolab   <- readRDS(file.path(dir_data_interim, "raw_cliolab.rds"))

## Sanity: anchor_1950 should have phi_g and phi_n per asset
## Columns expected: asset, Kg_1980, Kg_2003, Kn_1980, Kn_2003, phi_g, phi_n

## -------------------------
## 2. Convert Hofman Ig from 1980 CLP to 2003 CLP
## -------------------------
## Assumption: Ig uses the same price base as gross capital (phi_g).
## We do NOT overwrite raw_hofman_Ig; we create a 2003_CLP version.

phi_g_tbl <- anchor_1950 %>%
  select(asset, phi_g)

hofman_Ig_2003 <- raw_hofman_Ig %>%
  left_join(phi_g_tbl, by = "asset") %>%
  mutate(
    ## value in 2003 CLP; if phi_g is NA for some asset, result will be NA
    value      = value * phi_g,
    price_base = "2003_CLP",
    source     = paste0(source, "_2003")
  ) %>%
  select(year, asset, var, value, price_base, source) %>%
  arrange(year, asset)

## -------------------------
## 3. Convert Hofman Kg & Kn from 1980 CLP to 2003 CLP
## -------------------------
## Use phi_g for Kg (gross stocks), phi_n for Kn (net stocks).

phi_gn_tbl <- anchor_1950 %>%
  select(asset, phi_g, phi_n)

hofman_K_2003 <- raw_hofman_K %>%
  left_join(phi_gn_tbl, by = "asset") %>%
  mutate(
    value = dplyr::case_when(
      var == "Kg" ~ value * phi_g,
      var == "Kn" ~ value * phi_n,
      TRUE        ~ value
    ),
    price_base = dplyr::case_when(
      var %in% c("Kg", "Kn") ~ "2003_CLP",
      TRUE                   ~ price_base
    ),
    source = paste0(source, "_2003")
  ) %>%
  select(year, asset, var, value, price_base, source) %>%
  arrange(year, var, asset)

## -------------------------
## 4. Combine Hofman 2003_CLP series
## -------------------------
## hofman_2003 includes Ig, Kg, Kn in constant 2003 CLP.
## Original 1980_CLP series remain stored as raw_* objects.

hofman_2003 <- bind_rows(
  hofman_Ig_2003,
  hofman_K_2003
) %>%
  arrange(year, var, asset)

saveRDS(hofman_2003, file = file.path(dir_data_interim, "hofman_2003.rds"))

## -------------------------
## 5. Align Clio-Lab Kn to 1950 Hofman anchor (ME, C, T)
## -------------------------
## From raw_cliolab, we take the net stocks (var == "Kn").
## They are already in 2003 CLP but may differ in level from Hofman.
## We rescale ME, C, T so that in 1950:
##   value_Clio_1950 * lambda_a = Kn_2003_anchor_1950
## => lambda_a = Kn_2003_anchor_1950 / value_Clio_1950

clio_Kn_raw <- raw_cliolab %>%
  filter(var == "Kn", source == "ClioLab_Kn")

assets_anchor_Kn <- c("ME", "C", "T")

lambda_tbl <- clio_Kn_raw %>%
  filter(year == 1950, asset %in% assets_anchor_Kn) %>%
  left_join(
    anchor_1950 %>%
      select(asset, Kn_2003),
    by = "asset"
  ) %>%
  mutate(
    lambda = Kn_2003 / value
  ) %>%
  select(asset, lambda)

## Rescale the entire Clio-Lab Kn series for anchored assets;
## leave other assets unchanged (lambda = NA).
clio_Kn_2003 <- clio_Kn_raw %>%
  left_join(lambda_tbl, by = "asset") %>%
  mutate(
    value = if_else(!is.na(lambda), value * lambda, value),
    source = if_else(
      !is.na(lambda),
      "ClioLab_Kn_anchored1950",
      source
    )
  ) %>%
  select(year, asset, var, value, price_base, source) %>%
  arrange(year, asset)

saveRDS(clio_Kn_2003, file = file.path(dir_data_interim, "clio_Kn_2003.rds"))

## -------------------------
## 6. Optional combined stocks snapshot (2003 CLP)
## -------------------------
## This object simply gathers all stock series in 2003 CLP
## from Hofman and Clio-Lab for quick comparison. Multiple
## sources per (year, asset, var) are allowed and identified
## by the 'source' column.

stocks_2003_level0 <- bind_rows(
  hofman_2003 %>% filter(var %in% c("Kg", "Kn")),
  clio_Kn_2003 %>% filter(var == "Kn")
) %>%
  arrange(year, asset, var, source)

saveRDS(stocks_2003_level0, file = file.path(dir_data_interim, "stocks_2003_level0.rds"))

############################################################
## End of 02_harmonize_units.R
############################################################
