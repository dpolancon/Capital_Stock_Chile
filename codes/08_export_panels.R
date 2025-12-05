############################################################
## 08_export_panels.R
## Project: Stock–flow consistent capital stock reconstruction (Chile)
##
## Tasks:
##  - Load panel_sfc_2003.rds
##  - Export:
##      * Full SFC panel (RDS + CSV) to data/processed/
##      * Lighter regression-ready panel (RDS + CSV)
##  - Optionally export simple summary tables (avg delta, z by asset & period)
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
## 1. Load SFC panel
## -------------------------
panel_sfc <- readRDS(file.path(dir_data_interim, "panel_sfc_2003.rds"))

## -------------------------
## 2. Export full panel (RDS + CSV)
## -------------------------
## Full panel keeps all diagnostics and flags
full_out_rds  <- file.path(dir_data_processed, "panel_sfc_2003.rds")
full_out_csv  <- file.path(dir_data_processed, "panel_sfc_2003.csv")

saveRDS(panel_sfc, file = full_out_rds)

panel_sfc %>%
  arrange(asset, year) %>%
  write_csv(full_out_csv)

## -------------------------
## 3. Build lighter regression-ready panel
## -------------------------
## Keep only core quantitative variables:
##   year, asset, Ig, Kg, Kn, D, R, delta, z
reg_panel <- panel_sfc %>%
  select(year, asset, Ig, Kg, Kn, D, R, delta, z) %>%
  arrange(asset, year)

reg_out_rds <- file.path(dir_data_processed, "panel_sfc_2003_regression.rds")
reg_out_csv <- file.path(dir_data_processed, "panel_sfc_2003_regression.csv")

saveRDS(reg_panel, file = reg_out_rds)
write_csv(reg_panel, reg_out_csv)

## -------------------------
## 4. Optional: simple summary tables
## -------------------------
## Average delta and z by asset & period
rate_summary <- panel_sfc %>%
  group_by(asset, period) %>%
  summarise(
    mean_delta = mean(delta, na.rm = TRUE),
    sd_delta   = sd(delta,   na.rm = TRUE),
    mean_z     = mean(z,     na.rm = TRUE),
    sd_z       = sd(z,       na.rm = TRUE),
    n_obs      = dplyr::n(),
    .groups    = "drop"
  )

rates_out_csv <- file.path(dir_data_processed, "summary_rates_by_asset_period.csv")
write_csv(rate_summary, rates_out_csv)

############################################################
## End of 08_export_panels.R
############################################################
