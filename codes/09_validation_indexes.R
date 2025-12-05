############################################################
## 09_validation_indexes.R
## Project: Stock–flow consistent capital stock reconstruction (Chile)
##
## Tasks:
##  - Load panel_sfc_2003.rds
##  - For each (asset, period), compute residual-based metrics:
##      * MSE_rIg, RMSE_rIg, MAE_rIg, max_abs_rIg
##      * p_small_2 (|r_joint_Ig| <= 0.02)
##      * p_small_5 (|r_joint_Ig| <= 0.05)
##  - For each (asset, period), compute delta and z stability metrics:
##      * mean_delta, sd_delta, cv_delta, p_delta_high, p_delta_neg
##      * mean_z, sd_z, cv_z, p_z_high, p_z_neg
##  - Compute global indicators for:
##      * full_SFC_window (1950–1994, all assets)
##      * full_ME_window (extended ME window)
##  - Save:
##      * data/processed/residual_stats_by_asset_period.csv
##      * data/processed/rate_stats_by_asset_period.csv
##      * data/processed/global_stats.csv
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

## Expect at least:
##   year, asset, period,
##   r_joint_Ig, delta, z,
##   full_SFC_window, full_ME_window

## -------------------------
## 2. Residual-based metrics by (asset, period)
## -------------------------
residual_stats_by_asset_period <- panel_sfc %>%
  group_by(asset, period) %>%
  summarise(
    MSE_rIg     = mean(r_joint_Ig^2, na.rm = TRUE),
    RMSE_rIg    = sqrt(MSE_rIg),
    MAE_rIg     = mean(abs(r_joint_Ig), na.rm = TRUE),
    max_abs_rIg = {
      tmp <- max(abs(r_joint_Ig), na.rm = TRUE)
      if (is.infinite(tmp)) NA_real_ else tmp
    },
    p_small_2 = mean(abs(r_joint_Ig) <= 0.02, na.rm = TRUE),
    p_small_5 = mean(abs(r_joint_Ig) <= 0.05, na.rm = TRUE),
    n_obs     = sum(!is.na(r_joint_Ig)),
    .groups   = "drop"
  )

## -------------------------
## 3. Delta and z stability metrics by (asset, period)
## -------------------------
rate_stats_by_asset_period <- panel_sfc %>%
  group_by(asset, period) %>%
  summarise(
    ## Delta stats
    mean_delta   = mean(delta, na.rm = TRUE),
    sd_delta     = sd(delta,   na.rm = TRUE),
    cv_delta     = ifelse(!is.na(mean_delta) & mean_delta != 0,
                          sd_delta / abs(mean_delta),
                          NA_real_),
    p_delta_high = mean(delta > 0.20, na.rm = TRUE),
    p_delta_neg  = mean(delta < 0,    na.rm = TRUE),
    
    ## z stats (using the same 0.20 threshold for "high")
    mean_z   = mean(z, na.rm = TRUE),
    sd_z     = sd(z,   na.rm = TRUE),
    cv_z     = ifelse(!is.na(mean_z) & mean_z != 0,
                      sd_z / abs(mean_z),
                      NA_real_),
    p_z_high = mean(z > 0.20, na.rm = TRUE),
    p_z_neg  = mean(z < 0,    na.rm = TRUE),
    
    n_delta = sum(!is.na(delta)),
    n_z     = sum(!is.na(z)),
    .groups = "drop"
  )

## -------------------------
## 4. Global indicators (full SFC window & ME extended window)
## -------------------------

## Helper to compute global stats on a given subset
compute_global_stats <- function(df, scope_label) {
  df %>%
    summarise(
      scope = scope_label,
      
      ## Residual metrics
      MSE_rIg     = mean(r_joint_Ig^2, na.rm = TRUE),
      RMSE_rIg    = sqrt(MSE_rIg),
      MAE_rIg     = mean(abs(r_joint_Ig), na.rm = TRUE),
      max_abs_rIg = {
        tmp <- max(abs(r_joint_Ig), na.rm = TRUE)
        if (is.infinite(tmp)) NA_real_ else tmp
      },
      p_small_2 = mean(abs(r_joint_Ig) <= 0.02, na.rm = TRUE),
      p_small_5 = mean(abs(r_joint_Ig) <= 0.05, na.rm = TRUE),
      
      ## Delta stats
      mean_delta   = mean(delta, na.rm = TRUE),
      sd_delta     = sd(delta,   na.rm = TRUE),
      cv_delta     = ifelse(!is.na(mean_delta) & mean_delta != 0,
                            sd_delta / abs(mean_delta),
                            NA_real_),
      p_delta_high = mean(delta > 0.20, na.rm = TRUE),
      p_delta_neg  = mean(delta < 0,    na.rm = TRUE),
      
      ## z stats
      mean_z   = mean(z, na.rm = TRUE),
      sd_z     = sd(z,   na.rm = TRUE),
      cv_z     = ifelse(!is.na(mean_z) & mean_z != 0,
                        sd_z / abs(mean_z),
                        NA_real_),
      p_z_high = mean(z > 0.20, na.rm = TRUE),
      p_z_neg  = mean(z < 0,    na.rm = TRUE),
      
      n_obs_rIg = sum(!is.na(r_joint_Ig)),
      n_obs_delta = sum(!is.na(delta)),
      n_obs_z     = sum(!is.na(z))
    )
}

## 4.1 Full SFC window: years with full_SFC_window == TRUE
global_full_SFC <- panel_sfc %>%
  filter(full_SFC_window) %>%
  compute_global_stats("full_SFC_window")

## 4.2 Extended ME window: full_ME_window == TRUE
global_full_ME <- panel_sfc %>%
  filter(full_ME_window) %>%
  compute_global_stats("full_ME_window_ME")

global_stats <- bind_rows(global_full_SFC, global_full_ME)

## -------------------------
## 5. Save outputs
## -------------------------
file_residuals <- file.path(dir_data_processed, "residual_stats_by_asset_period.csv")
file_rates     <- file.path(dir_data_processed, "rate_stats_by_asset_period.csv")
file_global    <- file.path(dir_data_processed, "global_stats.csv")

write_csv(residual_stats_by_asset_period, file_residuals)
write_csv(rate_stats_by_asset_period,     file_rates)
write_csv(global_stats,                   file_global)

############################################################
## End of 09_validation_indexes.R
############################################################
