############################################################
## 07_diagnostics_plots.R
## Project: Stock–flow consistent capital stock reconstruction (Chile)
##
## Tasks:
##  - Load panel_sfc_2003.rds
##  - Produce diagnostics plots:
##      * Implicit depreciation rates (delta) by asset
##      * Implicit retirement/depletion rates (z) by asset
##      * SFC residuals (r_joint_Ig) by asset
##  - Mark key splice years with vertical lines (1900, 1940, 1950, 1994)
##  - Save PNGs to fig/
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

## Create figure directory
dir_fig <- file.path("fig")
dir.create(dir_fig, recursive = TRUE, showWarnings = FALSE)

## -------------------------
## 1. Load SFC panel
## -------------------------
panel_sfc <- readRDS(file.path(dir_data_interim, "panel_sfc_2003.rds"))

## Expect columns:
##   year, asset, Ig, Kg, Kn,
##   dKg, dKn, D, R, delta, z,
##   eps_joint, r_joint_Ig, r_joint_Kg, r_joint_Kn,
##   period, full_SFC_window, full_ME_window

## -------------------------
## 2. Splice / regime boundary years
## -------------------------
splice_years <- c(1900, 1940, 1950, 1994)

## Helper for vertical lines
add_splice_lines <- function(p) {
  p + geom_vline(xintercept = splice_years,
                 linetype = "dashed",
                 alpha = 0.4)
}

## -------------------------
## 3. Plot: implicit depreciation rates (delta) by asset
## -------------------------
p_delta <- panel_sfc %>%
  filter(!is.na(delta)) %>%
  ggplot(aes(x = year, y = delta)) +
  geom_line() +
  facet_wrap(~ asset, scales = "free_y") +
  add_splice_lines() +
  labs(
    title = "Implicit depreciation rates (delta_t = D_t / Kn_t)",
    x     = "Year",
    y     = "Depreciation rate (delta)"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(dir_fig, "delta_by_asset.png"),
  plot     = p_delta,
  width    = 10,
  height   = 6
)

## -------------------------
## 4. Plot: implicit retirement / depletion rates (z) by asset
## -------------------------
p_z <- panel_sfc %>%
  filter(!is.na(z)) %>%
  ggplot(aes(x = year, y = z)) +
  geom_line() +
  facet_wrap(~ asset, scales = "free_y") +
  add_splice_lines() +
  labs(
    title = "Implicit retirement/depletion rates (z_t = R_t / Kg_t)",
    x     = "Year",
    y     = "Retirement rate (z)"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(dir_fig, "z_by_asset.png"),
  plot     = p_z,
  width    = 10,
  height   = 6
)

## -------------------------
## 5. Plot: SFC residuals r_joint_Ig by asset
## -------------------------
p_rIg <- panel_sfc %>%
  filter(!is.na(r_joint_Ig)) %>%
  ggplot(aes(x = year, y = r_joint_Ig)) +
  geom_hline(yintercept = 0, linetype = "solid", alpha = 0.6) +
  geom_line() +
  facet_wrap(~ asset, scales = "free_y") +
  add_splice_lines() +
  labs(
    title = "Joint SFC residual (normalized by Ig_t)",
    subtitle = "r_joint_Ig = eps_joint_t / Ig_t",
    x     = "Year",
    y     = "SFC residual / Ig"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(dir_fig, "r_joint_Ig_by_asset.png"),
  plot     = p_rIg,
  width    = 10,
  height   = 6
)

############################################################
## End of 07_diagnostics_plots.R
############################################################
