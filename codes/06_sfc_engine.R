############################################################
## 06_sfc_engine.R
## Project: Stock–flow consistent capital stock reconstruction (Chile)
##
## Tasks:
##  - Load Ig_2003, Kg_2003, Kn_2003 (all in 2003 CLP)
##  - Build a combined wide panel by (year, asset)
##  - Compute:
##      dKg, dKn, D, R, delta, z
##      eps_joint and normalized residuals r_joint_*
##  - Define period flags (e.g. 1900–1949, 1950–1994, 1995+)
##  - Define full_SFC_window and full_ME_window
##  - Save panel_sfc_2003.rds in data/interim/
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
## 1. Load inputs (already in 2003 CLP)
## -------------------------
Ig_2003 <- readRDS(file.path(dir_data_interim, "Ig_2003.rds"))
Kg_2003 <- readRDS(file.path(dir_data_interim, "Kg_2003.rds"))
Kn_2003 <- readRDS(file.path(dir_data_interim, "Kn_2003.rds"))

## Sanity: all should have columns: year, asset, var, value, price_base, source
## and var ∈ {Ig, Kg, Kn} in the respective objects.

## -------------------------
## 2. Build combined wide panel (year, asset)
## -------------------------
panel_long <- bind_rows(
  Ig_2003 %>% select(year, asset, var, value),
  Kg_2003 %>% select(year, asset, var, value),
  Kn_2003 %>% select(year, asset, var, value)
)

panel_wide <- panel_long %>%
  distinct(year, asset, var, value) %>%   # guard against duplicates
  pivot_wider(
    names_from  = var,
    values_from = value
  ) %>%
  arrange(asset, year)

## -------------------------
## 3. Helper: safe division
## -------------------------
## Avoids division by zero / near-zero and propagates NA where needed.
safe_div <- function(num, denom, tol = 0) {
  if_else(!is.na(denom) & abs(denom) > tol, num / denom, NA_real_)
}

## -------------------------
## 4. Compute SFC-consistent flows & rates
## -------------------------
## For each asset, ordered by year:
##   dKg_t   = Kg_t - Kg_{t-1}
##   dKn_t   = Kn_t - Kn_{t-1}
##   D_t     = Ig_t - dKn_t
##   R_t     = Ig_t - dKg_t
##   delta_t = D_t / Kn_t
##   z_t     = R_t / Kg_t
##   eps_joint_t = (D_t - R_t) - (dKg_t - dKn_t)
##   r_joint_Ig  = eps_joint_t / Ig_t
##   r_joint_Kg  = eps_joint_t / Kg_t
##   r_joint_Kn  = eps_joint_t / Kn_t

panel_sfc <- panel_wide %>%
  group_by(asset) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    dKg = Kg - lag(Kg),
    dKn = Kn - lag(Kn),
    
    D   = Ig - dKn,
    R   = Ig - dKg,
    
    delta = safe_div(D, Kn),
    z     = safe_div(R, Kg),
    
    eps_joint = (D - R) - (dKg - dKn),
    
    r_joint_Ig = safe_div(eps_joint, Ig),
    r_joint_Kg = safe_div(eps_joint, Kg),
    r_joint_Kn = safe_div(eps_joint, Kn)
  ) %>%
  ungroup()

## -------------------------
## 5. Period labels & window flags
## -------------------------
## Period classification (example):
##   1) 1900–1949
##   2) 1950–1994  (core full SFC window)
##   3) 1995+      (tail)
panel_sfc <- panel_sfc %>%
  mutate(
    period = case_when(
      year <= 1949 ~ "1900_1949",
      year <= 1994 ~ "1950_1994",
      TRUE         ~ "1995_plus"
    ),
    ## Full SFC window: Hofman + Clio aligned 1950–1994
    full_SFC_window = year >= 1950 & year <= 1994,
    
    ## Extended ME window (as per plan, e.g. 1900–2008)
    ## If the data do not extend that far, the condition just
    ## trims to the actual observed years.
    full_ME_window = asset == "ME" & year >= 1900 & year <= 2008
  )

## -------------------------
## 6. Save SFC panel
## -------------------------
## Columns include:
##   year, asset, Ig, Kg, Kn,
##   dKg, dKn, D, R, delta, z,
##   eps_joint, r_joint_Ig, r_joint_Kg, r_joint_Kn,
##   period, full_SFC_window, full_ME_window
saveRDS(panel_sfc, file = file.path(dir_data_interim, "panel_sfc_2003.rds"))

############################################################
## End of 06_sfc_engine.R
############################################################
