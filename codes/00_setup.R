############################################################
## 00_setup.R
## Project: Stock–flow consistent capital stock reconstruction (Chile)
## Tasks:
##  - Load core packages
##  - Define global asset set
##  - Build 1950 anchor table from Hofman2000_anchors.xlsx
##  - Define helper functions shared across scripts
##  - Save anchor_1950 to data/interim/anchor_1950.rds
############################################################

## -------------------------
## 1. Load required packages
## -------------------------
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(ggplot2)
library(purrr)
library(stringr)

## -------------------------
## 2. Define directories
## -------------------------
dir_data_raw      <- file.path("data", "raw")
dir_data_interim  <- file.path("data", "interim")
dir_data_processed <- file.path("data", "processed")

## Ensure directories exist (idempotent)
dir.create(dir_data_raw,      recursive = TRUE, showWarnings = FALSE)
dir.create(dir_data_interim,  recursive = TRUE, showWarnings = FALSE)
dir.create(dir_data_processed, recursive = TRUE, showWarnings = FALSE)

## -------------------------
## 3. Define asset set
## -------------------------
## ME  = Machinery & Equipment
## NRC = Non-residential construction
## RC  = Residential construction
## C   = Construction total = NRC + RC
## NR  = Non-residential capital = ME + NRC
## T   = Total fixed capital = ME + NRC + RC = NR + RC
assets <- c("ME", "NRC", "RC", "C", "NR", "T")

## -------------------------
## 4. Build 1950 anchor table
## -------------------------
## Source: Hofman2000_anchors.xlsx stored in data/raw/
## Assumptions about the Excel file:
##  - It contains at least the following columns:
##      asset, Kg_1980, Kg_2003, Kn_1980, Kn_2003
##  - Optionally, it may contain a 'year' column; if so,
##    we filter year == 1950 to obtain the anchor row(s).
## Adjust column names here if your file uses different labels.

anchor_file <- file.path(dir_data_raw, "Hofman2000_anchors.xlsx")

anchor_raw <- read_excel(anchor_file)

if ("year" %in% names(anchor_raw)) {
  anchor_1950 <- anchor_raw %>%
    filter(year == 1950) %>%
    select(asset, Kg_1980, Kg_2003, Kn_1980, Kn_2003)
} else {
  ## If there is no year column, assume the file already contains
  ## only the 1950 anchors (one row per asset).
  anchor_1950 <- anchor_raw %>%
    select(asset, Kg_1980, Kg_2003, Kn_1980, Kn_2003)
}

## Ensure correct types and compute conversion factors:
##   phi_g = Kg_2003 / Kg_1980  (gross stock price conversion 1980 -> 2003)
##   phi_n = Kn_2003 / Kn_1980  (net stock price conversion 1980 -> 2003)
anchor_1950 <- anchor_1950 %>%
  mutate(
    asset   = as.character(asset),
    Kg_1980 = as.numeric(Kg_1980),
    Kg_2003 = as.numeric(Kg_2003),
    Kn_1980 = as.numeric(Kn_1980),
    Kn_2003 = as.numeric(Kn_2003),
    phi_g   = Kg_2003 / Kg_1980,
    phi_n   = Kn_2003 / Kn_1980
  )

## -------------------------
## 5. Helper functions
## -------------------------

## check_additivity()
## ------------------
## Given a long-format panel with columns:
##   year, asset, var, value
## and a specific variable name var_name (e.g., "Ig", "Kg", "Kn"),
## compute residuals for the additivity identities:
##   NR = ME + NRC
##   C  = NRC + RC
##   T  = ME + NRC + RC
## Returns a tibble with residuals per year.
check_additivity <- function(df, var_name) {
  df_wide <- df %>%
    filter(var == var_name,
           asset %in% c("ME", "NRC", "RC", "C", "NR", "T")) %>%
    select(year, asset, value) %>%
    distinct() %>%
    pivot_wider(
      names_from = asset,
      values_from = value
    )
  
  ## Compute residuals; if some assets are missing in a given year,
  ## the corresponding residuals will be NA.
  df_res <- df_wide %>%
    mutate(
      res_NR = if_else(!is.na(NR) & !is.na(ME) & !is.na(NRC),
                       NR - (ME + NRC), NA_real_),
      res_C  = if_else(!is.na(C) & !is.na(NRC) & !is.na(RC),
                       C  - (NRC + RC), NA_real_),
      res_T  = if_else(!is.na(T) & !is.na(ME) & !is.na(NRC) & !is.na(RC),
                       T  - (ME + NRC + RC), NA_real_)
    ) %>%
    select(year, res_NR, res_C, res_T)
  
  return(df_res)
}

## build_panel()
## -------------
## Standardize an input data frame to the long panel format used
## throughout the project:
##   year, asset, var, value, price_base, source
## 'df' must contain at least 'year' and 'value' columns.
## 'asset', 'var', 'source', and 'price_base' are scalars.
build_panel <- function(df,
                        asset,
                        var,
                        source,
                        price_base) {
  
  df %>%
    transmute(
      year       = as.integer(.data$year),
      asset      = asset,
      var        = var,
      value      = as.numeric(.data$value),
      price_base = price_base,
      source     = source
    ) %>%
    arrange(year)
}

## -------------------------
## 6. Save anchor_1950
## -------------------------
saveRDS(anchor_1950, file = file.path(dir_data_interim, "anchor_1950.rds"))
