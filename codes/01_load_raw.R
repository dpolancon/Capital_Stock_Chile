############################################################
## 01_load_raw.R
## Project: Stock–flow consistent capital stock reconstruction (Chile)
## Tasks:
##  - Load all raw Excel sources from data/raw/
##  - Standardize each into tidy long format:
##       year, asset (where applicable), var, value, price_base, source
##  - Save tidy objects as RDS files in data/interim/
##
## NOTE: This script assumes that:
##  - Column names for assets in the Excel files are already in a
##    reasonably usable form. If not, adjust the mappings / sheet names
##    where indicated.
##  - Price bases are set according to the documentation; adjust labels
##    if your files differ.
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

## Source global setup (assets, directories, helpers, anchor)
## Make sure 00_setup.R is in the project root.
source("00_setup.R")

## If you prefer not to rely on 00_setup.R for directories, uncomment:
# dir_data_raw      <- file.path("data", "raw")
# dir_data_interim  <- file.path("data", "interim")
# dir_data_processed <- file.path("data", "processed")

## -------------------------
## 1. Pérez–Eyzaguirre AD (aggregate demand)
## -------------------------
## File: PerezEyzaguirre_DemandaAgregada.xlsx
## Assumptions:
##  - Contains a 'year' column.
##  - Other columns are macro components (Y, C_priv, C_gov, FBKF_me, FBKF_construction, etc.).
##  - Values are in constant 2003 CLP.
##  - We keep all variables; Ig components will be picked up later by name.
file_AD <- file.path(dir_data_raw, "PerezEyzaguirre_DemandaAgregada.xlsx")

raw_AD <- read_excel(file_AD, sheet = 1) %>%  # adjust sheet if needed
  rename(year = 1) %>%                        # ensure first column is 'year'
  mutate(year = as.integer(year))

raw_AD_tidy <- raw_AD %>%
  pivot_longer(
    cols      = -year,
    names_to  = "var",
    values_to = "value"
  ) %>%
  mutate(
    asset      = NA_character_,          # AD is mostly non-asset-specific here
    price_base = "2003_CLP",             # documented base for Pérez–Eyzaguirre
    source     = "PerezEyzaguirre_AD"
  ) %>%
  select(year, asset, var, value, price_base, source) %>%
  arrange(year, var)

saveRDS(raw_AD_tidy, file = file.path(dir_data_interim, "raw_AD.rds"))

## -------------------------
## 2. Hofman 2000 – Gross investment (Ig)
## -------------------------
## File: Hofman2000_GrossInvestment.xlsx
## Assumptions:
##  - One sheet (or the first sheet) with:
##      year, ME, NRC, RC, C, T
##  - Values are in constant 1980 CLP.
##  - Column names for assets are already "ME", "NRC", "RC", "C", "T".
##    If not, rename them in Excel or adjust the code below.
file_hofman_Ig <- file.path(dir_data_raw, "Hofman2000_GrossInvestment.xlsx")

hofman_Ig_raw <- read_excel(file_hofman_Ig, sheet = 1) %>%  # adjust sheet if needed
  rename(year = 1) %>%
  mutate(year = as.integer(year))

raw_hofman_Ig <- hofman_Ig_raw %>%
  pivot_longer(
    cols      = -year,
    names_to  = "asset",
    values_to = "value"
  ) %>%
  mutate(
    var        = "Ig",
    price_base = "1980_CLP",
    source     = "Hofman2000_Ig"
  ) %>%
  select(year, asset, var, value, price_base, source) %>%
  arrange(year, asset)

saveRDS(raw_hofman_Ig, file = file.path(dir_data_interim, "raw_hofman_Ig.rds"))

## -------------------------
## 3. Hofman 2000 – Gross & net capital stocks (Kg, Kn) 1950–1994
## -------------------------
## File: Hofman_Kstock_gross_net_19501994.xlsx
## Assumptions:
##  - Workbook has 2 sheets, e.g. "Kg" and "Kn", or similar.
##  - Each sheet: year, ME, NRC, RC, C, T (possibly NR as well).
##  - Values in constant 1980 CLP.
##  - We infer 'var' from the sheet name using simple regex.
file_hofman_K <- file.path(dir_data_raw, "Hofman_Kstock_gross_net_19501994.xlsx")

sheets_hofman_K <- excel_sheets(file_hofman_K)

raw_hofman_K <- map_dfr(
  sheets_hofman_K,
  function(sht) {
    df <- read_excel(file_hofman_K, sheet = sht) %>%
      rename(year = 1) %>%
      mutate(year = as.integer(year))
    
    df_long <- df %>%
      pivot_longer(
        cols      = -year,
        names_to  = "asset",
        values_to = "value"
      )
    
    var_name <- case_when(
      str_detect(str_to_lower(sht), "gross") ~ "Kg",
      str_detect(str_to_lower(sht), "net")   ~ "Kn",
      str_detect(str_to_lower(sht), "kg")    ~ "Kg",
      str_detect(str_to_lower(sht), "kn")    ~ "Kn",
      TRUE                                   ~ sht   # fallback: use sheet name directly
    )
    
    df_long %>%
      mutate(
        var        = var_name,
        price_base = "1980_CLP",
        source     = "Hofman2000_K"
      ) %>%
      select(year, asset, var, value, price_base, source)
  }
) %>%
  arrange(year, var, asset)

saveRDS(raw_hofman_K, file = file.path(dir_data_interim, "raw_hofman_K.rds"))

## -------------------------
## 4. Clio-Lab – GFCF (real & nominal) and price index
## -------------------------
## File: ClioLab_GFKF_Freal_nominal__Pk.xlsx
## Assumptions:
##  - Multiple sheets, each corresponding to a different measure:
##      e.g. "F_real", "F_nominal", "Pk", etc.
##  - Each sheet: year, asset1, asset2, ...
##  - Real FBCF likely already in 2003 CLP; nominal in current prices;
##    Pk as an index.
file_clio_Ig <- file.path(dir_data_raw, "ClioLab_GFKF_Freal_nominal__Pk.xlsx")

sheets_clio_Ig <- excel_sheets(file_clio_Ig)

raw_clio_Ig <- map_dfr(
  sheets_clio_Ig,
  function(sht) {
    df <- read_excel(file_clio_Ig, sheet = sht) %>%
      rename(year = 1) %>%
      mutate(year = as.integer(year))
    
    df_long <- df %>%
      pivot_longer(
        cols      = -year,
        names_to  = "asset",
        values_to = "value"
      )
    
    price_label <- case_when(
      str_detect(str_to_lower(sht), "real")    ~ "2003_CLP",
      str_detect(str_to_lower(sht), "const")   ~ "2003_CLP",
      str_detect(str_to_lower(sht), "nom")     ~ "current_CLP",
      str_detect(str_to_lower(sht), "pk")      ~ "index",
      TRUE                                     ~ NA_character_
    )
    
    var_name <- sht  # keep exact sheet name as 'var' tag for traceability
    
    df_long %>%
      mutate(
        var        = var_name,
        price_base = price_label,
        source     = "ClioLab_GFCF"
      ) %>%
      select(year, asset, var, value, price_base, source)
  }
) %>%
  arrange(year, var, asset)

## -------------------------
## 5. Clio-Lab – Net capital stocks (Kn)
## -------------------------
## File: Kn_ClioLab.xlsx
## Assumptions:
##  - One sheet with: year, ME, C, T, (and potentially others).
##  - Values are already in constant 2003 CLP (as per documentation).
file_clio_Kn <- file.path(dir_data_raw, "Kn_ClioLab.xlsx")

clio_Kn_raw <- read_excel(file_clio_Kn, sheet = 1) %>%  # adjust sheet if needed
  rename(year = 1) %>%
  mutate(year = as.integer(year))

raw_clio_Kn <- clio_Kn_raw %>%
  pivot_longer(
    cols      = -year,
    names_to  = "asset",
    values_to = "value"
  ) %>%
  mutate(
    var        = "Kn",
    price_base = "2003_CLP",   # adjust if documentation says otherwise
    source     = "ClioLab_Kn"
  ) %>%
  select(year, asset, var, value, price_base, source) %>%
  arrange(year, asset)

## Combine GFCF and Kn into a single Clio-Lab object for convenience
raw_cliolab <- bind_rows(raw_clio_Ig, raw_clio_Kn) %>%
  arrange(year, var, asset)

saveRDS(raw_cliolab, file = file.path(dir_data_interim, "raw_cliolab.rds"))

## -------------------------
## 6. Tafunell 2013 – Investment paths (Ig indices)
## -------------------------
## File: tafunel_2013_Ig.xlsx
## Assumptions:
##  - One sheet with: year, ME, NRC, NR, etc., as indices (e.g., 1929=100).
##  - These are paths, not levels; base year and scaling are in the paper.
file_taf_Ig <- file.path(dir_data_raw, "tafunel_2013_Ig.xlsx")

taf_Ig_raw <- read_excel(file_taf_Ig, sheet = 1) %>%  # adjust sheet if needed
  rename(year = 1) %>%
  mutate(year = as.integer(year))

raw_tafunell_Ig <- taf_Ig_raw %>%
  pivot_longer(
    cols      = -year,
    names_to  = "asset",
    values_to = "value"
  ) %>%
  mutate(
    var        = "Ig_index",
    price_base = "index",             # generic tag; see Tafunell for exact base
    source     = "Tafunell2013_Ig"
  ) %>%
  select(year, asset, var, value, price_base, source) %>%
  arrange(year, asset)

saveRDS(raw_tafunell_Ig, file = file.path(dir_data_interim, "raw_tafunell_Ig.rds"))

## -------------------------
## 7. Tafunell & Ducoing 2016 – Kg indices
## -------------------------
## File: Tafunell_Ducoing_2016_Kg.xlsx
## Assumptions:
##  - One sheet with: year, ME, NRC, NR (and possibly others),
##    as Kg indices (e.g., 1929=100).
file_TD_Kg <- file.path(dir_data_raw, "Tafunell_Ducoing_2016_Kg.xlsx")

TD_Kg_raw <- read_excel(file_TD_Kg, sheet = 1) %>%  # adjust sheet if needed
  rename(year = 1) %>%
  mutate(year = as.integer(year))

raw_TD_indices <- TD_Kg_raw %>%
  pivot_longer(
    cols      = -year,
    names_to  = "asset",
    values_to = "value"
  ) %>%
  mutate(
    var        = "Kg_index",
    price_base = "index_1929_100",    # adjust if base year differs
    source     = "TafunellDucoing2016_Kg"
  ) %>%
  select(year, asset, var, value, price_base, source) %>%
  arrange(year, asset)

saveRDS(raw_TD_indices, file = file.path(dir_data_interim, "raw_TD_indices.rds"))

############################################################
## End of 01_load_raw.R
############################################################
