############################################################
## 00_setup.R  (TD–SFC Compliant Version)
## Project: Stock–flow consistent capital stock reconstruction (Chile)
##
## Tasks:
##  - Load core packages
##  - Define global directories and canonical asset sets
##  - Load Hofman (2000) 1950 anchors from TWO SHEETS (Kg + Kn)
##  - Clean, harmonize and merge anchors
##  - Validate identities (warnings only)
##  - Export data/interim/anchor_1950.rds
##
## NOTE:
##  This script sets ONLY the structural foundations.
##  Tafunell & Ducoing integration begins in scripts 02–06.
############################################################

## -------------------------
## 1. Load required packages
## -------------------------
suppressWarnings({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(readxl)
  library(ggplot2)
  library(purrr)
  library(stringr)
})

## -------------------------
## 2. Define directories
## -------------------------
dir_data_raw       <- file.path("data", "raw")
dir_data_interim   <- file.path("data", "interim")
dir_data_processed <- file.path("data", "processed")

dir.create(dir_data_raw, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_data_interim, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_data_processed, recursive = TRUE, showWarnings = FALSE)

## -------------------------
## 3. Canonical asset set
## -------------------------
assets <- c("ME", "NRC", "RC", "C", "NR", "T")

## -------------------------
## 4. Load Hofman (2000) anchors
## -------------------------
anchor_file <- file.path(dir_data_raw, "Hofman2000_anchors.xlsx")

kg_raw <- read_excel(
  anchor_file,
  sheet = "Hofman_2000_1950_anchors_Kg",
  col_names = TRUE
)

kn_raw <- read_excel(
  anchor_file,
  sheet = "Hofman_2000_1950_anchors_Kn",
  col_names = TRUE
)

## -------------------------
## 5. Clean & harmonize
## -------------------------
kg_raw <- kg_raw %>%
  rename(
    year      = year,
    prices    = prices,
    pk_index  = `pk 100 = 2003`,
    unit      = `Unit of Measure`,
    Kg_ME     = Kg_ME_H2000,
    Kg_C      = Kg_C_H2000,
    Kg_NRC    = Kg_NRC_H2000,
    Kg_RC     = Kg_RC_H2000,
    Kg_T      = K_gr_tot_H2000,
    Kg_NR     = K_gr_NR_H2000
  ) %>%
  mutate(across(-c(year, unit), as.numeric))

kn_raw <- kn_raw %>%
  rename(
    year      = year,
    prices    = prices,
    pk_index  = `pk 100 = 2003`,
    unit      = `Unit of Measure`,
    Kn_ME     = Kn_ME_H2000,
    Kn_C      = Kn_C_H2000,
    Kn_NRC    = Kn_NRC_H2000,
    Kn_RC     = K_net_RC_H2000,
    Kn_T      = K_net_tot_H2000,
    Kn_NR     = Kn_NR_H2000
  ) %>%
  mutate(across(-c(year, unit), as.numeric))

## -------------------------
## 6. Merge gross + net anchors
## -------------------------
anchor_1950 <- kg_raw %>%
  left_join(
    kn_raw %>% 
      select(year, prices, pk_index, unit, starts_with("Kn_")),
    by = c("year", "prices", "pk_index", "unit")
  ) %>%
  arrange(prices)

## -------------------------
## 7. Identity validation (warnings only)
## -------------------------

validate_identity <- function(df, g_var, nrc_var, rc_var, label) {
  lhs <- df[[g_var]]
  rhs <- df[[nrc_var]] + df[[rc_var]]
  if (any(abs(lhs - rhs) > 1e-6, na.rm = TRUE)) {
    warning(paste0(
      "Identity check failed for ", label, 
      ": ", g_var, " != ", nrc_var, " + ", rc_var,
      ". Downstream SFC flows may be affected."
    ))
  }
}

validate_identity(anchor_1950, "Kg_C", "Kg_NRC", "Kg_RC", "Gross C")
validate_identity(anchor_1950, "Kn_C", "Kn_NRC", "Kn_RC", "Net C")
validate_identity(anchor_1950, "Kg_NR", "Kg_ME", "Kg_NRC", "Gross NR")
validate_identity(anchor_1950, "Kn_NR", "Kn_ME", "Kn_NRC", "Net NR")

## -------------------------
## 8. Save final harmonized anchor table
## -------------------------
saveRDS(anchor_1950, file.path(dir_data_interim, "anchor_1950.rds"))
message("anchor_1950.rds successfully saved to data/interim/")

############################################################
## Modularity: check_additivity() (NOT EXECUTED HERE)
############################################################
check_additivity <- function(df, var_name = "Ig") {
  
  required_cols <- c("year", "asset", "value")
  if (!all(required_cols %in% names(df))) {
    stop("check_additivity(): df must include year, asset, value.")
  }
  
  panel <- df %>%
    select(year, asset, value) %>%
    pivot_wider(names_from = asset, values_from = value)
  
  panel <- panel %>%
    mutate(
      res_NR = NR - (ME + NRC),
      res_C  = C  - (NRC + RC),
      res_T  = T  - (ME + NRC + RC)
    )
  
  tol <- 1e-6
  max_res <- max(abs(panel$res_NR), abs(panel$res_C), abs(panel$res_T), na.rm = TRUE)
  
  if (max_res > tol) {
    warning(
      "check_additivity(): non-trivial additivity residuals. ",
      "Maximum = ", format(max_res, scientific = TRUE)
    )
  }
  
  return(panel)
}
