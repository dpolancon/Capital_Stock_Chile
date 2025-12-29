############################################################
## 00_setup.R — TD–SFC Compliant Version (Setup Only)
## Project: Stock–Flow Consistent Capital Stock Reconstruction (Chile)
##
## Purpose:
##   - Load project-level packages
##   - Define directory structure
##   - Define canonical asset taxonomy
##   - Define global file paths (but do NOT load anchors)
##   - This script contains NO anchor construction.
##
## Notes:
##   - Anchor construction is now separated into
##     00b_build_anchor_1950.R for unit auditing and validation.
############################################################


## ---------------------------------------------------------
## 1. Load packages
## ---------------------------------------------------------
suppressWarnings({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(readxl)
  library(ggplot2)
  library(purrr)
  library(stringr)
  library(here)
})


## ---------------------------------------------------------
## 2. Directory structure
## ---------------------------------------------------------
dir_data_raw       <- here("data", "raw")
dir_data_interim   <- here("data", "interim")
dir_data_processed <- here("data", "processed")

dir.create(dir_data_raw, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_data_interim, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_data_processed, recursive = TRUE, showWarnings = FALSE)


## ---------------------------------------------------------
## 3. Canonical asset taxonomy
## ---------------------------------------------------------
assets <- c("ME", "NRC", "RC", "C", "NR", "T")


## ---------------------------------------------------------
## 4. Define key file paths (not executed here)
## ---------------------------------------------------------
file_hof_K  <- file.path(dir_data_raw, "Hofman_Kstock_gross_net_19501994.xlsx")
file_clioPk <- file.path(dir_data_raw, "ClioLab_GFKF_Freal_nominal_Pk.xlsx")

# No anchor construction here.
# The anchor is built in 00b_build_anchor_1950.R.

message("00_setup.R loaded: directory structure, assets, and paths initialized.")
