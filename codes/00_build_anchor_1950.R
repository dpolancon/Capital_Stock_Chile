############################################################
## 00b_build_anchor_1950.R — Anchor Construction Only
## Stock–Flow Consistent Capital Stock Reconstruction (Chile)
##
## Purpose:
##   - Load Hofman (2000) K stocks (Kg, Kn) for 1950
##   - Correct Hofman units (thousands → single pesos)
##   - Convert from 1980 CLP → 2003 CLP → millions
##   - Build SFC-consistent aggregates (C, NR, T)
##   - Conduct additivity checks
##   - Export anchor_1950.rds for all downstream scripts
##
## Notes:
##   - Run 00_setup.R BEFORE this script.
##   - All unit validation must occur at this stage.
############################################################

library(dplyr)
library(tidyr)
library(readxl)
library(here)

## ---------------------------------------------------------
## 1. Load setup (defines file paths and directories)
## ---------------------------------------------------------
source(here("codes", "00_setup.R"))

## ---------------------------------------------------------
## 2. Load Hofman (2000) capital stock data (1980 CLP)
## ---------------------------------------------------------
hof_Kg_1980 <- read_excel(file_hof_K, sheet = "Kg") %>%
  rename(year = 1L) %>% mutate(year = as.integer(year))

hof_Kn_1980 <- read_excel(file_hof_K, sheet = "Kn") %>%
  rename(year = 1L) %>% mutate(year = as.integer(year))

## ---------------------------------------------------------
## 3. Extract 1950 rows
## ---------------------------------------------------------
hof_Kg_1950 <- hof_Kg_1980 %>% filter(year == 1950)
hof_Kn_1950 <- hof_Kn_1980 %>% filter(year == 1950)

if (nrow(hof_Kg_1950) != 1L | nrow(hof_Kn_1950) != 1L) {
  stop("Expected exactly one row for year 1950 in Hofman Kg and Kn.")
}

## Drop unnecessary totals if present
drop_cols <- intersect(names(hof_Kg_1950),
                       c("Total","TOTAL","Tot","tot"))
hof_Kg_1950 <- hof_Kg_1950 %>% select(-all_of(drop_cols))
hof_Kn_1950 <- hof_Kn_1950 %>% select(-all_of(drop_cols))

## Convert text → numeric if needed
hof_Kg_1950 <- hof_Kg_1950 %>% mutate(across(where(is.character),
                                             ~ suppressWarnings(as.numeric(gsub(",", ".", .x)))))
hof_Kn_1950 <- hof_Kn_1950 %>% mutate(across(where(is.character),
                                             ~ suppressWarnings(as.numeric(gsub(",", ".", .x)))))

## ---------------------------------------------------------
## 4. Construct SFC-consistent aggregates (1980 CLP — raw)
## ---------------------------------------------------------
get_MENRC <- function(df) {
  tibble(
    year = df$year,
    ME   = df$ME,
    NRC  = df$NRC,
    RC   = df$RC
  )
}

Kg_1950_1980 <- get_MENRC(hof_Kg_1950) %>%
  mutate(
    C  = NRC + RC,
    NR = ME + NRC,
    T  = ME + NRC + RC
  )

Kn_1950_1980 <- get_MENRC(hof_Kn_1950) %>%
  mutate(
    C  = NRC + RC,
    NR = ME + NRC,
    T  = ME + NRC + RC
  )

## ---------------------------------------------------------
## 4b. CRITICAL FIX: Hofman values are in *THOUSANDS* of 1980 CLP
## Convert to single pesos BEFORE price rebasing.
## ---------------------------------------------------------
message("Applying Hofman unit correction: ×1000 (thousands → pesos).")

Kg_1950_1980 <- Kg_1950_1980 %>% mutate(across(-year, ~ .x * 1000))
Kn_1950_1980 <- Kn_1950_1980 %>% mutate(across(-year, ~ .x * 1000))

## ---------------------------------------------------------
## 5. Load Pk and compute 1980 CLP → 2003 CLP → millions factor
## ---------------------------------------------------------
clio_Pk <- read_excel(file_clioPk, sheet = "ig_nom_ig_real_Pk") %>%
  rename(year = 1L) %>% mutate(year = as.integer(year))

Pk_1950 <- clio_Pk %>% filter(year == 1950) %>% pull(Pk)
Pk_2003 <- clio_Pk %>% filter(year == 2003) %>% pull(Pk)

if (length(Pk_1950) != 1 | length(Pk_2003) != 1) stop("Missing Pk for 1950 or 2003.")
if (abs(Pk_2003 - 100) > 1e-6) warning("Pk_2003 is not equal to 100.")

## Deflator from 1980 pesos → 2003 pesos → millions
scale_1980_to_2003_millions <- (100 / as.numeric(Pk_1950)) / 1e6

## ---------------------------------------------------------
## 6. Apply scaling (2003 CLP, millions)
## ---------------------------------------------------------
Kg_1950_2003 <- Kg_1950_1980 %>% mutate(across(-year, ~ .x * scale_1980_to_2003_millions))
Kn_1950_2003 <- Kn_1950_1980 %>% mutate(across(-year, ~ .x * scale_1980_to_2003_millions))

## ---------------------------------------------------------
## 7. Assemble anchor table
## ---------------------------------------------------------
make_anchor_row <- function(prices_value, pk_index_value, Kg_row, Kn_row, unit_label) {
  tibble(
    year     = Kg_row$year,
    prices   = prices_value,
    pk_index = pk_index_value,
    unit     = unit_label,
    Kg_ME    = Kg_row$ME,  Kg_C = Kg_row$C,   Kg_NRC = Kg_row$NRC,
    Kg_RC    = Kg_row$RC,  Kg_T = Kg_row$T,   Kg_NR  = Kg_row$NR,
    Kn_ME    = Kn_row$ME,  Kn_C = Kn_row$C,   Kn_NRC = Kn_row$NRC,
    Kn_RC    = Kn_row$RC,  Kn_T = Kn_row$T,   Kn_NR  = Kn_row$NR
  )
}

anchor_1950 <- bind_rows(
  make_anchor_row(1980, as.numeric(Pk_1950),
                  Kg_1950_1980, Kn_1950_1980, "Hofman_1980_CLP_corrected"),
  make_anchor_row(2003, 100,
                  Kg_1950_2003, Kn_1950_2003, "Rebased_2003_CLP_millions")
) %>% arrange(prices)

## ---------------------------------------------------------
## 8. Check additivity (C, NR, T)
## ---------------------------------------------------------
add_check <- anchor_1950 %>%
  transmute(
    year, prices,
    res_Kg_C  = Kg_C  - (Kg_NRC + Kg_RC),
    res_Kg_NR = Kg_NR - (Kg_ME  + Kg_NRC),
    res_Kg_T  = Kg_T  - (Kg_ME  + Kg_NRC + Kg_RC),
    res_Kn_C  = Kn_C  - (Kn_NRC + Kn_RC),
    res_Kn_NR = Kn_NR - (Kn_ME  + Kn_NRC),
    res_Kn_T  = Kn_T  - (Kn_ME  + Kn_NRC + Kn_RC)
  )

message("Max additivity residual = ",
        format(max(abs(unlist(add_check[ , -(1:2)])), na.rm = TRUE),
               scientific = TRUE))

## ---------------------------------------------------------
## 9. Export anchor
## ---------------------------------------------------------
saveRDS(anchor_1950, file.path(dir_data_interim, "anchor_1950.rds"))
message("anchor_1950.rds successfully saved.")
############################################################
