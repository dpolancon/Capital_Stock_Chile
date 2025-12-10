############################################################
## 02_audit.R — Auditor para 02_harmonize_units (versión anclada)
## Stock–flow consistent capital stock reconstruction
############################################################

library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(here)
library(tidyr)

dir_int  <- here("data","interim")

files <- list(
  Ig = file.path(dir_int, "Ig_2003_all.rds"),
  Kg = file.path(dir_int, "Kg_2003_all.rds"),
  Kn = file.path(dir_int, "Kn_2003_all.rds")
)

canonical <- c("ME","NRC","RC","C","T","NR")

############################################################
## Helper: check duplicates
############################################################
check_duplicates <- function(df) {
  df %>%
    group_by(year, asset, var) %>%
    filter(n() > 1)
}

############################################################
## Helper: monotonicity / plausible growth for stocks
############################################################
check_stock_growth <- function(df) {
  df %>%
    group_by(asset) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(growth = value - dplyr::lag(value)) %>%
    ungroup()
}

############################################################
## MAIN AUDIT FUNCTION
############################################################
audit <- function(label, path) {
  
  cat("\n\n==============================\n")
  cat(" AUDIT:", label, "\n")
  cat("==============================\n")
  
  df <- readRDS(path)
  
  cat("Rows:", nrow(df), "\n")
  cat("Years:", min(df$year), "→", max(df$year), "\n")
  cat("Distinct assets:", paste(unique(df$asset), collapse=", "), "\n")
  
  ## Check required columns
  required <- c("year","asset","var","value","price_base","source")
  missing <- setdiff(required, names(df))
  if (length(missing)>0) {
    cat("❌ Missing columns:", paste(missing, collapse=", "), "\n")
  } else {
    cat("✔ All required columns present\n")
  }
  
  ## Check NAs
  na_n <- sum(is.na(df$value))
  cat("NAs in value:", na_n, "\n")
  
  ## Asset check
  noncanon <- setdiff(unique(df$asset), canonical)
  if (length(noncanon)>0) {
    cat("⚠ Non-canonical assets:", paste(noncanon, collapse=", "), "\n")
  } else {
    cat("✔ All assets canonical\n")
  }
  
  ## Duplicates check
  dup <- check_duplicates(df)
  if (nrow(dup)>0) {
    cat("⚠ Duplicates detected:\n")
    print(dup, n=10)
  } else {
    cat("✔ No duplicates\n")
  }
  
  ## Value sanity check
  cat("Min value:", min(df$value, na.rm=TRUE), "\n")
  cat("Median value:", median(df$value, na.rm=TRUE), "\n")
  cat("Max value:", max(df$value, na.rm=TRUE), "\n")
  
  if (min(df$value, na.rm=TRUE) <= 0) {
    cat("⚠ WARNING: zero or negative values detected.\n")
  }
  
  ## SFC prior-consistency check for stocks
  if (label %in% c("Kg","Kn")) {
    cat("\n--- STOCK CONSISTENCY CHECK (ΔK_t pattern) ---\n")
    growth_check <- check_stock_growth(df)
    
    ## si todas las growth son NA, no hay nada que mirar
    if (all(is.na(growth_check$growth))) {
      cat("Todas las ΔK son NA (no se puede evaluar patrón de crecimiento).\n")
    } else {
      med_abs <- median(abs(growth_check$growth), na.rm = TRUE)
      
      neg_growth <- growth_check %>% filter(!is.na(growth), growth < 0)
      big_growth <- growth_check %>%
        filter(!is.na(growth),
               med_abs > 0,
               abs(growth) > med_abs * 20)
      
      cat("Negative ΔK:", nrow(neg_growth), "\n")
      cat("Explosive ΔK (>|20× median|):", nrow(big_growth), "\n")
      
      if (nrow(neg_growth)>0) {
        cat("⚠ Some negative changes appear (possible retirements or scaling mismatch)\n")
      }
      if (nrow(big_growth)>0) {
        cat("⚠ Possible scaling anomalies in stocks\n")
      }
    }
  }
  
  cat("\nAudit complete for", label, "\n")
}

############################################################
## RUN AUDIT
############################################################
walk2(names(files), files, audit)

cat("\n\n*** 02 AUDIT COMPLETE ***\n")
