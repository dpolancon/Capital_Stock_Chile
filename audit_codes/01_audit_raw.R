############################################################
## 01_audit_raw.R — Auditor automático de salidas del 01
############################################################

library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(glue)

dir_int <- here::here("data", "interim")

canonical_assets <- c("ME","NRC","RC","C","T","NR")

raw_files <- list.files(dir_int, pattern="^raw_.*\\.rds$", full.names=TRUE)

audit_one <- function(path) {
  
  df <- readRDS(path)
  name <- basename(path)
  
  cat("\n\n==============================\n")
  cat(" AUDIT:", name, "\n")
  cat("==============================\n")
  
  ## Check required columns
  required <- c("year","asset","var","value","price_base","source")
  missing_cols <- setdiff(required, names(df))
  
  if (length(missing_cols) > 0) {
    cat("❌ Missing required columns:", paste(missing_cols, collapse=", "), "\n")
  } else {
    cat("✔ All required columns present\n")
  }
  
  ## row count
  cat("Rows:", nrow(df), "\n")
  
  ## year summary
  if ("year" %in% names(df)) {
    yr <- range(df$year, na.rm=TRUE)
    cat("Year range:", yr[1], "→", yr[2], "\n")
  }
  
  ## NA check
  na_count <- sum(is.na(df$value))
  cat("NA in values:", na_count, "\n")
  
  ## asset check
  if ("asset" %in% names(df)) {
    noncanon <- df %>%
      filter(!is.na(asset), !asset %in% canonical_assets) %>%
      pull(asset) %>% unique()
    
    if (length(noncanon) > 0) {
      cat("⚠ Non-canonical asset labels:", paste(noncanon, collapse=", "), "\n")
    } else {
      cat("✔ All asset labels canonical or NA\n")
    }
  }
  
  ## var inspection
  cat("Distinct variables (var column):\n")
  print(df %>% distinct(var))
  
  ## duplicate detection
  dup <- df %>%
    group_by(year, asset, var) %>%
    filter(n() > 1)
  
  if (nrow(dup) > 0) {
    cat("⚠ Duplicated (year, asset, var) combinations found:\n")
    print(dup)
  } else {
    cat("✔ No duplicated (year, asset, var) entries\n")
  }
  
  cat("\nDONE:", name, "\n")
}

walk(raw_files, audit_one)

cat("\n\n*** AUDIT COMPLETE ***\n")
