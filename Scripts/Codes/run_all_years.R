# =============================================================================
# run_all_years.R  ·  Master driver
# Cleans the full 2014–2019 ISTAT ICT panel in one call.
#
# Usage:
#   Rscript Scripts/Codes/run_all_years.R
#   # or interactively:
#   source(here::here("Scripts", "Codes", "run_all_years.R"))
# =============================================================================

library(here)
library(dplyr)
library(readxl)
library(forcats)
library(mice)
library(pROC)
library(fastDummies)
library(jsonlite)

# `%||%` helper ---------------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a)) b else a

# Sources -------------------------------------------------------------------
source(here("Scripts", "Functions", "pipeline_utils.R"))
source(here("Scripts", "Functions", "year_configs.R"))
source(here("Scripts", "Codes",     "CP_Clean.R"))

# Run each year, collect manifests ------------------------------------------
all_manifests <- list()
for (y in c(2014, 2015, 2016, 2017, 2018, 2019)) {
  res <- tryCatch(
    clean_year_data(y),
    error = function(e) {
      message(sprintf("[run_all] Year %d FAILED: %s", y, conditionMessage(e)))
      NULL
    }
  )
  if (!is.null(res)) all_manifests[[as.character(y)]] <- res$manifest
}

# Cross-year summary --------------------------------------------------------
summary_tbl <- do.call(rbind, lapply(all_manifests, function(m) {
  data.frame(
    year      = m$year,
    raw_n     = m$raw_n %||% NA_integer_,
    final_n   = m$final_n %||% NA_integer_,
    cols_dropped = length(m$dropped_cols),
    blocks_imputed = length(m$imputation),
    stringsAsFactors = FALSE
  )
}))
print(summary_tbl)

# Save consolidated manifest -----------------------------------------------
out_path <- here("Data", "Processed", "manifest_panel_2014_2019.json")
jsonlite::write_json(all_manifests, out_path, pretty = TRUE, auto_unbox = TRUE)
message("\n[run_all] Panel manifest saved → ", out_path)
