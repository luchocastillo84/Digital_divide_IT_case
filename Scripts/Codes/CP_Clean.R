# =============================================================================
# CP_Clean.R  ·  Refactored
# Single, year-parameterised cleaning driver for the Digital Divide pipeline.
#
# Replaces the per-year scripts CP_Clean_2014.R … CP_Clean_2019.R with one
# function: clean_year_data(year, save = TRUE).
#
# Usage:
#   source(here("Scripts", "Functions", "pipeline_utils.R"))
#   source(here("Scripts", "Functions", "year_configs.R"))
#   source(here("Scripts", "Codes", "CP_Clean.R"))
#   clean_year_data(2016)
#   # or run the whole panel:
#   for (y in c(2014, 2015, 2016, 2017, 2018, 2019)) clean_year_data(y)
#
# Outputs per year:
#   Data/Processed/ICT{YY}.rda            — cleaned, imputed firm panel
#   Data/Processed/ict_skills_{YY}.rda    — Skills sub-dataset (B5-rich)
#   Data/Processed/manifest_{YYYY}.json   — audit log of decisions taken
#
# Refactor highlights vs. CP_Clean_2016.R:
#   - Year-agnostic; all year-specific logic lives in year_configs.R.
#   - Survey weight (coeffin) is preserved through the pipeline (was dropped).
#   - Revenue binned by ABSOLUTE EU thresholds (was yearly quantiles).
#   - B5 produces three parallel sub-indices (internal/external/either).
#   - set.seed() plumbed for reproducibility.
#   - A run manifest is written next to the .rda for every year (audit log).
#   - Robust to MICE failures: blocks that fail are logged, not silent.
# =============================================================================

clean_year_data <- function(year, save = TRUE) {

  if (!exists("year_configs"))
    stop("year_configs not loaded. Source year_configs.R first.", call. = FALSE)

  cfg <- year_configs[[as.character(year)]]
  if (is.null(cfg))
    stop(sprintf("No configuration for year %s", year), call. = FALSE)

  manifest <- init_run_manifest(year)
  set.seed(cfg$seed)

  # 1. LOAD RAW -------------------------------------------------------------
  message(sprintf("\n══ CLEANING YEAR %d ════════════════════════════════", year))
  raw <- load_raw_ict(cfg$raw_file)
  manifest$raw_n <- nrow(raw)

  # 2. PRE-CLEAN ------------------------------------------------------------
  raw <- clean_raw(raw)

  # 3. ATECO ---------------------------------------------------------------
  if (cfg$ateco_from_dom1) {
    raw$ateco_1 <- recode_ateco(raw[[cfg$ateco_raw_col]])
  } else {
    raw <- raw |> dplyr::rename(ateco_1 = dplyr::all_of(cfg$ateco_raw_col))
    if (cfg$collapse_D_E) raw$ateco_1 <- collapse_D_E(raw$ateco_1)
  }

  # 4. SIZE COLLAPSE ------------------------------------------------------
  raw[[cfg$size_col]] <- collapse_size(raw[[cfg$size_col]])

  # 5. B5 RECODING (three parallel specifications) ------------------------
  if (cfg$b5_coding != "absent") {
    raw <- recode_b5(raw, b5_cols = cfg$b5_cols, coding = cfg$b5_coding)
  } else {
    message("[b5] Year ", year, ": B5a–g structurally absent — ",
            "all three specs set to NA, to be cross-year-imputed in ",
            "CP.Binding.R if Skills construct uses that strategy.")
  }

  # 6. SAVE SKILLS SUBSET (pre-selection so B5 cols are accessible) -------
  b5_internal <- paste0(cfg$b5_cols, "_internal")
  b5_external <- paste0(cfg$b5_cols, "_external")
  b5_either   <- paste0(cfg$b5_cols, "_either")
  b5_keep     <- intersect(c(b5_internal, b5_external, b5_either), names(raw))

  skills_subset <- raw |>
    dplyr::select(
      codice = dplyr::all_of(cfg$id_col),
      clad4  = dplyr::all_of(cfg$size_col),
      ateco_1, rip,
      dplyr::any_of(c("C4", "C9a", "C9c", "C10a", "C10c")),
      dplyr::all_of(b5_keep)
    ) |>
    dplyr::mutate(year = factor(cfg$year))

  if (save) save(skills_subset, file = here::here(cfg$skills_file))

  # 7. SELECT & RENAME (vars&codes crosswalk) -----------------------------
  var_map <- readxl::read_xlsx(here::here("Data", "Processed", "vars&codes.xlsx"))
  data    <- select_and_rename(raw,
                               var_map = var_map,
                               raw_col = cfg$var_map_raw,
                               std_col = cfg$var_map_std)

  # Re-attach the recoded B5 columns (with standardised names like S_B5a_either).
  # The crosswalk currently maps B5a -> S_B5a; we keep all three sub-versions.
  for (sfx in c("_internal", "_external", "_either")) {
    src_cols <- paste0(cfg$b5_cols, sfx)
    src_avail <- intersect(src_cols, names(raw))
    if (length(src_avail) == 0) next
    new_names <- paste0("S_B5", letters[seq_along(src_avail)], sfx)
    add <- raw[, src_avail, drop = FALSE]
    names(add) <- new_names
    data <- dplyr::bind_cols(data, add)
  }

  # 8. ASSIGN TYPES (uses 'type' column if present, else prefix fallback) -
  data <- assign_types(data, var_map,
                       std_col    = cfg$var_map_std,
                       type_col   = "type",
                       prefix_col = "prefix")

  # 9. KEEP THE SURVEY WEIGHT --------------------------------------------
  if (cfg$weight_col %in% names(raw)) {
    data[[cfg$weight_col]] <- suppressWarnings(
      as.numeric(as.character(raw[[cfg$weight_col]]))
    )
  } else {
    message("[weight] Survey weight column ", cfg$weight_col,
            " not found — downstream weighted analyses will need re-derivation.")
  }

  # 10. REVENUE → ABSOLUTE BIN -------------------------------------------
  data$size_rev <- bin_revenue(data[[cfg$revenue_col]],
                               thresholds = EU_SME_THRESHOLDS)
  data <- data |>
    dplyr::select(-dplyr::any_of(c("Ricavi", "ricavi", "ricavi_cl")))

  # 11. DROP ADMIN COLUMNS (keep coeffin, dom4, ateco_1, rip) ------------
  drop_admin <- c("dom1", "mac")
  data <- data |> dplyr::select(-dplyr::any_of(drop_admin))

  # 12. MISSINGNESS ASSESSMENT -----------------------------------------------
  miss_info <- compute_missing_info(data, drop_threshold = 60, row_drop_pct = 0.01)
  message("\n[missing] Variables with > 0% missing:")
  print(miss_info[miss_info$miss_pct > 0, ])

  # Drop columns >60% missing
  drop_cols <- miss_info$var[miss_info$action == "drop_column"]
  if (length(drop_cols) > 0) {
    message("[missing] Dropping columns (>60% missing): ",
            paste(drop_cols, collapse = ", "))
    data <- data |> dplyr::select(-dplyr::all_of(drop_cols))
    manifest$dropped_cols <- as.list(drop_cols)
  }

  # Drop individual rows for near-0% missing variables
  drop_row_vars <- miss_info$var[miss_info$action == "drop_rows"]
  for (v in intersect(drop_row_vars, names(data))) {
    n_before <- nrow(data)
    data     <- data[!is.na(data[[v]]), ]
    n_dropped <- n_before - nrow(data)
    manifest$rows_dropped[[v]] <- n_dropped
    message(sprintf("[missing] Dropped %d rows with NA in %s", n_dropped, v))
  }

  # 13. ROW-LEVEL MISSINGNESS FILTER (80% threshold, standardised) -----
  threshold <- cfg$row_miss_threshold * ncol(data)
  n_before  <- nrow(data)
  data      <- data[rowSums(is.na(data)) < threshold, , drop = FALSE]
  manifest$rows_dropped[["row_threshold_80pct"]] <- n_before - nrow(data)
  message(sprintf("[rows] Removed %d firms with >%.0f%% missing (kept %d)",
                  n_before - nrow(data), cfg$row_miss_threshold * 100, nrow(data)))

  # Re-check after row drops. Variables just over the 1% threshold can become
  # near-complete once rows with common skip-pattern NAs have been removed.
  miss_info_after_rows <- compute_missing_info(data, drop_threshold = 60, row_drop_pct = 0.01)
  late_drop_row_vars <- miss_info_after_rows$var[miss_info_after_rows$action == "drop_rows"]
  for (v in intersect(late_drop_row_vars, names(data))) {
    if (!anyNA(data[[v]])) next
    n_before <- nrow(data)
    data     <- data[!is.na(data[[v]]), , drop = FALSE]
    n_dropped <- n_before - nrow(data)
    manifest$rows_dropped[[paste0(v, "_post_filter")]] <- n_dropped
    message(sprintf("[missing] Dropped %d post-filter rows with NA in %s", n_dropped, v))
  }

  # 14. LOW-VARIANCE REMOVAL --------------------------------------------
  low_var_dropped <- intersect(cfg$low_var, names(data))
  data <- drop_low_variance(data, cfg$low_var)
  data <- droplevels(data)
  manifest$low_var_dropped <- as.list(low_var_dropped)

  # 15. CODICE QC -------------------------------------------------------
  dropped_ids <- check_codice(data, id_col = cfg$id_col,
                              expected_n = cfg$expected_n)
  skills_subset <- skills_subset |>
    dplyr::filter(!codice %in% dropped_ids)
  if (save) save(skills_subset, file = here::here(cfg$skills_file))

  # 16. ONE-HOT ENCODE CONTROLS (for MICE predictor matrix) -------------
  data <- encode_controls(data,
                          cat_cols = c("clad4", "rip", "ateco_1", "size_rev"))
  data <- droplevels(data)

  # 17. MICE IMPUTATION — BLOCK BY BLOCK --------------------------------
  blocks_result <- vector("list", length(cfg$imp_blocks))
  names(blocks_result) <- names(cfg$imp_blocks)

  for (block_name in names(cfg$imp_blocks)) {
    blk <- cfg$imp_blocks[[block_name]]

    message(sprintf("\n── MICE Block: %s ──────────────────────────────",
                    block_name))

    cov_regex  <- paste0("^(", paste(blk$covariates, collapse = "|"), ")($|_)")
    block_vars <- unique(c(intersect(blk$targets, names(data)),
                           grep(cov_regex, names(data), value = TRUE)))

    blocks_result[[block_name]] <- tryCatch(
      run_mice_block(
        data            = data,
        block_vars      = block_vars,
        m               = 5,
        maxit           = 15,
        methods         = blk$methods,
        eval_target     = blk$eval_target,
        eval_predictors = blk$eval_preds,
        seed            = cfg$seed
      ),
      error = function(e) {
        message("  [mice] Block FAILED: ", conditionMessage(e))
        list(best_method = "FAILED", imp_object = NULL,
             completed_list = list(data[, intersect(block_vars, names(data)),
                                        drop = FALSE]),
             aucs = NULL)
      }
    )

    manifest$imputation[[block_name]] <- list(
      best_method = blocks_result[[block_name]]$best_method,
      aucs        = if (!is.null(blocks_result[[block_name]]$aucs))
                       as.list(blocks_result[[block_name]]$aucs) else NULL,
      targets     = blk$targets
    )
  }

  # 18. INTEGRATE IMPUTED BLOCKS BACK -----------------------------------
  target_cols_per_block <- lapply(cfg$imp_blocks, `[[`, "targets")
  data_imp <- integrate_blocks(
    original              = data,
    blocks_result         = blocks_result,
    target_cols_per_block = target_cols_per_block
  )

  # 19. DROP ONE-HOT DUMMIES (keep originals) --------------------------
  ohe_pattern <- "^(clad4_|rip_|ateco_1_|size_rev_)"
  data_imp    <- data_imp |> dplyr::select(-dplyr::matches(ohe_pattern))

  # 20. FINAL QC --------------------------------------------------------
  message(sprintf("\n[final] Year %d → %d firms × %d vars",
                  year, nrow(data_imp), ncol(data_imp)))
  miss_check    <- compute_missing_info(data_imp)
  remaining_na  <- miss_check[miss_check$miss_pct > 0, ]
  if (nrow(remaining_na) > 0) {
    message("[final] WARNING — remaining missing values:")
    print(remaining_na)
  } else {
    message("[final] No remaining missing values.")
  }
  manifest$final_n <- nrow(data_imp)

  # 21. SAVE -----------------------------------------------------------
  if (save) {
    obj_name <- sprintf("ict_%02d_impT", year %% 100)
    assign(obj_name, data_imp)
    save(list = obj_name, file = here::here(cfg$out_file))
    message("\n[saved] ", obj_name, " → ", cfg$out_file)

    manifest_path <- file.path(
      dirname(here::here(cfg$out_file)),
      sprintf("manifest_%d.json", year)
    )
    save_run_manifest(manifest, manifest_path)
  }

  invisible(list(data = data_imp, skills = skills_subset, manifest = manifest))
}
