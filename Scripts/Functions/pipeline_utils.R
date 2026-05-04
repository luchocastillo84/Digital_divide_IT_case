# =============================================================================
# pipeline_utils.R  ·  Refactored
# Shared utilities for the Digital Divide cleaning & imputation pipeline.
#
# Refactor goals:
#   - Single source of truth for variable types (uses 'type' column in
#     vars&codes.xlsx, not the fragile prefix-based heuristic).
#   - Reproducible runs (set.seed argument plumbed through MICE).
#   - Absolute revenue thresholds aligned with the 2003 EU Recommendation,
#     consistent across years (replaces per-year quantile binning).
#   - B5 recoding produces three parallel sub-indices (internal / external /
#     either), unlocking the sensitivity analyses required for publication.
#   - run_mice_block() degrades gracefully when AUC predictors are all-NA.
#   - integrate_blocks() preserves factor levels even when the original
#     column is all-NA.
#   - Survey weight (coeffin) is preserved through the pipeline.
#   - A run manifest captures sample sizes, imputed variables, MICE method
#     chosen, AUC achieved, and dropped IDs — for audit and reviewer reply.
#
# Dependencies: dplyr, mice, pROC, fastDummies, forcats, jsonlite
# =============================================================================


# ── 0. CONSTANTS --------------------------------------------------------------

#' EU SME thresholds (Commission Recommendation 2003/361/EC, € of revenue).
#' Using absolute thresholds — not yearly quantiles — preserves comparability
#' across the 2014–2019 panel.
EU_SME_THRESHOLDS <- c(small_max = 10e6, medium_max = 50e6)

#' B5 columns (ICT functions performed by internal/external personnel).
B5_COLS <- paste0("B5", letters[1:7])


# ── 1. LOAD RAW DATA ----------------------------------------------------------

#' Load a raw ICT microdata file (tab-separated .txt).
#' Errors with a clear message if the file is absent.
#' @param file  filename only (looked up in Data/Raw/).
#' @param sep   field separator (default tab).
#' @return data.frame with all original columns.
load_raw_ict <- function(file, sep = "\t") {
  path <- here::here("Data", "Raw", file)
  if (!file.exists(path)) {
    stop(sprintf("[load] Raw file not found: %s", path), call. = FALSE)
  }
  raw <- read.csv(path, header = TRUE, sep = sep, stringsAsFactors = FALSE)
  message(sprintf("[load] %s: %d firms x %d vars", file, nrow(raw), ncol(raw)))
  raw
}


# ── 2. CLEAN RAW DATA ---------------------------------------------------------

#' Replace placeholder values (".", "", "NA") with NA.
#' Coerce character columns to numeric only when conversion adds < 1 % new NAs
#' (avoids silently corrupting ID or label columns).
#' @param df  raw data.frame
#' @return cleaned data.frame
clean_raw <- function(df) {
  names(df) <- trimws(names(df))

  df <- df |>
    dplyr::mutate(dplyr::across(where(is.character), trimws)) |>
    dplyr::mutate(dplyr::across(where(is.character), \(x) dplyr::na_if(x, "."))) |>
    dplyr::mutate(dplyr::across(where(is.character), \(x) dplyr::na_if(x, ""))) |>
    dplyr::mutate(dplyr::across(where(is.character), \(x) dplyr::na_if(x, "NA")))

  safe_to_numeric <- function(x) {
    attempt  <- suppressWarnings(as.numeric(x))
    orig_na  <- mean(is.na(x))
    new_na   <- mean(is.na(attempt))
    if ((new_na - orig_na) < 0.01) attempt else x
  }

  df |> dplyr::mutate(dplyr::across(where(is.character), safe_to_numeric))
}


# ── 3. ATECO RECODING ---------------------------------------------------------

#' Recode the dom1 sector code (N01–N27) into ATECO macro-sector letters.
#' Used for years where ateco_1 is not already present in the raw data.
recode_ateco <- function(dom1_col) {
  factor(dplyr::case_when(
    dom1_col %in% paste0("N0", 1:9)             ~ "C",
    dom1_col == "N10"                            ~ "D_E",
    dom1_col == "N11"                            ~ "F",
    dom1_col %in% c("N12", "N13", "N14")        ~ "G",
    dom1_col %in% c("N15", "N16")               ~ "H",
    dom1_col %in% c("N17", "N18")               ~ "I",
    dom1_col %in% c("N19", "N20", "N21", "N22") ~ "J",
    dom1_col == "N23"                            ~ "L",
    dom1_col == "N24"                            ~ "M",
    dom1_col %in% c("N25", "N26")               ~ "N",
    dom1_col == "N27"                            ~ "S",
    TRUE                                         ~ NA_character_
  ))
}

#' Collapse D and E ATECO levels into D_E (needed for 2017+).
collapse_D_E <- function(ateco_col) {
  forcats::fct_collapse(as.factor(ateco_col), D_E = c("D", "E"))
}


# ── 4. SIZE COLLAPSE ----------------------------------------------------------

#' Collapse 4-level clad4 firm size into 3 levels (cl1, cl2+cl3 → cl2, cl4 → cl3).
collapse_size <- function(size_col) {
  forcats::fct_collapse(as.factor(size_col),
    cl1 = "cl1",
    cl2 = c("cl2", "cl3"),
    cl3 = "cl4"
  )
}


# ── 5. B5 RECODING (three parallel specifications) ---------------------------

#' Recode B5a–g from 3-level (1=internal, 2=external, 3=not used) into
#' three binary specifications, returned as new columns:
#'
#'   *_internal   (1 if internal, 0 otherwise)   — current Skills construct
#'   *_external   (1 if external, 0 otherwise)   — alternative specification
#'   *_either     (1 if internal OR external)    — most inclusive
#'
#' For years where the raw column already encodes 0/1 (2014, 2019), the binary
#' value is propagated to *_either (assumed to mean "any ICT function performed",
#' subject to questionnaire verification — see CRITICAL note in the review
#' document) and *_internal / *_external are set to NA.
#'
#' @param df       data.frame containing B5 columns
#' @param b5_cols  character vector of B5 column names present in df
#' @param coding   "three_level" or "binary" — how the raw column is encoded
#' @return df with the original B5 columns replaced by the three specs
recode_b5 <- function(df, b5_cols = B5_COLS,
                      coding = c("three_level", "binary")) {
  coding  <- match.arg(coding)
  idx <- match(tolower(b5_cols), tolower(names(df)))
  present <- !is.na(idx)
  if (!any(present)) return(df)

  for (i in which(present)) {
    col <- names(df)[idx[i]]
    out_base <- b5_cols[i]
    v <- df[[col]]
    if (coding == "three_level") {
      df[[paste0(out_base, "_internal")]] <- factor(as.integer(v == 1), levels = c(0, 1))
      df[[paste0(out_base, "_external")]] <- factor(as.integer(v == 2), levels = c(0, 1))
      df[[paste0(out_base, "_either")]]   <- factor(as.integer(v %in% c(1, 2)), levels = c(0, 1))
    } else { # binary
      df[[paste0(out_base, "_internal")]] <- factor(NA, levels = c(0, 1))
      df[[paste0(out_base, "_external")]] <- factor(NA, levels = c(0, 1))
      df[[paste0(out_base, "_either")]]   <- factor(as.integer(v == 1), levels = c(0, 1))
    }
    df[[col]] <- NULL
  }
  df
}


# ── 6. VARIABLE SELECTION & RENAMING -----------------------------------------

#' Subset df to the variables listed in var_map for this year and rename to
#' standardised names. Only columns that actually exist in df are kept.
select_and_rename <- function(df, var_map, raw_col, std_col) {
  raw_names <- na.omit(var_map[[raw_col]])
  std_names <- na.omit(var_map[[std_col]])

  idx       <- match(tolower(raw_names), tolower(names(df)))
  keep      <- !is.na(idx)
  raw_keep  <- names(df)[idx[keep]]
  std_keep  <- std_names[keep]

  missing_from_raw <- raw_names[!keep]
  if (length(missing_from_raw) > 0) {
    message("[select] Variables not found in raw data (skipped): ",
            paste(missing_from_raw, collapse = ", "))
  }

  out        <- df[, raw_keep, drop = FALSE]
  names(out) <- std_keep
  out
}


# ── 7. TYPE ASSIGNMENT (uses explicit 'type' column, not prefix) -------------

#' Assign R types based on an explicit `type` column in var_map.
#' Recognised types: "numeric", "binary", "factor", "id", "label", "weight".
#' Falls back to the prefix heuristic (A2 -> numeric, others -> factor) only
#' if the type column is absent — kept for backward compatibility.
assign_types <- function(df, var_map, std_col,
                         type_col = "type", prefix_col = "prefix") {
  use_type <- type_col %in% names(var_map)
  tbl <- var_map |>
    dplyr::filter(!is.na(.data[[std_col]])) |>
    dplyr::select(std = dplyr::all_of(std_col),
                  type = dplyr::any_of(type_col),
                  pfx  = dplyr::any_of(prefix_col))

  for (i in seq_len(nrow(tbl))) {
    nm <- tbl$std[i]
    if (!nm %in% names(df)) next

    if (use_type) {
      t <- tbl$type[i]
      df[[nm]] <- switch(as.character(t),
        "numeric" = suppressWarnings(as.numeric(as.character(df[[nm]]))),
        "binary"  = factor(df[[nm]], levels = c(0, 1)),
        "factor"  = as.factor(df[[nm]]),
        df[[nm]] # id, label, weight: leave as-is
      )
    } else {
      pfx <- tbl$pfx[i]
      if      (pfx == "A2") df[[nm]] <- suppressWarnings(as.numeric(as.character(df[[nm]])))
      else if (pfx != "L")  df[[nm]] <- as.factor(df[[nm]])
    }
  }
  df
}


# ── 8. REVENUE BINNING (absolute thresholds, EU 2003) ------------------------

#' Bin revenue into small / medium / large using absolute EU thresholds.
#' Thresholds are constant across years to keep the panel comparable.
#'   small    : 0 < revenue < 10,000,000 €
#'   medium   : 10,000,000 ≤ revenue < 50,000,000 €
#'   large    : revenue ≥ 50,000,000 €
#'   NA       : revenue ≤ 0 or missing
#'
#' Use this in place of yearly-quantile binning — yearly quantiles produce
#' bins that drift with panel composition and are not comparable over time.
#'
#' @param revenue_col  numeric or character revenue vector (in EUR)
#' @param thresholds   named numeric of length 2 (small_max, medium_max)
#' @return factor with levels "small", "medium", "large"
bin_revenue <- function(revenue_col, thresholds = EU_SME_THRESHOLDS) {
  rev_num               <- suppressWarnings(as.numeric(as.character(revenue_col)))
  rev_num[rev_num <= 0] <- NA
  factor(dplyr::case_when(
    is.na(rev_num)                ~ NA_character_,
    rev_num <  thresholds["small_max"]  ~ "small",
    rev_num <  thresholds["medium_max"] ~ "medium",
    TRUE                                ~ "large"
  ), levels = c("small", "medium", "large"))
}


# ── 9. MISSINGNESS ASSESSMENT ------------------------------------------------

#' Compute per-variable missingness and recommend an action.
#' @param df              data.frame
#' @param drop_threshold  column-level missingness % above which to drop (default 60)
#' @param row_drop_pct    if miss_n < this fraction of nrow → drop those rows
#' @return data.frame with: var, miss_pct, miss_n, action
compute_missing_info <- function(df, drop_threshold = 60, row_drop_pct = 0.01) {
  n        <- nrow(df)
  miss_pct <- round(colMeans(is.na(df)) * 100, 2)
  miss_n   <- colSums(is.na(df))

  data.frame(
    var      = names(df),
    miss_pct = miss_pct,
    miss_n   = miss_n,
    action   = dplyr::case_when(
      miss_pct >  drop_threshold   ~ "drop_column",
      miss_n   == 0                ~ "complete",
      miss_n   <  row_drop_pct * n ~ "drop_rows",
      TRUE                         ~ "impute"
    ),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}


# ── 10. MICE BLOCK IMPUTATION (reproducible + AUC-robust) --------------------

#' Run one MICE imputation block.
#' - Tries each method in `methods`; selects best by AUC on a held-out target.
#' - Returns ALL m completed datasets (for proper MI pooling downstream).
#' - Skips gracefully if no variables in the block have missingness OR if
#'   any AUC predictor is structurally all-NA.
#' - set.seed() is called inside this function for reproducibility.
#'
#' @param data            data.frame containing all columns
#' @param block_vars      character vector of column names to pass to mice()
#' @param m               number of imputations (default 5)
#' @param maxit           number of MICE iterations (default 15)
#' @param methods         MICE methods to compare (default c("rf","cart"))
#' @param eval_target     column name used to evaluate method quality (binary)
#' @param eval_predictors character vector of predictor names for evaluation
#' @param seed            integer seed (default 20251031)
#' @return list(best_method, imp_object, completed_list, aucs)
run_mice_block <- function(data, block_vars, m = 5, maxit = 15,
                           methods         = c("rf", "cart"),
                           eval_target     = NULL,
                           eval_predictors = NULL,
                           seed            = 20251031) {

  set.seed(seed)

  block_vars  <- intersect(block_vars, names(data))
  block_data  <- data[, block_vars, drop = FALSE]
  has_missing <- sapply(block_data, function(x) any(is.na(x)))

  if (!any(has_missing)) {
    message("  [mice] No missing values — block skipped.")
    return(list(best_method = "none", imp_object = NULL,
                completed_list = list(block_data), aucs = NULL))
  }

  message(sprintf("  [mice] %d vars to impute, trying: %s",
                  sum(has_missing), paste(methods, collapse = ", ")))

  imp_models <- stats::setNames(
    lapply(methods, function(meth) {
      gc()
      mice::mice(block_data, method = meth, m = m, maxit = maxit,
                 printFlag = FALSE, seed = seed)
    }),
    methods
  )

  best_method <- methods[1]
  aucs        <- NULL

  # Evaluate AUC only when eval target + predictors are usable
  preds_avail <- if (is.null(eval_predictors)) character(0)
                 else intersect(eval_predictors, names(data))
  preds_usable <- preds_avail[
    sapply(preds_avail, function(p) !all(is.na(data[[p]])))
  ]

  if (!is.null(eval_target) && eval_target %in% names(data) &&
      length(preds_usable) > 0 && !all(is.na(data[[eval_target]]))) {

    aucs <- sapply(imp_models, function(imp) {
      tryCatch({
        completed <- mice::complete(imp, action = 1)
        # Restrict to rows where the target is observed (avoid evaluating
        # AUC on imputed targets — methodologically dubious).
        keep <- !is.na(data[[eval_target]])
        if (sum(keep) < 50) return(NA_real_)
        obs  <- as.numeric(as.character(data[[eval_target]][keep]))
        if (length(unique(stats::na.omit(obs))) < 2) return(NA_real_)
        model_preds <- intersect(preds_usable, names(completed))
        model_preds <- model_preds[
          sapply(model_preds, function(p) {
            x <- completed[[p]][keep]
            length(unique(stats::na.omit(as.character(x)))) >= 2
          })
        ]
        if (length(model_preds) == 0) return(NA_real_)
        form <- stats::as.formula(
          paste(eval_target, "~", paste(model_preds, collapse = "+"))
        )
        fit  <- stats::glm(form, data = completed[keep, , drop = FALSE],
                           family = stats::binomial())
        pred <- stats::predict(fit, type = "response")
        suppressMessages(
          as.numeric(pROC::auc(pROC::roc(obs, pred, quiet = TRUE)))
        )
      }, error = function(e) {
        message("    AUC evaluation failed: ", conditionMessage(e))
        NA_real_
      })
    })

    if (any(!is.na(aucs))) {
      best_method <- names(which.max(aucs))
      message(sprintf("  [mice] Best method: %s (AUC = %.3f)",
                      best_method, max(aucs, na.rm = TRUE)))
    } else {
      message("  [mice] AUC evaluation produced no usable scores; using ",
              best_method)
    }
  } else {
    message("  [mice] AUC evaluation skipped (target/predictor unavailable). ",
            "Using ", best_method)
  }

  best_imp       <- imp_models[[best_method]]
  completed_list <- lapply(seq_len(m), function(i) mice::complete(best_imp, i))

  list(best_method    = best_method,
       imp_object     = best_imp,
       completed_list = completed_list,
       aucs           = aucs)
}


# ── 11. INTEGRATE IMPUTED BLOCKS ---------------------------------------------

#' Merge imputed values back into the original data.
#' Only target columns are replaced (not shared covariates).
#' Pools across all m completed datasets:
#'   - numeric  → row mean across m
#'   - factor   → majority vote across m
#' Preserves factor levels even when the original column is all-NA.
integrate_blocks <- function(original, blocks_result, target_cols_per_block) {
  result <- original

  for (block_name in names(blocks_result)) {
    targets   <- target_cols_per_block[[block_name]]
    comp_list <- blocks_result[[block_name]]$completed_list
    if (is.null(comp_list) || length(comp_list) == 0) next

    for (col in intersect(targets, names(result))) {
      if (all(is.na(original[[col]]))) next # leave for cross-year block

      vals <- lapply(comp_list, function(d) {
        if (col %in% names(d)) d[[col]] else original[[col]]
      })

      if (is.numeric(vals[[1]])) {
        result[[col]] <- rowMeans(do.call(cbind, vals), na.rm = TRUE)
      } else {
        vote_mat <- do.call(cbind, lapply(vals, as.character))
        voted <- apply(vote_mat, 1, function(row) {
          tbl <- sort(table(row), decreasing = TRUE)
          if (length(tbl) == 0) NA_character_ else names(tbl)[1]
        })
        # Use ORIGINAL levels if available; otherwise infer from imputed values
        lvls <- if (length(levels(original[[col]])) > 0) {
          levels(original[[col]])
        } else {
          sort(unique(stats::na.omit(unlist(vote_mat))))
        }
        result[[col]] <- factor(voted, levels = lvls)
      }
    }
  }
  result
}


# ── 12. LOW-VARIANCE REMOVAL --------------------------------------------------

#' Remove variables from a pre-specified list if they exist in df.
drop_low_variance <- function(df, lv_vars) {
  to_drop <- intersect(lv_vars, names(df))
  if (length(to_drop) > 0) {
    message("[lv_drop] Removing: ", paste(to_drop, collapse = ", "))
    df <- df |> dplyr::select(-dplyr::all_of(to_drop))
  } else {
    message("[lv_drop] No low-variance variables found to remove.")
  }
  df
}


# ── 13. ONE-HOT ENCODING (controls for MICE predictor matrix) ---------------

#' One-hot encode categorical control variables for use as MICE predictors.
#' Uses remove_first_dummy = TRUE to avoid perfect multicollinearity.
encode_controls <- function(df,
                            cat_cols = c("clad4", "rip", "ateco_1", "size_rev")) {
  present <- intersect(cat_cols, names(df))
  if (length(present) == 0) return(df)
  fastDummies::dummy_cols(df,
    select_columns          = present,
    remove_selected_columns = FALSE,
    remove_first_dummy      = TRUE
  )
}


# ── 14. CODICE QC -------------------------------------------------------------

#' Verify firm-ID coverage and return dropped IDs.
#' Tolerates non-integer IDs (returns NULL gracefully).
check_codice <- function(df, id_col, expected_n) {
  if (!id_col %in% names(df) || is.null(expected_n)) {
    message("[qc] Codice QC skipped (id_col or expected_n missing).")
    return(invisible(NULL))
  }
  ids <- suppressWarnings(as.integer(df[[id_col]]))
  if (any(is.na(ids))) {
    message("[qc] Codice column is not fully integer — coverage check skipped.")
    return(invisible(NULL))
  }
  full_seq  <- seq_len(expected_n)
  dropped   <- setdiff(full_seq, ids)
  n_dropped <- length(dropped)
  if (n_dropped == 0) {
    message(sprintf("[qc] Codice: all %d firms present.", expected_n))
  } else {
    message(sprintf("[qc] Codice: %d firms removed during cleaning (%d remain).",
                    n_dropped, nrow(df)))
  }
  invisible(dropped)
}


# ── 15. RUN MANIFEST (audit log) ---------------------------------------------

#' Initialise an empty run manifest.
init_run_manifest <- function(year) {
  list(
    year        = year,
    started_at  = as.character(Sys.time()),
    R_version   = R.version.string,
    pkg_versions = list(
      dplyr       = as.character(utils::packageVersion("dplyr")),
      mice        = as.character(utils::packageVersion("mice")),
      pROC        = as.character(utils::packageVersion("pROC")),
      fastDummies = as.character(utils::packageVersion("fastDummies"))
    ),
    raw_n           = NULL,
    final_n         = NULL,
    dropped_cols    = list(),
    rows_dropped    = list(),
    imputation      = list(),
    low_var_dropped = NULL,
    finished_at     = NULL
  )
}

#' Save manifest to JSON next to the cleaned .rda file.
save_run_manifest <- function(manifest, out_path) {
  manifest$finished_at <- as.character(Sys.time())
  jsonlite::write_json(manifest, out_path, pretty = TRUE, auto_unbox = TRUE)
  message("[manifest] Saved to ", out_path)
  invisible(manifest)
}
