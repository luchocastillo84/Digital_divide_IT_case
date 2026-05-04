# =============================================================================
# year_configs.R  ·  Refactored
# Year-specific configuration for the Digital Divide cleaning pipeline.
#
# Refactor goals:
#   - Single shared DEFAULTS list; per-year configs only override what differs.
#   - Explicit `b5_coding`: "three_level" | "binary" | "absent"  (was just a
#     boolean flag — too coarse to capture the 2014/2019 binary case).
#   - Documented data caveats inline (the 2014/2019 B5 binary issue).
#   - Each year config is validated at load time (validate_year_configs()).
#   - imp_blocks are written defensively: predictors include S_B5a only when
#     the year actually has a usable S_B5a column.
#   - row_miss_threshold standardised to 0.80 across all years (already fixed
#     in the previous version — kept here to preserve the fix).
#
# Variable naming in var_map (vars&codes.xlsx):
#   acrom_1/var_dd1 = 2014
#   acrom_2/var_dd2 = 2015
#   acrom_3/var_dd3 = 2016  (prototype year)
#   acrom_4/var_dd4 = 2017
#   acrom_5/var_dd5 = 2018
#   acrom_6/var_dd6 = 2019
# =============================================================================

# ── DEFAULTS shared across years --------------------------------------------

YEAR_DEFAULTS <- list(
  id_col              = "codice_",
  revenue_col         = "ricavi_cl",
  size_col            = "clad3",
  ateco_from_dom1     = FALSE,
  ateco_raw_col       = "Ateco_1",
  collapse_D_E        = TRUE,
  b5_coding           = "three_level",   # see CRITICAL note below
  b5_cols             = paste0("B5", letters[1:7]),
  extra_keep          = c("dom4"),
  row_miss_threshold  = 0.80,
  weight_col          = "coeffin",       # kept through the pipeline now
  seed                = 20251031
)

# ── CRITICAL data caveat (B5 coding) ----------------------------------------
# Inspection of the raw files shows three different B5a coding regimes:
#   2014, 2019:           binary (0/1) — meaning to be verified against
#                         questionnaire (likely "any ICT function performed").
#                         For 2014/2019 we set b5_coding = "binary" and only
#                         the *_either specification is populated.
#   2015, 2016, 2018:     three-level (1=internal, 2=external, 3=neither).
#   2017:                 column structurally absent (b5_coding = "absent").
# This replaces the old binary flag b5_observed, which mis-classified 2014
# and 2019 (the B5 column exists in both, just in a different coding).
# ---------------------------------------------------------------------------

year_configs <- list(

  # ──────────────────────────────────────────────────────────────────────────
  # 2016  ·  PROTOTYPE YEAR
  # B5a–g three-level. Social media = C10a/C10c. Website = C8. WS = C9*.
  # ERP = E1 (still legacy). ateco_1 must be recoded from dom1.
  # ──────────────────────────────────────────────────────────────────────────
  `2016` = utils::modifyList(YEAR_DEFAULTS, list(
    year             = 2016,
    raw_file         = "ICT_Microdati_Anno_2016.txt",
    var_map_raw      = "acrom_3",
    var_map_std      = "var_dd3",
    id_col           = "codice",
    revenue_col      = "ricavi",
    size_col         = "clad4",
    ateco_from_dom1  = TRUE,
    ateco_raw_col    = "dom1",
    collapse_D_E     = FALSE,
    b5_coding        = "three_level",
    extra_keep       = c("dom4", "dom1"),
    imp_blocks = list(
      lmv = list(
        targets      = c("UMK_C8", "A2_C5"),
        covariates   = c("ateco_1", "clad4", "rip",
                         "A2_A2", "S_B1", "S_B2a", "S_B5a_internal"),
        methods      = c("rf", "cart"),
        eval_target  = "S_B1",
        eval_preds   = c("UMK_C8", "A2_C5", "S_B5a_internal")
      ),
      ws = list(
        targets      = c("UC_C9a", "UMK_C9c", "UM_C9f", "UMK_C9g"),
        covariates   = c("ateco_1", "clad4", "rip",
                         "A2_A2", "S_B1", "S_B2a", "UMK_C8"),
        methods      = c("rf", "cart", "logreg"),
        eval_target  = "UC_C9a",
        eval_preds   = c("A2_A2", "S_B1", "UMK_C8")
      )
    ),
    low_var          = c("S_B3", "A2_C1", "A2_C3", "A2_C4",
                         "UC_C9b", "UMK_C9d", "UMK_C9e"),
    expected_n       = 19089,
    out_file         = "Data/Processed/ICT16.rda",
    skills_file      = "Data/Processed/ict_skills_16.rda"
  )),

  # ──────────────────────────────────────────────────────────────────────────
  # 2014  ·  B5 BINARY (not three-level — only *_either is populated).
  # Social media = C9a/C9c. Website = C7. WS = C8*. ERP = E1.
  # ──────────────────────────────────────────────────────────────────────────
  `2014` = utils::modifyList(YEAR_DEFAULTS, list(
    year             = 2014,
    raw_file         = "ICT_Microdati_Anno_2014.txt",
    var_map_raw      = "acrom_1",
    var_map_std      = "var_dd1",
    id_col           = "Codice",
    revenue_col      = "Ricavi",
    size_col         = "clad4",
    ateco_from_dom1  = TRUE,
    ateco_raw_col    = "dom1",
    collapse_D_E     = FALSE,
    b5_coding        = "binary",              # see CRITICAL note above
    extra_keep       = c("dom4", "dom1"),
    imp_blocks = list(
      lmv = list(
        targets      = c("UMK_C7", "A2_C5a"),
        covariates   = c("ateco_1", "clad4", "rip",
                         "A2_A2", "S_B1", "S_B2a"),
        methods      = c("rf", "cart"),
        eval_target  = "S_B1",
        eval_preds   = c("UMK_C7", "A2_C5a")
      ),
      ws = list(
        targets      = c("UC_C8a", "UMK_C8c", "UM_C8g", "UMK_C8h"),
        covariates   = c("ateco_1", "clad4", "rip",
                         "A2_A2", "S_B1", "S_B2a", "UMK_C7"),
        methods      = c("rf", "cart", "logreg"),
        eval_target  = "UC_C8a",
        eval_preds   = c("A2_A2", "S_B1", "UMK_C7")
      )
    ),
    low_var          = c("S_B3", "A2_C1", "A2_C3", "A2_C4",
                         "UC_C8b", "UMK_C8d", "UMK_C8e"),
    expected_n       = 18953,
    out_file         = "Data/Processed/ICT14.rda",
    skills_file      = "Data/Processed/ict_skills_14.rda"
  )),

  # ──────────────────────────────────────────────────────────────────────────
  # 2015  ·  B5a–g three-level. Social media = C9a/C9c. Website = C7.
  # ──────────────────────────────────────────────────────────────────────────
  `2015` = utils::modifyList(YEAR_DEFAULTS, list(
    year             = 2015,
    raw_file         = "ICT_Microdati_Anno_2015.txt",
    var_map_raw      = "acrom_2",
    var_map_std      = "var_dd2",
    id_col           = "Codice",
    revenue_col      = "Ricavi",
    size_col         = "clad4",
    ateco_from_dom1  = TRUE,
    ateco_raw_col    = "dom1",
    collapse_D_E     = FALSE,
    b5_coding        = "three_level",
    extra_keep       = c("dom4", "dom1"),
    imp_blocks = list(
      lmv = list(
        targets      = c("UMK_C7", "A2_C5"),
        covariates   = c("ateco_1", "clad4", "rip",
                         "A2_A2", "S_B1", "S_B2a", "S_B5a_internal"),
        methods      = c("rf", "cart"),
        eval_target  = "S_B1",
        eval_preds   = c("UMK_C7", "A2_C5", "S_B5a_internal")
      ),
      ws = list(
        targets      = c("UC_C8a", "UMK_C8c", "UM_C8g", "UMK_C8h"),
        covariates   = c("ateco_1", "clad4", "rip",
                         "A2_A2", "S_B1", "S_B2a", "UMK_C7"),
        methods      = c("rf", "cart", "logreg"),
        eval_target  = "UC_C8a",
        eval_preds   = c("A2_A2", "S_B1", "UMK_C7")
      )
    ),
    low_var          = c("S_B3", "A2_C1", "A2_C3", "A2_C4",
                         "UC_C8b", "UMK_C8d", "UMK_C8e"),
    expected_n       = 19475,
    out_file         = "Data/Processed/ICT15.rda",
    skills_file      = "Data/Processed/ict_skills_15.rda"
  )),

  # ──────────────────────────────────────────────────────────────────────────
  # 2017  ·  B5a–g STRUCTURALLY MISSING. Names: D1/D2 (ERP/CRM).
  # ──────────────────────────────────────────────────────────────────────────
  `2017` = utils::modifyList(YEAR_DEFAULTS, list(
    year             = 2017,
    raw_file         = "ICT_Microdati_Anno_2017.txt",
    var_map_raw      = "acrom_4",
    var_map_std      = "var_dd4",
    b5_coding        = "absent",
    imp_blocks = list(
      ws = list(
        targets      = c("UMK_C8", "UC_C9a", "UMK_C9c", "UM_C9f", "UMK_C9g"),
        covariates   = c("ateco_1", "clad4", "rip",
                         "A2_A2", "S_B1", "S_B2a"),
        methods      = c("rf", "logreg", "cart"),
        eval_target  = "UMK_C8",
        eval_preds   = c("A2_A2", "S_B1", "S_B2a")
      )
    ),
    low_var          = c("S_B3", "A2_C1", "A2_C3", "A2_C4",
                         "UC_C9b", "UMK_C9d", "UMK_C9e"),
    expected_n       = 21410,
    out_file         = "Data/Processed/ICT17.rda",
    skills_file      = "Data/Processed/ict_skills_17.rda"
  )),

  # ──────────────────────────────────────────────────────────────────────────
  # 2018  ·  B5a–g three-level. ERP = D1. Website = C8. WS = C9*.
  # NOTE: C9f/C9g swapped vs 2017 — handled by var_map crosswalk.
  # ──────────────────────────────────────────────────────────────────────────
  `2018` = utils::modifyList(YEAR_DEFAULTS, list(
    year             = 2018,
    raw_file         = "ICT_Microdati_Anno_2018.txt",
    var_map_raw      = "acrom_5",
    var_map_std      = "var_dd5",
    b5_coding        = "three_level",
    imp_blocks = list(
      lmv = list(
        targets      = c("UMK_C8", "A2_C5"),
        covariates   = c("ateco_1", "clad4", "rip",
                         "A2_A2", "S_B1", "S_B2a", "S_B5a_internal"),
        methods      = c("rf", "cart"),
        eval_target  = "S_B1",
        eval_preds   = c("UMK_C8", "A2_C5", "S_B5a_internal")
      ),
      ws = list(
        targets      = c("UC_C9a", "UMK_C9c", "UM_C9g", "UMK_C9f"),
        covariates   = c("ateco_1", "clad4", "rip",
                         "A2_A2", "S_B1", "S_B2a", "UMK_C8"),
        methods      = c("rf", "cart", "logreg"),
        eval_target  = "UC_C9a",
        eval_preds   = c("A2_A2", "S_B1", "UMK_C8")
      )
    ),
    low_var          = c("S_B3", "A2_C1", "A2_C3", "A2_C4",
                         "UC_C9b", "UMK_C9d", "UMK_C9e"),
    expected_n       = 22079,
    out_file         = "Data/Processed/ICT18.rda",
    skills_file      = "Data/Processed/ict_skills_18.rda"
  )),

  # ──────────────────────────────────────────────────────────────────────────
  # 2019  ·  B5 BINARY (lowercase column names; only *_either populated).
  # ──────────────────────────────────────────────────────────────────────────
  `2019` = utils::modifyList(YEAR_DEFAULTS, list(
    year             = 2019,
    raw_file         = "ICT_Microdati_Anno_2019.txt",
    var_map_raw      = "acrom_6",
    var_map_std      = "var_dd6",
    b5_coding        = "binary",              # see CRITICAL note above
    imp_blocks = list(
      ws = list(
        targets      = c("UMK_C7", "UC_C8a", "UMK_C8c", "UM_C8f", "UMK_C8g"),
        covariates   = c("ateco_1", "clad4", "rip",
                         "A2_A2", "S_B1", "S_B2a"),
        methods      = c("rf", "cart", "logreg"),
        eval_target  = "UMK_C7",
        eval_preds   = c("A2_A2", "S_B1", "S_B2a")
      )
    ),
    low_var          = c("S_B3", "A2_C1", "A2_C3", "A2_C4"),
    expected_n       = 19915,                  # verified from raw file
    out_file         = "Data/Processed/ICT19.rda",
    skills_file      = "Data/Processed/ict_skills_19.rda"
  ))
)


# ── VALIDATION ---------------------------------------------------------------

#' Validate year_configs at load time. Raises a clean error if anything is
#' missing or inconsistent — fails loudly rather than silently downstream.
validate_year_configs <- function(cfgs = year_configs) {
  required <- c("year", "raw_file", "var_map_raw", "var_map_std", "id_col",
                "revenue_col", "size_col", "b5_coding", "imp_blocks",
                "low_var", "expected_n", "out_file", "skills_file")
  ok_b5 <- c("three_level", "binary", "absent")
  for (nm in names(cfgs)) {
    cfg <- cfgs[[nm]]
    miss <- setdiff(required, names(cfg))
    if (length(miss) > 0)
      stop(sprintf("[year_configs] %s missing: %s",
                   nm, paste(miss, collapse = ", ")), call. = FALSE)
    if (!cfg$b5_coding %in% ok_b5)
      stop(sprintf("[year_configs] %s b5_coding must be one of %s",
                   nm, paste(ok_b5, collapse = "|")), call. = FALSE)
  }
  message("[year_configs] All ", length(cfgs), " year configurations validated.")
  invisible(TRUE)
}

# Validate immediately on source().
validate_year_configs(year_configs)
