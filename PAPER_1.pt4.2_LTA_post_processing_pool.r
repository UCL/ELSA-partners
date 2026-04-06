################################################################################
# Post-processing: rebase, extract summaries, check state ordering, pool
################################################################################
# PURPOSE:
#   1. For each of 22 imputations and each model (TE / DE):
#      a. Load raw_results from imputation_loop.r
#      b. Determine state ordering (severity-based)
#      c. Rebase + refit with SE
#      d. Extract regression_summaries() → clean tibbles
#      e. Save summaries_{TE/DE}_imp{i}.rds
#   2. Sense-check state ordering consistency across 22 imputations
#   3. Pool summaries using Rubin's rules
#   4. Produce key results tables (exposure transitions + maintenance)
#
# INPUTS:
#   raw_results_{TE/DE}_imp{i}.rds  (from imputation_loop.r)
#
# OUTPUTS:
#   summaries_{TE/DE}_imp{i}.rds    — per-imputation clean tibbles
#   state_order_log_{TE/DE}.rds     — ordering sense-check log
#   pooled_{TE/DE}.rds              — Rubin's rules pooled results
################################################################################


# ==============================================================================
# 1. LIBRARIES AND FUNCTIONS
# ==============================================================================

library(dplyr)
library(purrr)
library(tidyr)
library(mice)
library(LMest)
library(knitr)

source("Sources/Paper1_func.R")


# ==============================================================================
# 2. PATHS
# ==============================================================================

rds_path    <- "~/private_WP5/WP5_data/rds/"
models_path <- "~/private_WP5/WP5_data/model_fits/imputations/"

n_imputations <- 22


# ==============================================================================
# 3. REBUILD FORMULAS
# (identical to imputation_loop.r — must stay in sync)
# ==============================================================================

updated_mids <- readRDS(paste0(rds_path, "mice_updatedMIDS_VH.rds"))

prep_lta_data <- function(imp_df) {
  imp_df %>%
    mutate(
      ssupport6     = expm1(ssupport6_log1p),
      ssupport6_cat = case_when(
        partner == "No-partner"  ~ "No-partner",
        ssupport6 <= 1.5         ~ "supportive partner",
        ssupport6 >  1.5         ~ "less than supportive partner",
        TRUE                     ~ NA_character_
      ) %>% factor(levels = c("No-partner",
                               "less than supportive partner",
                               "supportive partner"))
    ) %>%
    select(all_of(Variables))
}

Variables <- c(
  "idauniq", "time",
  "raagey_z", "Sex_BIN", "raeduc_e",
  "wealthQ_bin", "MLTC", "cesd_lat_shifted", "loneliness_group",
  "partner", "ssupport6_cat",
  "painchr_ext", "lvimp_shifted_cat3"
)

one_imp            <- prep_lta_data(mice::complete(updated_mids, 1))
latent_state_vars  <- c("lvimp_shifted_cat3", "painchr_ext")

TE_initial_prob_vars <- c("Sex_BIN", "raeduc_e", "wealthQ_bin",
                           "ssupport6_cat", "MLTC", "raagey_z")
DE_vars              <- c(TE_initial_prob_vars, "cesd_lat_shifted", "loneliness_group")

fmLatent_TE <- lmestFormula(data = one_imp, response = latent_state_vars,
                             LatentInitial    = TE_initial_prob_vars,
                             LatentTransition = TE_initial_prob_vars)

fmLatent_DE <- lmestFormula(data = one_imp, response = latent_state_vars,
                             LatentInitial    = DE_vars,
                             LatentTransition = DE_vars)

ordering_indicators <- c("lvimp_shifted_cat3", "painchr_ext")
ordering_weights    <- c(0.6, 0.4)
# Weight-based state ordering: composite score = 0.6 × E[lvimp | state] + 0.4 × E[painchr | state]
# States ranked order(composite_score): rank 1 = healthiest, rank 4 = HICP.
# Weights are needed because the two indicators have different ranges:
#   lvimp_shifted_cat3: 0-2 (3 levels); painchr_ext: 0-1 (binary).
# Equal weights would let the wider-range indicator dominate; 0.6/0.4 gives
# pain impact (functional consequence) slightly more pull than the chronic/acute binary.
# Weights do not need to be precisely calibrated — they only need to be consistent
# across imputations. A state swap can only occur if two states have nearly identical
# composite scores; this is flagged by check_state_ordering() (n_unique_orig > 1).


# ==============================================================================
# 4. PER-IMPUTATION EXTRACTION FUNCTION
# ==============================================================================

extract_one <- function(imp, label, fm_latent, fm_responses) {

  cat(sprintf("\n[%s] Imputation %d of %d\n", label, imp, n_imputations))

  raw <- readRDS(paste0(models_path, "raw_results_", label, "_imp", imp, ".rds"))

  # ── state ordering ──────────────────────────────────────────────────────────
  ordering <- Determine_state_ordering(
    raw$best_model,
    indicator_names   = ordering_indicators,
    indicator_weights = ordering_weights
  )
  state_order <- ordering$severity_order

  cat(sprintf("  State order: %s | Weighted composite scores: %s\n",
              paste(state_order, collapse = "-"),
              paste(round(ordering$composite_severity, 3), collapse = " ")))

  # ── rebase + SE ─────────────────────────────────────────────────────────────
  model_rebased <- rebase_and_refit_lmest(
    fit_model        = raw$best_model,
    state_order      = state_order,
    latent_formula   = fm_latent,
    response_formula = fm_responses,
    out_SE           = TRUE,
    plot             = FALSE
  )

  # ── summaries ───────────────────────────────────────────────────────────────
  summaries <- regression_summaries(Model_fit = model_rebased)

  saveRDS(summaries,
          paste0(models_path, "summaries_", label, "_imp", imp, ".rds"))

  # ── state order log entry ───────────────────────────────────────────────────
  log_entry <- tibble(
    imp             = imp,
    new_state       = 1:4,
    orig_state      = state_order,
    severity_score  = ordering$composite_severity[state_order]
  )

  rm(raw, model_rebased, summaries)
  gc()

  return(log_entry)
}


# ==============================================================================
# 5. LOOP — TE
# ==============================================================================

cat("\n", rep("#", 80), "\n")
cat("POST-PROCESSING: TOTAL EFFECTS\n")
cat(rep("#", 80), "\n")

state_order_log_TE <- map(1:n_imputations, function(i) {
  extract_one(i, "TE", fmLatent_TE$latentFormula, fmLatent_TE$responsesFormula)
})

state_order_log_TE <- bind_rows(state_order_log_TE)
saveRDS(state_order_log_TE, paste0(models_path, "state_order_log_TE.rds"))
cat("\nTE post-processing complete.\n")


# ==============================================================================
# 6. LOOP — DE
# ==============================================================================

cat("\n", rep("#", 80), "\n")
cat("POST-PROCESSING: DIRECT EFFECTS\n")
cat(rep("#", 80), "\n")

state_order_log_DE <- map(1:n_imputations, function(i) {
  extract_one(i, "DE", fmLatent_DE$latentFormula, fmLatent_DE$responsesFormula)
})

state_order_log_DE <- bind_rows(state_order_log_DE)
saveRDS(state_order_log_DE, paste0(models_path, "state_order_log_DE.rds"))
cat("\nDE post-processing complete.\n")


# ==============================================================================
# 7. STATE ORDERING SENSE-CHECK
# ==============================================================================

check_state_ordering <- function(log_df, label) {

  cat(sprintf("\n--- State ordering consistency: %s ---\n", label))

  # How many unique orig_state values map to each new_state?
  consistency <- log_df %>%
    group_by(new_state) %>%
    summarise(
      n_unique_orig   = n_distinct(orig_state),
      orig_states     = paste(sort(unique(orig_state)), collapse = ","),
      sev_mean        = round(mean(severity_score), 3),
      sev_sd          = round(sd(severity_score),   3),
      sev_min         = round(min(severity_score),  3),
      sev_max         = round(max(severity_score),  3),
      .groups = "drop"
    )

  print(kable(consistency,
              caption = sprintf("%s — state ordering consistency across %d imputations",
                                label, n_imputations)))

  # Flag any swaps
  swaps <- consistency %>% filter(n_unique_orig > 1)
  if (nrow(swaps) == 0) {
    cat("  ✓ All imputations produced identical state ordering.\n")
  } else {
    cat(sprintf("  ⚠ STATE SWAP DETECTED in %d state(s):\n", nrow(swaps)))
    print(swaps)
    cat("  → Inspect conditional probability plots for affected imputations.\n")

    # Which imputations have non-modal ordering?
    modal_order <- log_df %>%
      group_by(new_state) %>%
      count(orig_state) %>%
      slice_max(n, n = 1) %>%
      select(new_state, modal_orig = orig_state)

    deviating <- log_df %>%
      left_join(modal_order, by = "new_state") %>%
      filter(orig_state != modal_orig) %>%
      distinct(imp) %>%
      pull(imp)

    cat(sprintf("  Deviating imputations: %s\n", paste(deviating, collapse = ", ")))
  }

  invisible(consistency)
}

check_state_ordering(state_order_log_TE, "TE")
check_state_ordering(state_order_log_DE, "DE")


# ==============================================================================
# 8. RUBIN'S RULES POOLING
# ==============================================================================

pool_summaries <- function(label, n_imp, path) {

  cat(sprintf("\nPooling %s across %d imputations...\n", label, n_imp))

  summaries_list <- map(1:n_imp, function(i) {
    f <- paste0(path, "summaries_", label, "_imp", i, ".rds")
    if (!file.exists(f)) stop(sprintf("Missing: %s", f))
    readRDS(f)
  })

  # ── transitions ─────────────────────────────────────────────────────────────
  trans_all <- map_dfr(summaries_list, ~ .x$transitions, .id = "imp") %>%
    mutate(imp = as.integer(imp))

  trans_pooled <- trans_all %>%
    group_by(varname, Transition) %>%
    summarise(
      m        = n(),
      Q_bar    = mean(Coefficient,  na.rm = TRUE),
      U_bar    = mean(StdError^2,   na.rm = TRUE),   # within-imputation variance
      B        = var(Coefficient,   na.rm = TRUE),    # between-imputation variance
      T_var    = U_bar + (1 + 1/m) * B,              # Rubin total variance
      SE_pool  = sqrt(T_var),
      t_pool   = Q_bar / SE_pool,
      n_NA_SE  = sum(is.na(StdError)),                # count NaN/NA SEs (sparse transitions)
      N_obs    = mean(N_obs_trans, na.rm = TRUE),
      EPV      = mean(EPV, na.rm = TRUE),
      .groups  = "drop"
    ) %>%
    mutate(
      Sep_Flag = case_when(
        n_NA_SE > 0          ~ sprintf("SE NA in %d/%d imps", n_NA_SE, m),
        abs(Q_bar) > 8       ~ "!!!",
        abs(t_pool) > 2.58   ~ "***",
        abs(t_pool) > 1.96   ~ "**",
        abs(t_pool) > 1.64   ~ "*",
        TRUE                 ~ ""
      )
    )

  # ── initial states ──────────────────────────────────────────────────────────
  init_all <- map_dfr(summaries_list, ~ .x$initial_states, .id = "imp") %>%
    mutate(imp = as.integer(imp))

  init_pooled <- init_all %>%
    group_by(varname, State) %>%
    summarise(
      m        = n(),
      Q_bar    = mean(Coefficient, na.rm = TRUE),
      U_bar    = mean(StdError^2,  na.rm = TRUE),
      B        = var(Coefficient,  na.rm = TRUE),
      T_var    = U_bar + (1 + 1/m) * B,
      SE_pool  = sqrt(T_var),
      t_pool   = Q_bar / SE_pool,
      n_NA_SE  = sum(is.na(StdError)),
      .groups  = "drop"
    )

  list(transitions = trans_pooled, initial_states = init_pooled)
}

pooled_TE <- pool_summaries("TE", n_imputations, models_path)
pooled_DE <- pool_summaries("DE", n_imputations, models_path)

saveRDS(pooled_TE, paste0(models_path, "pooled_TE.rds"))
saveRDS(pooled_DE, paste0(models_path, "pooled_DE.rds"))


# ==============================================================================
# 9. KEY RESULTS TABLES
# (mirrors LTA_pull_res_tables.r logic, applied to pooled output)
# ==============================================================================

# Transitions toward HICP (State 4)
hicp_transitions <- c("1 -> 2", "1 -> 3", "1 -> 4", "2 -> 4", "3 -> 4")

# Transitions out of HICP (recovery)
recovery_transitions <- c("4 -> 1", "4 -> 2", "4 -> 3")

print_key_tables <- function(pooled, label) {

  cat(sprintf("\n=== %s KEY RESULTS ===\n", label))

  cat("\n-- Initial state probabilities (ssupport6_cat) --\n")
  pooled$initial_states %>%
    filter(grepl("^ssu", varname)) %>%
    arrange(State, varname) %>%
    kable(caption = sprintf("%s — initial state probabilities (ssupport6_cat)", label)) %>%
    print()

  cat("\n-- Transitions toward HICP (ssupport6_cat) --\n")
  pooled$transitions %>%
    filter(Transition %in% hicp_transitions, grepl("^ssu", varname)) %>%
    arrange(Transition, varname) %>%
    kable(caption = sprintf("%s — transitions toward HICP", label)) %>%
    print()

  cat("\n-- Transitions out of HICP / recovery (ssupport6_cat) --\n")
  pooled$transitions %>%
    filter(Transition %in% recovery_transitions, grepl("^ssu", varname)) %>%
    arrange(Transition, varname) %>%
    kable(caption = sprintf("%s — transitions out of HICP (recovery)", label)) %>%
    print()
}

print_key_tables(pooled_TE, "TE")
print_key_tables(pooled_DE, "DE")

cat("\nPost-processing complete.\n")
