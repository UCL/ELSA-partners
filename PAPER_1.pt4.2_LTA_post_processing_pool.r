################################################################################
# Post-processing: rebase, extract summaries, check state ordering, pool
################################################################################
# PURPOSE:
#   1. For each of 22 imputations and each model (TE / DE):
#      a. Load raw_results from PAPER_1.pt4.1_imputation_loop.r
#      b. Determine state ordering (severity-based)
#      c. Rebase + refit (once) to extract estimates and SE, sense check plots for state ordering
#      d. Extract regression_summaries() → clean tibbles
#      e. Save summaries_{TE/DE}_imp{i}.rds
#   2. Pool summaries using Rubin's rules
#   3. Produce key results tables (exposure transitions + maintenance)
#
# INPUTS:
#   raw_results_{TE/DE}_imp{i}.rds  (from PAPER_1.pt4.1_imputation_loop.r)
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

rds_path    <- path.expand("~/private_WP5/WP5_data/rds/")
models_path <- path.expand("~/private_WP5/WP5_data/model_fits/imputations/")

n_imputations <- 22


# ==============================================================================
# 3. REBUILD FORMULAS
# (identical to PAPER_1.pt4.1_imputation_loop.r — must stay in sync)
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

  scores_ordered <- round(ordering$composite_severity[state_order], 3)
  gaps           <- diff(scores_ordered)
  min_gap_idx    <- which.min(gaps)
  min_gap_label  <- sprintf("S%d-S%d=%.3f", min_gap_idx, min_gap_idx + 1, gaps[min_gap_idx])
  gap_flag       <- if (any(gaps < 0.05)) " !!! NEAR-TIE — check ordering" else " OK"
  cat(sprintf("  S1=%.3f  S2=%.3f  S3=%.3f  S4=%.3f  (orig order: %s)  min gap: %s%s\n",
              scores_ordered[1], scores_ordered[2],
              scores_ordered[3], scores_ordered[4],
              paste(state_order, collapse = "-"),
              min_gap_label, gap_flag))

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

  log_entry
}


# ==============================================================================
# 4b. CROSS-IMPUTATION LL CHECK
# ==============================================================================
# Each of the 22 imputations is a slightly different completed dataset drawn from
# the same MICE posterior, so all 22 should yield similar best log-likelihoods
# (same n, same model, similar data). A large negative outlier LL indicates that
# the optimiser settled in a local maximum for that imputation — its parameters
# describe a different solution and should not be pooled with the other 21.
#
# check_ll_consistency() reads best_model$lk from every raw_results_{label}_imp{i}.rds,
# prints the range/median/SD, flags any imputation whose best LL is more than 2 SD
# below the median with a warning, and plots a histogram with the median (green)
# and the 2-SD threshold (red dashed) marked.
#
# Action if outlier flagged: inspect that imputation's raw_results manually.
# Options are (a) refit with more random starts, or (b) exclude from pooling and
# document the decision.
# ==============================================================================

# check_ll_consistency() — defined in Sources/Paper1_func.R

lls_TE <- check_ll_consistency("TE", n_imputations, models_path)
lls_DE <- check_ll_consistency("DE", n_imputations, models_path)


# ==============================================================================
# 5. LOOP — TE
# ==============================================================================

cat("\n", rep("#", 80), "\n")
cat("POST-PROCESSING: TOTAL EFFECTS\n")
cat(rep("#", 80), "\n")

# For each of the 22 imputations, extract_one():
#   1. Loads raw_results_TE_imp{i}.rds (best lmest model from the imputation loop)
#   2. Computes a weighted composite severity score per state from Psi (conditional
#      response probabilities): 0.6 × E[lvimp|state] + 0.4 × E[painchr|state]
#   3. Orders the 4 states by composite score (rank 1 = healthiest, 4 = HICP)
#      and prints the ordered scores + minimum adjacent gap for a quick sanity check
#   4. Rebases the model: permutes parameter matrices (Piv, Pi, Ga, Be) so that
#      states follow the severity order, then refits with out_SE=TRUE to obtain
#      numerical Hessian SEs at the rebased solution
#   5. Calls regression_summaries() to extract clean tibbles of transition and
#      initial-state coefficients with SEs
#   6. Saves summaries_TE_imp{i}.rds — these are the per-imputation inputs to
#      Rubin's rules pooling in section 8
#   7. Returns a 4-row log tibble: imp, new_state (1-4), orig_state (lmest label),
#      severity_score — used below to check cross-imputation ordering consistency
#
# bind_rows() collects the 22 × 4 log tibbles into state_order_log_TE (88 rows),
# which is then saved and passed to check_state_ordering() in section 7.
state_order_log_TE <- bind_rows(map(1:n_imputations, function(i) {
  extract_one(i, "TE", fmLatent_TE$latentFormula, fmLatent_TE$responsesFormula)
}))
saveRDS(state_order_log_TE, paste0(models_path, "state_order_log_TE.rds"))
cat("\nTE post-processing complete.\n")


# ==============================================================================
# 6. LOOP — DE
# ==============================================================================

cat("\n", rep("#", 80), "\n")
cat("POST-PROCESSING: DIRECT EFFECTS\n")
cat(rep("#", 80), "\n")

state_order_log_DE <- bind_rows(map(1:n_imputations, function(i) {
  extract_one(i, "DE", fmLatent_DE$latentFormula, fmLatent_DE$responsesFormula)
}))
saveRDS(state_order_log_DE, paste0(models_path, "state_order_log_DE.rds"))
cat("\nDE post-processing complete.\n")


# ==============================================================================
# 7. STATE ORDERING SENSE-CHECK
# ==============================================================================
# check_state_ordering()
# ------------------------------------------------------------------------------
# PURPOSE:
#   Verify that the severity-based state reordering (Determine_state_ordering +
#   rebase_and_refit_lmest) produced a consistent mapping across all M imputations.
#   Must pass before Rubin's rules pooling: pooling averages parameters labelled
#   "State 2" across imputations, so "State 2" must refer to the same latent
#   content in every imputation.
#
# INPUT:
#   log_df  — the 4 × M tibble produced by bind_rows() of extract_one() log
#             entries. Columns: imp, new_state (1–4), orig_state (lmest label
#             before rebase), severity_score (composite score at rebase).
#
# HOW THE CHECK WORKS:
#   Within any single imputation, the reordering is always monotone by
#   construction: order(composite_score) guarantees score[S1] < score[S2] <
#   score[S3] < score[S4]. A within-imputation violation is mathematically
#   impossible.
#
#   The meaningful question is cross-imputation: does the same latent content
#   consistently land in the same slot? This is assessed via the severity score
#   ranges. For each new_state k, sev_min and sev_max are the lowest and highest
#   composite scores that landed in slot k across all M imputations.
#
#   OVERLAP TEST: if sev_max[k] > sev_min[k+1], the score ranges of adjacent
#   slots bleed into each other. This means the content assigned to slot k in
#   some imputations is more severe than the content assigned to slot k+1 in
#   others — the boundary between these two states is unstable and the states
#   are not consistently identified across imputations.
#
#   NOTE: n_unique_orig is reported in the table but is NOT used for the hard
#   stop. lmest assigns arbitrary labels (1–4) at each fit, so across M
#   imputations all four original labels will inevitably appear in every slot.
#   n_unique_orig = 4 for all slots is normal and expected — it carries no
#   information about ordering stability.
#
# OUTPUT:
#   Prints a summary table (new_state, n_unique_orig, orig_states, sev_mean,
#   sev_sd, sev_min, sev_max) and a pass/fail message. Calls stop() if any
#   adjacent pair overlaps, aborting execution before pooling runs.
#   Returns the consistency tibble invisibly.
# ------------------------------------------------------------------------------

# check_state_ordering() — defined in Sources/Paper1_func.R

check_state_ordering(state_order_log_TE, "TE", n_imputations)
check_state_ordering(state_order_log_DE, "DE", n_imputations)


# ==============================================================================
# 8. RUBIN'S RULES POOLING
# ==============================================================================
#
# pool_summaries() reads all summaries_{label}_imp{i}.rds and pools using
# Rubin's rules. Output is a list with $transitions and $initial_states tibbles.
#
# Output columns:
#   Q_bar    — pooled coefficient (mean across imputations)
#   U_bar    — within-imputation variance (mean of squared SEs)
#   B        — between-imputation variance (variance of coefficients)
#   T_var    — total variance: U_bar + (1 + 1/m) × B
#   SE_pool  — pooled SE: sqrt(T_var)
#   t_pool   — pooled t-statistic: Q_bar / SE_pool
#   nu_BR    — Barnard-Rubin df: (m-1) × (1 + U_bar/((1+1/m)×B))²
#              Large (>> 21): B negligible, t ≈ normal. Equal to 21: B dominates.
#   p_pool   — two-sided p-value from t(nu_BR)
#   n_NA_SE  — imputations where SE was NaN (0 = all valid)
#   N_obs    — mean observed count in the from-state / state across imputations
#   EPV      — events per variable (N_obs / n_predictors), mean across imputations
#   Sep_Flag   — data quality flag: "OK", "LOW EPV=x", "SE NA in k/22 imps"
#   Signif     — significance stars: "***"/"**"/"*" for p<0.01/0.05/0.10, "" otherwise.
#                Empty when EPV < 10 (coefficient not reportable).
#   Comment    — "Reliable" (p<0.05, EPV OK), "n.s", "Exclude — low EPV",
#                "Unreliable SE"
#
# ==============================================================================

# pool_summaries() — defined in Sources/Paper1_func.R

pooled_TE <- pool_summaries("TE", n_imputations, models_path)
pooled_DE <- pool_summaries("DE", n_imputations, models_path)

saveRDS(pooled_TE, paste0(models_path, "pooled_TE.rds"))
saveRDS(pooled_DE, paste0(models_path, "pooled_DE.rds"))


# ==============================================================================
# 8b. BARNARD-RUBIN df DIAGNOSTIC
# Assesses whether finite df materially changes significance conclusions vs
# z-thresholds. If nu_BR is consistently near m-1=21, z-based inference would
# have been adequate. Low nu_BR (< 10) with p_pool near 0.05/0.01 flags rows
# where the correction matters.
# ==============================================================================

# diagnose_barnard_rubin() — defined in Sources/Paper1_func.R

diagnose_barnard_rubin(pooled_TE, "TE")
diagnose_barnard_rubin(pooled_DE, "DE")


# ==============================================================================
# 9. KEY RESULTS TABLES
# (mirrors LTA_pull_res_tables.r logic, applied to pooled output)
# ==============================================================================

print_key_tables <- function(pooled, label, EPV_threshold = 20, round = 6) {
  
  hicp_transitions     <- c("1 -> 2", "1 -> 3", "1 -> 4", "2 -> 4", "3 -> 4")
  recovery_transitions <- c("4 -> 1", "4 -> 2", "4 -> 3")
  
  cat(sprintf("\n=== %s KEY RESULTS ===\n", label))
  
  cat("\n-- Initial state probabilities (ssupport6_cat) --\n")
  pooled$initial_states %>%
    filter(grepl("^ssu", varname), EPV >= EPV_threshold) %>%
    arrange(State, varname) %>%
    mutate(across(where(is.numeric), ~ round(., round))) %>%
    kable(caption = sprintf("%s — initial state probabilities (ssupport6_cat)", label)) %>%
    print()
  
  cat("\n-- Transitions toward HICP (ssupport6_cat) --\n")
  pooled$transitions %>%
    filter(Transition %in% hicp_transitions, grepl("^ssu", varname), EPV >= EPV_threshold) %>%
    arrange(Transition, varname) %>%
    mutate(across(where(is.numeric), ~ round(., round))) %>%
    kable(caption = sprintf("%s — transitions toward HICP", label)) %>%
    print()
  
  cat("\n-- Transitions out of HICP / recovery (ssupport6_cat) --\n")
  pooled$transitions %>%
    filter(Transition %in% recovery_transitions, grepl("^ssu", varname), EPV >= EPV_threshold) %>%
    arrange(Transition, varname) %>%
    mutate(across(where(is.numeric), ~ round(., round))) %>%
    kable(caption = sprintf("%s — transitions out of HICP (recovery)", label)) %>%
    print()
}

print_key_tables(pooled_TE, "TE", EPV_threshold = 10, round = 3)
print_key_tables(pooled_DE, "DE", EPV_threshold = 10, round = 3)


# ==============================================================================
# 10. EFFECT PLOTS
# ==============================================================================

EPV_PLOT_THRESHOLD <- 10

hicp_transitions     <- c("1 -> 2", "1 -> 3", "1 -> 4", "2 -> 4", "3 -> 4")
recovery_transitions <- c("4 -> 1", "4 -> 2", "4 -> 3")

# ── label lookups ─────────────────────────────────────────────────────────────
varname_labels <- c(
  "ssupport6_catless than supportive partner" = "Less than supportive partner \n vs no partner",
  "ssupport6_catsupportive partner"           = "Supportive partner \n vs no partner",
  "cesd_lat_shifted"                          = "CES-D (continuous)",
  "loneliness_groupHigh"                      = "High loneliness \n vs not high loneliness"
)

state_labels <- c(
  "State 1" = "No pain, No impact on functioning",
  "State 2" = "AP or CP, no high-impact \n vs No pain, no impact",
  "State 3" = "NO CP, high-impact \n vs No pain, no impact",
  "State 4" = "HICP vs \nNo pain, no impact"
)

# Caption used across all plots
state_caption <- stringr::str_wrap(paste(
  "Latent pain states — State 1: no pain and no functional limitations;",
  "State 2: acute or chronic pain without high-impact limitations;",
  "State 3: high-impact limitations without chronic pain;",
  "State 4: high-impact chronic pain (HICP).",
  "CES-D and loneliness included in the direct effects (DE) model only.",
  "Estimates with EPV < 10 excluded. Error bars: 95% CI (Barnard-Rubin df)."
), width = 100)

# shared theme applied to all three figures
plot_theme <- theme_minimal(base_size = 11) +
  theme(legend.position  = "bottom",
        plot.caption      = element_text(hjust = 0, size = 8),
        strip.text.y      = element_text(angle = 0))

# ── combine TE and DE, compute Barnard-Rubin CIs ─────────────────────────────
init_combined <- bind_rows(
  pooled_TE$initial_states %>% mutate(Model = "TE"),
  pooled_DE$initial_states %>% mutate(Model = "DE")
) %>%
  filter(EPV >= EPV_PLOT_THRESHOLD) %>%
  filter(grepl("^ssu", varname) | grepl("^loneli", varname) | grepl("^cesd", varname)) %>%
  mutate(
    CI_lo   = Q_bar - qt(0.975, df = nu_BR) * SE_pool,
    CI_hi   = Q_bar + qt(0.975, df = nu_BR) * SE_pool,
    varname = recode(varname, !!!varname_labels),
    State   = recode(State,   !!!state_labels)
  )

trans_combined <- bind_rows(
  pooled_TE$transitions %>% mutate(Model = "TE"),
  pooled_DE$transitions %>% mutate(Model = "DE")
) %>%
  filter(EPV >= EPV_PLOT_THRESHOLD) %>%
  filter(grepl("^ssu", varname) | grepl("^loneli", varname) | grepl("^cesd", varname)) %>%
  mutate(
    CI_lo    = Q_bar - qt(0.975, df = nu_BR) * SE_pool,
    CI_hi    = Q_bar + qt(0.975, df = nu_BR) * SE_pool,
    varname  = recode(varname, !!!varname_labels),
    Direction = case_when(
      Transition %in% hicp_transitions     ~ "Toward HICP",
      Transition %in% recovery_transitions ~ "Recovery from HICP",
      TRUE                                  ~ "Maintenance"
    )
  )

# ── Figure 1: initial state membership — all covariates, TE vs DE ─────────────
# Faceted by State (2, 3, 4 vs State 1 reference).
# TE and DE offset vertically per predictor for direct comparison.
p_init <- ggplot(init_combined,
                 aes(x = Q_bar, y = varname, colour = Model,
                     xmin = CI_lo, xmax = CI_hi)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(position = position_dodge(width = 0.3), size = 0.4, linewidth = .6) +
  facet_wrap(~ State, nrow = 1) +
  labs(title = "Initial state membership — pooled log-ORs (TE vs DE)",
       x = "Log-OR (95% CI)", y = NULL, colour = "Model",
       caption = state_caption) +
  plot_theme

print(p_init)

# ── Figure 2: transitions by clinical direction — all covariates, TE vs DE ────
# Two column-facets: toward HICP | recovery from HICP.
# Within each, rows = transition label; TE/DE dodged.
p_trans <- trans_combined %>%
  filter(Direction != "Maintenance") %>%
  ggplot(aes(x = Q_bar, y = varname, colour = Model,
             xmin = CI_lo, xmax = CI_hi)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(position = position_dodge(width = 0.3), size = 0.4, linewidth = .6) +
  facet_grid(Transition ~ Direction, scales = "free_y", space = "free_y") +
  labs(title = "Transitions — pooled log-ORs (TE vs DE)",
       x = "Log-OR (95% CI)", y = NULL, colour = "Model",
       caption = state_caption) +
  plot_theme

print(p_trans)

# ── Figure 3: exposure-only summary — ssupport6_cat across states + transitions
# Combines initial states and key transitions into one figure.
# y-axis = state/transition label, grouped by section.
exposure_vars <- c("Less than supportive partner", "Supportive partner")

init_exp <- init_combined %>%
  filter(varname %in% exposure_vars) %>%
  mutate(Panel = "Initial state", Label = State)

trans_exp <- trans_combined %>%
  filter(varname %in% exposure_vars,
         Direction %in% c("Toward HICP", "Recovery from HICP")) %>%
  mutate(Panel = Direction, Label = Transition)

exp_combined <- bind_rows(init_exp, trans_exp) %>%
  mutate(Panel = factor(Panel,
                        levels = c("Initial state",
                                   "Toward HICP",
                                   "Recovery from HICP")))

p_exposure <- ggplot(exp_combined,
                     aes(x = Q_bar, y = Label, colour = Model,
                         xmin = CI_lo, xmax = CI_hi,
                         shape = varname)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(position = position_dodge(width = 0.6), size = 0.5) +
  facet_wrap(~ Panel, ncol = 1, scales = "free_y") +
  labs(title = "Partnership & spousal support — pooled log-ORs (TE vs DE)",
       x = "Log-OR (95% CI)", y = NULL, caption = state_caption,
       colour = "Model", shape = "Category") +
  plot_theme

print(p_exposure)

cat("\nPost-processing complete.\n")


