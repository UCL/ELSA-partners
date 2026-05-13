################################################################################
# LMest across multiple imputations
################################################################################
# PURPOSE: Fit LMest LTA models (TE and DE) across all 22 imputed datasets,
#          rebase state labels to a consistent severity ordering per imputation,
#          extract regression summaries, and save per-imputation results for
#          later pooling via Rubin's rules.
#
# INPUTS:
#   - mice_updatedMIDS_VH.rds  (output of pt2.1)
#   - Sources/Paper1_func.R
#
# OUTPUTS: For each imputation i and each model (TE / DE):
#   - raw_results_TE_imp{i}.rds  — full run_lmest_parallel_seeds() output (no SE)
#   - raw_results_DE_imp{i}.rds
#
# NOTE: rebase + SE + regression_summaries are deferred. SE computation is slow
#       and produces NaN for sparse transitions (<20 obs). Run this loop first to
#       confirm stable fits across imputations, then handle SE pooling separately.
################################################################################


# ==============================================================================
# 1. LIBRARIES AND FUNCTIONS
# ==============================================================================

library(dplyr)
library(mice)
library(future)
library(future.apply)
library(LMest)

source("Sources/Paper1_func.R")


# ==============================================================================
# 2. PATHS
# ==============================================================================

rds_path    <- "~/private_WP5/WP5_data/rds/"
models_path <- "~/private_WP5/WP5_data/model_fits/imputations/"


# ==============================================================================
# 3. LOAD MIDS
# ==============================================================================

updated_mids   <- readRDS(paste0(rds_path, "mice_updatedMIDS_VH.rds"))
n_imputations  <- updated_mids$m   # 60


# ==============================================================================
# 4. ANALYSIS VARIABLES
# (mirrors pt4 Variables vector — keep in sync if pt4 changes)
# ==============================================================================

Variables <- c(
  "idauniq",                # ID
  "time",                   # Integer: 1=Wave4, 2=Wave5, 3=Wave6 (LMest 1-based)
  # time-invariant predictors
  "raagey_z",               # age (standardised)
  "Sex_BIN",                # Female / Male
  "raeduc_e",               # education
  # time-varying predictors
  "wealthQ_bin",            # most deprived Q vs rest
  "MLTC",                   # count of long-term conditions
  "cesd_lat_shifted",       # depression latent score (shifted)
  "lnlys",                  # loneliness continuous (log1p-UCLA-4)
  "partner",                # No-partner / has-partner
  "ssupport6_cat",          # No-partner / less than supportive / supportive
  # LTA indicators
  "painchr_ext",            # ordered integer 0–3
  "lvimp_shifted_cat3"      # ordered integer 0–2 (0=no impact, 1=low, 2=mid/high)
)


# ==============================================================================
# 5. BUILD FORMULAS ONCE (on imputation 1)
# ==============================================================================

prep_lta_data <- function(imp_df) {
  imp_df %>%
    mutate(
      ssupport6     = expm1(ssupport6_log1p),
      ssupport6_cat = case_when(
        partner == "No-partner"       ~ "No-partner",
        ssupport6 <= 1.5              ~ "supportive partner",
        ssupport6 >  1.5              ~ "less than supportive partner",
        TRUE                          ~ NA_character_
      ) %>% factor(levels = c("No-partner",
                               "less than supportive partner",
                               "supportive partner"))
    ) %>%
    select(all_of(Variables))
}

one_imp <- prep_lta_data(mice::complete(updated_mids, 1))

latent_state_vars    <- c("lvimp_shifted_cat3", "painchr_ext")

TE_initial_prob_vars <- c("Sex_BIN", "raeduc_e", "wealthQ_bin",
                          "ssupport6_cat", "MLTC", "raagey_z")
TE_trans_prob_vars   <- TE_initial_prob_vars

DE_vars              <- c(TE_initial_prob_vars, "cesd_lat_shifted", "lnlys")

fmLatent_TE <- lmestFormula(data = one_imp, response = latent_state_vars,
                            LatentInitial   = TE_initial_prob_vars,
                            LatentTransition = TE_trans_prob_vars)

fmLatent_DE <- lmestFormula(data = one_imp, response = latent_state_vars,
                            LatentInitial   = DE_vars,
                            LatentTransition = DE_vars)

model_configs <- list(
  TE = list(name = "TE", description = "Total Effects",
            responsesFormula = fmLatent_TE$responsesFormula,
            latentFormula    = fmLatent_TE$latentFormula),
  DE = list(name = "DE", description = "Direct Effects",
            responsesFormula = fmLatent_DE$responsesFormula,
            latentFormula    = fmLatent_DE$latentFormula)
)

k <- 4

# ==============================================================================
# 6. PARALLELISATION SETTINGS TE
# ==============================================================================

n_reps  <- 36   # 36 reps scheduled across 24 workers (work queue, not rigid batches)
ntry    <- 5    # total starts per imputation = n_reps × k × ntry = 720
n_cores <- 24


# ==============================================================================
# 7. HELPER: fit one model config for one imputed dataset
# ==============================================================================

fit_one <- function(imp, config, label) {

  out_file <- paste0(models_path, "raw_results_", label, "_imp", imp, ".rds")
  tmp_file <- paste0(out_file, ".tmp")

  if (file.exists(out_file)) {
    cat(sprintf("[%s] Imputation %d already on disk — skipping.\n", label, imp))
    return(invisible(NULL))
  }

  cat("\n", rep("=", 80), "\n")
  cat(sprintf("[%s] Processing imputation %d of %d\n", label, imp, n_imputations))
  cat(rep("=", 80), "\n\n")

  imp_data <- prep_lta_data(mice::complete(updated_mids, imp))

  t0 <- proc.time()
  raw_results <- run_lmest_parallel_seeds(
    config  = config,
    data    = imp_data,
    n_reps  = n_reps,
    ntry    = ntry,
    n_cores = n_cores,
    k       = k
  )
  elapsed <- round((proc.time() - t0)["elapsed"] / 60, 1)

  cat(sprintf("Best LL: %.4f | Stability: %.1f%% | %.1f min\n",
              raw_results$abs_best_lk,
              100 * raw_results$n_at_max_all / length(raw_results$all_lls),
              elapsed))

  saveRDS(raw_results, tmp_file)
  file.rename(tmp_file, out_file)  # atomic: .rds only exists if fully written

  rm(imp_data, raw_results)
  gc()

  cat(sprintf("[%s] Imputation %d complete\n", label, imp))
}


# ==============================================================================
# 8. LOOP — TE
# ==============================================================================

cat("\n", rep("#", 80), "\n")
cat("TOTAL EFFECTS LOOP\n")
cat(rep("#", 80), "\n")

for (imp in 1:n_imputations) {
  fit_one(imp, config = model_configs$TE, label = "TE")
}

cat("\nTE loop complete.\n")


# ==============================================================================
# 9. PARALLELISATION SETTINGS DE OVERRIDE
# ==============================================================================

### I am reducing this slightly to reduce wall time. in a pilot ron of 40reps 100% reach
### a model solution withing 5 lk of absolute best model fit across all 400 random starts

n_reps  <- 24   # 24 reps scheduled across 24 workers (work queue, not rigid batches)
ntry    <- 5    # total starts per imputation = n_reps × k × ntry = 480
n_cores <- 24

# ==============================================================================
# 10. LOOP — DE
# ==============================================================================

cat("\n", rep("#", 80), "\n")
cat("DIRECT EFFECTS LOOP\n")
cat(rep("#", 80), "\n")

for (imp in 1:n_imputations) {
  fit_one(imp, config = model_configs$DE, label = "DE")
}

cat("\nDE loop complete.\n")
cat("\nAll imputations complete. Results saved to:", models_path, "\n")
