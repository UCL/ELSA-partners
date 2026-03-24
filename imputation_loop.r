################################################################################
# LMest across multiple imputations — template
##############
# PURPOSE: * Fit a single LMest model across all imputed datasets
#          * rebase state labels to a consistent ordering
#          * extract regression summaries
#          * save per-imputation results to disk for later pooling with Rubin's rules.
#
##############
# REQUIREMENTS (must exist before running this script):
#   - A mice MIDS object (your imputed data)
#   - An LMest formula object built with lmestFormula() on any one imputed dataset (e.g.)
#   - Paper1_func.R (provides the auxillary functions: 
#                       * run_lmest_parallel_seeds()
#                       * Determine_state_ordering()
#                       * rebase_and_refit_lmest()
#                       * regression_summaries)
#
#
##############
# OUTPUT: For each imputation i, two .rds files saved to models_path:
#   - raw_results_imp{i}.rds  — full run_lmest_parallel_seeds() output
#   - summaries_imp{i}.rds    — regression_summaries() tibbles (transitions +
#                                initial_states), ready for Rubin's rules pooling
################################################################################


# ==============================================================================
# 1. LIBRARIES AND FUNCTIONS
# ==============================================================================

library(dplyr)
library(mice)
library(future)
library(future.apply)
library(LMest)

source("Sources/Paper1_func.R")   # adjust path if running from a different working directory


# ==============================================================================
# 2. PATHS  —  edit these
# ==============================================================================

rds_path    <- "~/private_WP5/WP5_data/rds/"          # where your MIDS lives
models_path <- "~/private_WP5/WP5_data/model_fits/"   # where outputs will be saved


# ==============================================================================
# 3. INPUTS  —  edit these
# ==============================================================================

# 3a. Load your imputed data
mids_object <- readRDS(paste0(rds_path, "your_mids_file.rds"))   # replace filename

# 3b. Variables to keep in the analysis dataset (must include idauniq and time)
analysis_vars <- c(
  "idauniq", "time"
  # add your predictors and outcome indicators here
)


# 3c. LMest formula — build once on any single imputed dataset, reuse across loop.
#     Example:
#       one_imp <- complete(mids_object, 1) %>% prepare_data() %>% select(all_of(analysis_vars))
#       fm      <- lmestFormula(data = one_imp,
#                               response          = c("indicator1", "indicator2"),
#                               LatentInitial     = initial_vars,
#                               LatentTransition  = transition_vars)
#
fm <- NULL   # replace NULL with your lmestFormula() output

# 3d. State ordering indicators — used by Determine_state_ordering() to align
#     state labels consistently across imputations, preventing label switching.
#     Provide response variable names and relative weights.
ordering_indicators <- c("indicator1", "indicator2")   # replace
ordering_weights    <- c(0.5, 0.5)                     # replace

# 3f. Number of latent states
k <- 4   # replace with your k


# ==============================================================================
# 4. MODEL CONFIG
# ==============================================================================

model_config <- list(
  name             = "TE",
  description      = "Total Effects model",
  responsesFormula = fm$responsesFormula,
  latentFormula    = fm$latentFormula
)


# ==============================================================================
# 5. PARALLELISATION SETTINGS  —  edit these
# ==============================================================================

n_imputations <- mids_object$m   # total number of imputations in the MIDS

n_reps  <- 10    # independent parallel replications per imputation, each with a
                 # different random seed. total random starts = n_reps * k * ntry.
                 # more reps = better likelihood surface coverage, longer wall time.

ntry    <- 5     # random starts per replication passed to lmest(); lmest runs
                 # k * ntry starts internally (here: k * 5). increase if
                 # stability % across reps is low.

n_cores <- 10    # parallel workers (future::multisession).
                 # no benefit setting n_cores > n_reps — excess workers stay idle.


# ==============================================================================
# 6. LOOP
# ==============================================================================

for (imp in 1:n_imputations) {

  cat("\n", rep("=", 80), "\n")
  cat(sprintf("Processing imputation %d of %d\n", imp, n_imputations))
  cat(rep("=", 80), "\n\n")

  # --- prepare dataset for this imputation ---
  imp_data <- complete(mids_object, imp) %>%
    prepare_data() %>%
    select(all_of(analysis_vars))

  # --- fit model ---
  cat(">>> Fitting model...\n")
  raw_results <- run_lmest_parallel_seeds(
    config  = model_config,
    data    = imp_data,
    n_reps  = n_reps,
    ntry    = ntry,
    n_cores = n_cores,
    k       = k
  )
  saveRDS(raw_results, paste0(models_path, "raw_results_imp", imp, ".rds"))
  cat(sprintf("Best LL: %.4f | Stability: %.1f%%\n",
              raw_results$abs_best_lk,
              100 * raw_results$n_at_max_all / length(raw_results$all_lls)))

  # --- rebase state labels to consistent ordering ---
  state_order <- Determine_state_ordering(
    raw_results$best_model,
    indicator_names   = ordering_indicators,
    indicator_weights = ordering_weights
  )$severity_order

  model_rebased <- rebase_and_refit_lmest(
    fit_model        = raw_results$best_model,
    state_order      = state_order,
    latent_formula   = fm$latentFormula,
    response_formula = fm$responsesFormula,
    out_SE           = TRUE,
    plot             = FALSE
  )

  # --- extract summaries and save ---
  summaries <- regression_summaries(Model_fit = model_rebased)
  saveRDS(summaries, paste0(models_path, "summaries_imp", imp, ".rds"))

  rm(imp_data, raw_results, model_rebased, summaries)
  gc()

  cat(sprintf("Completed imputation %d\n", imp))
}

cat("\n", rep("=", 80), "\n")
cat("ALL IMPUTATIONS COMPLETE\n")
cat(rep("=", 80), "\n")


# ==============================================================================
# 7. LOAD RESULTS FOR POOLING
# ==============================================================================

# After the loop, reload all summaries into a list ready for Rubin's rules:
#
# summaries_list <- purrr::map(
#   1:n_imputations,
#   ~ readRDS(paste0(models_path, "summaries_imp", .x, ".rds"))
# )
#
# summaries_list[[i]]$transitions    — varname, Transition, Coefficient, StdError, Sep_Flag
# summaries_list[[i]]$initial_states — varname, State,      Coefficient, StdError, Sep_Flag
