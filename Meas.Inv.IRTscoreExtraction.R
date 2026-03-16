# ============================================================
# Extract IRT impact scores under partial scalar invariance
# ============================================================
# Prerequisite: MI_2PL_ELSA.Rmd must have been run in this
# session, providing:
#   - final_partial_Scalar_model  (mirt multipleGroup object)
#   - group                       (factor: wave membership per row)
#   - sc_df_2 ... sc_df_8         (imputed item response data)
#
# The unconstrained wave-specific models in the previous script
# (impactEstimates2-8.tab) are NOT used here. Scores are anchored
# to a common metric via the partial scalar invariant model.
#
# Invariance summary (from Measurement_invariance_IRT.Rmd):
#   - Discrimination (a) constrained: items 1-5, 8-9
#     (shopa, housewka, walkra, mealsa, hlthlm, dressa, toilta)
#   - Difficulty (d) constrained:     items 1-5, 8-9
#   - Items 6-7 (clim1a, lifta) freed at metric and scalar level
#   - Practical impact of constraints: latent score differences
#     vs unconstrained < +/-0.05 across all waves
# ============================================================

library(tidyverse)
library(mirt)

# Verify prerequisites are present
# these objects exists after running the pipeline for measurement invariance
# Measurement_invariance_IRT.Rmd
required_objects <- c("final_partial_Scalar_model", "group",
                      paste0("sc_df_", 2:8))

missing <- required_objects[!sapply(required_objects, exists)]
if (length(missing) > 0) {
  stop(
    "Missing objects from invariance Rmd session:\n",
    paste(" -", missing, collapse = "\n"),
    "\nPlease run Measurement_invariance_IRT.Rmd before this script."
  )
}

# ── Extract EAP scores from partial scalar model ─────────────

# fscores() on a multipleGroup model returns scores for all
# observations stacked in the same order as the combined_data
# object used to fit the model. We use the group factor to
# index back to wave-specific rows.

all_eap <- fscores(
  final_partial_Scalar_model,
  method       = "EAP",
  full.scores  = TRUE,
  full.scores.SE = FALSE   # set TRUE if SE needed downstream
)

scores_list <- list()

for (wave in 2:8) {
  
  df <- get(paste0("sc_df_", wave))
  
  # Extract rows belonging to this wave
  wave_scores <- all_eap[group == wave, , drop = FALSE]
  
  if (nrow(wave_scores) != nrow(df)) {
    stop(sprintf(
      "Row mismatch at wave %d: sc_df_%d has %d rows, ",
      "group index yields %d rows. Check group alignment.",
      wave, wave, nrow(df), nrow(wave_scores)
    ))
  }
  
  scores_list[[paste0("wave_", wave)]] <- data.frame(
    idauniq = df$idauniq,
    score   = wave_scores[, 1]
  )
}

# ── Reshape to wide format ────────────────────────────────────

scores_wide <- scores_list[["wave_2"]]
names(scores_wide) <- c("idauniq", "r2lvimp")

for (wave in 3:8) {
  temp <- scores_list[[paste0("wave_", wave)]]
  names(temp) <- c("idauniq", paste0("r", wave, "lvimp"))
  scores_wide <- merge(scores_wide, temp, by = "idauniq", all = TRUE)
}

# ── Sanity checks before export ──────────────────────────────

# 1. Score range should be consistent across waves (common metric)
score_vars <- paste0("r", 2:8, "lvimp")
score_summary <- sapply(score_vars, function(v) {
  x <- scores_wide[[v]]
  c(n    = sum(!is.na(x)),
    mean = round(mean(x, na.rm = TRUE), 3),
    sd   = round(sd(x,   na.rm = TRUE), 3),
    min  = round(min(x,  na.rm = TRUE), 3),
    max  = round(max(x,  na.rm = TRUE), 3))
})
cat("\n── Score summary by wave ──\n")
print(t(score_summary))

# 2. Warn if SD varies substantially across waves
# (would suggest anchoring failed or group misalignment)
sds <- score_summary["sd", ]
if (max(sds) - min(sds) > 0.15) {
  warning(
    "SD of latent scores varies by more than 0.15 across waves.\n",
    "Check group alignment and model object."
  )
}

# ── Export ────────────────────────────────────────────────────

out_path <- file.path(
  "~/private/WP5_data/rds/",
  "impactEstimates2-8_partialScalar.rds"
)

saveRDS(scores_wide, file = out_path)


cat(sprintf(
  "\nScores saved to:\n  %s\n  N = %d respondents, waves 2-8\n",
  out_path, nrow(scores_wide)
))

