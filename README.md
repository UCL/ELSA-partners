# Supplementary material

Supplementary material and full code documentation for the following paper:

Vitali, D., Hipkiss, S., Andrayas, A., Ehrhardt, B., Oporto-Lisboa, L., Pickering, A., Keogh, E., & Williams, A. C. de C. (under review). Partnership quality and pain state transitions in older adults: a latent transition analysis. *PAIN*.

## Contents

The analysis runs as a sequence of documents. A diagram of the full pipeline is in `Data-flow-diagram/` (`data_flow.md` is the light version, `data_flow_detailed.md` the detailed one).

| File | Description |
|---|---|
| `IMPACT_IRT_Measurement_invariance.Rmd` | Measurement invariance testing of the 2PL IRT model of pain impact |
| `IMPACT_IRT_scoresExtraction.R` | Extraction of IRT scores |
| `suppl_pt1_Data_prep.Rmd` | Data preparation |
| `suppl_pt1.1_MAR_exploration.Rmd` | Missing data diagnostics and MAR assumption tests |
| `suppl_pt2_IMPUTATION_MICE_long.Rmd` | Multiple imputation (MICE) specifications and diagnostics |
| `suppl_pt2.1_DERIVED VARIABLES_VH.Rmd` | Derived variables construction and validation |
| `suppl_pt3_change of pain classes_descriptives_VH.Rmd` | Variable definitions and descriptive statistics |
| `suppl_pt4_LTA_models.Rmd` | Latent transition analysis (LTA) model specification |
| `suppl_pt4.1_imputation_loop.r` | Model fitting loop across imputed datasets |
| `suppl_pt4.2_LTA_post_processing_pool.Rmd` | Pooling of estimates across imputations and diagnostics |

Shared helper functions are in `Sources/`. References are in `references.bib`.

## Reproducibility

All analyses were conducted in R. Raw data are not included in this repository.

## Funding

[TODO: funding statement]
