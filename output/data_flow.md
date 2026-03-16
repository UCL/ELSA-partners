# Data Flow Diagram — ELSA Chronic Pain LTA Pipeline

**Date:** 2026-03-16

---

```mermaid
flowchart TD

    %% ─────────────────────────────────────────────
    %%  STAGE 0 — Raw inputs
    %% ─────────────────────────────────────────────
    subgraph RAW["📦 Raw ELSA Data (read-only)"]
        direction TB
        HARM["Harmonised ELSA w1–9\nHg3_w1_9_ALL.tab"]
        TABS["Wave-specific tab files\nw4/5/6 core · nurse · financial\n(8 files)"]
    end

    %% ─────────────────────────────────────────────
    %%  STAGE 1.2 — Measurement invariance & IRT
    %% ─────────────────────────────────────────────
    subgraph IRT["🔬 Step 1.2 — IRT Measurement Model (established)"]
        direction TB
        PT12["Measurement_invariance_IRT.Rmd\n(review only — do not re-run)"]
        EXTRACT["Meas.Inv.IRTscoreExtraction.R"]
        IRT_RDS[("impactEstimates2-8\n_partialScalar.rds")]
        PT12 -.->|"run separately"| EXTRACT
        EXTRACT --> IRT_RDS
    end

    %% ─────────────────────────────────────────────
    %%  STAGE 1 — Data preparation
    %% ─────────────────────────────────────────────
    subgraph PREP["🛠️ Step 1 — Data Preparation"]
        direction TB
        CESD_MOD[("cesd_lat_metricInv\nfit…rds\n(pre-fitted CFA)")]
        PT1["PAPER_1.pt1_Data_prep.Rmd\n· variable derivation\n· sample selection w4–6\n· chronic pain classification\n· MLTC · depression · partner"]
        W1[("H_elsa_w1.rds")]
        W46[("H_elsa_w4_6.rds\n✅ Clean w4–6 dataset")]
        CESD_MOD --> PT1
        PT1 --> W1
        PT1 --> W46
    end

    %% ─────────────────────────────────────────────
    %%  STAGE 1.1 — MAR (MISSING)
    %% ─────────────────────────────────────────────
    MAR["⚠️ Step 1.1 — MAR Exploration\nPAPER_1.pt1.1_MAR_exploration.Rmd\nNOT YET BUILT"]

    %% ─────────────────────────────────────────────
    %%  STAGE 2 — Multiple imputation
    %% ─────────────────────────────────────────────
    subgraph IMPUTE["🔁 Step 2 — Multiple Imputation (MICE)"]
        direction TB
        PT2["PAPER_1.pt2_IMPUTATION\n_MICE_long.Rmd\n· wide → long format\n· 2-level FCS imputation\n· conditional imputation\n  (marital strain | partner)"]
        MICE[("mice_imputationlong\n_rev4.rds\n12 imputed datasets")]
        PT2 --> MICE
    end

    %% ─────────────────────────────────────────────
    %%  STAGE 2.1 — Derived variables
    %% ─────────────────────────────────────────────
    subgraph DERIVE["⚙️ Step 2.1 — Derived Variables"]
        direction TB
        STAB[("Impact_clustStability\n_boot2000.rds")]
        PT21["PAPER_1.pt2.1_DERIVED\nVARIABLES_rev.4.Rmd\n· lvimp_shifted_cat (PAM w4 medoids)\n· wealthQ_bin (quintiles)\n· loneliness_group (median)\n· time · Sex_BIN · raagey_z"]
        MIDS[("mice_updatedMIDS_rev4.rds\n✅ Final analysis dataset")]
        STAB --> PT21
        PT21 --> MIDS
    end

    %% ─────────────────────────────────────────────
    %%  STAGE 3 — Descriptives
    %% ─────────────────────────────────────────────
    subgraph DESC["📊 Step 3 — Descriptive Statistics"]
        PT3["PAPER_1.pt3_descriptives_rev.4.Rmd\n· Table 1 · missingness\n· pain class summaries"]
    end

    %% ─────────────────────────────────────────────
    %%  STAGE 4 — LTA model (out of scope)
    %% ─────────────────────────────────────────────
    subgraph LTA["📈 Step 4 — LTA Model (out of scope this run)"]
        PT4["PAPER_1_pt4_LTA_models.Rmd\n· ssupport6_cat derivation\n· k=4 state selection\n· TE and DE models\n· reduced nested models"]
        MODELS[("model_fits/\nTotalEffects · DirectEffects\nfit_reordered_TE/DE\netc.")]
        PT4 --> MODELS
    end

    %% ─────────────────────────────────────────────
    %%  CONNECTIONS BETWEEN STAGES
    %% ─────────────────────────────────────────────
    HARM --> PT12
    HARM --> PT1
    TABS --> PT1
    IRT_RDS --> PT1

    W46 --> MAR
    W46 --> PT2
    W46 --> PT3
    W1  --> PT3

    MICE --> PT21
    MICE --> PT3

    MIDS --> PT4

    %% ─────────────────────────────────────────────
    %%  STYLING
    %% ─────────────────────────────────────────────
    style MAR fill:#ffcccc,stroke:#cc0000,color:#000,font-weight:bold

    style W46  fill:#c3e6cb,stroke:#28a745,color:#000,font-weight:bold
    style MIDS fill:#c3e6cb,stroke:#28a745,color:#000,font-weight:bold

    style LTA  fill:#fff9e6,stroke:#ffc107
    style PT4  fill:#fff3cd,stroke:#ffc107,color:#000
```

---

## Pipeline stages

| Stage | File | Key output |
|---|---|---|
| 1.2 | `Measurement_invariance_IRT.Rmd` + `Meas.Inv.IRTscoreExtraction.R` | `impactEstimates2-8_partialScalar.rds` |
| 1 | `PAPER_1.pt1_Data_prep.Rmd` | `H_elsa_w4_6.rds` ✅ clean w4–6 dataset |
| **1.1** | **⚠️ NOT YET BUILT** | MAR exploration Rmd |
| 2 | `PAPER_1.pt2_IMPUTATION_MICE_long.Rmd` | `mice_imputationlong_rev4.rds` |
| 2.1 | `PAPER_1.pt2.1_DERIVED VARIABLES_rev.4.Rmd` | `mice_updatedMIDS_rev4.rds` ✅ final analysis dataset |
| 3 | `PAPER_1.pt3_…_descriptives_rev.4.Rmd` | tables / figures (no RDS output) |
| 4 | `PAPER_1_pt4_LTA_models.Rmd` *(out of scope)* | fitted LTA model RDS files |

All `eval=F` save chunks are intentional re-run guards — output files pre-exist on disk.
