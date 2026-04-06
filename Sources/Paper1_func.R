##############################################################
# CHRONIC PAIN DERIVED VARIABLE DEFINITION
############################################################
chronic_pain_var <- function(df, pain_fr_var, hepag_var, painlv_var) {
  result <- case_when(
    # No pain cases
    df[[pain_fr_var]] == 0 ~ 0,                                # No pain reported
    
    # Acute pain cases (either mild pain or short duration)
    df[[pain_fr_var]] == 1 & df[[painlv_var]] <= 1 ~ 1,        # Mild pain (regardless of duration)
    df[[pain_fr_var]] == 1 & df[[hepag_var]] == 1 ~ 1,         # Less than 3 months duration
    
    # Chronic pain cases (moderate/severe AND 3+ months)
    df[[pain_fr_var]] == 1 & df[[painlv_var]] >= 2 & df[[hepag_var]] >= 2 ~ 2,    # Moderate/severe pain lasting 3+ months
    df[[pain_fr_var]] == 1 & df[[painlv_var]] >= 2 & is.na(df[[hepag_var]]) ~ 2,  # in this case we assume is chronic 
    
    
    # Handle missing values
    is.na(df[[pain_fr_var]]) ~ NA_real_,
    df[[pain_fr_var]] == 1 & is.na(df[[painlv_var]]) ~ 1,  # conservative: pain reported but severity missing → classified as acute (not NA)
    
    # Default to acute pain for any other combinations
    TRUE ~ NA_real_
  )
  
  return(result)
}

##############################################################
# END OF CHRONIC PAIN VARIABLE DEFINITION
############################################################


##############################################################
# CHRONIC PAIN SUMMARY TABLES
##############################################################
# Function to create summary table for a wave
create_CPsummary <- function(df, var_name, wave_label) {
  pain_table <- table(df[[var_name]], useNA = "ifany")
  pain_df <- as.data.frame(pain_table)
  names(pain_df) <- c("Pain Category", "Frequency")
  pain_df$Percentage <- round(100 * pain_df$Frequency / sum(pain_df$Frequency), 2)
  pain_df$Wave <- wave_label
  
  # Add labels for better readability
  pain_df$`Pain Category` <- factor(pain_df$`Pain Category`, 
                                    levels = c("0", "1", "2", "NA"),
                                    labels = c("No pain","Acute pain", "Chronic pain", "Missing"))
  return(pain_df)
}

##############################################################
# END OF CHRONIC PAIN SUMMARY TABLES
##############################################################


####################################################################
##### HSE COMPARISON CHARTS (for binary variables)
####################################################################

binary_var_chart <- function(data_w1, data_w4, var_w1, var_w4, 
                             positive_code, negative_code,
                             var_name = "Variable", 
                             positive_label, 
                             negative_label) {
  # Prepare data for Wave 1
  dataw1 <- data_w1 %>%
    group_by(!!sym(var_w1)) %>%
    summarise(count = n()) %>%
    mutate(
      category = case_when(
        !!sym(var_w1) == negative_code ~ negative_label,
        !!sym(var_w1) == positive_code ~ positive_label,
        is.na(!!sym(var_w1)) ~ "Missing"
      ),
      percentage = count / sum(count) * 100,
      wave = "Wave 1 (HSE)"
    )
  
  # Prepare data for Wave 4
  dataw4 <- data_w4 %>%
    group_by(!!sym(var_w4)) %>%
    summarise(count = n()) %>%
    mutate(
      category = case_when(
        !!sym(var_w4) == negative_code ~ negative_label,
        !!sym(var_w4) == positive_code ~ positive_label,
        is.na(!!sym(var_w4)) ~ "Missing"
      ),
      percentage = count / sum(count) * 100,
      wave = "Wave 4"
    )
  
  # Combine data
  plot_data <- bind_rows(dataw1, dataw4) %>%
    group_by(wave) %>%
    mutate(
      percentage = count / sum(count) * 100,
      category = factor(
        category, 
        levels = c(negative_label, positive_label, "Missing")
      )
    )
  
  # Create plot with percentage on y-axis
  ggplot(plot_data, aes(x = category, y = percentage, fill = wave)) +
    geom_bar(
      stat = "identity", 
      position = position_dodge(width = .9),
      alpha = 0.7
    ) +
    scale_fill_brewer(palette = "Paired")+
    geom_text(
      aes(
        label = paste0(round(percentage,1),"%"),
        group = wave
      ),
      position = position_dodge(width = 1),
      vjust = -0.3,
      size = 3
    ) +
    labs(
      title = paste(var_name, "Distribution: Wave 1 (HSE) vs Wave 4"),
      x = var_name,
      y = "Percentage of Respondents",
      caption = paste0("Wave 1 (HSE) n =", nrow(data_w1),
                       "; Wave 4 n = ", nrow(data_w4))
    ) +
    theme_minimal() +
    scale_y_continuous(labels = function(x) paste0(x, "%"))
}
####################################################################
##### END OF HSE COMPARISON CHARTS (for binary variables)
####################################################################


####################################################################
######### DERIVED VARIABLES
####################################################################


############ PHYSICAL ACTIVITY (categorical variable)
############################################################
create_PA_categories <- function(df, wave) {
  w_prefix <- paste0("r", wave)
  vig_var <- paste0(w_prefix, "vgactx_e")
  mod_var <- paste0(w_prefix, "mdactx_e") 
  mild_var <- paste0(w_prefix, "ltactx_e")
  output_var <- paste0(w_prefix, "PA_cat")
  
  df %>% 
    rowwise() %>%
    mutate(
      "{output_var}" := case_when(
        # Category 4: Vigorous activity at least once per week
        (get(vig_var) == 2 | get(vig_var) == 3) ~ 4,
        
        # Category 3: At least moderate but no vigorous activity at least once per week
        (get(mod_var) == 2 | get(mod_var) == 3) & 
          (get(vig_var) %in% c(4, 5) | is.na(get(vig_var))) ~ 3,
        
        # Category 2: Only mild activity at least once per week
        (get(mild_var) == 2 | get(mild_var) == 3) & 
          (get(mod_var) %in% c(4, 5) | is.na(get(mod_var))) &
          (get(vig_var) %in% c(4, 5) | is.na(get(vig_var))) ~ 2,
        
        # Category 1: Inactive (no activity on a weekly basis)
        (get(mild_var) %in% c(4, 5)) &
          (get(mod_var) %in% c(4, 5) | is.na(get(mod_var))) &
          (get(vig_var) %in% c(4, 5) | is.na(get(vig_var))) ~ 1,
        
        TRUE ~ NA_real_
      )
    ) %>%
    ungroup()
}

############ MLTC  (categorical variable)
###################################################################
create_MLTC_variable <- function(data, varName, variables) {
  # Check if all variables exist in the dataframe
  missing_vars <- setdiff(variables, names(data))
  if (length(missing_vars) > 0) {
    stop(paste("Missing variables:", paste(missing_vars, collapse = ", ")))
  }
  
  # Create MLTC count variable
  data <- data %>%
    mutate(
      # Count NAs across all condition variables
      na_count = rowSums(is.na(across(all_of(variables)))),
      
      # Create MLTC variable
      !!varName := case_when(
        # If all variables are NA, result is NA
        na_count == length(variables) ~ NA_real_,
        # Otherwise sum non-NA values
        TRUE ~ rowSums(
          across(
            all_of(variables), 
            ~ case_when(
              . == 1 ~ 1,
              . == 0 ~ 0,
              is.na(.) ~ NA_real_  
            )
          )
        )
      )
    ) %>%
    select(-na_count)  # Remove the temporary na_count variable
  
  # Create categorical version
  cat_varName <- paste0(varName, "_cat")
  data <- data %>%
    mutate(
      !!cat_varName := case_when(
        is.na(!!sym(varName)) ~ NA_character_,
        !!sym(varName) == 0 ~ "0 conditions",
        !!sym(varName) == 1 ~ "1 H. condition",
        !!sym(varName) == 2 ~ "2 H. conditions",
        !!sym(varName) >= 3 ~ "3+ H. conditions"
      ) %>% factor()
    )
  
  return(data)
}


############ N of PAIN LOCATIONS  (NUMERIC Variable)
####################################################################
create_pain_locations_count <- function(data, varName, variables) {
  # Check if all variables exist in the dataframe
  missing_vars <- setdiff(variables, names(data))
  if (length(missing_vars) > 0) {
    stop(paste("Missing variables:", paste(missing_vars, collapse = ", ")))
  }
  
  # Create pain locations count variable
  data <- data %>%
    mutate(
      # First identify people with no pain at all (any variable with -1)
      has_no_pain = rowSums(across(all_of(variables), ~ . == -1), na.rm = TRUE) > 0,
      
      # Count NAs across all pain variables
      na_count = rowSums(is.na(across(all_of(variables)))),
      
      # Create pain locations count variable
      !!varName := case_when(
        # If all variables are NA, result is NA
        na_count == length(variables) ~ NA_real_,
        # If has_no_pain is TRUE, count is 0
        has_no_pain ~ 0,
        # Otherwise count locations with pain (code 1)
        TRUE ~ rowSums(
          across(
            all_of(variables), 
            ~ case_when(
              . == 1 ~ 1,              # Pain in this location
              . == 0 ~ 0,              # Pain but not in this location
              . == -1 ~ 0,             # No pain at all
              . == -8 ~ 0,             # dont know
              is.na(.) ~ NA_real_      # Treat NA as 0 for counting
            )
          )
        )
      )
    ) %>%
    select(-has_no_pain, -na_count)  # Remove temporary variables
  
  # Create categorical version
  cat_varName <- paste0(varName, "_cat")
  data <- data %>%
    mutate(
      !!cat_varName := case_when(
        is.na(!!sym(varName)) ~ NA_character_,
        !!sym(varName) == 0 ~ "0 or No pain",
        !!sym(varName) <= 2 ~ "2 or less location",
        !!sym(varName) > 2 ~ "3 or more locations"
      ) %>%  factor()
    )
  
  return(data)
}

##########################################################################
#  END OF DERIVED VARIABLES
#########################################################################


##########################################################################
#  TABLE 1 DATAPREP
#########################################################################
prepare_table1_data <- function(data, var_Ys, var_Xs, var_Covs, strat_var = "r4painchr", palette = "") {
  library(dplyr)
  
  # Select relevant variables
  table1DF <- data %>% 
    select(all_of(c("idauniq", var_Ys, var_Xs, var_Covs)))
  
  # Handle missing values in stratification variable
  if (strat_var %in% names(table1DF)) {
    # Remove rows with missing values in stratification variable
    table1DF <- table1DF %>%
      filter(!is.na(!!sym(strat_var)))
  }
  
  # Format factors with meaningful labels
  table1DF <- table1DF %>%
    mutate(
      # Pain-related variables
      r4painchr = factor(r4painchr, 
                         levels = c(0, 1, 2), 
                         labels = c("No pain", "Acute pain", "Chronic pain")),
      
      # Demographic variables
      ragender = factor(ragender, 
                        levels = c(1, 2), 
                        labels = c("Male", "Female")),
      raracem = factor(raracem, 
                       levels = c(1, 4), 
                       labels = c("White", "Non-White")),
      r4partner = factor(r4partner, 
                         levels = c(0, 1), 
                         labels = c("Without partner", "With partner")),
      
      # Work and lifestyle variables
      r4work = factor(r4work, 
                      levels = c(0, 1), 
                      labels = c("Not working", "Working")),
      
      r4smoken = factor(r4smoken, 
                        levels = c(0, 1), 
                        labels = c("Non-smoker", "Current smoker")),
      
      # Education and wealth variables
      raeduc_e = factor(raeduc_e,
                        levels = c(1,2,3,4,5),
                        labels = c("Less than secondary","Other", "Secondary", 
                                   "Some college", "College and above")),
      
      r4totwq5_bu_s = factor(r4totwq5_bu_s,
                             levels = 1:5,
                             labels = c("Lowest quintile", "2nd quintile", "3rd quintile", 
                                        "4th quintile", "Highest quintile")),
      
      # Multi-morbidity categorical variable
      r4MLTC_cat = factor(case_when(
        is.na(r4MLTC) ~ NA_character_,
        r4MLTC == 0 ~ "No conditions",
        r4MLTC == 1 ~ "One condition",
        r4MLTC == 2 ~ "Two conditions",
        r4MLTC >= 3 ~ "Three or more conditions"
      )
      )
    )
  
  # Add variable labels
  label(table1DF$r4lvimp_shifted) <- "Health impact IRT score (> is worse)"
  label(table1DF$r4painchr) <- "Chronic Pain Status"
  label(table1DF$r4ssupport6) <- "Marital Strain (> worse, max = 4)"
  label(table1DF$r4partner)<- "Partnership status"
  label(table1DF$r4pain_locN) <- "Number of Pain Locations"
  label(table1DF$r4pain_locN_cat) <- "Number of Pain Locations"
  label(table1DF$r4cesd) <- "CESD score (7-itm > is worse, max = 7)"
  label(table1DF$r4lnlys) <- "UCLA - Loneliness (4-itm > is worse)"
  label(table1DF$r4agey) <- "Age (years)"
  label(table1DF$r4agey_cat) <- "Age Category"
  label(table1DF$ragender) <- "Gender"
  label(table1DF$raracem) <- "Race"
  label(table1DF$r4nettotw_bu_s) <- "Net Total Wealth (£)"
  label(table1DF$r4totwq5_bu_s) <- "Wealth Quintile"
  label(table1DF$r4wealthBIN ) <- "Wealth Quintile (Binary)"
  label(table1DF$raeduc_e) <- "Education Level"
  label(table1DF$r4work) <- "Employment Status"
  label(table1DF$r4smoken) <- "Smoking Status"
  label(table1DF$r4drinkwn_e) <- "Weekly Alcohol Consumption"
  label(table1DF$r4mbmi) <- "BMI"
  label(table1DF$r4MLTC) <- "Number of Long-Term Conditions"
  label(table1DF$r4MLTC_cat) <- "Multi-morbidity Status"
  
  # Add units for continuous variables
  units(table1DF$r4nettotw_bu_s) <- "£"
  units(table1DF$r4mbmi) <- "kg/m^2"
  units(table1DF$r4drinkwn_e) <- "units/week"
  
  return(table1DF)
}

# Define custom rendering functions
my_render_continuous <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=2), 
       c("", "Mean (SD)" = sprintf("%s (%s)", MEAN, SD),
         "Median [IQR]" = sprintf("%s [%s, %s]", MEDIAN, Q1, Q3)))
}

my_render_categorical <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y, sprintf("%d (%0.1f%%)", FREQ, PCT))))
}

############################################################################
######### END of TABLE1 functions
###########################################################################



############################################################################
############ DIAGNOSTIC FUNCTIONS
############################################################################

######### Bivariate associations plots
###########################################################################

create_BiVar_plot <- function(x_var, y_var, plot_type, fill_color = NULL, title_text) {
  # Create complete cases dataset
  plot_data <- H_elsa_w4_6 %>% 
    select(all_of(c(x_var, y_var))) %>%
    na.omit()
  
  # Get counts
  total_count <- nrow(H_elsa_w4_6)
  complete_count <- nrow(plot_data)
  na_count <- total_count - complete_count
  
  # Create caption
  na_caption <- paste0("Complete cases: ", complete_count, "/", total_count, 
                       " (", round(complete_count/total_count*100), "%)")
  
  if (plot_type == "boxplot") {
    p <- ggplot(plot_data, aes_string(x = paste0("factor(", x_var, ")"), y = y_var)) +
      geom_boxplot(fill = fill_color) +
      labs(x = "Social Support", 
           y = gsub("r4", "", y_var), 
           title = title_text,
           caption = na_caption)
  } else if (plot_type == "bar") {
    p <- ggplot(plot_data, aes_string(x = paste0("factor(", x_var, ")"), 
                                      fill = paste0("factor(", y_var, ")"))) +
      geom_bar(position = "fill") +
      labs(x = "Social Support", 
           y = "Proportion", 
           title = title_text,
           fill = gsub("r4|ra", "", y_var),
           caption = na_caption)
  }
  
  p + theme_minimal() +
    theme(plot.title = element_text(size = 10),
          plot.caption = element_text(hjust = 0, size = 8),
          legend.position = ifelse(plot_type == "bar", "bottom", "none"))
}

############################################################################
## END OF DIAGNOSTIC FUNCTIONS
############################################################################


##############################################################
# DROPOUT SUMMARY VARIABLE 
##############################################################

create_dropout_variable <- function(data, start_wave = 4, num_waves = 3) {
  iwstatVars <- paste0("r", start_wave:(start_wave + num_waves - 1), "iwstat")
  dropout <- apply(data[iwstatVars], 1, function(row) {
    # Case 1: All responses are 1 (complete response in all waves)
    if (all(row == 1)) {
      return(0)
    }
    
    # Case 2: All waves are >= 4 (completely missing)
    if (all(row >= 4)) {
      return(-3)
    }
    
    # Case 3: Check for dropout pattern: 
    # Find the first wave where response is 1 and all subsequent waves are >=4
    for (i in seq_along(row)) {
      if (row[i] == 1 && all(row[(i + 1):length(row)] >= 4)) {
        return(start_wave + i)  # dropout at wave i+start_wave-1 (1-based index)
      }
    }
    
    # Case 4: Check for single missing with return
    # Missing in an earlier wave but present in a later wave
    for (i in seq_len(length(row) - 1)) {
      if (row[i] >= 4 && any(row[(i + 1):length(row)] == 1)) {
        return(-1)
      }
    }
    
    # Case 5: Any other pattern (including intermittent missing)
    return(-2)
  })
  
  # Correct dropout status for death (status == 5)
  for (i in seq_along(dropout)) {
    if (dropout[i] < 0) {
      for (wave in start_wave:(start_wave + num_waves - 1)) {
        varname <- paste0("r", wave, "iwstat")
        if (!is.na(data[i, varname]) && data[i, varname] == 5) {
          dropout[i] <- wave
          break
        }
      }
    }
  }
  return(dropout)
}


##############################################################
## END OF DROPOUT SUMMARY VARIABLE
##############################################################


##############################################################
## BEGIN OF LMEST associated functions
##############################################################


## RE-Order latent LMM trait category by severity
##############################################################

Determine_state_ordering <- function(lmest_model, indicator_names = NULL, indicator_weights = NULL) {
  
  # Extract dimensions
  k <- lmest_model$k  # number of states
  n_indicators <- dim(lmest_model$Psi)[3]  # number of indicators
  
  ####### TOTAL EFFECTS ORDER
  #################################################################
  # Get mean or most likely category for each state
  # Example: for 4 states and 2 items, average expected score per state
  # Calculate expected values for each state across indicators
  ##################
  # IMPORTANT ASSUMPTIONS: 
  # 1. Indicators are monotonic and ordered: All indicators are assumed as monotonic and ordered from low to high severity
  # 2. Lower severity states are more prevalent: higher severity states are less likely to occur than lower severity states
  # 3  Composite severity is additive and equally weighted between indicators: Summing the expected means across indicators 
  #        provides a single composite severity measure per state (Additive decomposition: summing across disease components 
  #        yields the total burden https://pmc.ncbi.nlm.nih.gov/articles/PMC4140376/). 
  
  # indicator's Weight
  # we either implight the have the same weight (equally important in determining order of the latent state) or we set a weight
  #################
  # Set default equal weights if none provided
  if (is.null(indicator_weights)) {
    indicator_weights <- rep(1, n_indicators)
  } else {
    # Ensure weights sum to 1 for interpretability
    indicator_weights <- indicator_weights / sum(indicator_weights)
  }
  
  state_means <- sapply(1:k, function(s) {
    indicator_means <- numeric(n_indicators)
    
    for (i in 1:n_indicators) {
      # Here we calculate the weighted mean across categories for each indicator.
      #
      # psi1 captures the probability of being in "s" state for each category of the indicator
      #  (e.g none", "mild", "moderate", "severe")
      # That is the probability of of (e.g.) each of the three possible scores of item 1 (pain impact) given state "1" membership
      psi <- lmest_model$Psi[, s, i]  # probabilities for state s, indicator i
      
      # we take (e.g.) pain impact categories as numbers in order of "severity"
      # NOTE: the same assumption of a monotonic order from low to high severity is assumed for both indicators
      categories <- suppressWarnings(as.integer(names(psi)))
      if (any(is.na(categories))) stop(paste("Non-integer category names in indicator", i))
      # The observed categorical response for health impact are integers that go from (e.g.) 0 to 2, and those
      # of health impact go from 0 to 3. For both we compute the expected value of each level of categorical
      # variables. We do this multiplying the 4 ordered levels of impact (i.e. 0, 1, 2, 3) by their respective
      # PSI probabilities within each state. For each state, the sum will give the average severity score of 
      # each indicator weighted by its probability value:
      # for example: mean_item1 <- sum(as.integer(names(psi1)) * psi1, na.rm = TRUE)
      indicator_means[i] <- sum(categories * psi, na.rm = TRUE)
    }
    
    return(indicator_means)
  })
  
  # Transpose to get k x n_indicators matrix
  state_means <- t(state_means)
  
  # Set column names if provided
  if (!is.null(indicator_names) && length(indicator_names) == n_indicators) {
    colnames(state_means) <- indicator_names
  } else {
    colnames(state_means) <- paste0("indicator_", 1:n_indicators)
  }
  
  # Add row names for states
  rownames(state_means) <- paste("State", 1:k)
  
  # Calculate composite severity (sum across indicators)
  composite_severity <- rowSums(state_means)
  composite_severity <- as.vector(state_means %*% indicator_weights)
  
  
  # Find healthiest state (lowest composite severity)
  # If this worked as expected we will find the index with the lowest means as the 
  # healthier state (both indicators closest to zero)
  healthiest_idx <- which.min(composite_severity)
  
  # Order states from healthiest to most severe
  # similarly we take "composite_severity" (which contains the rowsums) and we reorder all the states:
  #########
  #### ASSUMPTION: Summing the expected means across indicators provides a single 
  #### composite severity measure per state. Equal weighting reflects no prior evidence 
  #### that one indicator is more clinically salient.
  ########
  severity_order <- order(composite_severity)
  
  # Return comprehensive results
  return(list(
    state_means = state_means,
    composite_severity = composite_severity,
    healthiest_state = healthiest_idx,
    severity_order = severity_order,
    ordered_states = paste("State", severity_order),
    severity_labels = c("Healthiest", rep("Intermediate", k-2), "Most Severe")[rank(composite_severity)]
  ))
}

## REBASE Be_coefficients according new order
###################################################

rebase_Be_latent_coefficients <- function(fit, state_order) {
  # fit: a fitted latent Markov model object with $Be matrix and $k states
  # state_order: integer vector indicating the expected order of states e.g. state_order_TE
  
  ncov_be <- nrow(fit$Be)
  k <- fit$k
  
  # Add zero baseline column for original baseline (usually state 1)
  Be_full <- cbind(0, fit$Be)
  colnames(Be_full) <- as.character(1:k)
  #cat("Original Be_full columns:\n")
  #print(colnames(Be_full))
  
  # Reorder columns according to state_order
  # this now shows the order of the columns as they should be
  # but coefficients need rebaseling
  Be_reordered <- Be_full[, as.character(state_order), drop=FALSE]
  
  # Subtract baseline state's coefficients to reset baseline
  # baseline_be this now contains the new baseline (\beta_4 in the example above)
  baseline_be <- Be_reordered[, 1, drop = FALSE]
  # here we do the re-baselining \beta_j-baseline_be$
  Be_rebased <- sweep(Be_reordered, MARGIN = 1, baseline_be, FUN = "-")
  
  colnames(Be_rebased) <- 1:k #as.character(seq_len(k))
  
  # Drop baseline column to reflect new reference state
  Be_rebased <- Be_rebased[, -1, drop = FALSE]
  
  dimnames(Be_rebased) <- list(
    rownames(fit$Be),  # Preserve row names
    colnames(fit$Be)   # Preserve column names and their attributes
  )
  
  return(Be_rebased)
}

## REBASE Ga_coefficients according new order
###################################################

rebase_Ga_transition_coefficients <- function(Ga, state_order) {
  
  # Ga: original 3D array (covariates x logits x states) from LMest output
  # state_order: integer vector specifying desired latent state order
  k <- dim(Ga)[3]      # Number of states
  ncov_ga <- dim(Ga)[1]
  nlogits_ga <- dim(Ga)[2] # Fixed to k-1 (number of logits)
  
  # Prepare expanded array with zero baseline logits (k logits including baseline)
  Gaf <- array(0, dim = c(ncov_ga, nlogits_ga + 1, k))
  
  # Insert original Ga slices into expanded array, skipping baseline per slice
  for (st in seq_len(k)) {
    non_baseline_cols <- setdiff(seq_len(nlogits_ga + 1), st)
    Gaf[, non_baseline_cols, st] <- Ga[, , st]
  }
  
  # Initialise rebased Ga array with original dimensions
  Ga_rebased <- array(NA, dim = c(ncov_ga, nlogits_ga, k))
  
  # Inputs for the slice reordering step:
  # k: number of latent states
  # baseline_state: the state index used as baseline in this slice (original start state)
  # orig_slice: matrix of coefficients for this starting state (covariates x contrast columns)
  # new_contrast_order: vector of latent state IDs excluding baseline that defines desired contrast order
  
  reorder_slice_with_mapping <- function(orig_slice, baseline_state, new_contrast_order, k) {
    # 1. Get non-baseline columns from the expanded slice
    non_baseline_cols <- setdiff(1:ncol(orig_slice), baseline_state)
    
    # 2. Extract the contrast data
    contrast_data <- orig_slice[, non_baseline_cols, drop = FALSE]
    
    # 3. Figure out which destination states these columns represent
    dest_states <- setdiff(1:k, baseline_state)
    
    # 4. Map new_contrast_order to the correct column positions
    reorder_indices <- match(new_contrast_order, dest_states)
    
    # 5. Reorder the columns
    reordered_data <- contrast_data[, reorder_indices, drop = FALSE]
    
    return(reordered_data)
  }
  
  # Loop through each state for slice reordering and rebasing
  
  for (i in seq_len(k)) {
    baseline_state <- state_order[i]          # Original baseline for this slice
    orig_slice <- Gaf[, , baseline_state]     # Original slice expanded with baseline zero
    new_contrast_order <- state_order[-i]     # New contrast order excluding baseline state
    # Get reordered slice
    reordered_slice <- reorder_slice_with_mapping(orig_slice, baseline_state, new_contrast_order, k)
    # baseline contrast vector is always zero because LMest takes as "staying in the ame state".
    # this means that we simply need to reorder the Ga according to the new order
    # the following can be safely commented out
    #baseline_coeff <- Gaf[, baseline_state, baseline_state]  # Will be zeros
    ## Subtract baseline from each column
    #for (col in 1:ncol(reordered_slice)) {
    #  reordered_slice[, col] <- reordered_slice[, col] - baseline_coeff
    #}
    Ga_rebased[, , i] <- reordered_slice       # Assign reordered slice
  }
  dimnames(Ga_rebased) <- list(
    rownames(Ga),  # Preserve row names
    as.numeric(1:(k-1)),  # Column names as "1", "2", "3" 
    paste("row (of the transition matrix) =", 1:k)  # Slice names
  )
  
  return(Ga_rebased)
}

rebase_and_refit_lmest <- function(fit_model, state_order, latent_formula, response_formula, plot = TRUE, out_SE = FALSE) {
  
  # Rebase Be coefficients (initial state probabilities)
  Be_rebased <- rebase_Be_latent_coefficients(fit_model, state_order)
  
  # Rebase Ga coefficients (transition probabilities)
  Ga_rebased <- rebase_Ga_transition_coefficients(fit_model$Ga, state_order)
  
  # Get number of states
  k <- dim(fit_model$Ga)[3]
  
  # Reorder Psi - represents the conditional response probabilities
  # The second dimension indexes latent states, so we reorder it by state_order
  Psi_reordered <- fit_model$Psi[, state_order, , drop = FALSE]
  dimnames(Psi_reordered)[[2]] <- as.numeric(1:k)
  
  # Create new start values
  new_start_values <- list(
    Be = Be_rebased,
    Ga = Ga_rebased,
    Psi = Psi_reordered
  )
  
  # Refit model with reordered states
  fit_reordered <- lmest(
    responsesFormula = response_formula,
    latentFormula = latent_formula,
    data = fit_model$data,
    index = c("idauniq", "time"),
    paramLatent = "multilogit",
    k = k,
    start = 2,
    fort = TRUE,
    parInit = new_start_values,
    output = TRUE,
    out_se = out_SE
  )
  
  # Plot conditional probabilities if requested
  if (plot) {
    plot(fit_reordered, what = "CondProb")
  }
  
  return(fit_reordered)
}

regression_summaries <- function(Model_fit) {
  results <- list()
  
  N_obs_states <- get_observed_InitialStates_counts(Model_fit)
  Be_varnames <- (Model_fit$Be %>% row.names())[-1]
  
  initial_States_summary <- initial_prob_Diagnostics(
    model = Model_fit,
    N_obs_states = N_obs_states,
    skip_intercept = TRUE, 
    conf_level = 0.95
  )
  
  Nobs_trans_between_states <- get_observed_transition_counts(Model_fit, option = "global")
  Ga_varnames <- (Model_fit$Ga[,,1] %>% row.names())[-1]
  
  results_trans <- list()
  for (state in 1:4) {
    results_trans[[state]] <- transition_Diagnostics_multilogit(Model_fit, state, Ga_varnames, Nobs_trans_between_states)
  }
  
  trans_summary <- dplyr::bind_rows(results_trans)
  
  results$initial_states<- initial_States_summary
  results$transitions<- trans_summary
  
  return(results)
}



## OBSERVED INITIAL States (Decoded COUNTS)
##################################

get_observed_InitialStates_counts <- function(model) {
  # Ul is a matrix of dimensions [n_subjects, n_timepoints]
  # We want the first time point (initial states)
  dec <- lmestDecoding(model)
  
  # Choose decoding type: "global" (Viterbi) or "local"
  # For global:
  initial_states <- dec$Ug[, 1]  # First column = time 1
  
  # Count observations in each state
  state_counts <- table(initial_states)
  
  # Convert to named vector with proper labels
  state_names <- paste("State", 1:model$k)
  N_obs_states <- setNames(as.numeric(state_counts), paste("State", names(state_counts)))
  
  # Ensure all states are represented even if count is 0
  all_states <- setNames(rep(0, model$k), state_names)
  all_states[names(N_obs_states)] <- N_obs_states
  
  return(all_states)
}

## OBSERVED Transitions (decoded COUNTS)
# hard assignment: It then counts how many people actually moved from state i to state
# j across those crisp assignments. Transitions are treated as observed facts
##################################

get_observed_transition_counts <- function(model, option = "global") {
  # Get decoded sequences
  dec <- lmestDecoding(model)
  
  if (option == "global") {
    cat("Using global decoding: select option = local for local decoding ")
    latseq <- dec$Ug  # global decoding
  } else if (option == "local") {
    cat("Using local decoding: select option = global for global decoding ")
    latseq <- dec$Ul  # local decoding
  } else {
    stop("Invalid option. Use 'global' or 'local'.")
  }
  
  k <- model$k
  transition_counts <- matrix(0, nrow=k, ncol=k)
  
  for (i in 1:nrow(latseq)) {
    for (t in 1:(ncol(latseq)-1)) {
      from <- latseq[i, t]
      to   <- latseq[i, t+1]
      if (!is.na(from) && !is.na(to)) {
        transition_counts[from, to] <- transition_counts[from, to] + 1
      }
    }
  }
  
  rownames(transition_counts) <- paste0("From State ", 1:k)
  colnames(transition_counts) <- paste0("To State ",   1:k)
  
  return(transition_counts)
}

## TRANSITION PROBABILITIES (based on posterior probability)
# uses soft assignment: model$PI[from, to, subject, time] contains the joint 
# posterior probability P(U_t = u, U_{t-1} = ū | Y) — This is the probability 
# that subject n made the transition from state ū to state u at time t, given all 
# their observed responses. These probabilities are averaged across subjects and 
# time points, then row-normalised. This is different from "decoded" ("observed") 
# states where an "hard" assignment is  made to cristalise the state into the most probable one.
##################################
calc_transition_matrix_prob <- function(model) {
  # Get dimensions
  TT <- dim(model$PI)[4]
  
  # Calculate mean transition probabilities across subjects and time points
  # Note: using time points 2:TT as specified in the plot function
  PM <- round(apply(model$PI[, , , 2:TT], c(1, 2), mean), 3)
  
  # Normalize rows to sum to 1 using the same method as the plot function
  PM <- round(diag(1/rowSums(PM)) %*% PM, 3)  # This is not just cosmetic. The raw averages of PI across 
                                              # subjects and time don't necessarily sum to 1 per row 
                                              # because PI captures joint probabilities integrated over 
                                              # the posterior — the normalisation enforces the constraint 
                                              # that transition probabilities out of each state sum to 1. 
  
  # Add row and column names
  rownames(PM) <- paste("From State", 1:model$k)
  colnames(PM) <- paste("To State", 1:model$k)
  
  return(PM)
}



##################################
## INITIAL States regression Diagnostics
##################################

initial_prob_Diagnostics <- function(model, var_names = NULL, N_obs_states, conf_level = 0.95, skip_intercept = TRUE) {
  # Get number of states
  k <- model$k
  
  # Get covariate names from Be dimensions
  all_covars <- dimnames(model$Be)[[1]]
  
  # If var_names is NULL, use all variables (optionally excluding intercept)
  if (is.null(var_names)) {
    if (skip_intercept && "(Intercept)" %in% all_covars) {
      var_names <- all_covars[all_covars != "(Intercept)"]
    } else {
      var_names <- all_covars
    }
    covariate_indices <- match(var_names, all_covars)
  } else {
    covariate_indices <- match(var_names, all_covars)
    
    if (any(is.na(covariate_indices))) {
      invalid_vars <- var_names[is.na(covariate_indices)]
      message("Variables requested:", paste(var_names, collapse = ", "))
      message("Variables available in model:", paste(all_covars, collapse = ", "))
      stop(paste("Variable(s) not found in model:", paste(invalid_vars, collapse = ", ")))
    }
  }
  
  # Get coefficient matrix
  coeff_mat <- model$Be
  
  # Check if SE available
  has_se <- !is.null(model$seBe)
  se_mat <- if (has_se) model$seBe else NULL
  
  # Critical value for confidence intervals
  z_crit <- qnorm((1 + conf_level) / 2)
  
  # States 2, 3, .., k (assuming state 1 is reference)
  states <- 2:k
  state_labels <- paste("State", states)
  
  # Get N_obs for each state
  N_obs_vec <- N_obs_states[state_labels]
  
  # Build dataframe for each state
  df_list <- lapply(seq_along(states), function(i) {
    state <- states[i]
    ref_state <- 1
    col_idx <- i
    
    coef_val <- coeff_mat[covariate_indices, col_idx]
    
    # Only extract se_val if SE exists
    se_val <- if (has_se) {
      se_mat[covariate_indices, col_idx]
    } else {
      NA_real_
    }
    
    data.frame(
      varname = var_names,
      State = paste("State", state),
      Ref_state = paste("State", ref_state),
      N_obs_states = N_obs_vec[[i]],
      Coefficient = round(coef_val, 3),
      StdError = if (has_se && !any(is.na(se_val))) round(se_val, 3) else NA_real_,
      t_value = if (has_se && !any(is.na(se_val))) round(coef_val / se_val, 2) else NA_real_,
      CI_lower = if (has_se && !any(is.na(se_val))) round(coef_val - z_crit * se_val, 3) else NA_real_,
      CI_upper = if (has_se && !any(is.na(se_val))) round(coef_val + z_crit * se_val, 3) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  
  # Combine all dataframes
  df <- do.call(rbind, df_list)
  
  # Add EPV
  p_params <- dim(model$Be)[1]
  df$EPV <- round(df$N_obs_states / p_params, 2)
  
  # Add diagnostics
  df <- df %>% 
    mutate(
      Signif = if (has_se) {
        case_when(
          abs(t_value) > 2.58 ~ "***",
          abs(t_value) > 1.96 ~ "**",
          abs(t_value) > 1.64 ~ "*",
          TRUE ~ ""
        )
      } else NA_character_,
      Sep_Flag = case_when(
        abs(Coefficient) > 10 ~ "!!!",
        has_se & abs(Coefficient) > 5 & StdError > 2 ~ "!!",
        abs(Coefficient) > 5 ~ "!",
        EPV < 10 ~ "EPV < 10 !!",
        EPV < 20 ~ "EPV < 20 !",
        has_se & StdError > 5 ~ "Extreme SE!",
        TRUE ~ "OK"
      ),
      Comment = case_when(
        !has_se ~ "No SE available",
        is.infinite(Coefficient) ~ "WARNING: Inf. coef - compl. separation",
        Sep_Flag == "!!!" ~ "WARNING: Separation likely",
        Sep_Flag == "!!" ~ "WARNING: Quasi-separation",
        Sep_Flag == "!" ~ "WARNING: Very large coef",
        StdError > 5 ~ "WARNING: Extreme SE!",
        EPV < 10 ~ "WARNING: EPV < 10 overfitting",
        N_obs_states < 20 ~ "WARNING: < 20 obs in state",
        N_obs_states < 30 ~ "WARNING: < 30 obs in state",
        EPV < 20 ~ "Low EPV: coef requires shrinkage",
        StdError > 1 ~ "CAUTION: SE > 1",
        abs(t_value) >= 2 & StdError <= 1 ~ "Reliable",
        abs(t_value) >= 1.6 & StdError <= 1 ~ "Marginal",
        abs(t_value) < 1.6 & StdError <= 1 ~ "n.s",
        TRUE ~ "Check_manually"
      )
    )
  
  df <- df %>% 
    select(varname, State, Ref_state, N_obs_states, Coefficient, StdError, t_value, 
           CI_lower, CI_upper, EPV, Sep_Flag, Signif, Comment) %>% 
    as_tibble()
  
  return(df)
}


##################################
## Transitions regression Diagnostics
##################################

#########################################################################
########################### MULTILOGIT  MODEL
#########################################################################

transition_Diagnostics_multilogit <- function(model, start_state, covariates_of_interest, N_obs_trans, conf_level = 0.95) {
  k <- model$k
  all_states <- 1:k
  dest_states <- setdiff(all_states, start_state) 
  col_idx <- match(dest_states, all_states[-start_state])
  
  all_covars <- dimnames(model$Ga)[[1]]
  
  if (is.null(all_covars)) {
    var_names <- covariates_of_interest
  } else {
    covariate_indices <- if (is.numeric(covariates_of_interest)) {
      covariates_of_interest
    } else {
      match(covariates_of_interest, all_covars)
    }
    var_names <- all_covars[covariate_indices]
  }
  
  coeff_mat <- model$Ga[, , start_state]
  
  # Check if SE available
  has_se <- !is.null(model$seGa)
  se_mat <- if (has_se) model$seGa[, , start_state] else NULL
  
  z_crit <- qnorm((1 + conf_level) / 2)
  from_label <- paste("From State", start_state)
  dest_labels <- paste("To State", dest_states)
  
  N_obs_vec <- N_obs_trans[from_label, dest_labels]
  
  df_list <- lapply(seq_along(dest_states), function(i) {
    dest <- dest_states[i]
    coef_val <- coeff_mat[covariates_of_interest, col_idx[i]]
    se_val <- if (has_se) {
      se_mat[covariates_of_interest, col_idx[i]]
    } else {
      NA_real_
    }
    varname  <- var_names
    
    data.frame(
      varname = varname,
      Transition = paste(start_state, "->", dest),
      N_obs_trans = N_obs_vec[[i]],
      Coefficient = round(coef_val, 3),
      StdError = if (has_se) round(se_val, 3) else NA_real_,
      t_value = if (has_se) round(coef_val / se_val, 2) else NA_real_,
      CI_lower = if (has_se) round(coef_val - z_crit * se_val, 3) else NA_real_,
      CI_upper = if (has_se) round(coef_val + z_crit * se_val, 3) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  
  df <- do.call(rbind, df_list)
  
  p_per_logit <- dim(model$Ga)[1]
  df$EPV <- round(df$N_obs_trans / p_per_logit, 2)
  
  df <- df %>% 
    mutate(
      Signif = if (has_se) {
        case_when(
          abs(t_value) > 2.58 ~ "***",
          abs(t_value) > 1.96 ~ "**",
          abs(t_value) > 1.64 ~ "*",
          TRUE ~ ""
        )
      } else NA_character_,
      Sep_Flag = case_when(
        abs(Coefficient) > 8 ~ "!!!",
        has_se & abs(Coefficient) > 5 & StdError > 2 ~ "!!",
        abs(Coefficient) > 5 ~ "!",
        EPV < 10 ~ "EPV < 10 !!",
        EPV < 20 ~ "EPV < 20 !",
        has_se & StdError > 5 ~ "Extreme SE!",
        TRUE ~ "OK"
      ),
      Comment = case_when(
        !has_se ~ "No SE available",
        is.infinite(Coefficient) ~ "WARNING: Inf. coef - compl. separation",
        Sep_Flag == "!!!" ~ "WARNING: Separation",
        Sep_Flag == "!!"  ~ "WARNING: Quasi-separation",
        Sep_Flag == "!"   ~ "WARNING: Extreme coef",
        StdError > 5      ~ "WARNING: Extreme SE!",
        EPV < 10          ~ "WARNING: EPV < 10 overfitting",
        N_obs_trans < 20  ~ "WARNING: < 20 obs",
        N_obs_trans < 30  ~ "WARNING: < 30 obs",
        EPV < 20          ~ "Low EPV: coef requires shrinkage",
        StdError > 1      ~ "CAUTION: SE > 1",
        abs(t_value) >= 2 & StdError <= 1 ~ "Reliable",
        abs(t_value) >= 1.6 &  StdError <= 1 ~ "Marginal",
        abs(t_value) < 1.6 & StdError <= 1 ~ "n.s",
        TRUE ~ "Check_manually"
      )
    )
  
  df <- df %>% 
    select(varname, Transition, N_obs_trans, Coefficient, StdError, t_value, 
           CI_lower, CI_upper, EPV, Sep_Flag, Signif, Comment) %>% 
    as_tibble()
  
  return(df)
}
#########################################################################
########################### DIFFLOGIT MODEL
#########################################################################

transition_table_Diagnostics_difflogit <- function(model, start_state, covariates_of_interest, N_obs_trans, conf_level = 0.95) {
  # Check that model uses difflogit parametrization
  if (model$paramLatent != "difflogit") {
    stop("This function is for difflogit models only. Use transition_table_Diagnostics for multilogit models.")
  }
  
  k <- model$k
  all_states <- 1:k
  
  # In difflogit, Ga is a list with two elements:
  # Ga[[1]] contains intercepts [states x destination_states matrix]
  # Ga[[2]] contains covariate effects [covariates x destination_states matrix]
  
  # Get intercepts for the start_state
  intercept_mat <- model$Ga[[1]]
  intercept_se_mat <- model$seGa[[1]]
  
  # Get covariate effects
  covar_mat <- model$Ga[[2]]
  covar_se_mat <- model$seGa[[2]]
  
  # Get available destination states from column names
  available_dests <- as.numeric(colnames(covar_mat))
  
  # Determine which destinations are available from this start state
  # In difflogit, not all transitions may be modeled
  dest_states <- setdiff(available_dests, start_state)
  
  if (length(dest_states) == 0) {
    warning(paste("No valid destination states found for start state", start_state))
    return(NULL)
  }
  
  # Get all covariate names
  all_covars <- rownames(covar_mat)
  
  if (is.null(all_covars) || length(all_covars) == 0) {
    stop("No covariates found in difflogit model")
  }
  
  # Handle covariate selection
  if (is.character(covariates_of_interest)) {
    covariate_indices <- match(covariates_of_interest, all_covars)
    var_names <- covariates_of_interest
  } else if (is.numeric(covariates_of_interest)) {
    covariate_indices <- covariates_of_interest
    var_names <- all_covars[covariate_indices]
  }
  
  # Check for invalid indices
  if (any(is.na(covariate_indices))) {
    invalid <- covariates_of_interest[is.na(covariate_indices)]
    stop(paste("Covariates not found:", paste(invalid, collapse = ", ")))
  }
  
  # Get N_obs for transitions
  z_crit <- qnorm((1 + conf_level) / 2)
  from_label <- paste("From State", start_state)
  
  # Build diagnostics for each destination state
  df_list <- lapply(dest_states, function(dest) {
    dest_label <- paste("To State", dest)
    
    # Get N_obs for this specific transition
    if (!is.null(N_obs_trans) && from_label %in% rownames(N_obs_trans) && 
        dest_label %in% colnames(N_obs_trans)) {
      n_obs <- N_obs_trans[from_label, dest_label]
    } else {
      n_obs <- NA
      warning(paste("Could not find N_obs for transition", start_state, "->", dest))
    }
    
    # Find column index for this destination in the coefficient matrices
    dest_col <- which(colnames(covar_mat) == as.character(dest))
    
    if (length(dest_col) == 0) {
      # This destination is not in the coefficient matrix
      # This can happen if certain transitions are not modeled
      return(NULL)
    }
    
    # Get coefficients and SEs for selected covariates
    coef_vals <- covar_mat[covariate_indices, dest_col]
    
    # Get standard errors
    if (!is.null(covar_se_mat) && ncol(covar_se_mat) >= dest_col) {
      se_vals <- covar_se_mat[covariate_indices, dest_col]
    } else {
      se_vals <- rep(NA, length(covariate_indices))
    }
    
    # Create dataframe for this destination
    data.frame(
      varname = var_names,
      Transition = paste(start_state, "->", dest),
      N_obs_trans = n_obs,
      Coefficient = round(coef_vals, 3),
      StdError = round(se_vals, 3),
      t_value = round(coef_vals / se_vals, 2),
      CI_lower = round(coef_vals - z_crit * se_vals, 3),
      CI_upper = round(coef_vals + z_crit * se_vals, 3),
      stringsAsFactors = FALSE
    )
  })
  
  # Remove NULL entries (destinations not in coefficient matrix)
  df_list <- df_list[!sapply(df_list, is.null)]
  
  if (length(df_list) == 0) {
    warning(paste("No valid transitions found for start state", start_state))
    return(NULL)
  }
  
  # Combine all dataframes
  df <- do.call(rbind, df_list)
  
  # Calculate EPV (Events per parameter)
  # For difflogit: count intercept + all covariates
  p_per_transition <- length(all_covars) + 1  # covariates + intercept
  df$EPV <- round(df$N_obs_trans / p_per_transition, 2)
  
  # Add significance and diagnostic flags (same criteria as multilogit)
  df <- df %>% 
    mutate(
      Signif = case_when(
        is.na(StdError) ~ "NA",
        abs(t_value) > 2.58 ~ "***",
        abs(t_value) > 1.96 ~ "**",
        abs(t_value) > 1.64 ~ "*",
        TRUE ~ ""
      ),
      Sep_Flag = case_when(
        is.na(StdError) ~ "No SE",
        is.na(N_obs_trans) ~ "Unknown N",
        abs(Coefficient) > 10 ~ "!!!",
        abs(Coefficient) > 5 & StdError > 2 ~ "!!",
        abs(Coefficient) > 5 ~ "!",
        EPV < 10 ~ "EPV < 10 !!",
        EPV < 20 ~ "EPV < 20 !",
        StdError > 5 ~ "Extreme SE!",
        TRUE ~ "OK"
      ),
      Comment = case_when(
        is.na(StdError) ~ "WARNING: No SE available",
        is.na(N_obs_trans) ~ "WARNING: N_obs not found",
        is.infinite(Coefficient) ~ "WARNING: Inf. coef - compl. separation",
        Sep_Flag == "!!!" ~ "WARNING: Separation likely",
        Sep_Flag == "!!" ~ "WARNING: Quasi-separation",
        Sep_Flag == "!" ~ "WARNING: Very large coef",
        StdError > 5 ~ "WARNING: Extreme SE!",
        EPV < 10 ~ "WARNING: EPV < 10 overfitting",
        N_obs_trans < 20 ~ "WARNING: < 20 obs",
        N_obs_trans < 30 ~ "WARNING: < 30 obs",
        EPV < 20 ~ "Low EPV: coef requires shrinkage",
        StdError > 1 ~ "CAUTION: SE > 1",
        abs(t_value) >= 2 & StdError <= 1 ~ "Reliable",
        abs(t_value) >= 1.6 & StdError <= 1 ~ "Marginal",
        abs(t_value) < 1.6 & StdError <= 1 ~ "n.s",
        TRUE ~ "Check_manually"
      )
    )
  
  # Reorder columns for better readability
  df <- df %>% 
    select(varname, Transition, N_obs_trans, Coefficient, StdError, t_value, 
           CI_lower, CI_upper, EPV, Sep_Flag, Signif, Comment) %>% 
    as_tibble()
  
  return(df)
}

# Helper function to get all transitions for difflogit model
get_all_transitions_difflogit <- function(model, covariates_of_interest, N_obs_trans, conf_level = 0.95) {
  if (model$paramLatent != "difflogit") {
    stop("This function is for difflogit models only")
  }
  
  k <- model$k
  all_results <- list()
  
  for (start_state in 1:k) {
    tryCatch({
      result <- transition_table_Diagnostics_difflogit(
        model = model,
        start_state = start_state,
        covariates_of_interest = covariates_of_interest,
        N_obs_trans = N_obs_trans,
        conf_level = conf_level
      )
      all_results[[paste0("From_State_", start_state)]] <- result
    }, error = function(e) {
      warning(paste("Could not compute transitions from state", start_state, ":", e$message))
    })
  }
  
  # Combine all results
  if (length(all_results) > 0) {
    combined <- do.call(rbind, all_results)
    rownames(combined) <- NULL
    return(combined)
  } else {
    return(NULL)
  }
}

##############################################################
## END OF LTA transition table
##############################################################



################################################################################
################ BEGINNING OF LMEST SEARCH DIAGNOSTIC FUNCTION
################################################################################

lmestSearch_plot <- function(all_lks, k = 4, k_multiplier = 3, plot_hist = TRUE) {
  best_lk <- all_lks[[k]]$lk
  lktrace_all <- all_lks[[k]]$lktrace
  
  # Assume that lktrace contains:
  #  . one start=0 fit as first lk,
  #  . nrep*k-1 start = 1 lk
  #  . one start=2 fit as last run
  # I exclude both start0 and start2 to focus on the random start distribution
  if(length(lktrace_all) > 2){
    random_lktrace <- lktrace_all[2:(length(lktrace_all) - 1)]
  } else {
    random_lktrace <- lktrace_all
  }
  
  # Store absolute best (probably from start=2, last run)
  # perhaps here we could compare with last run to flag whenever the absolute 
  # best lk is not coming from start=2
  abs_best_lk <- lktrace_all[length(lktrace_all)]
  
  max_lk <- max(random_lktrace)
  diff_array <- random_lktrace - max_lk
  abs_diff <- abs(diff_array)
  
  median_diff <- median(abs_diff)
  mad_diff <- mad(abs_diff)
  # Cutoff calculation
  # Define cutoff as median + 3 * MAD [https://pmc.ncbi.nlm.nih.gov/articles/PMC8801745/] 
  # "For example, Miller (1991) recommends using 2, 2.5, or 3 as the value k, depending on 
  # the purpose of outlier detection, while Leys et al. (2013) recommend a criterion of 2.5 as the value k"
  cutoff <- median_diff + k_multiplier * mad_diff
  
  filtered_diffs <- diff_array[abs_diff <= cutoff]
  dropped_diffs <- diff_array[abs_diff > cutoff]
  drop_ratio <- length(dropped_diffs) / length(abs_diff)
  
  df_all <- data.frame(diffs = diff_array, type = "Random starts")
  df_filtered <- data.frame(diffs = filtered_diffs, type = "Filtered")
  
  if(plot_hist){
    p1 <- ggplot(df_all, aes(x = diffs)) +
      geom_histogram(fill = "lightblue", color = "black", bins = 50) +
      geom_vline(xintercept = abs_best_lk - max_lk, color = "red", linetype = "dashed") +
      labs(title = paste0("Random starts log-lik diffs for k=", k," and n =",length(abs_diff)," random starts"),
           x = "Log-likelihood difference from max",
           y = "Count",
           caption = "Red dashed line: absolute best log-likelihood found (start=2 refinement run)") +
      theme_minimal()
    
    p2 <- ggplot(df_filtered, aes(x = diffs)) +
      geom_histogram(fill = "lightblue", color = "black", bins = 50) +
      geom_vline(xintercept = abs_best_lk - max_lk, color = "red", linetype = "dashed") +
      labs(title = paste0("Filtered log-lik diffs for k=", k," and n =",length(abs_diff)," random starts 
                          (tol = median + ", k_multiplier, "*MAD)"),
           x = "Log-likelihood difference from max",
           y = "Count",
           caption = paste0("Red dashed line: absolute best log-likelihood found (start=2 refinement run). \n Drop ratio: ", round(drop_ratio, 3))) +
      theme_minimal()
    
    grid.arrange(p1, p2, ncol = 1)
  }
  
  return(list(
    median_diff = median_diff,
    mad_diff = mad_diff,
    cutoff = cutoff,
    drop_ratio = drop_ratio,
    filtered_diffs = filtered_diffs,
    dropped_diffs = dropped_diffs,
    abs_best_lk = abs_best_lk
  ))
}


################################################################################
################ END OF LMEST SEARCH DIAGNOSTIC FUNCTION
################################################################################


################################################################################
################ BEGINNING OF LMEST PARALLEL FUNCTION
################################################################################
run_LMEST_models_parallel <- function(model_configs, 
                                      log_path, save_path, data,
                                      n_cores = 4, # max number of cores to use (lmest can only run one lmest command X core)
                                      modBasic = 1, paramLatent = "multilogit", start = 0, maxit = 1000, ntry = 1, k = 4, tol = 10^-8, fort = F,
                                      stability_tol = 1, # this is the comparison parameter for multiply tries' lk
                                      out_SE = F,
                                      save_models = TRUE) {
  
  # Create log file
  log_file <- paste0(log_path,"lmest_models_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
  
  # Function to log messages
  log_message <- function(message, model_name = "GENERAL") {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    log_entry <- paste0("[", timestamp, "] [", model_name, "] ", message, "\n")
    cat(log_entry)
    cat(log_entry, file = log_file, append = TRUE)
  }
  
  log_message("=== STARTING PARALLEL LMEST MODEL EXECUTION ===")
  log_message(paste("Log file:", log_file))
  log_message(paste("Number of available cores:", detectCores()))
  
  # Function to run individual model with comprehensive logging
  run_single_model <- function(config) {
    model_name <- config$name
    warnings_list <- character(0)
    messages_list <- character(0)
    
    # Create model-specific log function
    model_log <- function(msg) {
      timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      log_entry <- paste0("[", timestamp, "] [", model_name, "] ", msg, "\n")
      cat(log_entry)
      return(log_entry)
    }
    
    model_log("Starting model execution...")
    start_time <- Sys.time()
    
    result <- tryCatch({
      withCallingHandlers({
        # Initialize LL tracking
        ll_runs <- list()
        parse_counter <- 0
        
        model_log("Calling lmest function...")
        
        # Capture lmest output for LL parsing
        capture_start_time <- Sys.time()
        out_lines <- capture.output({
          model_result <- lmest(
            responsesFormula = eval(config$responsesFormula),
            latentFormula = eval(config$latentFormula),
            index = c("idauniq","time"),
            modBasic = modBasic,
            data = data,
            paramLatent = "multilogit",
            start = start,
            fort = fort,
            seed = 200720,
            maxit = maxit,
            ntry = ntry,
            k = k,
            output = TRUE,
            tol = tol,
            out_se = out_SE
          )
        })
        
        # Parse LL values from captured output
        current_start <- 0
        current_type <- ""
        for (line in out_lines) {
          # Detect initialization headers
          if (grepl("\\* Deterministic", line)) {
            current_start <- 0
            current_type <- "deterministic"
          } else if (grepl("\\* Random initialization \\(([0-9]+)/([0-9]+)\\)", line)) {
            matches <- regmatches(line, regexpr("\\(([0-9]+)/([0-9]+)\\)", line))
            current_start <- as.numeric(gsub("[^0-9]", "", strsplit(matches, "/")[[1]][1]))
            current_type <- "random"
          }
          
          # Extract final LL from iteration lines
          if (grepl("^\\s*4\\s*\\|\\s*[01]\\s*\\|", line) && current_type != "") {
            parts <- strsplit(trimws(line), "\\|")[[1]]
            if (length(parts) >= 4) {
              step_val <- as.numeric(trimws(parts[3]))
              lk_val <- as.numeric(trimws(parts[4]))
              
              if (!is.na(step_val) && !is.na(lk_val)) {
                # Update or add this start's final LL
                start_key <- paste0(current_type, "_", current_start)
                ll_runs[[start_key]] <- list(
                  start_id = current_start,
                  init_type = current_type, 
                  final_ll = lk_val,
                  final_step = step_val,
                  elapsed_sec = as.numeric(difftime(Sys.time(), capture_start_time, units = "secs"))
                )
              }
            }
          }
        }
        
        # Convert to data frame and compute replication summary
        if (length(ll_runs) > 0) {
          ll_runs_df <- do.call(rbind, lapply(ll_runs, data.frame))
          
          max_ll <- max(ll_runs_df$final_ll, na.rm = TRUE)
          ll_diffs <- max_ll - ll_runs_df$final_ll
          median_diff <- median(ll_diffs, na.rm = TRUE)
          mad_diff <- mad(ll_diffs, constant = 1, na.rm = TRUE) # constant = 1 for Raw median absolute deviation
          # Cutoff calculation
          # Define cutoff as median + 3 * MAD [https://pmc.ncbi.nlm.nih.gov/articles/PMC8801745/] 
          # "For example, Miller (1991) recommends using 2, 2.5, or 3 as the value k, depending on 
          # the purpose of outlier detection, while Leys et al. (2013) recommend a criterion of 2.5 as the value k"
          cutoff_tol <- median_diff + 3 * mad_diff
          
          model_log(sprintf("LL differences: median=%.4f, MAD=%.4f, observed stability for ll <=%.4f",
                            median_diff, mad_diff, cutoff_tol))
          
          abs_diff <- abs(ll_diffs)
          filtered_diffs <- ll_diffs[abs_diff <= cutoff_tol]
          dropped_diffs <- ll_diffs[abs_diff > cutoff_tol]
          drop_ratio <- length(dropped_diffs) / length(abs_diff)
          
          n_at_max <- sum(ll_diffs <= stability_tol, na.rm = TRUE)
          n_starts <- nrow(ll_runs_df)
          replication_ok <- n_at_max >= 2
          
          # Plot histogram (optional)
          plot_df <- data.frame(lldiff = ll_diffs)
          library(ggplot2)
          p1 <- ggplot(plot_df, aes(x = lldiff)) +
            geom_histogram(bins = 30, fill = "skyblue", color = "black")+
            labs(title = paste0("Histogram of LL differences for model", model_name),
                 x = "Log-likelihood difference from max",
                 y = "Count",
                 caption = paste0("\n Drop ratio: ", round(drop_ratio, 3))
            ) 
          
          p2 <- ggplot(plot_df%>%filter(lldiff>=cutoff_tol), aes(x = lldiff)) +
            geom_histogram(bins = 30, fill = "skyblue", color = "black")+
            labs(title = paste0("Histogram of LL differences for model", model_name, "Filtered by ll_diff < (median_diff + 3*MAD_diff)"),
                 x = "Log-likelihood difference from max",
                 y = "Count",
                 caption = paste0("\n Drop ratio: ", round(drop_ratio, 3))
            )
          
          #ggsave(filename = paste0("LL_diffs_histogram_", model_name, ".png"), plot = p, width = 7, height = 5)
          
          ll_replication <- list(
            max_ll = max_ll,
            n_at_max = n_at_max,
            n_starts = n_starts,
            stability_tol = stability_tol,
            replication_ok = replication_ok,
            median_diff = median_diff,
            mad_diff = mad_diff
          )
          
          # Log replication summary
          model_log(sprintf("LL replication: max = %.4f, matched in %d/%d starts (stability_tol=%.4f)", 
                            max_ll, n_at_max, n_starts, stability_tol))
          
          # More informative status based on replication pattern
          if (n_at_max >= max(2, ceiling(n_starts * 0.6))) {
            model_log(sprintf("Convergence quality: High consistency (%d%% of starts reached maximum)", 
                              round(100 * n_at_max / n_starts)))
          } else if (n_at_max >= 2) {
            model_log(sprintf("Convergence quality: Moderate consistency (%d starts reached maximum, others may be local optima)", 
                              n_at_max))
          } else {
            model_log(sprintf("Convergence quality: Low consistency (only %d start reached maximum, consider increasing ntry)", 
                              n_at_max))
          }
          
          # Add range information if there's variation
          ll_range <- max(ll_runs_df$final_ll) - min(ll_runs_df$final_ll)
          if (ll_range > stability_tol) {
            model_log(sprintf("LL range across starts: %.6f", ll_range))
          }
        } else {
          ll_runs_df <- NULL
          ll_replication <- NULL
          model_log("Warning: Could not parse LL replication data")
        }
        
        end_time <- Sys.time()
        duration <- difftime(end_time, start_time, units = "mins")
        model_log(paste("Model completed successfully in", round(duration, 2), "minutes"))
        
        # Log model summary information
        model_log(paste("Description:", config$description))
        if (!is.null(model_result$lk)) {
          model_log(paste("Log-likelihood:", round(model_result$lk, 4)))
        }
        if (!is.null(model_result$aic)) {
          model_log(paste("AIC:", round(model_result$aic, 4)))
        }
        if (!is.null(model_result$bic)) {
          model_log(paste("BIC:", round(model_result$bic, 4)))
        }
        if (!is.null(model_result$np)) {
          model_log(paste("Number of parameters:", model_result$np))
        }
        
        # Save model immediately upon successful completion (not at the end)
        # this part operates within each model in the list of models run
        if (!is.null(model_result) && save_models && !is.null(config$save_FULLpath)) {
          tryCatch({
            # Save with timestamp to avoid conflicts
            timestamp_suffix <- format(Sys.time(), "%Y%m%d_%H%M%S")
            # gsub replace the pattern "\\.rds$" with the timestamp to create a time-based copy
            save_path_timestamped <- gsub("\\.rds$", paste0("_", timestamp_suffix, ".rds"), config$save_FULLpath)
            
            saveRDS(model_result, save_path_timestamped)
            model_log(paste("IMMEDIATELY SAVED to:", save_path_timestamped))
            
            # Also save to original path (overwrite old version)
            saveRDS(model_result, config$save_FULLpath)
            model_log(paste("Also saved to original path:", config$save_FULLpath))
            
          }, error = function(save_err) {
            model_log(paste("SAVE ERROR:", save_err$message))
          })
        }
        
      }, warning = function(w) {
        warning_msg <- paste("WARNING:", w$message)
        warnings_list <<- c(warnings_list, warning_msg)
        model_log(warning_msg)
        invokeRestart("muffleWarning")
      }, message = function(m) {
        message_msg <- paste("MESSAGE:", m$message)
        messages_list <<- c(messages_list, message_msg)
        model_log(message_msg)
        invokeRestart("muffleMessage")
      })
      
    }, error = function(e) {
      end_time <- Sys.time()
      duration <- difftime(end_time, start_time, units = "mins")
      error_msg <- paste("ERROR after", round(duration, 2), "minutes:", e$message)
      model_log(error_msg)
      return(NULL)
    })
    
    return(list(
      model = result,
      name = model_name,
      config = config,
      warnings = warnings_list,
      messages = messages_list,
      duration = difftime(Sys.time(), start_time, units = "mins"),
      ll_runs = ll_runs_df,           
      ll_replication = ll_replication,
      rep_plot = p1, # replication lot of ll diffs
      rep_plot_filtered = p2
    ))
  }
  
  # Limit to n_cores = 4 workers for stability and memory management
  max_workers <- min(n_cores, length(model_configs), detectCores() - 1)
  log_message("Setting up future parallel processing...")
  log_message(paste("Available cores:", detectCores(), "- Using max", max_workers, "workers"))
  plan(multisession, workers = max_workers)
  
  log_message(paste("Running", length(model_configs), "models in parallel..."))
  overall_start_time <- Sys.time()
  
  # Run all models in parallel using future
  model_results <- future_lapply(model_configs, run_single_model, 
                                 future.seed = TRUE)
  overall_end_time <- Sys.time()
  total_duration <- difftime(overall_end_time, overall_start_time, units = "mins")
  
  log_message(paste("=== ALL MODELS COMPLETED ==="))
  log_message(paste("Total execution time:", round(total_duration, 2), "minutes"))
  
  # Process results and create summary
  successful_models <- list() ## preparing the empty list of model summaries
  failed_models <- character(0) ## preparing the counter of failed models
  
  log_message("\n=== EXECUTION SUMMARY ===")
  
  for (i in seq_along(model_results)) {
    result <- model_results[[i]]
    model_name <- result$name
    
    log_message(paste("\n--- Model:", model_name, "---"))
    log_message(paste("Duration:", round(result$duration, 2), "minutes"))
    
    if (!is.null(result$model)) {
      log_message("Status: ✓ SUCCESS")
      successful_models[[model_name]] <- result
    } else {
      log_message("Status: ✗ FAILED")
      failed_models <- c(failed_models, model_name)
    }
    
    # Log warnings
    if (length(result$warnings) > 0) {
      log_message(paste("Warnings (", length(result$warnings), "):"))
      for (w in result$warnings) {
        log_message(paste("  -", w))
      }
    } else {
      log_message("Warnings: None")
    }
    
    # Log messages  
    if (length(result$messages) > 0) {
      log_message(paste("Messages (", length(result$messages), "):"))
      for (m in result$messages) {
        log_message(paste("  -", m))
      }
    }
    # Log replication summary in main summary
    if (!is.null(result$ll_replication)) {
      rep_sum <- result$ll_replication
      log_message(sprintf("  LL Replication: %d/%d starts reached max (%.4f)", 
                          rep_sum$n_at_max, rep_sum$n_starts, rep_sum$max_ll))
    }
  }
  
  # Final summary
  log_message(paste("\n=== FINAL SUMMARY ==="))
  log_message(paste("Successful models:", length(successful_models)))
  log_message(paste("Failed models:", length(failed_models)))
  
  if (length(failed_models) > 0) {
    log_message(paste("Failed model names:", paste(failed_models, collapse = ", ")))
  }
  
  log_message(paste("Log saved to:", log_file))
  
  # Clean up
  plan(sequential)
  
  # Return results
  return(list(
    models = successful_models,
    failed_models = failed_models,
    log_file = log_file,
    total_duration = total_duration
  ))
}


################################################################################
################ END OF LMEST PARALLEL FUNCTION
################################################################################
run_lmest_parallel_seeds <- function(config, data, n_reps = 200, ntry = 1, n_cores = 8,
                                     modBasic = 1, start = 1, maxit = 10000, k = 4, 
                                     tol = 1e-8, fort = TRUE, out_SE = FALSE,
                                     base_seed = 200720, plot_hist = TRUE,
                                     exclude_deterministic = TRUE) {
  
  library(future.apply)
  library(ggplot2)
  
  plan(multisession, workers = n_cores)
  
  cat(sprintf("Running %s with %d parallel replications...\n", config$name, n_reps))
  start_time <- Sys.time()
  
  all_results <- future_lapply(1:n_reps, function(rep_id) {
    
    ll_runs <- list()
    
    out_lines <- capture.output({
      model_result <- lmest(
        responsesFormula = eval(config$responsesFormula),
        latentFormula = eval(config$latentFormula),
        index = c("idauniq", "time"),
        modBasic = modBasic,
        data = data,
        paramLatent = "multilogit",
        start = start,
        fort = fort,
        seed = base_seed + rep_id,
        maxit = maxit,
        ntry = ntry,
        k = k,
        output = TRUE,
        tol = tol,
        out_se = out_SE
      )
    })
    
    # Parse LL from output
    current_start <- 0
    current_type <- ""
    for (line in out_lines) {
      if (grepl("\\* Deterministic", line)) {
        current_start <- 0
        current_type <- "deterministic"
      } else if (grepl("\\* Random initialization \\(([0-9]+)/([0-9]+)\\)", line)) {
        matches <- regmatches(line, regexpr("\\(([0-9]+)/([0-9]+)\\)", line))
        current_start <- as.numeric(gsub("[^0-9]", "", strsplit(matches, "/")[[1]][1]))
        current_type <- "random"
      }
      
      if (grepl("^\\s*4\\s*\\|\\s*[01]\\s*\\|", line) && current_type != "") {
        parts <- strsplit(trimws(line), "\\|")[[1]]
        if (length(parts) >= 4) {
          lk_val <- as.numeric(trimws(parts[4]))
          if (!is.na(lk_val)) {
            start_key <- paste0(current_type, "_", current_start)
            
            # ONLY include if random (exclude deterministic start=0 and start=2)
            if (!exclude_deterministic || current_type == "random") {
              ll_runs[[start_key]] <- lk_val
            }
          }
        }
      }
    }
    
    return(list(
      model = model_result,
      lk = model_result$lk,
      rep_id = rep_id,
      seed = base_seed + rep_id,
      ll_runs = ll_runs
    ))
    
  }, future.seed = TRUE)
  
  plan(sequential)
  
  duration <- difftime(Sys.time(), start_time, units = "mins")
  
  lks <- sapply(all_results, function(x) x$lk)
  best_idx <- which.max(lks)
  best_result <- all_results[[best_idx]]
  
  # Aggregate all LL values
  all_lls <- unlist(lapply(all_results, function(x) unlist(x$ll_runs)))
  
  max_lk <- max(lks)
  lk_diffs <- max_lk - lks
  n_at_max <- sum(lk_diffs <= 5)
  
  abs_best_lk <- max(all_lls, na.rm = TRUE)
  all_diffs <- abs_best_lk - all_lls
  n_at_max_all <- sum(all_diffs <= 5)
  
  
  cat(sprintf("\nCompleted in %.1f minutes\n", duration))
  cat(sprintf("Best LL: %.4f (rep %d, seed %d)\n", best_result$lk, best_result$rep_id, best_result$seed))
  cat(sprintf("Stability(replications): %d/%d within 5 of max (%.1f%%)\n", n_at_max, n_reps, 100*n_at_max/n_reps))
  cat(sprintf("Stability (all starts): %d/%d within 5 of max (%.1f%%)\n", 
              n_at_max_all, length(all_lls), 100*n_at_max_all/length(all_lls)))
  cat(sprintf("Total random starts captured: %d\n", length(all_lls)))
  
  p1 <- NULL
  if (plot_hist && length(all_lls) > 0) {
    df_all <- data.frame(diffs = all_diffs)
    
    p1 <- ggplot(df_all, aes(x = diffs)) +
      geom_histogram(fill = "lightblue", color = "black", bins = 50) +
      geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
      labs(
        title = paste0("Random starts LL diffs: ", config$name, " (k=", k, ", n=", length(all_lls), " starts)"),
        x = "Log-likelihood difference from max",
        y = "Count",
        caption = if(exclude_deterministic) "Only random starts (start=1). Excludes deterministic (start=0,2)" else "All starts including deterministic"
      ) +
      theme_minimal()
    
    print(p1)
  }
  
  return(list(
    name = config$name,
    best_model = best_result$model,
    all_results = all_results,
    lks = lks,
    lk_diffs = lk_diffs,
    n_at_max = n_at_max,
    n_reps = n_reps,
    duration_mins = as.numeric(duration),
    all_lls = all_lls,
    abs_best_lk = abs_best_lk,
    plot = p1
  ))
}
##############################################################
## Health Impact lv PARAMETRISATION using cluster's medoids
##############################################################


categorize_wave <- function(data, var_name, ref_centers) {
  # Create new categorical variable name
  cat_var_name <- paste0(var_name, "_cat")
  
  # Initialize all values as NA
  data[[cat_var_name]] <- NA
  
  # Set all values below 0.1 to category 0 (the "zero" score), but keep NAs as NA
  data[[cat_var_name]][data[[var_name]] < 0.1 & !is.na(data[[var_name]])] <- 0
  
  # Get non-zero indices (values >= 0.1), excluding NAs.
  # Threshold 0.1 excludes artefactual near-zero scores: under partial scalar
  # invariance, freed item intercepts (lifta, hlthlm) produce scores ~0.00079
  # for respondents with genuinely zero health impact. These are structural
  # zeros on the IRT scale and are assigned category 0 above, not clustered.
  nonzero_indices <- which(data[[var_name]] >= 0.1 & !is.na(data[[var_name]]))
  
  if(length(nonzero_indices) > 0) {
    # Get the non-zero values
    wave_nonzero <- data[[var_name]][nonzero_indices]
    
    # For each observation, find the closest reference center using Manhattan distance
    clusters <- sapply(wave_nonzero, function(x) {
      # Calculate Manhattan distances to all reference centers
      # data here is *univariate* therefore we can calculate distances directly from raw data to medoids
      manhattan_distances <- abs(x - ref_centers)  # Manhattan distance for 1D data
      # Return the cluster number of the closest center
      which.min(manhattan_distances)
    })
    
    # Assign cluster numbers
    data[[cat_var_name]][nonzero_indices] <- clusters
  }
  
  # NA values in the original variable will remain NA in the categorical variable
  # because we initialized all values as NA and only modified non-NA entries
  data[[cat_var_name]] <- as.integer(data[[cat_var_name]])
  return(data)
}

####################################################################
## END OF Health Impact lv PARAMETRISATION
####################################################################

##############################################################
## BEGIN OF MICE RELATED FUNCTIONS
##############################################################


IMPACT_parametrisation_to_mice_objs <- function(mids_object, ref_centers) {
  
  # Extract long format data
  long_data <- mice::complete(mids_object, action = "long", include = TRUE)
  
  # Apply clustering to each imputed dataset
  processed_long <- long_data %>%
    group_by(.imp) %>%
    do({
      current_data <- .
      
      # Apply your categorize_wave function with fixed reference centers
      # the categorised wave ouputs a 4 level variable zero level, level 1,
      # and level 2,3 both the latter correspond to highest levels of the 
      # latent trait
      current_data <- categorize_wave(current_data, "lvimp_shifted", ref_centers)
      
      # Create the 3-level categorization (diagnostic; lvimp_shifted_cat is the LTA input)
      current_data <- current_data %>%
        mutate(lvimp_shifted_cat3 = case_when(
          lvimp_shifted_cat == 0 ~ 0,
          lvimp_shifted_cat < 2 ~ 1,
          lvimp_shifted_cat >= 2 ~ 2,
          TRUE ~ NA_real_
        ))
      
      current_data
    }) %>%
    ungroup()
  
  # Convert back to mids object
  new_mids <- as.mids(processed_long, 
                      .imp = ".imp",
                      .id = ".id")
  
  return(new_mids)
}

ssupport_median_parametrisation_to_mids <- function(mids_object, median_value) {
  require(progress)
  
  long_data <- mice::complete(mids_object, action = "long", include = TRUE)
  
  # Create categorical variable ssupport6_cat for all rows with non-missing ssupport6
  long_data <- long_data %>%
    mutate(ssupport6_cat = case_when(
      is.na(ssupport6) & partner == 0 ~ "No partner",
      ssupport6 <= median_value ~ "Above median SS",
      ssupport6 > median_value ~ "Below median SS",
      TRUE ~ NA_character_) %>% as.factor
    )
  
  
  # Convert back to mids object
  new_mids <- as.mids(long_data, .imp = ".imp", .id = ".id")
  return(new_mids)
}

# Function to add wealth quintiles to MICE object
add_wealth_quintiles_to_mice <- function(mids_object, w4quintile_breaks_log) {
  
  # Extract long format data
  long_data <- mice::complete(mids_object, action = "long", include = TRUE)
  
  # Apply wealth quintiles to each imputed dataset
  processed_long <- long_data %>%
    group_by(.imp) %>%
    do({
      current_data <- .
      
      # Add wealth quintile variables
      current_data <- current_data %>%
        mutate(
          # Intermediate quintile variable; wealthQ_bin is the LTA input (nettotw_bu_s_Q retained for traceability)
          nettotw_bu_s_Q = cut(nettotw_bu_s_log,
                               breaks = w4quintile_breaks_log, 
                               labels = FALSE,
                               include.lowest = TRUE),
          wealthQ_bin = case_when(
            nettotw_bu_s_Q == 1 ~ "Most deprived Q",
            nettotw_bu_s_Q > 1 ~ "Least deprived Qs",
            TRUE ~ NA_character_) %>% factor()
        )
      
      current_data
    }) %>%
    ungroup()
  
  # Convert back to mids object
  new_mids <- as.mids(processed_long, 
                      .imp = ".imp",
                      .id = ".id")
  
  return(new_mids)
}

### This function labels the categories of binary and nominal variables and 
### sets as numeric the indicators for 

label_variables_for_LTA <- function(mids_object) {
  
  # Extract long format data
  long_data <- mice::complete(mids_object, action = "long", include = TRUE)
  
  # Apply transformations to each imputed dataset
  processed_long <- long_data %>%
    group_by(.imp) %>%
    do({
      current_data <- .
      
      current_data <- current_data %>%
        mutate(
          wave = as.integer(wave),
          time = wave - 3,
          # Binary MLTC variable
          Sex_BIN = case_when(
            ragender == "0" ~ "Male",   # ragender is a 2-level factor ("0"=male, "1"=female) after mice 2l.bin imputation
            ragender == "1" ~ "Female",
            TRUE ~ NA_character_
          )%>% factor(levels = c("Female", "Male")),
          partner = case_when(
            partner == "0" ~ "No-partner",
            partner == "1" ~ "has-partner",
            TRUE ~ NA_character_
          )%>% factor(levels = c("No-partner", "has-partner")), 
          raagey_z        = as.numeric(scale(raagey)[, 1]),  # re-center per imp
          Sex_numeric     = as.integer(Sex_BIN) - 1L,        # Female=0, Male=1
          partner_numeric = as.integer(partner) - 1L,        # No-partner=0, has-partner=1
          # Recode pain variable to start from 0 (LMEST requirement)
          painchr_ext = case_when(
            painchr_ext == 1 ~ 0,  # "No pain" ~ 0
            painchr_ext == 2 ~ 1,  # "Acute pain" ~ 1
            painchr_ext == 3 ~ 2,  # "Chronic pain" ~ 2
            painchr_ext == 4 ~ 3,  # "Chronic widespread" ~ 3
            TRUE ~ NA_real_
          )
        )
      
      current_data
    }) %>%
    ungroup()
  
  # Convert back to mids object
  new_mids <- as.mids(processed_long, 
                      .imp = ".imp",
                      .id = ".id")
  
  return(new_mids)
}


################################################################################
######## DAGS: INITIAL STATE PROBABILITY
################################################################################
init_prob_DAG<-'dag {
"Depression BL" [pos="-0.788,-1.481"]
"Initial Health State" [outcome,pos="-0.595,-0.451"]
"Loneliness BL" [pos="-0.789,-1.024"]
"MLTC BL" [pos="-0.565,1.503"]
"Partnership Status BL" [exposure,pos="-1.118,-0.305"]
"Wealth BL" [pos="-1.146,1.437"]
"Education (lv)" [pos="-1.02,1.600"]
Age [pos="-0.864,1.539"]
Sex [pos="-0.712,1.522"]
"Depression BL" -> "Initial Health State"
"Loneliness BL" -> "Initial Health State"
"MLTC BL" -> "Initial Health State"
"MLTC BL" -> "Partnership Status BL"
"Partnership Status BL" -> "Initial Health State"
"Wealth BL" -> "Initial Health State"
"Wealth BL" -> "Partnership Status BL"
"Education (lv)" -> "Partnership Status BL"
"Education (lv)" -> "Initial Health State"
Age -> "Initial Health State"
Age -> "Partnership Status BL"
Sex -> "Initial Health State"
Sex -> "Partnership Status BL"
}
'

################################################################################
######## DAGS: STATE TRANS PROBABILITY
################################################################################


trans_prob_DAG<-'dag {
"Health State Transitions" [outcome,pos="-0.300,-0.249"]
"Partnership Status" [exposure,pos="-1.063,-0.647"]
Age [pos="-0.542,1.466"]
Depression [pos="-0.550,-1.241"]
Loneliness [pos="-0.575,-0.913"]
MLTC [pos="-0.295,1.519"]
Sex [pos="-0.804,1.513"]
Wealth [pos="-1.067,1.450"]
"Education (lv)" [pos="-.930,1.600"]
"Partnership Status" -> "Health State Transitions"
"Partnership Status" -> Depression
"Partnership Status" -> Loneliness
Age -> "Health State Transitions"
Age -> "Partnership Status"
Depression -> "Health State Transitions"
"Education (lv)" -> "Health State Transitions"
"Education (lv)" -> "Partnership Status"
Loneliness -> "Health State Transitions"
MLTC -> "Health State Transitions"
MLTC -> "Partnership Status"
Sex -> "Health State Transitions"
Sex -> "Partnership Status"
Wealth -> "Health State Transitions"
Wealth -> "Partnership Status"
}



'
