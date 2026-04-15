################################################################################
# chronic_pain_var.R
#
# PURPOSE: Diagnostic classifier used EXCLUSIVELY in the IRT measurement
#          invariance pipeline (Measurement_invariance_IRT.Rmd).
#
# THIS IS NOT the chronic pain classifier used in the main analysis.
# The analysis classifier is `painchr_ext`, derived in:
#   suppl_pt1_Data_prep.Rmd (lines ~726–760)
# using `create_pain_locations_count()` from Sources/Paper1_func.R.
# `painchr_ext` is a 4-category extended classifier (No pain / Acute /
# Chronic / Chronic widespread 3+ locations).
#
# ROLE IN MEASUREMENT INVARIANCE:
# `create_chrpain_variable()` produces a binary chronic pain flag (rXchrpain)
# across ELSA waves 1–9 using a two-consecutive-wave criterion. This flag is
# used to stratify participants into chronic vs. non-chronic pain groups so
# that the 2PL IRT model of pain impact items can be tested for measurement
# invariance across these groups and across waves. The goal is to verify that
# the IRT-derived pain impact scores (impactEstimates2-8_partialScalar.rds)
# are on a common metric — i.e. that the items measure the same construct
# regardless of chronic pain status or wave.
#
# The `chronic_pain_var()` function (3-category single-wave classifier below)
# is defined here but is NOT called by Measurement_invariance_IRT.Rmd.
################################################################################

# Define a function to create the chrpain variables for a specific wave
create_chrpain_variable <- function(data, wave) {
  if(wave == 1){
    data[[paste0("r1chrpain")]]<-ifelse(data[[paste0("r1painlv")]] < 0,
                                        # report the specific missing data code
                                        data[[paste0("r1painlv")]],
                                        0)
  }else{
  prev_wave<-wave-1
  var_name <- paste0("r", wave, "chrpain")
  # check respondent is in both current and previus wave
  data[[var_name]] <- ifelse(data[[paste0("inw",prev_wave)]] & data[[paste0("inw",wave)]],
                                  # check respondent has no missing data at current wave 
                                  ifelse(data[[paste0("r", wave, "painlv")]] < 0, 
                                         # report the specific missing data code
                                         data[[paste0("r", wave, "painlv")]],
                                         # check respondent has no missing data at previus wave
                                         ifelse(data[[paste0("r", prev_wave, "painlv")]] < 0, 
                                                # report the specific missing data code
                                                data[[paste0("r", prev_wave, "painlv")]],
                                                # when data is not missing assign chronic pain code
                                                ifelse(data[[paste0("r", wave, "painlv")]] >= 2 & data[[paste0("r", prev_wave, "painlv")]] >= 1,
                                                       1, 
                                                       0
                                                )
                                         )
                                  ),
                                  # not measured in in bothw consecutive waves
                                  -99 
  )
  # If respondent was categorised as chronic this wave (two consecutive waves), 
  # then categorise this respondent as chronic in the previous wave too (this is the onset)
  prevW_var_name<- paste0("r", prev_wave, "chrpain")
  
  # index all instances of chronic pain at current wave
  indices <- data[[var_name]] == 1
#  
  data[[prevW_var_name]][indices] <- 1
  
  }
  
  return(data)
}
###########################################################
### e.g. for many waves you can use something like this:
##########################################################

## Create chrpain variables for waves 2 to 9

#waves <- 2:9
#for (wave in waves) {
#  H_elsa_w1_9_merged <- create_chrpain_variable(H_elsa_w1_9_merged, wave)
#}

## Create chrpain variable for wave 1

#H_elsa_w1_9_merged$r1chrpain<-ifelse((H_elsa_w1_9_merged$inw1 & H_elsa_w1_9_merged$inw2),
#                                     ifelse(H_elsa_w1_9_merged$r1painlv >=1 & H_elsa_w1_9_merged$r2chrpain == 1,
#                                            1,
#                                            0),
#                                     -99 # not in bothw waves
#)


