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


