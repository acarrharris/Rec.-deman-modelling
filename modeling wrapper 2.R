
# This is the modeling wrapper 

# Steps in the process


  #1) simulate the fishery for the basleline year (2019) under actual regulations/fishing conditions
      # Requires:
        # a) catch-per-trip distribution. These are based on trips that caught or targeted summer flounder. 
        #    I use 2019 data to create fitted dist'ns for scup, WF, RD. Catch-per-trip distributions for 
        #     SF and BSB are based on copula modeling.
        # b) Sets of utility parameters from each of the four surveys
        # c) Distributions of trip costs derived from the 2017 expenditure survey
        # d) a file containing regulations for each state and species. There are 24 bi-monthly periods for 
        #    for each state. Some periods differ in  regualtions across species. 
      
        # After running the calibration, retain keep- and release-at-length for summer flounder (#'s), total keep and release 
        # for other species, and number of choice occasions for each state/period. Note that for these outputs, 
        # there will be X estimates based on X draws of utility parameters. 

  #2) Simulate the fishery under alternative regulations and a new catch-at-length distribution for summer flounder
        # a) Create new catch-at-length/catch-per-trip distributions for summer flounder based on population numbers at length. 
        # a) Calcualte angler welfare/fishing effort changes and changes in catch



# Modeling wrapper test
install.packages("readxl")
install.packages("tidyr")
install.packages("reshape2")
install.packages("splitstackshape")
install.packages("doBy")
install.packages("WriteXLS")
install.packages("Writexl")
install.packages('Rcpp')
install.packages("ggplot2")
install.packages("ggplot")
install.packages("dplyr")
install.packages("rlist")

install.packages("fitdistrplus")
install.packages("MASS")
install.packages("psych")
install.packages("rgl")
install.packages("copula")
install.packages("VineCopula")

install.packages("scales")
install.packages("univariateML")
install.packages("xlsx")
install.packages("writexl")
install.packages("logspline")
install.packages("xtable")

library(psych)
library(rgl)
library(copula)
library(VineCopula)
library(readxl)
library(scales)
library(univariateML)
library(xlsx)
library(fitdistrplus)
library(logspline)

library(Rcpp)
library(writexl)
library(readxl)
library(dplyr)
library(tidyr)
library(reshape2)
library(data.table)
library(splitstackshape)
library(doBy)
library(WriteXLS)
library(rlist)
library(xtable)
library(MASS)
library(stats)
# Set the wd to wherever all the code/data is 

#setwd("C:/Users/andrew.carr-harris/Dropbox/NMFS/fluke_mse/simulation_R_code/")
#setwd("C:/Users/Lou/Dropbox/NMFS/fluke_mse/simulation_R_code/")

# Start the clock!
ptm <- proc.time()


########## 
# 1) Run the calibration files

source("calibration4 MA.R")
source("calibration4 RI.R")
source("calibration4 CT.R")
source("calibration4 NY.R")
source("calibration4 NJ.R")
source("calibration4 DE.R")
source("calibration4 MD.R")
source("calibration4 VA.R")
source("calibration4 NC.R")
  

# Combine the results  
calibration_output_by_period = as.data.frame(bind_rows(pds_new_all_MA, pds_new_all_RI, pds_new_all_CT,
                                                       pds_new_all_NY, pds_new_all_NJ, pds_new_all_DE,
                                                       pds_new_all_MD, pds_new_all_VA, pds_new_all_NC))

#calibration_output_by_period = as.data.frame(bind_rows(pds_new_all_MA))
calibration_output_by_period[is.na(calibration_output_by_period)] = 0
write_xlsx(calibration_output_by_period,"calibration_output_by_period.xlsx")

#Calibration by state
aggregate(observed_trips ~ state, calibration_output_by_period, sum)
aggregate(tot_keep ~ state, calibration_output_by_period, sum)
aggregate(n_choice_occasions ~ state, calibration_output_by_period, sum)


aggregate_calibration_output= subset(calibration_output_by_period, select=-c(state, alt_regs, period))
aggregate_calibration_output = aggregate(aggregate_calibration_output, by=list(calibration_output_by_period$sim),FUN=sum, na.rm=TRUE)
write_xlsx(aggregate_calibration_output,"aggregate_calibration_output.xlsx")


#Apply the calibration estimates of total catch to the catch-at-length distribution used in the assessment 
tot_sf_keep = aggregate_calibration_output$tot_keep
tot_sf_rel = aggregate_calibration_output$tot_rel
tot_sf_catch = tot_sf_keep+tot_sf_rel

assment_CAL = data.frame(read_excel("assessment_catch_at_length.xlsx"))                                                                            
assment_CAL$calibration_keep_at_length=assment_CAL$ab1_prop*tot_sf_keep
assment_CAL$calibration_release_at_length=assment_CAL$b2_prop*tot_sf_rel
calibration_catch_at_length= subset(assment_CAL, select=c(l_in_bin, calibration_keep_at_length, calibration_release_at_length))
calibration_catch_at_length$tot_catch= calibration_catch_at_length$calibration_keep_at_length+calibration_catch_at_length$calibration_release_at_length




##########  




##########
# Estimate the catch-per-trip copulas so we don't re-estimate every time
source("calc_catch_per_trip_copulas.R")
##########



ptm <- proc.time()

state_output = data.frame()
for (x in 1:30){

##########  
# Input new population numbers-at-age distribution (numbers_at_age_YYYY) in the following script to create population adjusted 
# catch-at-length and catch-per-trip for summer flounder
source("catch at length given stock structure - prediction.R")
##########  



##########  
# run the simulation code under the new set of regulations (regulatiopn file is directed_trips_region - alternative regs test.xlsx)


source("prediction3 MA.R")
source("prediction3 RI.R")
source("prediction3 CT.R")
source("prediction3 NY.R")
source("prediction3 NJ.R")
source("prediction3 DE.R")
source("prediction3 MD.R")
source("prediction3 VA.R")
source("prediction3 NC.R")


prediction_output_by_period = as.data.frame(bind_rows(pds_new_all_MA, pds_new_all_RI, pds_new_all_CT,
                                                      pds_new_all_NY, pds_new_all_NJ, pds_new_all_DE,
                                                      pds_new_all_MD, pds_new_all_VA, pds_new_all_NC))

prediction_output_by_period[is.na(prediction_output_by_period)] = 0


#prediction by state
aggregate(observed_trips ~ state, prediction_output_by_period, sum)
aggregate(tot_keep ~ state, prediction_output_by_period, sum)
aggregate(n_choice_occasions ~ state, prediction_output_by_period, sum)
aggregate(observed_trips ~ state, prediction_output_by_period, sum)
aggregate(observed_trips ~ state, calibration_output_by_period, sum)



state_prediction_output= subset(prediction_output_by_period, select=c(tot_keep, tot_rel,tot_keep_bsb, tot_rel_bsb,tot_keep_scup, tot_rel_scup,
                                                                      tot_keep_wf, tot_rel_wf, tot_keep_rd, tot_rel_rd, observed_trips,
                                                                      n_choice_occasions, period, state, change_CS ))


state_prediction_output$state1=with(state_prediction_output, match(state, unique(state)))
state_prediction_output1= subset(state_prediction_output, select=-c(state, period))
state_prediction_output1=aggregate(state_prediction_output1, by=list(state_prediction_output1$state1),FUN=sum, na.rm=TRUE)
state_prediction_output1= subset(state_prediction_output1, select=-c(state1))
names(state_prediction_output1)[names(state_prediction_output1) == "Group.1"] = "state1"


state_names=subset(state_prediction_output, select=c(state, state1))
state_names = state_names[!duplicated(state_names), ]
state_prediction_output1 =  merge(state_prediction_output1,state_names,by="state1", all.x=TRUE, all.y=TRUE)

state_prediction_output1$draw = x


state_output =rbind(state_output, state_prediction_output1)

}

write_xlsx(state_output,"state_output_sim30.xlsx")
regional_output =subset(state_output, select=-c(state))
regional_output=aggregate(regional_output, by=list(regional_output$draw),FUN=sum, na.rm=TRUE)
regional_output =subset(regional_output, select=-c(state1, draw))
names(regional_output)[names(regional_output) == "Group.1"] = "draw"
names(regional_output)[names(regional_output) == "tot_keep"] = "tot_keep_sf"
names(regional_output)[names(regional_output) == "tot_rel"] = "tot_rel_sf"
write_xlsx(regional_output,"regional_output_sim30.xlsx")



#aggregate_prediction_output= subset(prediction_output_by_period, select=-c(state, alt_regs, period))
#aggregate_prediction_output = aggregate(aggregate_prediction_output, by=list(aggregate_prediction_output$sim),FUN=sum, na.rm=TRUE)
#write_xlsx(aggregate_prediction_output,"aggregate_prediction_output.xlsx")

##########  


# Stop the clock
proc.time() - ptm



###
# Calculate ouput statisitics for calibration and prediction year
source("simulation output stats.R")


