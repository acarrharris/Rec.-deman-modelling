
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
install.packages("devtools")
install.packages("plyr")

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
library(plyr)

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


state_output = data.frame()
state_cal_output = data.frame()

for (x in 1:5){
##########
# Estimate the catch-per-trip copulas so we don't re-estimate every time
source("calc_catch_per_trip_copulas.R")
##########



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


# calibration_output_by_period = as.data.frame(bind_rows(pds_new_all_MA, pds_new_all_RI, pds_new_all_CT,
#                                                        pds_new_all_NY, pds_new_all_NJ, pds_new_all_DE,
#                                                        pds_new_all_MD, pds_new_all_VA))


calibration_output_by_period[is.na(calibration_output_by_period)] = 0
write_xlsx(calibration_output_by_period,"calibration_output_by_period.xlsx")



aggregate_calibration_output= subset(calibration_output_by_period, select=-c(state, alt_regs, period))
aggregate_calibration_output = aggregate(aggregate_calibration_output, by=list(calibration_output_by_period$sim),FUN=sum, na.rm=TRUE)
write_xlsx(aggregate_calibration_output,"aggregate_calibration_output.xlsx")

calibration_output_by_period$draw = x
state_cal_output =rbind.fill(state_cal_output, calibration_output_by_period)
# 
# }
# #write_xlsx(state_output,"state_output_nostop_2017test.xlsx")
# 
# write_xlsx(state_cal_output,"state_cal_output_all.xlsx")


#Apply the calibration estimates of total catch to the catch-at-length distribution used in the assessment 
# tot_sf_keep = aggregate_calibration_output$tot_keep
# tot_sf_rel = aggregate_calibration_output$tot_rel
# tot_sf_catch = tot_sf_keep+tot_sf_rel
# 
# assment_CAL = data.frame(read_excel("assessment_catch_at_length.xlsx"))                                                                            
# assment_CAL$calibration_keep_at_length=assment_CAL$ab1_prop*tot_sf_keep
# assment_CAL$calibration_release_at_length=assment_CAL$b2_prop*tot_sf_rel
# 
# calibration_catch_at_length= subset(assment_CAL, select=c(l_in_bin, calibration_keep_at_length, calibration_release_at_length))
# write_xlsx(calibration_catch_at_length,"calibration_catch_at_length.xlsx")

##########  

# regs <- c("plus1", "minus1", "minus2", "plus1_bag2", "minus1_bag2", "minus2_bag2")
# regs <- c("minus1")

#for (r in regs){
#  regulation=r

# 
# state_output = data.frame()
# for (x in 1:20){
    regulation="2019_test"
  
  ##########  
  # Input new population numbers-at-age distribution (numbers_at_age_YYYY) in the following script to create population adjusted 
  # catch-at-length and catch-per-trip for summer flounder
  #source("CAL given stock structure - assessment coastwide - prediction.R")
  #source("catch at length given stock structure - prediction.R")
  
  # THIS IS WHERE TO IMPORT THE NUMBERS AT AGE FROM THE OPERATING MODEL
  #2016 numbers (from stock assessment document)
  #numbers_at_age = data.frame(read_excel("numbers_at_age_2016.xlsx"))
  #numbers_at_age$Na=numbers_at_age$Na*1000
  
  #2017 numbers (from stock assessment document)
  #numbers_at_age = data.frame(read_excel("numbers_at_age_2017.xlsx"))
  #numbers_at_age$Na=numbers_at_age$Na*1000
  
  #2018 numbers (median)
  #numbers_at_age = data.frame(read_excel("numbers_at_age_2018.xlsx"))
  #numbers_at_age$Na=numbers_at_age$Na*1000
  
  #2019 numbers (median)
   numbers_at_age = data.frame(read_excel("numbers_at_age_2019.xlsx"))
   numbers_at_age$Na=numbers_at_age$Na*1000
  
  #2022 numbers (median)
  # numbers_at_age = data.frame(read_excel("F2021_2019_ALLPROJ_2022_STOCKN_median.xlsx"))
  
  #2022 numbers (draw from a sample of 100) - use this to incorporate uncertainty 
  # numbers_at_age = data.frame(read_excel("F2021_2019_ALLPROJ_2022_STOCKN_sample100.xlsx"))
  # numbers_at_age = subset(numbers_at_age, numbers_at_age$draw==x)
  

  
  source("CAL given stock structure by state.R")
  
  ##########  
  
  
  
  ##########  
  # run the simulation code under the new set of regulations (regulation file is directed_trips_region - alternative regs test.xlsx)
  
  #directed_trip_alt_regs=data.frame(read_excel(paste0("directed_trips_regions_bimonthly_HCR_",regulation,".xlsx")))
  #directed_trip_alt_regs=data.frame(read_excel("directed_trips_regions_bimonthly.xlsx"))
  #directed_trip_alt_regs=data.frame(read_excel("directed_trips_regions_bimonthly_19_16.xlsx"))
  directed_trip_alt_regs=data.frame(read_excel("directed_trips_regions_bimonthly_test.xlsx"))
   
  directed_trip_alt_regs$dtrip_2019=round(directed_trip_alt_regs$dtrip_2019)


  source("prediction3 MA.R")
  state1
  source("prediction3 RI.R")
  state1
  source("prediction3 CT.R")
  state1
  source("prediction3 NY.R")
  state1
  source("prediction3 NJ.R")
  state1
  source("prediction3 DE.R")
  state1
  source("prediction3 MD.R")
  state1
  source("prediction3 VA.R")
  state1
  source("prediction3 NC.R")
  

  prediction_output_by_period = as.data.frame(bind_rows(pds_new_all_MA, pds_new_all_RI, pds_new_all_CT,
                                                        pds_new_all_NY, pds_new_all_NJ, pds_new_all_DE,
                                                        pds_new_all_MD, pds_new_all_VA, pds_new_all_NC))
  # prediction_output_by_period = as.data.frame(bind_rows(pds_new_all_MA, pds_new_all_RI, pds_new_all_CT,
  #                                                       pds_new_all_NY, pds_new_all_NJ, pds_new_all_DE,
  #                                                       pds_new_all_MD, pds_new_all_VA))
  
  prediction_output_by_period[is.na(prediction_output_by_period)] = 0
  write_xlsx(prediction_output_by_period,"prediction_output_by_period.xlsx")
  

  
  # state_prediction_output= subset(prediction_output_by_period, select=c(tot_keep, tot_rel,tot_keep_bsb, tot_rel_bsb,tot_keep_scup, tot_rel_scup,
  #                                                                       tot_keep_wf, tot_rel_wf, tot_keep_rd, tot_rel_rd, observed_trips,
  #                                                                       n_choice_occasions, period, state, change_CS))
  state_prediction_output=prediction_output_by_period

  
  state_prediction_output$state1=with(state_prediction_output, match(state, unique(state)))
  state_prediction_output1= subset(state_prediction_output, select=-c(state, period))
  state_prediction_output1=aggregate(state_prediction_output1, by=list(state_prediction_output1$state1),FUN=sum, na.rm=TRUE)
  state_prediction_output1= subset(state_prediction_output1, select=-c(state1))
  names(state_prediction_output1)[names(state_prediction_output1) == "Group.1"] = "state1"
  
  
  state_names=subset(state_prediction_output, select=c(state, state1))
  state_names = state_names[!duplicated(state_names), ]
  state_prediction_output1 =  merge(state_prediction_output1,state_names,by="state1", all.x=TRUE, all.y=TRUE)
  
  state_prediction_output1$draw = x
  
  
  state_output =rbind.fill(state_output, state_prediction_output1)
  state_output$reg=regulation

}
#write_xlsx(state_output,"state_output_nostop_2017test.xlsx")

write_xlsx(state_output,"state_pred_output_all.xlsx")
write_xlsx(state_cal_output,"state_cal_output_all.xlsx")

#}


#aggregate_prediction_output= subset(prediction_output_by_period, select=-c(state, alt_regs, period))
#aggregate_prediction_output = aggregate(aggregate_prediction_output, by=list(aggregate_prediction_output$sim),FUN=sum, na.rm=TRUE)
#write_xlsx(aggregate_prediction_output,"aggregate_prediction_output.xlsx")

##########  


# Stop the clock
proc.time() - ptm



###
# Calculate ouput statisitics for calibration and prediction year
#source("simulation output stats.R")


