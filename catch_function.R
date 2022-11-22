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
library(conflicted)


pkgs_to_use <- c("tidyr",
                 "magrittr",
                 "reshape2",
                 "splitstackshape",
                 "doBy",
                 "WriteXLS",
                 'Rcpp',
                 "ggplot2",
                 "dplyr",
                 "rlist",
                 "fitdistrplus",
                 "MASS",
                 "psych",
                 "rgl",
                 "copula",
                 "VineCopula",
                 "scales",
                 "univariateML",
                 "logspline",
                 "readr",
                 "data.table",
                 "conflicted", "readxl", "writexl")
#install.packages(setdiff(pkgs_to_use, rownames(installed.packages())))  
lapply(pkgs_to_use, library, character.only = TRUE, quietly = TRUE)


conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("summarise", "dplyr")
conflict_prefer("count", "dplyr")

costs_new_all_MA<-readRDS("costs_new_all_MA.rds")
costs_new_all_MA$state<-"MA"

costs_new_all_RI<-readRDS("costs_new_all_RI.rds")
costs_new_all_RI$state<-"RI"

costs_new_all_CT<-readRDS("costs_new_all_CT.rds")
costs_new_all_CT$state<-"CT"

costs_new_all_NY<-readRDS("costs_new_all_NY.rds")
costs_new_all_NY$state<-"NY"

costs_new_all_NJ<-readRDS("costs_new_all_NJ.rds")
costs_new_all_NJ$state<-"NJ"

costs_new_all_DE<-readRDS("costs_new_all_DE.rds")
costs_new_all_DE$state<-"DE"

costs_new_all_MD<-readRDS("costs_new_all_MD.rds")
costs_new_all_MD$state<-"MD"

costs_new_all_VA<-readRDS("costs_new_all_VA.rds")
costs_new_all_VA$state<-"VA"

costs_new_all_NC<-readRDS("costs_new_all_NC.rds")
costs_new_all_NC$state<-"NC"


costs_all = dplyr::bind_rows(costs_new_all_MA, costs_new_all_RI,costs_new_all_CT,costs_new_all_NY,costs_new_all_NJ,
                             costs_new_all_DE, costs_new_all_MD, costs_new_all_VA, costs_new_all_NC)
costs_all_base <- split(costs_all, costs_all$state)

# Input the calibration output which contains the number of choice occasions needed to simulate
calibration_data_table <- readRDS("calibration_output_by_period.rds")
calibration_data_table_base <- split(calibration_data_table, calibration_data_table$state)

##Regulations file 
directed_trips_table <- readRDS("directed_trips_regions_bimonthly_test.rds")
directed_trips_table_base <- split(directed_trips_table, directed_trips_table$state)

##Sizes 
size_data_read <- readRDS("sf_fitted_sizes_y2plus.rds") %>% tibble()
size_data_read_base <- split(size_data_read, size_data_read$region)

#Predicted catches

#catch data
sf_catch_data_ma <- readRDS("predicted_catch_MA.rds") %>%
  tibble() %>%
  rename(tot_sf_catch = sf_t_nb,
         tot_bsb_catch = bsb_t_nb) %>%
  I()
sf_catch_data_ri <- readRDS("predicted_catch_RI.rds") %>%
  tibble() %>%
  rename(tot_sf_catch = sf_t_nb,
         tot_bsb_catch = bsb_t_nb) %>%
  I()
sf_catch_data_ct <- readRDS("predicted_catch_CT.rds") %>%
  tibble() %>%
  rename(tot_sf_catch = sf_t_nb,
         tot_bsb_catch = bsb_t_nb) %>%
  I()

sf_catch_data_ny <- readRDS("predicted_catch_NY.rds") %>%
  tibble() %>%
  rename(tot_sf_catch = sf_t_nb,
         tot_bsb_catch = bsb_t_nb) %>%
  I()
sf_catch_data_nj <- readRDS("predicted_catch_NJ.rds") %>%
  tibble() %>%
  rename(tot_sf_catch = sf_t_nb,
         tot_bsb_catch = bsb_t_nb) %>%
  I()
sf_catch_data_de <- readRDS("predicted_catch_DE.rds") %>%
  tibble() %>%
  rename(tot_sf_catch = sf_t_nb,
         tot_bsb_catch = bsb_t_nb) %>%
  I()

sf_catch_data_md <- readRDS("predicted_catch_MD.rds") %>%
  tibble() %>%
  rename(tot_sf_catch = sf_t_nb,
         tot_bsb_catch = bsb_t_nb) %>%
  I()
sf_catch_data_va <- readRDS("predicted_catch_VA.rds") %>%
  tibble() %>%
  rename(tot_sf_catch = sf_t_nb,
         tot_bsb_catch = bsb_t_nb) %>%
  I()

sf_catch_data_nc <- readRDS("predicted_catch_NC.rds") %>%
  tibble() %>%
  rename(tot_sf_catch = sf_t_nb,
         tot_bsb_catch = bsb_t_nb) %>%
  I()


#Predicted catches
sf_catch_data_all = dplyr::bind_rows(sf_catch_data_ma, sf_catch_data_ri,sf_catch_data_ct,sf_catch_data_ny,sf_catch_data_nj,
                                     sf_catch_data_de, sf_catch_data_md, sf_catch_data_va, sf_catch_data_nc)

sf_catch_data_all <- split(sf_catch_data_all, sf_catch_data_all$region)

sf_catch_data_all = dplyr::bind_rows(sf_catch_data_ma, sf_catch_data_ri,sf_catch_data_ct,sf_catch_data_ny,sf_catch_data_nj,
                                     sf_catch_data_de, sf_catch_data_md, sf_catch_data_va, sf_catch_data_nc)

sf_catch_data_all_base <- split(sf_catch_data_all, sf_catch_data_all$region)

#Parameters
source("gen_params.R")

# 
predict_rec_catch <- function(state1,
                              region1,
                              calibration_data_table,
                              directed_trips_table,
                              size_data_read,
                              param_draws_MA,
                              costs_new_all_MA,
                              sf_catch_data_all,
                              prop_bsb_keep,
                              dchoose,
                              mgmt_scen){

# state1 <- "CT"
# region1 <- "NO"
# calibration_data_table <- calibration_data_table[[1]]
# directed_trips_table <- directed_trips_table[[1]]
# size_data_read <- size_data_read[[1]]
# param_draws_MA <- param_draws_all[[1]]
# costs_new_all_MA <- costs_all[[1]]
# sf_catch_data_all <- sf_catch_data_all[[1]]
# prop_bsb_keep <- 1-0.53
# dchoose <- 1
# mgmt_scen <- 1

# Input the calibration output which contains the number of choice occasions needed to simulate
#calibration_data = data.frame(read_excel("calibration_output_by_period.xlsx"))
#calibration_data = subset(calibration_data, state == state1, select=c(period, sim, state, n_choice_occasions))

calibration_data <- calibration_data_table %>% tibble() %>% dplyr::filter(state == state1) #%>% 
directed_trips <- directed_trips_table %>% tibble() %>% dplyr::filter(state == state1)
size_data <- size_data_read %>% filter(region == state1)


# Input the data set containing alterntative regulations and directed trips
directed_trips$dtrip <- round(directed_trips$dtrip_2019)
#directed_trips <- subset(directed_trips, state == state1)

min_period <- min(directed_trips$period)
max_period <- max(directed_trips$period)


######################################
##   Begin simulating trip outcomes ##
######################################

# Set up an output file for the separately simulated within-season regulatory periods  
directed_trips_p <- directed_trips %>% #subset(directed_trips, period == p)
  mutate(period = as.character(period)) %>% 
  #group_by(period) %>% 
  mutate(#n_trips = floor(mean(dtrip_2019)),
    n_trips = floor(dtrip_2019),
    n_draws = floor(min(1000,n_trips*2.5)))# %>% 
#ungroup()
#n_draws = floor(min(30000,n_trips*2.5)))
nsamp = 10
niter <- nsamp*sum(directed_trips_p$n_draws)

period_vec <- directed_trips_p %>% 
  dplyr::select(period, n_draws) %>% 
  uncount(n_draws)

regs <- directed_trips_p %>% 
  dplyr::select(period, fluke_bag, fluke_min, fluke_max,
                bsb_bag,
                bsb_min,
                scup_bag,
                scup_min,
                wf_bag,
                wf_min,
                rd_bag,
                rd_min,
                rd_max)
  
  
sf_catch_data <- sf_catch_data_all %>% 
  slice_sample(n = niter, replace = TRUE) %>%
  mutate(period = rep(period_vec$period, each = nsamp),
         catch_draw = rep(1:nsamp, length.out = niter),
         tripid = rep(unlist(purrr::map(directed_trips_p$n_draws, seq_len)), each = nsamp)) #%>% 
#tibble::rowid_to_column("tripid") %>%
#I()

# subset trips with zero catch, as no size draws are required
sf_zero_catch <- filter(sf_catch_data, tot_sf_catch == 0)

#remove trips with zero summer flounder catch
#sf_catch_data=sf_catch_data[sf_catch_data$tot_sf_catch!=0, ]
sf_catch_data <- filter(sf_catch_data, tot_sf_catch > 0) 

#expand the sf_catch_data so that each row represents a fish
row_inds <- seq_len(nrow(sf_catch_data))
sf_catch_data <- sf_catch_data[c(rep(row_inds, sf_catch_data$tot_sf_catch)), ]
rownames(sf_catch_data) <- NULL
sf_catch_data$fishid <- 1:nrow(sf_catch_data)


# generate lengths for each fish
catch_size_data <- sf_catch_data %>% 
  mutate(fitted_length = sample(size_data$fitted_length,
                                nrow(.),
                                prob = size_data$fitted_prob,
                                replace = TRUE)) #%>%
##I()

# Impose regulations, calculate keep and release per trip
# For summer flounder, retain keep- and release-at-length

if (state1 !="NJ") {
  
  catch_size_data <- catch_size_data %>% 
    left_join(regs, by = "period") %>% 
    mutate(posskeep = ifelse(fitted_length>=fluke_min & fitted_length<=fluke_max,1,0)) %>% 
    group_by(period, tripid) %>% 
    # keep = case_when(
    # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
    # TRUE ~ 0),
    mutate(csum_keep = cumsum(posskeep)) %>% 
    ungroup() %>% 
    mutate(
      keep_adj = case_when(
        fluke_bag > 0 ~ ifelse(csum_keep<=fluke_bag & posskeep==1,1,0),
        TRUE ~ 0),
      # keep_adj = case_when(
      #   csum_keep<=bag & keep==1 ~ 1,
      #   TRUE ~ 0),
      release = case_when(
        fluke_bag > 0 ~ ifelse(posskeep==0 | (posskeep==1 & csum_keep>fluke_bag ), 1,0)))
  
  
  catch_size_data<- subset(catch_size_data, select=c(fishid, fitted_length, tripid, keep_adj, release, period, catch_draw)) %>% 
    rename(keep = keep_adj)
  
}

# if (state1 =="NJ"){
#   
#   catch_size_data <- catch_size_data %>% 
#     left_join(regs, by = "period") %>% 
#     mutate(posskeep = ifelse(fitted_length>=fluke_min1 & fitted_length<fluke_max1,1,0)) %>% 
#     group_by(tripid, period) %>% 
#     # keep = case_when(
#     # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
#     # TRUE ~ 0),
#     mutate(csum_keep = cumsum(posskeep)) %>% 
#     ungroup() %>% 
#     mutate(
#       keep_adj = case_when(
#         fluke_bag1 > 0 ~ ifelse(csum_keep<=fluke_bag1 & posskeep==1,1,0),
#         TRUE ~ 0)) %>%
#     
#     mutate(posskeep2 = ifelse(fitted_length>=fluke_min2 & fitted_length<fluke_max2,1,0)) %>% 
#     group_by(tripid) %>% 
#     # keep = case_when(
#     # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
#     # TRUE ~ 0),
#     mutate(csum_keep2 = cumsum(posskeep2)) %>% 
#     ungroup() %>% 
#     mutate(
#       keep_adj2 = case_when(
#         fluke_bag2 > 0 ~ ifelse(csum_keep2<=fluke_bag2 & posskeep2==1,1,0)))
#   
#   catch_size_data[is.na(catch_size_data)] <- 0
#   
#   catch_size_data$release<-ifelse((catch_size_data$keep_adj==0 & catch_size_data$keep_adj2==0), 1,0)
#   catch_size_data$keep_tot<-catch_size_data$keep_adj+catch_size_data$keep_adj2
#   
#   
#   catch_size_data<- subset(catch_size_data, select=c(fishid, fitted_length, tripid, keep_tot, release, period, catch_draw)) %>% 
#     rename(keep = keep_tot)
#   
#   
#   new_size_data <- catch_size_data %>% 
#     group_by( tripid, fitted_length) %>% 
#     summarize(keep = sum(keep),
#               release = sum(release), .groups = "drop") #%>% 
#   
# }



# system.time({
new_size_data <- catch_size_data %>%
  group_by(period, catch_draw, tripid, fitted_length) %>%
  summarize(keep = sum(keep),
            release = sum(release), .groups = "drop") #%>%

summed_catch_data <- catch_size_data %>%
  group_by(period, catch_draw, tripid) %>%
  summarize(tot_keep = sum(keep),
            tot_rel = sum(release),
            .groups = "drop") #%>%

keep_size_data <- new_size_data %>%
  #ungroup() %>%
  dplyr::select(-release) %>% 
  pivot_wider(names_from = fitted_length, #_length,
              names_glue = "keep_length_{fitted_length}",
              names_sort = TRUE,
              values_from = keep, 
              values_fill = 0) # %>% 
#I()
#keep_size_data

release_size_data <- new_size_data %>%
  #ungroup() %>% 
  dplyr::select(-keep) %>% 
  pivot_wider(names_from = fitted_length, #_length,
              names_glue = "release_length_{fitted_length}",
              names_sort = TRUE,
              values_from = release, 
              values_fill = 0) #%>% 


trip_data <- summed_catch_data %>% 
  left_join(keep_size_data, by = c("period", "catch_draw","tripid")) %>% 
  left_join(release_size_data, by = c("period", "catch_draw","tripid")) #%>% 

#add the zero catch trips 
trip_data <- bind_rows(trip_data, sf_zero_catch) %>% 
  #arrange(period, catch_draw, tripid) %>% 
  mutate_if(is.numeric, replace_na, replace = 0) %>% 
  mutate(region = state1,
         tot_sf_catch = tot_keep + tot_rel)  



# merge catch information for other species. Assume per-trip catch outcomes for these species are the same as the calibration. 
# This info is contained in the costs_new_all_state datasets
if (region1 == "NO") {
  #bsb_sc_data <- subset(costs_new_all_MA, catch_draw<=nsamp, select=c(period, catch_draw, tripid,tot_keep_scup_base, tot_rel_scup_base)) %>%
  #  tibble()
  bsb_sc_data <- costs_new_all_MA %>% #tibble() %>% 
    filter(catch_draw<=nsamp) %>% 
    select(period, catch_draw, tripid,tot_keep_scup_base, tot_rel_scup_base) %>%
    #tibble()
    rename(tot_keep_scup = tot_keep_scup_base,
           tot_rel_scup = tot_rel_scup_base)
  #names(bsb_sc_data)[names(bsb_sc_data) == "tot_keep_scup_base"] <- "tot_keep_scup"
  #names(bsb_sc_data)[names(bsb_sc_data) == "tot_rel_scup_base"] <- "tot_rel_scup"
}

if (region1 == "NJ") {
  # bsb_sc_data <- subset(costs_new_all_MA, catch_draw<=nsamp, select=c(period, catch_draw,tripid,tot_keep_scup_base, tot_rel_scup_base,
  #                                                                  tot_keep_wf_base, tot_rel_wf_base)) %>%
  #   tibble()
  bsb_sc_data <- costs_new_all_MA %>% 
    #tibble() %>% 
    filter(catch_draw<=nsamp) %>% 
    select(period, catch_draw,tripid,tot_keep_scup_base, tot_rel_scup_base,
           tot_keep_wf_base, tot_rel_wf_base) %>%
    rename(tot_keep_scup = tot_keep_scup_base,
           tot_rel_scup = tot_rel_scup_base,
           tot_keep_wf = tot_keep_wf_base,
           tot_rel_wf = tot_rel_wf_base)
  # names(bsb_sc_data)[names(bsb_sc_data) == "tot_keep_wf_base"] <- "tot_keep_wf"
  # names(bsb_sc_data)[names(bsb_sc_data) == "tot_rel_wf_base"] <- "tot_rel_wf"
  # names(bsb_sc_data)[names(bsb_sc_data) == "tot_keep_scup_base"] <- "tot_keep_scup"
  # names(bsb_sc_data)[names(bsb_sc_data) == "tot_rel_scup_base"] <- "tot_rel_scup"
}

if (state1 %in% c("DE","MD")) {
  # bsb_sc_data <- subset(costs_new_all_MA, catch_draw<=nsamp, select=c(period, catch_draw,tripid,tot_keep_wf_base, tot_rel_wf_base)) %>%
  # 
  #    tibble()
  bsb_sc_data <- costs_new_all_MA %>% 
    #tibble() %>% 
    filter(catch_draw<=nsamp) %>% 
    select(period, catch_draw,tripid,tot_keep_wf_base, tot_rel_wf_base) %>%
    rename(tot_keep_wf = tot_keep_wf_base,
           tot_rel_wf = tot_rel_wf_base)
  # names(bsb_sc_data)[names(bsb_sc_data) == "tot_keep_wf_base"] <- "tot_keep_wf"
  # names(bsb_sc_data)[names(bsb_sc_data) == "tot_rel_wf_base"] <- "tot_rel_wf"
}

if (state1 %in% c("VA","NC")) {
  # bsb_sc_data=subset(costs_new_all_MA, catch_draw<=nsamp, select=c(period, catch_draw, tripid,
  #                                                                  tot_keep_wf_base, tot_rel_wf_base,
  #                                                                  tot_keep_rd_base, tot_rel_rd_base)) %>%
  #   tibble()
  bsb_sc_data <- costs_new_all_MA %>% 
    #tibble() %>% 
    filter(catch_draw<=nsamp) %>%
    select(period, catch_draw, tripid,tot_keep_wf_base, tot_rel_wf_base,tot_keep_rd_base, tot_rel_rd_base) %>%
    rename(tot_keep_wf = tot_keep_wf_base,
           tot_rel_wf = tot_rel_wf_base,
           tot_keep_rd = tot_keep_rd_base,
           tot_rel_rd = tot_rel_rd_base)
  # names(bsb_sc_data)[names(bsb_sc_data) == "tot_keep_wf_base"] = "tot_keep_wf"
  # names(bsb_sc_data)[names(bsb_sc_data) == "tot_rel_wf_base"] = "tot_rel_wf"
  # names(bsb_sc_data)[names(bsb_sc_data) == "tot_keep_rd_base"] = "tot_keep_rd"
  # names(bsb_sc_data)[names(bsb_sc_data) == "tot_rel_rd_base"] = "tot_rel_rd"
}

#names(bsb_sc_data)[names(bsb_sc_data) == "tot_keep_bsb_base"] = "tot_keep_bsb"
#names(bsb_sc_data)[names(bsb_sc_data) == "tot_rel_bsb_base"] = "tot_rel_bsb"

# merge the trip data (summer flounder catch + lengths) with the other species data (numbers kept and released))
#trip_data <-  merge(trip_data,bsb_sc_data,by="tripid") %>% 
dfs <- trip_data %>% 
  #ungroup() %>% 
  left_join(bsb_sc_data, by = c("period","catch_draw","tripid")) %>% 
  # zap <- bsb_sc_data %>% data.table()
  # zip <- trip_data %>% ungroup() %>% data.table()
  # zop <- zap[zip,on=c("period","catch_draw","tripid")]
  #mutate_if(is.numeric, replace_na, replace = 0) %>% 
  left_join(regs %>% dplyr::select(period, bsb_bag), by = c("period")) %>% 
  mutate(region = region1,
         keep_bsb = rbinom(nrow(.), tot_bsb_catch, prop_bsb_keep),   # GF adding BSB catch from draws to retain correlation
         tot_keep_bsb = ifelse(keep_bsb <= bsb_bag, keep_bsb, bsb_bag),
         tot_rel_bsb = tot_bsb_catch - tot_keep_bsb) %>%
  dplyr::select(-keep_bsb,-bsb_bag) %>% 
  dplyr::select(period, catch_draw, everything()) #%>% 
    


cost_data <- costs_new_all_MA %>% dplyr::select(-tot_sf_catch)
trip_data <- left_join(dfs,cost_data,by=c("period", "tripid", "catch_draw")) #%>% 
#mutate_if(is.numeric, replace_na, replace = 0) #%>% 
#I()
#  trip_data[is.na(trip_data)] = 0


#set up an output file for each draw of utility parameters. For now, only taking one draw. 
parameter_draws = list()

#for(d in 1:1) {
d <- as.integer(1)
#dchoose = 1  
# Use the previously drawn set of utility parameters to calculate expected utility, welfare, and effort in the prediction year
#param_draws_MA_prediction = subset(param_draws_MA, parameter_draw=i)
param_draws_MA_prediction = param_draws_MA %>% filter(parameter_draw==dchoose) #%>% tibble() %>% filter(parameter_draw==dchoose)
#trip_data =  merge(param_draws_MA_prediction,trip_data,by="tripid")
trip_data <- right_join(param_draws_MA_prediction,trip_data,by=c("tripid", "state"))
#trip_data <- na.omit(trip_data)


if (region1 %in% c("NO","NJ")) {
  trip_data <- trip_data %>% 
    # Expected utility (prediction year)
    mutate(vA = beta_sqrt_sf_keep*sqrt(tot_keep) +
             beta_sqrt_sf_release*sqrt(tot_rel) +  
             beta_sqrt_bsb_keep*sqrt(tot_keep_bsb) +
             beta_sqrt_bsb_release*sqrt(tot_rel_bsb) +  
             beta_sqrt_scup_keep*sqrt(tot_keep_scup) +
             beta_sqrt_scup_release*sqrt(tot_rel_scup) +    
             beta_cost*cost,
           # Expected utility (base year)
           v0 = beta_sqrt_sf_keep*sqrt(tot_keep_sf_base) +
             beta_sqrt_sf_release*sqrt(tot_rel_sf_base) +  
             beta_sqrt_bsb_keep*sqrt(tot_keep_bsb_base) +
             beta_sqrt_bsb_release*sqrt(tot_rel_bsb_base) +  
             beta_sqrt_scup_keep*sqrt(tot_keep_scup_base) +
             beta_sqrt_scup_release*sqrt(tot_rel_scup_base) +    
             beta_cost*cost)
}

if (region1 == "NJ") {
  trip_data <- trip_data %>% 
    mutate(vA = vA + beta_sqrt_wf_keep*sqrt(tot_keep_wf) +
             beta_sqrt_wf_release*sqrt(tot_rel_wf),
           v0 = v0 + beta_sqrt_wf_keep*sqrt(tot_keep_wf_base) +
             beta_sqrt_wf_release*sqrt(tot_rel_wf_base))       
}


if (state1 %in% c("DE","MD")) {
  # Expected utility (prediction year)
  trip_data <- trip_data %>% 
    mutate(
      vA = beta_sqrt_sf_keep*sqrt(tot_keep) +
        beta_sqrt_sf_release*sqrt(tot_rel) +  
        beta_sqrt_bsb_keep*sqrt(tot_keep_bsb) +
        beta_sqrt_bsb_release*sqrt(tot_rel_bsb) +  
        beta_sqrt_wf_keep*sqrt(tot_keep_wf) +
        beta_sqrt_wf_release*sqrt(tot_rel_wf) +
        beta_cost*cost, 
      
      # Expected utility (base year)
      v0 = beta_sqrt_sf_keep*sqrt(tot_keep_sf_base) +
        beta_sqrt_sf_release*sqrt(tot_rel_sf_base) +  
        beta_sqrt_bsb_keep*sqrt(tot_keep_bsb_base) +
        beta_sqrt_bsb_release*sqrt(tot_rel_bsb_base) +  
        beta_sqrt_wf_keep*sqrt(tot_keep_wf_base) +
        beta_sqrt_wf_release*sqrt(tot_rel_wf_base) +       
        beta_cost*cost)

}

if (state1 %in% c("VA","NC")) {
  # Expected utility (prediction year)
  trip_data <- trip_data %>% 
    mutate(
      vA = beta_sqrt_sf_keep*sqrt(tot_keep) +
        beta_sqrt_sf_release*sqrt(tot_rel) +  
        beta_sqrt_bsb_keep*sqrt(tot_keep_bsb) +
        beta_sqrt_bsb_release*sqrt(tot_rel_bsb) +  
        beta_sqrt_wf_keep*sqrt(tot_keep_wf) +
        beta_sqrt_wf_release*sqrt(tot_rel_wf) +   
        beta_sqrt_rd_keep*sqrt(tot_keep_rd) +
        beta_sqrt_rd_release*sqrt(tot_rel_rd) +   
        beta_cost*cost,

      # Expected utility (base year)
      v0 = beta_sqrt_sf_keep*sqrt(tot_keep_sf_base) +
        beta_sqrt_sf_release*sqrt(tot_rel_sf_base) +  
        beta_sqrt_bsb_keep*sqrt(tot_keep_bsb_base) +
        beta_sqrt_bsb_release*sqrt(tot_rel_bsb_base) +  
        beta_sqrt_wf_keep*sqrt(tot_keep_wf_base) +
        beta_sqrt_wf_release*sqrt(tot_rel_wf_base) + 
        beta_sqrt_rd_keep*sqrt(tot_keep_rd_base) +
        beta_sqrt_rd_release*sqrt(tot_rel_rd_base) + 
        beta_cost*cost)
}


trip_data <- trip_data %>% 
  mutate(period = as.numeric(period))

# Collapse data from the X catch draws so that each row contains mean values
#mean_trip_data <-aggregate(trip_data, by=list(trip_data$tripid),FUN=mean, na.rm=TRUE)
mean_trip_data <- trip_data %>% 
  dplyr::select(-c("region", "state")) %>% 
  data.table()
mean_trip_data <- mean_trip_data[, lapply(.SD, mean), by=list(period,tripid)] %>% 
  tibble() #%>% 



nkeep <- trip_data %>%
  group_by(period, tripid) %>%
  summarise(keep_one = length(which(tot_keep>0))/length(tot_keep), #n(),
            .groups = "drop")
mean_trip_data <- left_join(mean_trip_data, nkeep, by = c("period", "tripid"))


# Now expand the data to create three alternatives, representing the alternatives available in choice survey
#mean_trip_data <- expandRows(mean_trip_data, 3, count.is.col = FALSE)
mean_trip_data <- mean_trip_data %>% 
  mutate(n_alt = rep(3,nrow(.))) %>% 
  uncount(n_alt) %>% 
  mutate(alt = rep(1:3,nrow(.)/3),
         opt_out = ifelse(alt == 3, 1, 0),
         striper_blue = ifelse(alt == 2, 1, 0))


#Caluculate the expected utility of alts 2 and 3 based on the parameters of the utility function
#These will be the same for both v0 and v1
# mean_trip_data$vA_optout <- mean_trip_data$beta_opt_out*mean_trip_data$opt_out 
# mean_trip_data$vA_striper_blue <- mean_trip_data$beta_striper_blue*mean_trip_data$striper_blue 
mean_trip_data <- mean_trip_data %>% 
  mutate(vA_optout = beta_opt_out*opt_out,
         vA_striper_blue = beta_striper_blue*striper_blue + beta_cost*cost,
         vA = ifelse(alt==1,vA,0),
         v0 = ifelse(alt==1,v0,0))

mean_trip_data <- mean_trip_data %>%  
    mutate(vA_striper_blue = ifelse(alt==2,vA_striper_blue,0 ))


#Now put these three values in the same column, exponentiate, and calculate their sum (vA_col_sum)
# mean_trip_data$vA[mean_trip_data$alt!=1] <- 0
# mean_trip_data$v0[mean_trip_data$alt!=1] <- 0

mean_trip_data <- mean_trip_data %>% 
  group_by(period, tripid) %>% 
  mutate(vA_row_sum = exp(vA + vA_striper_blue + vA_optout),
         vA_col_sum = sum(vA_row_sum),
         v0_row_sum = exp(v0 + vA_striper_blue + vA_optout),
         v0_col_sum = sum(v0_row_sum)) %>% 
  ungroup()


mean_trip_data <- mean_trip_data %>% 
  mutate(change_CS = (1/beta_cost)*(log(vA_col_sum) - log(v0_col_sum))) %>% 
  
  # Calculate the probability of a respondent selected each alternative based on 
  # exponentiated expected utility of the alternative [exp(expected utility, alt=i] 
  # and the sum of exponentiated expected utility across the three altenratives.
  # You will notice the striper_blue alternative has a large probability based on the utility parameters
  #mean_trip_data$probA = mean_trip_data$vA_row_sum/mean_trip_data$vA_col_sum
  #mean_trip_data$prob0 = mean_trip_data$v0_row_sum/mean_trip_data$v0_col_sum
  mutate(probA = vA_row_sum/vA_col_sum,
         prob0 = v0_row_sum/v0_col_sum)




# Get rid of things we don't need. 
if (region1 == "NO") {
  mean_trip_data <- subset(mean_trip_data, alt==1, select=-c(alt, opt_out, striper_blue, vA_optout, vA_striper_blue, vA_row_sum, vA_col_sum, v0_row_sum, v0_col_sum,
                                                             beta_cost, beta_striper_blue, beta_opt_out, beta_sqrt_scup_release, beta_sqrt_scup_keep,
                                                             beta_sqrt_bsb_release, beta_sqrt_bsb_keep, beta_sqrt_sf_release, beta_sqrt_sf_keep))
}
if (region1 == "NJ") {
  mean_trip_data <- subset(mean_trip_data, alt==1, select=-c(alt, opt_out, striper_blue, vA_optout, vA_striper_blue, vA_row_sum, vA_col_sum, v0_row_sum, v0_col_sum,
                                                             beta_cost, beta_striper_blue, beta_opt_out, beta_sqrt_scup_release, beta_sqrt_scup_keep,
                                                             beta_sqrt_bsb_release, beta_sqrt_bsb_keep, beta_sqrt_sf_release, beta_sqrt_sf_keep,
                                                             beta_sqrt_wf_release,beta_sqrt_wf_keep))
}
if (state1 %in% c("DE","MD")) {
  mean_trip_data <- subset(mean_trip_data, alt==1, select=-c(alt, opt_out, striper_blue, vA_optout, vA_striper_blue, vA_row_sum, vA_col_sum, v0_row_sum, v0_col_sum,
                                                             beta_cost, beta_striper_blue, beta_opt_out,
                                                             beta_sqrt_bsb_release, beta_sqrt_bsb_keep, beta_sqrt_sf_release, beta_sqrt_sf_keep,
                                                             beta_sqrt_wf_release,beta_sqrt_wf_keep))
}
if (state1 %in% c("VA","NC")) {
  mean_trip_data <- subset(mean_trip_data, alt==1, select=-c(alt, opt_out, striper_blue, vA_optout, vA_striper_blue, vA_row_sum, vA_col_sum, v0_row_sum, v0_col_sum,
                                                             beta_cost, beta_striper_blue, beta_opt_out,
                                                             beta_sqrt_bsb_release, beta_sqrt_bsb_keep, beta_sqrt_sf_release, beta_sqrt_sf_keep, 
                                                             beta_sqrt_wf_release, beta_sqrt_wf_keep, beta_sqrt_rd_release, beta_sqrt_rd_keep))
}

# Multiply the average trip probability by each of the catch variables (not the variable below) to get probability-weighted catch
list_names <- colnames(mean_trip_data)[colnames(mean_trip_data) !="Group.1" & colnames(mean_trip_data) !="tripid" 
                                       & colnames(mean_trip_data) !="catch_draw" & colnames(mean_trip_data) !="period" #& colnames(mean_trip_data) !="cost" 
                                       & colnames(mean_trip_data) !="vA" & colnames(mean_trip_data) !="v0"  & colnames(mean_trip_data) !="probA"
                                       & colnames(mean_trip_data) !="prob0" & colnames(mean_trip_data) !="change_CS"]    

# mean_trip_data <- mean_trip_data %>% 
#   filter(!is.na(probA)) %>% 
#   mutate(chosen = rbinom(nrow(.), 1, prob = probA)) %>% 
#   mutate(across(.cols = all_of(list_names),.fns=function(x) chosen*x),
#          n_choice_occasions = rep(1,nrow(.)))
for (l in list_names){
  mean_trip_data[,l] <- mean_trip_data[,l]*mean_trip_data$probA
}

mean_trip_data <- mean_trip_data %>%
  mutate( n_choice_occasions = rep(1,nrow(.)))


#Now multiply the trip outcomes (catch, trip probabilities) for each choice occasion in 
#mean_trip_pool by the expansion factor (expand), so that  each choice occasion represents a certain number of choice occasions
sims <- calibration_data %>% filter(sim==1) %>% 
  dplyr::select(c(n_choice_occasions, period)) %>% 
  left_join(mean_trip_data %>% count(period, name = "ndraws") %>% mutate(period = as.character(period)), by = "period") %>% 
  mutate(expand = n_choice_occasions/ndraws)
#n_choice_occasions = mean(sims$n_choice_occasions)
#ndraws = nrow(mean_trip_data)
#expand=n_choice_occasions/ndraws


#mean_trip_data$sim=1
mean_trip_data <- mean_trip_data %>%
  mutate(sim = rep(1,nrow(.))) #,
#keep_one = ifelse(tot_keep>0,1,0))

#sum probability weighted catch over all choice occasions
#aggregate_trip_data <-aggregate(mean_trip_data, by=list(mean_trip_data$sim),FUN=sum, na.rm=TRUE)
aggregate_trip_data <- mean_trip_data %>% 
  group_by(period, sim) %>% 
  summarize_all(sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  left_join(sims %>% mutate(period = as.numeric(period)), by = c("period"))
  # right_join(sims %>% mutate(period = as.numeric(period)) %>% dplyr::select(-n_choice_occasions),
  #            by = c("period","sim"))



ls(aggregate_trip_data)
list_names = colnames(aggregate_trip_data)[ colnames(aggregate_trip_data) !="tripid" 
                                      & colnames(aggregate_trip_data) !="catch_draw" & colnames(aggregate_trip_data) !="period"
                                      & colnames(aggregate_trip_data) !="vA" & colnames(aggregate_trip_data) !="v0"
                                      & colnames(aggregate_trip_data) != "state" 
                                      & colnames(aggregate_trip_data) != "ndraws" 
                                      & colnames(aggregate_trip_data) != "expand" & colnames(aggregate_trip_data) != "n_choice_occasions.y"
                                      & colnames(aggregate_trip_data) != "parameter_draw" & colnames(aggregate_trip_data) != "prob0"]
#& colnames(mean_trip_data) !="cost" & colnames(mean_trip_data) !="vA" & colnames(mean_trip_data) !="v0"


# for (l in list_names){
#   mean_trip_data[,l] = mean_trip_data[,l]*expand
# }


aggregate_trip_data[,list_names] <- aggregate_trip_data$expand*aggregate_trip_data[,list_names]



pds_new_all_MA <- aggregate_trip_data %>%  #list.stack(pds_new, fill=TRUE) %>% 
  #mutate_at(vars(contains("length")), replace_na, replace = 0) %>% 
  #pds_new_all_MA[is.na(pds_new_all_MA)] = 0
  mutate(state = state1,
         region = region1,
         alt_regs = 1)
#  pds_new_all_MA$state = state1
#pds_new_all_MA$region = region1
#pds_new_all_MA$alt_regs = 1
# pds_new_all_MA=subset(pds_new_all_MA, select=-c(Group.1, tot_keep_sf_base, tot_rel_sf_base, 
#                                                 tot_keep_scup_base, tot_rel_scup_base, 
#                                                 tot_keep_bsb_base, tot_rel_bsb_base, tot_sf_catch))
if (region1 == "NO") {
  pds_new_all_MA <- subset(pds_new_all_MA, select=-c(tot_keep_sf_base, tot_rel_sf_base, 
                                                     tot_keep_scup_base, tot_rel_scup_base, 
                                                     tot_keep_bsb_base, tot_rel_bsb_base, tot_sf_catch))
}
if (region1 == "NJ") {
  pds_new_all_MA <- subset(pds_new_all_MA, select=-c(tot_keep_sf_base, tot_rel_sf_base, 
                                                     tot_keep_scup_base, tot_rel_scup_base, 
                                                     tot_keep_wf_base, tot_rel_wf_base, 
                                                     tot_keep_bsb_base, tot_rel_bsb_base, tot_sf_catch))
}
if (state1 %in% c("DE","MD")) {
  pds_new_all_MA <- subset(pds_new_all_MA, select=-c(tot_keep_sf_base, tot_rel_sf_base, 
                                                     tot_keep_wf_base, tot_rel_wf_base, 
                                                     tot_keep_bsb_base, tot_rel_bsb_base, tot_sf_catch))
}
if (state1 %in% c("VA","NC")) {
  pds_new_all_MA <- subset(pds_new_all_MA, select=-c(tot_keep_sf_base, tot_rel_sf_base, 
                                                     tot_keep_wf_base, tot_rel_wf_base, tot_keep_rd_base, tot_rel_rd_base,
                                                     tot_keep_bsb_base, tot_rel_bsb_base, tot_sf_catch))
}

#})
# write_xlsx(pds_new_all_MA,"MA_prediction_output_check.xlsx")
return(pds_new_all_MA)

#end function
}





# 
predictions = list()

for (d in 1:5){
#2022 numbers (draw from a sample of 100) - use this to incorporate uncertainty 
numbers_at_age = data.frame(read_excel("F2021_2019_ALLPROJ_2022_STOCKN_sample100.xlsx"))
numbers_at_age = subset(numbers_at_age, numbers_at_age$draw==d)

source("CAL given stock structure by state.R")

##Sizes 
size_data_read <- readRDS("sf_fitted_sizes_y2plus.rds") %>% tibble()
size_data_read_base <- split(size_data_read, size_data_read$region)

#Predicted catches

#catch data
sf_catch_data_ma <- readRDS("predicted_catch_MA.rds") %>%
  tibble() %>%
  rename(tot_sf_catch = sf_t_nb,
         tot_bsb_catch = bsb_t_nb) %>%
  I()
sf_catch_data_ri <- readRDS("predicted_catch_RI.rds") %>%
  tibble() %>%
  rename(tot_sf_catch = sf_t_nb,
         tot_bsb_catch = bsb_t_nb) %>%
  I()
sf_catch_data_ct <- readRDS("predicted_catch_CT.rds") %>%
  tibble() %>%
  rename(tot_sf_catch = sf_t_nb,
         tot_bsb_catch = bsb_t_nb) %>%
  I()

sf_catch_data_ny <- readRDS("predicted_catch_NY.rds") %>%
  tibble() %>%
  rename(tot_sf_catch = sf_t_nb,
         tot_bsb_catch = bsb_t_nb) %>%
  I()
sf_catch_data_nj <- readRDS("predicted_catch_NJ.rds") %>%
  tibble() %>%
  rename(tot_sf_catch = sf_t_nb,
         tot_bsb_catch = bsb_t_nb) %>%
  I()
sf_catch_data_de <- readRDS("predicted_catch_DE.rds") %>%
  tibble() %>%
  rename(tot_sf_catch = sf_t_nb,
         tot_bsb_catch = bsb_t_nb) %>%
  I()

sf_catch_data_md <- readRDS("predicted_catch_MD.rds") %>%
  tibble() %>%
  rename(tot_sf_catch = sf_t_nb,
         tot_bsb_catch = bsb_t_nb) %>%
  I()
sf_catch_data_va <- readRDS("predicted_catch_VA.rds") %>%
  tibble() %>%
  rename(tot_sf_catch = sf_t_nb,
         tot_bsb_catch = bsb_t_nb) %>%
  I()

sf_catch_data_nc <- readRDS("predicted_catch_NC.rds") %>%
  tibble() %>%
  rename(tot_sf_catch = sf_t_nb,
         tot_bsb_catch = bsb_t_nb) %>%
  I()


#Predicted catches
sf_catch_data_all = dplyr::bind_rows(sf_catch_data_ma, sf_catch_data_ri,sf_catch_data_ct,sf_catch_data_ny,sf_catch_data_nj,
                                     sf_catch_data_de, sf_catch_data_md, sf_catch_data_va, sf_catch_data_nc)

# sf_catch_data_all <- split(sf_catch_data_all, sf_catch_data_all$region)
# 
# sf_catch_data_all = dplyr::bind_rows(sf_catch_data_ma, sf_catch_data_ri,sf_catch_data_ct,sf_catch_data_ny,sf_catch_data_nj,
#                                      sf_catch_data_de, sf_catch_data_md, sf_catch_data_va, sf_catch_data_nc)
# 
# sf_catch_data_all_base <- split(sf_catch_data_all, sf_catch_data_all$region)



####
MA_pred <- predict_rec_catch(state1 <- "MA",
                             region1 <- "NO",
                             calibration_data_table <- calibration_data_table_base[[3]],
                             directed_trips_table <- directed_trips_table_base[[3]],
                             size_data_read <- size_data_read_base[[3]],
                             param_draws_MA <- param_draws_all_base[[3]],
                             costs_new_all_MA <- costs_all_base[[3]],
                             sf_catch_data_all <- sf_catch_data_ma,
                             prop_bsb_keep <- 1-.53,
                             dchoose <- 1,
                             mgmt_scen <- 1)
MA_pred$draw<-d
MA_pred$state<-state1


RI_pred <- predict_rec_catch(state1 <- "RI",
                             region1 <- "NO",
                             calibration_data_table <- calibration_data_table_base[[8]],
                             directed_trips_table <- directed_trips_table_base[[8]],
                             size_data_read <- size_data_read_base[[8]],
                             param_draws_MA <- param_draws_all_base[[8]],
                             costs_new_all_MA <- costs_all_base[[8]],
                             sf_catch_data_all <- sf_catch_data_ri,
                             prop_bsb_keep <- 1-.38,
                             dchoose <- 1,
                             mgmt_scen <- 1)
RI_pred$draw<-d
RI_pred$state<-state1





NC_pred <- predict_rec_catch(state1 <- "NC",
                        region1 <- "SO",
                        calibration_data_table <- calibration_data_table_base[[5]],
                        directed_trips_table <- directed_trips_table_base[[5]],
                        size_data_read <- size_data_read_base[[5]],
                        param_draws_MA <- param_draws_all_base[[5]],
                        costs_new_all_MA <- costs_all_base[[5]],
                        sf_catch_data_all <- sf_catch_data_nc,
                        prop_bsb_keep <- 0.001,
                        dchoose <- 1,
                        mgmt_scen <- 1)
NC_pred$draw<-d
NC_pred$state<-state1


predictions[[d]]<- dplyr::bind_rows(MA_pred, NC_pred, RI_pred)
rm(MA_pred,NC_pred, RI_pred )


}


predictions_all= list.stack(predictions, fill=TRUE)
predictions_all[is.na(predictions_all)] = 0



prediction_check <- subset(predictions_all, select=c(period, draw, state, tot_keep))

xx$draw<-d

prediction_output_by_period <- purrr::map(xx, 1)
output <- as.data.frame(prediction_output_by_period)

aggregate_prediction_output <- prediction_output_by_period %>% 
  list.stack(fill = TRUE) 






#####

# loop over states
params <- list(state1 = c("MA","RI","CT","NY","NJ","DE","MD","VA", "NC"),
               region1 = c(rep("NO",4),"NJ",rep("SO",4)),
               calibration_data_table = calibration_data_table_base, #rep(list(calibration_data_table),9),
               directed_trips_table = directed_trips_table_base, #rep(list(directed_trips_table),9),   #split(directed_trips_table,directed_trips_table$state), #
               size_data_read = size_data_read_base, #rep(list(size_data_read),9),
               param_draws_MA = param_draws_all_base,
               costs_new_all_MA = costs_all_base,
               
               #sf_catch_data_all = c(rep(list(sf_catch_data_no),4),list(sf_catch_data_nj),rep(list(sf_catch_data_so),4)),
               sf_catch_data_all = c(list(sf_catch_data_ma),list(sf_catch_data_ri),
                                     list(sf_catch_data_ct),list(sf_catch_data_ny),
                                     list(sf_catch_data_nj),list(sf_catch_data_de),
                                     list(sf_catch_data_md),list(sf_catch_data_va), list(sf_catch_data_nc)),
               # prop_bsb_keep = #rep(0.33,9))  # add Lou's p* values here!
               #   c(1-0.67,
               #     1-0.66,
               #     1-0.77,
               #     1-0.87,
               #     1-0.93,
               #     1-0.945,
               #     1-0.96,
               #     1-0.92,
               #     0.001),
               prop_bsb_keep = c(
                 1-.53,
                 1-.38,
                 1-.7,
                 1-.83,
                 1-.92,
                 1-.942,
                 1-.96,
                 1-.92,
                 0.001), #1),
               dchoose = rep(1,9),
               mgmt_scen = rep(mgmt_scen,9)) #1-1.1))

safe_predict_rec_catch <- purrr::safely(predict_rec_catch, otherwise = NA_real_)
xx <- purrr::pmap(params, safe_predict_rec_catch)



prediction_output_by_period <- purrr::map(xx, 1)
aggregate_prediction_output <- xx %>% 
  list.stack(fill = TRUE) 

#saveRDS(prediction_output_by_period, file = "prediction_output_by_period.rds")

#aggregate_prediction_output= subset(prediction_output_by_period, select=-c(state, alt_regs, period))
aggregate_prediction_output <- prediction_output_by_period %>% 
  list.stack(fill = TRUE) %>% 
  mutate_at(vars(contains("length")), replace_na, replace = 0) %>% 
  dplyr::select(-state, -alt_regs) %>%  #, -period) %>% 
  group_by(sim) %>% 
  summarize_if(is.numeric, .funs = sum,na.rm=TRUE)# %>% 
#dplyr::select(order(colnames(.))) %>% 
#I()
#  = aggregate(aggregate_prediction_output, by=list(aggregate_prediction_output$sim),FUN=sum, na.rm=TRUE)
#write_xlsx(aggregate_prediction_output,"aggregate_prediction_output.xlsx")
#saveRDS(aggregate_prediction_output, file = "aggregate_prediction_output.rds")



##############



%>% 
  mutate_at(vars(contains("length")), replace_na, replace = 0) %>% 
  #dplyr::select(-state, -alt_regs) %>%  #, -period) %>% 
  group_by(sim) %>% 
  summarize_if(is.numeric, .funs = sum,na.rm=TRUE)# %>% 



params <- list(
  state1 <- "CT",
  region1 <- "NO",
  calibration_data_table <- calibration_data_table[[1]],
  directed_trips_table <- directed_trips_table[[1]],
  size_data_read <- size_data_read[[1]],
  param_draws_MA <- param_draws_all[[1]],
  costs_new_all_MA <- costs_new[[1]],
  sf_catch_data_all <- sf_catch_data_all[[1]],
  prop_bsb_keep <- 1-0.53,
  dchoose <- 1,
  mgmt_scen <- 1)

safe_predict_rec_catch <- purrr::safely(predict_rec_catch, otherwise = NA_real_)
xx <- purrr::pmap(params, safe_predict_rec_catch)
prediction_output_by_period <- purrr::map(xx, 1)
output <- as.data.frame(prediction_output_by_period)