

#load needed packages and install if not currently installed.
pkgs_to_use <- c("tidyr",
                 "magrittr",
                 #"reshape2",
                 #"splitstackshape",
                 #"doBy",
                 #"WriteXLS",
                 #'Rcpp',
                 #"ggplot2",
                 "dplyr",
                 "rlist",
                 #"fitdistrplus",
                 #"MASS",
                 #"psych",
                 #"rgl",
                 "copula",
                 #"VineCopula",
                 #"scales",
                 #"univariateML",
                 #"logspline",
                 "readr",
                 "readxl",
                 "writexl",
                 "data.table")
#install.packages(setdiff(pkgs_to_use, rownames(installed.packages())))  
lapply(pkgs_to_use, library, character.only = TRUE)

### 

# Input the data set containing alternative regulations and directed trips (directed_trips_region - alternative regs test.xlsx)
directed_trips_table <- data.frame(read_excel("directed_trips_regions_bimonthly_test.xlsx"))
directed_trips_table<-saveRDS(directed_trips_table, file = "directed_trips_regions_bimonthly_test.rds")
directed_trips_table <- readRDS("directed_trips_regions_bimonthly_test.rds")


#directed_trips_table <- readRDS("coastwide_regulations_scenario.rds")
# Input the calibration output which contains the number of choice occasions needed to simulate
calibration_data_table <- readRDS("calibration_output_by_period.rds")

#utility parameter draws
param_draws_all <- readRDS("param_draws_all.rds")

#costs
costs_new <- readRDS( "costs_all.rds")


# Read-in current population length composition (from sinatra output)
# Nlen <- 42
# om_length_cm <- scan("om-length.dat",n=Nlen+1)
# cm2in <- read_csv("cm2in.csv", col_names = FALSE)
# lenbinuse <- as.integer(unlist(cm2in[,1]))
# Nlen_in <- length(lenbinuse)
# cm2in <- cm2in %>% 
#   dplyr::select(-1) %>% 
#   as.matrix() %>% 
#   I()
# om_length_in <- om_length_cm[-1] %*% t(cm2in)
# size_data <- data.frame(fitted_prob = rep(om_length_in,3),
#                         fitted_length = rep(lenbinuse,3),
#                         region = rep(c("SO","NJ","NO"), each = Nlen_in),
#                         year = rep("y2",3*Nlen_in))


#2019 numbers (median)
numbers_at_age = data.frame(read_excel("numbers_at_age_2019.xlsx"))
numbers_at_age$Na=numbers_at_age$Na*1000

#2022 numbers (median)
# numbers_at_age = data.frame(read_excel("F2021_2019_ALLPROJ_2022_STOCKN_median.xlsx"))

#2022 numbers (draw from a sample of 100) - use this to incorporate uncertainty 
# numbers_at_age = data.frame(read_excel("F2021_2019_ALLPROJ_2022_STOCKN_sample100.xlsx"))
# numbers_at_age = subset(numbers_at_age, numbers_at_age$draw==x)
# 
# ma_test = data.frame()
# for (s in 1:30){
  
source("CAL given stock structure by state.R")


#catch data
sf_catch_data_ma <- readRDS("predicted_catch_MA.rds") %>% 
  tibble() %>% 
  dplyr::rename(tot_sf_catch = sf_t_nb,
         tot_bsb_catch = bsb_t_nb) %>%
  I()
sf_catch_data_ri <- readRDS("predicted_catch_RI.rds") %>% 
  tibble() %>% 
  dplyr::rename(tot_sf_catch = sf_t_nb,
         tot_bsb_catch = bsb_t_nb) %>%
  I()
sf_catch_data_ct <- readRDS("predicted_catch_CT.rds") %>% 
  tibble() %>% 
  dplyr::rename(tot_sf_catch = sf_t_nb,
         tot_bsb_catch = bsb_t_nb) %>%
  I()

sf_catch_data_ny <- readRDS("predicted_catch_NY.rds") %>% 
  tibble() %>% 
  dplyr::rename(tot_sf_catch = sf_t_nb,
         tot_bsb_catch = bsb_t_nb) %>%
  I()
sf_catch_data_nj <- readRDS("predicted_catch_NJ.rds") %>% 
  tibble() %>% 
  dplyr::rename(tot_sf_catch = sf_t_nb,
         tot_bsb_catch = bsb_t_nb) %>%
  I()
sf_catch_data_de <- readRDS("predicted_catch_DE.rds") %>% 
  tibble() %>% 
  dplyr::rename(tot_sf_catch = sf_t_nb,
         tot_bsb_catch = bsb_t_nb) %>%
  I()

sf_catch_data_md <- readRDS("predicted_catch_MD.rds") %>% 
  tibble() %>% 
  dplyr::rename(tot_sf_catch = sf_t_nb,
         tot_bsb_catch = bsb_t_nb) %>%
  I()
sf_catch_data_va <- readRDS("predicted_catch_VA.rds") %>% 
  tibble() %>% 
  dplyr::rename(tot_sf_catch = sf_t_nb,
         tot_bsb_catch = bsb_t_nb) %>%
  I()

sf_catch_data_nc <- readRDS("predicted_catch_NC.rds") %>% 
  tibble() %>% 
  dplyr::rename(tot_sf_catch = sf_t_nb,
         tot_bsb_catch = bsb_t_nb) %>%
  I()


# Read-in the current population length composition  #don't need this in final as it's already an object.
#size_data_read <- data.frame(read_excel("sf_fitted_sizes_y2plus.xlsx"))
size_data_read <- readRDS("sf_fitted_sizes_y2plus.rds") %>% tibble()

# loop over states(NC omitted for now)
params <- list(state1 = c("MA","RI","CT","NY","NJ","DE","MD","VA", "NC"),
               region1 = c(rep("NO",4),"NJ",rep("SO",4)),
               calibration_data_table = rep(list(calibration_data_table),9),
               directed_trips_table = rep(list(directed_trips_table),9),
               size_data_read = rep(list(size_data_read),9),
               param_draws_MA = param_draws_all,
               costs_new_all_MA = costs_new,

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
               dchoose = rep(1,9)) #1-1.1))

# params <- list(state1 = "MA",
#                region1 = "NO",
#                calibration_data_table = list(calibration_data_table),
#                directed_trips_table = list(directed_trips_table),
#                size_data_read = list(size_data_read),
#                param_draws_MA = list(param_draws_all[[1]]),
#                costs_new_all_MA = list(costs_new[[1]]),
#                sf_catch_data_all = list(sf_catch_data_ma),
#                prop_bsb_keep = 1-0.53,
#                dchoose = 1)
# 
# params <- list(state1 = "NJ",
#                region1 = "NJ",
#                calibration_data_table = list(calibration_data_table),
#                directed_trips_table = list(directed_trips_table),
#                size_data_read = list(size_data_read),
#                param_draws_MA = list(param_draws_all[[5]]),
#                costs_new_all_MA = list(costs_new[[5]]),
#                sf_catch_data_all = list(sf_catch_data_nj))

#source("prediction-all.R")
#source("prediction-vec.R")
source("prediction-vec-sim-GF.R")

# pds_new_all_MA$draw = s
# 
# 
# ma_test =rbind.fill(ma_test, pds_new_all_MA)
# }
# 
# write_xlsx(ma_test,"ma_test_GF.xlsx")
test = data.frame()

set.seed(1989)
simkeep <- NULL
simrel <- NULL
simagg <- NULL
#for (jsim in 1:2) {
  ##########  need to add link to OM scenario regulations
  
  #params$dchoose <- rep(sample(1:1000,1),9)
  
  safe_predict_rec_catch <- purrr::safely(predict_rec_catch, otherwise = NA_real_)
  xx <- purrr::pmap(params, safe_predict_rec_catch)
  
  
  # profvis::profvis(testMA <- predict_rec_catch(state1 = "MA",
  #                         region1 = "NO",
  #                         calibration_data_table = calibration_data_table,
  #                         directed_trips_table = directed_trips_table,
  #                         size_data_read = size_data_read,
  #                         param_draws_MA = param_draws_all[[1]],
  #                         costs_new_all_MA = costs_new[[1]],
  #                         sf_catch_data_all = sf_catch_data_no,
  #                         prop_bsb_keep = 0.33))
  # 
  # xx <- predict_rec_catch(state1 = "NC",
  #                         region1 = "SO",
  #                         calibration_data_table = calibration_data_table,
  #                         directed_trips_table = directed_trips_table,
  #                         size_data_read = size_data_read,
  #                         param_draws_MA = param_draws_all[[9]],
  #                         costs_new_all_MA = costs_new[[9]],
  #                         sf_catch_data_all = sf_catch_data_so)
  
  
  prediction_output_by_period <- purrr::map(xx, 1)
  output <- as.data.frame(prediction_output_by_period)
  output$draw<-jsim
  saveRDS(prediction_output_by_period, file = "prediction_output_by_period.rds")
}
output=list.stack(output, fill=TRUE)

test =rbind.fill(test, output)
# pds_new_all_MA$draw = s
# 
# 
# ma_test =rbind.fill(ma_test, pds_new_all_MA)
# }
# 
# write_xlsx(ma_test,"ma_test_GF.xls
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
  saveRDS(aggregate_prediction_output, file = "aggregate_prediction_output.rds")
  
  ##########  
  
  pred_len <- tibble(aggregate_prediction_output) %>% 
    dplyr::select(contains("length")) %>% 
    pivot_longer(cols = 1:ncol(.), names_to = "bin",values_to = "num") %>% 
    separate(bin, into =c("type","len"),sep = "_length_") %>% 
    mutate(len = as.numeric(len)) #%>% 
  #I()
  #pred_len
  out_lens <- tibble(type = rep(c("release","keep"),each=Nlen_in),
                     len = rep(lenbinuse,2)) %>% 
    left_join(pred_len) %>% 
    replace_na(list(num=0)) #%>% 
  #I()
  #out_lens
  in2cm <- readr::read_csv("in2cm.csv", col_names = FALSE)[,-1]
  keep <- out_lens %>% 
    filter(type == "keep") %>% 
    dplyr::select(num) %>% 
    unlist()# %>%
  #I()
  keep <- keep %*% t(in2cm)
  release <- out_lens %>% 
    filter(type == "release") %>% 
    dplyr::select(num) %>% 
    unlist() #%>%
  #I()
  release <- release %*% t(in2cm)
  write.table(round(rbind(keep,release)/1000,3),file = "rec-catch.out", row.names = FALSE, col.names = FALSE)
  write(with(aggregate_prediction_output,observed_trips),file = "rec-catch.out", append = TRUE)
  
  print("keep")
  print(keep)
  print("release")
  print(release)
  #compare.results[[jsim]] <- NULL
  simkeep <- rbind(simkeep,keep)
  simrel <- rbind(simrel,release)
  simagg <- rbind(simagg, dplyr::select(aggregate_prediction_output, -starts_with("keep_"), -starts_with("release_")))
}
#####
# Stop the clock
#proc.time() - ptm


# ###
# # Calculate ouput statisitics for calibration and prediction year
# source("simulation output stats.R")
# 

