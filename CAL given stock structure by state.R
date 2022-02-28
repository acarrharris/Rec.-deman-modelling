

# This code uses  recreational selectivity (q) in the baseline year to determine catch-at-length given any size structure of the stock

# Code requires the following data: 
# AgE length key (for now using 2018 commercial survey ALK ): com_sv_len_age_adj_2018.xlsx
# Numbers at age file, in 1000s of fish: numbers_at_age_2018.csv <-- This will be the output from the operating model
# Baseline recreational selectivity: rec_selectivity.xlsx





# Import the 2018 ALK (in centimeters) provided by M. Terceiro
age_length_key = data.frame(read_excel("com_sv_len_age_adj_2018.xlsx"))                                                                            


#import a test numbers-at-age dataset
# THIS IS WHERE TO IMPORT THE NUMBERS AT AGE FROM THE OPERATING MODEL
#numbers_at_age = data.frame(read_excel("numbers_at_age_2018.xlsx"))

#numbers_at_age = data.frame(read_excel("F2021_2019_ALLPROJ_2022_STOCKN_sample100.xlsx"))
#numbers_at_age = subset(numbers_at_age, numbers_at_age$draw==x)
numbers_at_age = data.frame(read_excel("numbers_at_age_2019.xlsx"))
#numbers_at_age = data.frame(read_excel("F2021_2019_ALLPROJ_2022_STOCKN_median.xlsx"))


# Merge the two above datasets and create population numbers-at-length (inches)
numbers_at_length =  merge(age_length_key,numbers_at_age,by="age", all.x=TRUE, all.y=TRUE)
numbers_at_length$N_l = numbers_at_length$proportion*numbers_at_length$Na

numbers_at_length = aggregate(numbers_at_length, by=list(numbers_at_length$l_in_bin),FUN=sum, na.rm=TRUE)
numbers_at_length <-subset(numbers_at_length, select=c(Group.1,N_l))
names(numbers_at_length)[names(numbers_at_length) == "Group.1"] = "l_in_bin"


# Translate cms's to inches 
numbers_at_length$l_in_bin = round(numbers_at_length$l_in_bin/2.54)
numbers_at_length = aggregate(numbers_at_length, by=list(numbers_at_length$l_in_bin),FUN=sum, na.rm=TRUE)
numbers_at_length <-subset(numbers_at_length, select=c(Group.1,N_l))
names(numbers_at_length)[names(numbers_at_length) == "Group.1"] = "l_in_bin"


#Translate numbers from 1,000's of fish
numbers_at_length$N_l=numbers_at_length$N_l*1000
sum(numbers_at_length$N_l)


# Import and merge the selectivity data to this file 
#selectivity = data.frame(read_excel("rec_selectivity_MRIP_ALS.xlsx"))
#selectivity <-subset(selectivity, select=c(l_in_bin, region, q, E, C_l))

selectivity = data.frame(read_excel("rec_selectivity_by_state_cdf_star_raw_18_19.xlsx"))
selectivity <-subset(selectivity, select=c(l_in_bin, state, q, E, C_l))

numbers_at_length_new =  merge(selectivity,numbers_at_length,by="l_in_bin", all.x=TRUE, all.y=TRUE)

numbers_at_length_new[is.na(numbers_at_length_new)] = 0
numbers_at_length_new <-subset(numbers_at_length_new, N_l!=0 & state!=0)

# Create catch-at-length based on the new numbers-at-length
numbers_at_length_new$q = as.numeric(numbers_at_length_new$q)
numbers_at_length_new$C_l_new = (numbers_at_length_new$q)*(numbers_at_length_new$N_l)*(numbers_at_length_new$E)
sum(numbers_at_length_new$C_l_new)



# subset the catch-at-length new datstet by region and fit gamma to the distirbution 
numbers_at_length_MA <-subset(numbers_at_length_new, state=="MA", select=c(l_in_bin, C_l_new))
numbers_at_length_RI <-subset(numbers_at_length_new, state=="RI", select=c(l_in_bin, C_l_new))
numbers_at_length_CT <-subset(numbers_at_length_new, state=="CT", select=c(l_in_bin, C_l_new))
numbers_at_length_NY <-subset(numbers_at_length_new, state=="NY", select=c(l_in_bin, C_l_new))
numbers_at_length_NJ <-subset(numbers_at_length_new, state=="NJ", select=c(l_in_bin, C_l_new))
numbers_at_length_DE <-subset(numbers_at_length_new, state=="DE", select=c(l_in_bin, C_l_new))
numbers_at_length_MD <-subset(numbers_at_length_new, state=="MD", select=c(l_in_bin, C_l_new))
numbers_at_length_VA <-subset(numbers_at_length_new, state=="VA", select=c(l_in_bin, C_l_new))



numbers_at_length_MA$C_l_new=round(numbers_at_length_MA$C_l_new)
numbers_at_length_RI$C_l_new=round(numbers_at_length_RI$C_l_new)
numbers_at_length_CT$C_l_new=round(numbers_at_length_CT$C_l_new)
numbers_at_length_NY$C_l_new=round(numbers_at_length_NY$C_l_new)
numbers_at_length_NJ$C_l_new=round(numbers_at_length_NJ$C_l_new)
numbers_at_length_DE$C_l_new=round(numbers_at_length_DE$C_l_new)
numbers_at_length_MD$C_l_new=round(numbers_at_length_MD$C_l_new)
numbers_at_length_VA$C_l_new=round(numbers_at_length_VA$C_l_new)



tot_cat_MA_predicted=sum(numbers_at_length_MA$C_l_new)
tot_cat_RI_predicted=sum(numbers_at_length_RI$C_l_new)
tot_cat_CT_predicted=sum(numbers_at_length_CT$C_l_new)
tot_cat_NY_predicted=sum(numbers_at_length_NY$C_l_new)
tot_cat_NJ_predicted=sum(numbers_at_length_NJ$C_l_new)
tot_cat_DE_predicted=sum(numbers_at_length_DE$C_l_new)
tot_cat_MD_predicted=sum(numbers_at_length_MD$C_l_new)
tot_cat_VA_predicted=sum(numbers_at_length_VA$C_l_new)

tot_cat_MA_base=sum(subset(selectivity, state == "MA")$C_l)
tot_cat_RI_base=sum(subset(selectivity, state == "RI")$C_l)
tot_cat_CT_base=sum(subset(selectivity, state == "CT")$C_l)
tot_cat_NY_base=sum(subset(selectivity, state == "NY")$C_l)
tot_cat_NJ_base=sum(subset(selectivity, state == "NJ")$C_l)
tot_cat_DE_base=sum(subset(selectivity, state == "DE")$C_l)
tot_cat_MD_base=sum(subset(selectivity, state == "MD")$C_l)
tot_cat_VA_base=sum(subset(selectivity, state == "VA")$C_l)



#Create a factor that expands total catch in the prediction year
catch_expansion_factor_MA=round(tot_cat_MA_predicted/tot_cat_MA_base, digits=4)
catch_expansion_factor_RI=round(tot_cat_RI_predicted/tot_cat_RI_base, digits=4)
catch_expansion_factor_CT=round(tot_cat_CT_predicted/tot_cat_CT_base, digits=4)
catch_expansion_factor_NY=round(tot_cat_NY_predicted/tot_cat_NY_base, digits=4)
catch_expansion_factor_NJ=round(tot_cat_NJ_predicted/tot_cat_NJ_base, digits=4)
catch_expansion_factor_DE=round(tot_cat_DE_predicted/tot_cat_DE_base, digits=4)
catch_expansion_factor_MD=round(tot_cat_MD_predicted/tot_cat_MD_base, digits=4)
catch_expansion_factor_VA=round(tot_cat_VA_predicted/tot_cat_VA_base, digits=4)

sum(numbers_at_length_new$C_l)
sum(numbers_at_length_new$C_l_new)

##########
# Here, execute the catch-per trip file. 
# This file adjusts the expected number of catch per trip by population abundances. 
source("predicted catch per trip by state.R")


#####


#expand the sf_catch_data so that each row represents a fish
# Massachusetts
row_inds = seq_len(nrow(numbers_at_length_MA))
numbers_at_length_MA = numbers_at_length_MA[c(rep(row_inds, numbers_at_length_MA$C_l_new)), ]
rownames(numbers_at_length_MA) = NULL


numbers_at_length_MA$nfish = 1
numbers_at_length_MA <-aggregate(numbers_at_length_MA$nfish, by=list(numbers_at_length_MA$l_in_bin),FUN=sum, na.rm=TRUE)
names(numbers_at_length_MA)[names(numbers_at_length_MA) == "Group.1"] = "fitted_length"
names(numbers_at_length_MA)[names(numbers_at_length_MA) == "x"] = "nfish"
sum_nfish= sum(numbers_at_length_MA$nfish)
numbers_at_length_MA$fitted_prob = numbers_at_length_MA$nfish/sum_nfish
numbers_at_length_MA$region = "MA"
numbers_at_length_MA$year = "y2"


# Rhode Island
row_inds = seq_len(nrow(numbers_at_length_RI))
numbers_at_length_RI = numbers_at_length_RI[c(rep(row_inds, numbers_at_length_RI$C_l_new)), ]
rownames(numbers_at_length_RI) = NULL


numbers_at_length_RI$nfish = 1
numbers_at_length_RI <-aggregate(numbers_at_length_RI$nfish, by=list(numbers_at_length_RI$l_in_bin),FUN=sum, na.rm=TRUE)
names(numbers_at_length_RI)[names(numbers_at_length_RI) == "Group.1"] = "fitted_length"
names(numbers_at_length_RI)[names(numbers_at_length_RI) == "x"] = "nfish"
sum_nfish= sum(numbers_at_length_RI$nfish)
numbers_at_length_RI$fitted_prob = numbers_at_length_RI$nfish/sum_nfish
numbers_at_length_RI$region = "RI"
numbers_at_length_RI$year = "y2"



# CT
row_inds = seq_len(nrow(numbers_at_length_CT))
numbers_at_length_CT = numbers_at_length_CT[c(rep(row_inds, numbers_at_length_CT$C_l_new)), ]
rownames(numbers_at_length_CT) = NULL


numbers_at_length_CT$nfish = 1
numbers_at_length_CT <-aggregate(numbers_at_length_CT$nfish, by=list(numbers_at_length_CT$l_in_bin),FUN=sum, na.rm=TRUE)
names(numbers_at_length_CT)[names(numbers_at_length_CT) == "Group.1"] = "fitted_length"
names(numbers_at_length_CT)[names(numbers_at_length_CT) == "x"] = "nfish"
sum_nfish= sum(numbers_at_length_CT$nfish)
numbers_at_length_CT$fitted_prob = numbers_at_length_CT$nfish/sum_nfish
numbers_at_length_CT$region = "CT"
numbers_at_length_CT$year = "y2"



# NY
row_inds = seq_len(nrow(numbers_at_length_NY))
numbers_at_length_NY = numbers_at_length_NY[c(rep(row_inds, numbers_at_length_NY$C_l_new)), ]
rownames(numbers_at_length_NY) = NULL


numbers_at_length_NY$nfish = 1
numbers_at_length_NY <-aggregate(numbers_at_length_NY$nfish, by=list(numbers_at_length_NY$l_in_bin),FUN=sum, na.rm=TRUE)
names(numbers_at_length_NY)[names(numbers_at_length_NY) == "Group.1"] = "fitted_length"
names(numbers_at_length_NY)[names(numbers_at_length_NY) == "x"] = "nfish"
sum_nfish= sum(numbers_at_length_NY$nfish)
numbers_at_length_NY$fitted_prob = numbers_at_length_NY$nfish/sum_nfish
numbers_at_length_NY$region = "NY"
numbers_at_length_NY$year = "y2"



# NJ
row_inds = seq_len(nrow(numbers_at_length_NJ))
numbers_at_length_NJ = numbers_at_length_NJ[c(rep(row_inds, numbers_at_length_NJ$C_l_new)), ]
rownames(numbers_at_length_NJ) = NULL


numbers_at_length_NJ$nfish = 1
numbers_at_length_NJ <-aggregate(numbers_at_length_NJ$nfish, by=list(numbers_at_length_NJ$l_in_bin),FUN=sum, na.rm=TRUE)
names(numbers_at_length_NJ)[names(numbers_at_length_NJ) == "Group.1"] = "fitted_length"
names(numbers_at_length_NJ)[names(numbers_at_length_NJ) == "x"] = "nfish"
sum_nfish= sum(numbers_at_length_NJ$nfish)
numbers_at_length_NJ$fitted_prob = numbers_at_length_NJ$nfish/sum_nfish
numbers_at_length_NJ$region = "NJ"
numbers_at_length_NJ$year = "y2"



# DE
row_inds = seq_len(nrow(numbers_at_length_DE))
numbers_at_length_DE = numbers_at_length_DE[c(rep(row_inds, numbers_at_length_DE$C_l_new)), ]
rownames(numbers_at_length_DE) = NULL


numbers_at_length_DE$nfish = 1
numbers_at_length_DE <-aggregate(numbers_at_length_DE$nfish, by=list(numbers_at_length_DE$l_in_bin),FUN=sum, na.rm=TRUE)
names(numbers_at_length_DE)[names(numbers_at_length_DE) == "Group.1"] = "fitted_length"
names(numbers_at_length_DE)[names(numbers_at_length_DE) == "x"] = "nfish"
sum_nfish= sum(numbers_at_length_DE$nfish)
numbers_at_length_DE$fitted_prob = numbers_at_length_DE$nfish/sum_nfish
numbers_at_length_DE$region = "DE"
numbers_at_length_DE$year = "y2"


# MD
row_inds = seq_len(nrow(numbers_at_length_MD))
numbers_at_length_MD = numbers_at_length_MD[c(rep(row_inds, numbers_at_length_MD$C_l_new)), ]
rownames(numbers_at_length_MD) = NULL


numbers_at_length_MD$nfish = 1
numbers_at_length_MD <-aggregate(numbers_at_length_MD$nfish, by=list(numbers_at_length_MD$l_in_bin),FUN=sum, na.rm=TRUE)
names(numbers_at_length_MD)[names(numbers_at_length_MD) == "Group.1"] = "fitted_length"
names(numbers_at_length_MD)[names(numbers_at_length_MD) == "x"] = "nfish"
sum_nfish= sum(numbers_at_length_MD$nfish)
numbers_at_length_MD$fitted_prob = numbers_at_length_MD$nfish/sum_nfish
numbers_at_length_MD$region = "MD"
numbers_at_length_MD$year = "y2"



# VA
row_inds = seq_len(nrow(numbers_at_length_VA))
numbers_at_length_VA = numbers_at_length_VA[c(rep(row_inds, numbers_at_length_VA$C_l_new)), ]
rownames(numbers_at_length_VA) = NULL


numbers_at_length_VA$nfish = 1
numbers_at_length_VA <-aggregate(numbers_at_length_VA$nfish, by=list(numbers_at_length_VA$l_in_bin),FUN=sum, na.rm=TRUE)
names(numbers_at_length_VA)[names(numbers_at_length_VA) == "Group.1"] = "fitted_length"
names(numbers_at_length_VA)[names(numbers_at_length_VA) == "x"] = "nfish"
sum_nfish= sum(numbers_at_length_VA$nfish)
numbers_at_length_VA$fitted_prob = numbers_at_length_VA$nfish/sum_nfish
numbers_at_length_VA$region = "VA"
numbers_at_length_VA$year = "y2"


#combine the datasets
fitted_sizes_region_all_y2 = as.data.frame(bind_rows(numbers_at_length_MA, numbers_at_length_RI,
                                                     numbers_at_length_CT, numbers_at_length_NY,
                                                     numbers_at_length_NJ, numbers_at_length_DE, 
                                                     numbers_at_length_MD, numbers_at_length_VA))
fitted_sizes_region_all_y2 = subset(fitted_sizes_region_all_y2, select=c(fitted_prob, fitted_length, region, year))

fitted_sizes_region_all_y2$cdf <- ave(fitted_sizes_region_all_y2$fitted_prob, fitted_sizes_region_all_y2$region, FUN=cumsum)
# This file contains the new catch-at-length distribution for the prediction year
write_xlsx(fitted_sizes_region_all_y2,"sf_fitted_sizes_y2plus.xlsx")

