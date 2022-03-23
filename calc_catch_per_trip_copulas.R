#The following creates an cathc-per-trip dataset adjusted to reflect the population size

###Northern states
catch_data <- read_excel("observed_catch_NO_19.xlsx")
#catch_data <- readRDS("observed_catch_NO_19.rds")

sf <- catch_data$sf_tot_cat
bsb <- catch_data$bsb_tot_cat

cor(sf, bsb, method = c("kendall"))

#estimate the nb parameters
nbfit_sf = fitdistr(sf, "Negative Binomial")
saveRDS(nbfit_sf, "nb_catch_parameters_sf_NO_19.rds")


sf_mu <- nbfit_sf$estimate['mu']
sf_mu

sf_size <- nbfit_sf$estimate['size']
sf_size

nbfit_bsb <- fitdistr(bsb, "Negative Binomial")
saveRDS(nbfit_bsb, "nb_catch_parameters_bsb_NO_19.rds")

bsb_mu <- nbfit_bsb$estimate['mu']
bsb_mu
bsb_size <- nbfit_bsb$estimate['size']
bsb_size

#t copula
t_cop_model <- tCopula(dim = 2)
m <- pobs(as.matrix(cbind(sf,bsb)))
fit <- fitCopula(t_cop_model, m, method = 'ml')
fit
coef(fit)

saveRDS(fit, "catch_copula_NO_19.rds")


rho <- coef(fit)[1]
df <- coef(fit)[2]



t_copula_nb <- mvdc(copula=tCopula(rho,dim=2,df=df),  margins = c("nbinom","nbinom"), 
                    paramMargins=list(list(mu=sf_mu, size=sf_size),
                                      list(mu=bsb_mu, size=bsb_size)))

sim_t_cop_nb <- rMvdc(30000, t_copula_nb )

sf_t_nb=sim_t_cop_nb[,1]
bsb_t_nb=sim_t_cop_nb[,2]

region="NO"
catch_data_sim=data.frame( sf_t_nb, bsb_t_nb, region)
mean(catch_data_sim$sf_t_nb)
mean(catch_data$sf_tot_cat)

cor(sf_t_nb, bsb_t_nb, method = c("kendall"))


write_xlsx(catch_data_sim, "NO_catch_data_sim1.xlsx") 





###New Jersey
catch_data <- read_excel("observed_catch_NJ_19.xlsx")
#catch_data <- readRDS("observed_catch_NJ_19.rds")


sf <- catch_data$sf_tot_cat
bsb <- catch_data$bsb_tot_cat

cor(sf, bsb, method = c("kendall"))

#estimate the nb parameters
nbfit_sf = fitdistr(sf, "Negative Binomial")
saveRDS(nbfit_sf, "nb_catch_parameters_sf_NJ_19.rds")


sf_mu <- nbfit_sf$estimate['mu']
sf_mu

sf_size <- nbfit_sf$estimate['size']
sf_size

nbfit_bsb <- fitdistr(bsb, "Negative Binomial")
saveRDS(nbfit_bsb, "nb_catch_parameters_bsb_NJ_19.rds")

bsb_mu <- nbfit_bsb$estimate['mu']
bsb_mu
bsb_size <- nbfit_bsb$estimate['size']
bsb_size

#t copula
t_cop_model <- tCopula(dim = 2)
m <- pobs(as.matrix(cbind(sf,bsb)))
fit <- fitCopula(t_cop_model, m, method = 'ml')
fit
coef(fit)

saveRDS(fit, "catch_copula_NJ_19.rds")


rho <- coef(fit)[1]
df <- coef(fit)[2]



t_copula_nb <- mvdc(copula=tCopula(rho,dim=2,df=df),  margins = c("nbinom","nbinom"), 
                    paramMargins=list(list(mu=sf_mu, size=sf_size),
                                      list(mu=bsb_mu, size=bsb_size)))

sim_t_cop_nb <- rMvdc(30000, t_copula_nb )

sf_t_nb=sim_t_cop_nb[,1]
bsb_t_nb=sim_t_cop_nb[,2]

region="NJ"
catch_data_sim=data.frame( sf_t_nb, bsb_t_nb, region)
mean(catch_data_sim$sf_t_nb)
mean(catch_data$sf_tot_cat)

cor(sf_t_nb, bsb_t_nb, method = c("kendall"))

write_xlsx(catch_data_sim, "NJ_catch_data_sim1.xlsx") 


###Southern states
catch_data <- read_excel("observed_catch_SO_19.xlsx")
#catch_data <- readRDS("observed_catch_SO_19.rds")

sf <- catch_data$sf_tot_cat
bsb <- catch_data$bsb_tot_cat


#estimate the nb parameters
nbfit_sf = fitdistr(sf, "Negative Binomial")
saveRDS(nbfit_sf, "nb_catch_parameters_sf_SO_19.rds")


sf_mu <- nbfit_sf$estimate['mu']
sf_mu

sf_size <- nbfit_sf$estimate['size']
sf_size

nbfit_bsb <- fitdistr(bsb, "Negative Binomial")
saveRDS(nbfit_bsb, "nb_catch_parameters_bsb_SO_19.rds")

bsb_mu <- nbfit_bsb$estimate['mu']
bsb_mu
bsb_size <- nbfit_bsb$estimate['size']
bsb_size

#t copula
t_cop_model <- tCopula(dim = 2)
m <- pobs(as.matrix(cbind(sf,bsb)))
fit <- fitCopula(t_cop_model, m, method = 'ml')
fit
coef(fit)

saveRDS(fit, "catch_copula_SO_19.rds")


rho <- coef(fit)[1]
df <- coef(fit)[2]



t_copula_nb <- mvdc(copula=tCopula(rho,dim=2,df=df),  margins = c("nbinom","nbinom"), 
                    paramMargins=list(list(mu=sf_mu, size=sf_size),
                                      list(mu=bsb_mu, size=bsb_size)))

sim_t_cop_nb <- rMvdc(30000, t_copula_nb )

sf_t_nb=sim_t_cop_nb[,1]
bsb_t_nb=sim_t_cop_nb[,2]

region="SO"
catch_data_sim=data.frame( sf_t_nb, bsb_t_nb, region)
mean(catch_data_sim$sf_t_nb)
mean(catch_data$sf_tot_cat)


write_xlsx(catch_data_sim, "SO_catch_data_sim1.xlsx") 
