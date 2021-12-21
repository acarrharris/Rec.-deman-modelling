#The following creates an cathc-per-trip dataset adjusted to reflect the population size

###Northern states
catch_data <- read_excel("observed_catch_NO_19.xlsx")

sf <- catch_data$sf_tot_cat
bsb <- catch_data$bsb_tot_cat


#estimate the nb parameters
nbfit_sf = fitdistr(sf, "Negative Binomial")
summary(nbfit_sf)

sf_mu <- nbfit_sf$estimate['mu']
sf_mu

sf_size <- nbfit_sf$estimate['size']
sf_size


# Now we want to adjust mean catch per trip by the expansion factor. 
# We will assume that in the prediction year under a new mean catch per trip, 
# the variance relative to the mean remains as it was in the baseline year. 

# From ?NegBinomial:
# An alternative parametrization (often used in ecology) is by the mean mu (see above), 
# and size, the dispersion parameter, where prob = size/(size+mu). The variance is mu + mu^2/size 
# in this parametrization. 

var_sf=sf_mu+(sf_mu^2)/sf_size
cv_sf_base = sqrt(var_sf)/sf_mu
cv_sf_base

# New mean catch per trip
sf_mu_new=sf_mu*catch_expansion_factor_NO

#solve for new size parameter 
sf_size_new=(sf_mu_new^2)/(((cv_sf_base*sf_mu_new)^2)-sf_mu_new)

#new variance and CV
var_sf_new=sf_mu_new+(sf_mu_new^2)/sf_size_new
cv_sf_new = sqrt(var_sf_new)/sf_mu_new

#Check that CV old and CV new are the same
cv_sf_base
cv_sf_new


# Get the black sea bass parameters 
nbfit_bsb <- fitdistr(bsb, "Negative Binomial")
nbfit_bsb

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


# Set the parameters
rho <- coef(fit)[1]
df <- coef(fit)[2]


t_copula_nb <- mvdc(copula=tCopula(rho,dim=2,df=df),  margins = c("nbinom","nbinom"), 
                    paramMargins=list(list(mu=sf_mu_new, size=sf_size_new),
                                      list(mu=bsb_mu, size=bsb_size)))


sim_t_cop_nb <- rMvdc(30000, t_copula_nb )

sf_t_nb=sim_t_cop_nb[,1]
bsb_t_nb=sim_t_cop_nb[,2]

mean(sf_t_nb)

region="NO"
catch_data_sim=data.frame(sf_t_nb, bsb_t_nb, region)
write_xlsx(catch_data_sim, "predicted_catch_NO.xlsx") 


###New Jersey
catch_data <- read_excel("observed_catch_NJ_19.xlsx")

sf <- catch_data$sf_tot_cat
bsb <- catch_data$bsb_tot_cat


#estimate the nb parameters
nbfit_sf = fitdistr(sf, "Negative Binomial")
nbfit_sf

sf_mu <- nbfit_sf$estimate['mu']
sf_mu

#Here, scale sf_mu by the scale factor 
sf_mu=sf_mu*catch_expansion_factor_NJ
sf_mu

sf_size <- nbfit_sf$estimate['size']
sf_size

nbfit_bsb <- fitdistr(bsb, "Negative Binomial")
nbfit_bsb

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


# Set the parameters
rho <- coef(fit)[1]
df <- coef(fit)[2]


t_copula_nb <- mvdc(copula=tCopula(rho,dim=2,df=df),  margins = c("nbinom","nbinom"), 
                    paramMargins=list(list(mu=sf_mu, size=sf_size),
                                      list(mu=bsb_mu, size=bsb_size)))

sim_t_cop_nb <- rMvdc(30000, t_copula_nb )

sf_t_nb=sim_t_cop_nb[,1]
bsb_t_nb=sim_t_cop_nb[,2]

mean(sf_t_nb)

region="NJ"
catch_data_sim=data.frame(sf_t_nb, bsb_t_nb, region)
write_xlsx(catch_data_sim, "predicted_catch_NJ.xlsx") 



###Southern states
catch_data <- read_excel("observed_catch_SO_19.xlsx")

sf <- catch_data$sf_tot_cat
bsb <- catch_data$bsb_tot_cat


#estimate the nb parameters
nbfit_sf = fitdistr(sf, "Negative Binomial")
nbfit_sf

sf_mu <- nbfit_sf$estimate['mu']
sf_mu

#Here, scale sf_mu by the scale factor 
sf_mu=sf_mu*catch_expansion_factor_SO
sf_mu

sf_size <- nbfit_sf$estimate['size']
sf_size

nbfit_bsb <- fitdistr(bsb, "Negative Binomial")
nbfit_bsb

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


# Set the parameters
rho <- coef(fit)[1]
df <- coef(fit)[2]


t_copula_nb <- mvdc(copula=tCopula(rho,dim=2,df=df),  margins = c("nbinom","nbinom"), 
                    paramMargins=list(list(mu=sf_mu, size=sf_size),
                                      list(mu=bsb_mu, size=bsb_size)))

sim_t_cop_nb <- rMvdc(30000, t_copula_nb )

sf_t_nb=sim_t_cop_nb[,1]
bsb_t_nb=sim_t_cop_nb[,2]

mean(sf_t_nb)

region="SO"
catch_data_sim=data.frame(sf_t_nb, bsb_t_nb, region)
write_xlsx(catch_data_sim, "predicted_catch_SO.xlsx")
