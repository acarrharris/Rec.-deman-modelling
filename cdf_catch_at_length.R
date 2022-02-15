

cdf = data.frame(read_excel("C:/Users/andrew.carr-harris/Dropbox/NMFS/fluke_mse/simulation_R_code/coastwide_emp_cd_star.xlsx"))   

X = cdf$l_in_bin
X = log(X)
Y = cdf$CDF_star

# use nlm to find parameters that minimize the squared differences 
# from your observed Y values and the expected for a normal distribution.

plot(X,Y)

fn <- function(x) {
  mu <- x[1];
  sigma <- exp(x[2])
  sum((Y-pnorm(X,mu,sigma))^2)
}
est <- nlm(fn, c(1,1))$estimate
cdf_fit <- data.frame(curve(pnorm(x, est[1], exp(est[2])), add=T))
cdf_fit[,1] <- exp(cdf_fit[,1])

format(cdf_fit$x, scientific=F)

cdf_fit$ylag= lag(cdf_fit$y, n = 1L, default = NA)
cdf_fit$prob= cdf_fit$y-cdf_fit$ylag
cdf_fit[1,4]= cdf_fit[1,2]
plot(cdf_fit$x,cdf_fit$prob)


cdf_star = subset(cdf_fit, select = c(x, prob))
names(cdf_star)[names(cdf_star) == "x"] = "fitted_length"
names(cdf_star)[names(cdf_star) == "prob"] = "fitted_prob"

cdf_star[is.na(cdf_star)] = 0
sum(cdf_star$fitted_prob )

write_xlsx(cdf_star,"cdf_star_coastwide.xlsx")





##############
#CDf star by state 
states_list = list()


cdf = data.frame(read_excel("cdf_star_by_state.xlsx"))   

statez=as.factor(cdf$state)

levels(statez)

for(s in levels(statez)){
  cdf_state = subset(cdf, state == s)
  
  
X = cdf_state$l_in_bin
X = log(X)
Y = cdf_state$cdf_star

# use nlm to find parameters that minimize the squared differences 
# from your observed Y values and the expected for a normal distribution.

plot(X,Y)

fn <- function(x) {
  mu <- x[1];
  sigma <- exp(x[2])
  sum((Y-pnorm(X,mu,sigma))^2)
}
est <- nlm(fn, c(1,1))$estimate
cdf_fit <- data.frame(curve(pnorm(x, est[1], exp(est[2])), add=T))
cdf_fit[,1] <- exp(cdf_fit[,1])

format(cdf_fit$x, scientific=F)

cdf_fit$ylag= lag(cdf_fit$y, n = 1L, default = NA)
cdf_fit$prob= cdf_fit$y-cdf_fit$ylag
cdf_fit[1,4]= cdf_fit[1,2]
plot(cdf_fit$x,cdf_fit$prob)


cdf_star = subset(cdf_fit, select = c(x, prob))
names(cdf_star)[names(cdf_star) == "x"] = "fitted_length"
names(cdf_star)[names(cdf_star) == "prob"] = "fitted_prob"

cdf_star[is.na(cdf_star)] = 0
sum(cdf_star$fitted_prob )
cdf_star$state<-s

states_list[[s]] = cdf_star


}
states_list_all= list.stack(states_list, fill=TRUE)


write_xlsx(states_list_all,"cdf_star_smoothed_by_state.xlsx")


