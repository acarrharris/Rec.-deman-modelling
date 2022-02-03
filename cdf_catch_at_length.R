

cdf = data.frame(read_excel("C:/Users/andrew.carr-harris/Dropbox/NMFS/fluke_mse/simulation_R_code/cdf_star.xlsx"))   

X = cdf$l_in_bin
Y = cdf$cdf_star

# use nlm to find parameters that minimize the squared differences 
# from your observed Y values and the expected for a normal distribution.

plot(X,Y)

fn <- function(x) {
  mu <- x[1];
  sigma <- exp(x[2])
  sum((Y-pnorm(X,mu,sigma))^2)
}
est <- nlm(fn, c(1,1))$estimate
curve(pnorm(x, est[1], exp(est[2])), add=T)


cdf_fit <- data.frame(curve(pnorm(x, est[1], exp(est[2])), add=T,from=0, to=31, n=32))
format(cdf_fit$y, scientific=F)
cdf_fit$y[cdf_fit$x==19]

cdf_fit$ylag= lag(cdf_fit$y, n = 1L, default = NA)
cdf_fit$prob= cdf_fit$y-cdf_fit$ylag
plot(cdf_fit$x,cdf_fit$prob)



cdf_star = subset(cdf_fit, select = c(x, prob))
names(cdf_star)[names(cdf_star) == "x"] = "fitted_length"
names(cdf_star)[names(cdf_star) == "prob"] = "fitted_prob"

cdf_star[is.na(cdf_star)] = 0
sum(cdf_star$fitted_prob )

write_xlsx(cdf_star,"cdf_star.xlsx")




#Combine CDF* and Numbers at length cdf 
cdf = data.frame(read_excel("C:/Users/andrew.carr-harris/Dropbox/NMFS/fluke_mse/simulation_R_code/cdf_star.xlsx"))   




