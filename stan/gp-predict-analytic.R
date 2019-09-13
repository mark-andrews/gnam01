library(rstan)

stan_dat <- read_rdump('gp-predict.data.R')

fit_predict_analytic <- stan(file="gp-predict-analytic.stan",   
                             data=stan_dat,
                             iter=200, chains=3)

y2 <- rstan::extract(fit_predict_analytic, pars = c('y2'))$y2
print(fit_predict_analytic, pars = c('rho','alpha','sigma'))

# plot fits vs. simulated value from which fits drawn
df1 <- data.frame(x1 = stan_dat$x1,
                  y1 = stan_dat$y1)

df2 <- data.frame(x=stan_dat$x2,
                  y=colMeans(y2))


ggplot() + geom_point(data=df1,mapping=aes(x = x1, y = y1) ) + 
  geom_point(data=df2,mapping=aes(x = x, y = y), col='red' )

