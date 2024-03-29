load('mirman_gca.Rdata')

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


df <- aggregate(meanFix ~ Time + Object, 
                data=FunctThemePts, 
                FUN=mean)

condition <- df$Object
levels(condition) <- seq(1, length(levels(condition)))
condition <- as.numeric(levels(condition))[condition] 

J <- max(condition)
N <- length(condition)
fixation <- df$meanFix
time <- df$Time
centers <- seq(min(time), max(time), by=100)
K <- length(centers)

gca_data=list('time'=time, 
              'y'=fixation, 
              'centers'=centers,
              'condition'=condition,
              'J'=J,
              'N'=N, 
              'K'=K)
# data {
#   int<lower=1>  N; // num observations
#   int<lower=1>  K; // num of centers
#   int<lower=1>  J; // number of conditions	
#   
#   vector[N] time; // time points
#   int<lower=1> condition[N]; // condition
#   vector[K] centers; // time intervals
#   vector[N] y; // response vector
# }
fit <- stan(file = 'gca_rbf.2.stan', 
            data = gca_data, 
            iter = 1000, 
            chains = 3)

print(fit, pars=c('width'))
