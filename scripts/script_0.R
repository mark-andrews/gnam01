
# Load packages -----------------------------------------------------------

library(mgcv)
library(tidyverse)

# Load data ---------------------------------------------------------------

mcycle <- MASS::mcycle

# Normal linear model -----------------------------------------------------

M_0 <- gam(accel ~ times, data = mcycle)

# Spline normal model -----------------------------------------------------

M_1 <- gam(accel ~ s(times), data = mcycle)

# Summarize etc
coef(M_1)
summary(M_1)
plot(M_1, residuals = T)

# Model comparison --------------------------------------------------------

map_dbl(list(linear=M_0, spline=M_1), AIC)

# Contol number of bases --------------------------------------------------

M_2 <- gam(accel ~ s(times, k = 20), data = mcycle)

map_dbl(list(M_1, M_2), AIC)


# Control smoothing penalty -----------------------------------------------

M_3 <- gam(accel ~ s(times, sp = 0.001), data = mcycle)

map_dbl(list(M_1, M_3), AIC)

