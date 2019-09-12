
# load packages -----------------------------------------------------------

library(tidyverse)
library(magrittr)
library(mgcv)

# Understanding interactions

insul_df <- read_csv('insulation.csv')

M <- lm(Temp ~ Gas * Insul, data = insul_df)


# Spatial data ------------------------------------------------------------

meuse <- read_csv('meuse.csv')

M_1 <- gam(copper ~ s(x, y), data = meuse)

M_2 <- gam(copper ~ s(x, y) + s(elev) + s(dist), data = meuse)



# visualization -----------------------------------------------------------

plot(M_1)
plot(M_1, se=F)
plot(M_1, scheme = 1)
plot(M_1, scheme = 2)
vis.gam(M_1, view = c('x' ,'y'), plot.type = 'persp')
vis.gam(M_1, view = c('x' ,'y'), plot.type = 'contour')
vis.gam(M_1, view = c('x' ,'y'), plot.type = 'contour', too.far = 0.1)
vis.gam(M_1, view = c('x' ,'y'), plot.type = 'persp', se = 1)
vis.gam(M_1, view = c('x' ,'y'), plot.type = 'persp', theta = 100, phi = 0, r = 20.1)
vis.gam(M_1, view = c('x' ,'y'), plot.type = 'contour', nlevels = 3)


# vis cat x cont ----------------------------------------------------------

eyefix_df <- read_csv('funct_theme_pts.csv')

eyefix_df_avg <- eyefix_df %>% 
  group_by(Time, Object) %>% 
  summarize(mean_fix = mean(meanFix)) %>% 
  ungroup()

M <- gam(mean_fix ~ s(Time, Object, bs = 'fs'), data = eyefix_df_avg)

vis.gam(M, plot.type = 'persp', theta = -20)


# tensor smooths for interactions ----------------------------------------------------------

M <- gam(copper ~ te(x, y, elev), data = meuse)

M <- gam(copper ~ s(x,y) + s(elev) + ti(x, y, elev), data = meuse)
