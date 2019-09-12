
# Load library ------------------------------------------------------------
library(mgcv)
library(tidyverse)
library(magrittr)

# Load data ------------------------------------------------------------
eyefix_df <- read_csv('funct_theme_pts.csv')

eyefix_df_avg <- eyefix_df %>% 
  group_by(Time, Object) %>% 
  summarize(mean_fix = mean(meanFix)) %>% 
  ungroup()

eyefix_df_avg_targ <- filter(eyefix_df_avg, Object == 'Target')


# Varying intercepts ------------------------------------------------------

M <- gam(mean_fix ~ s(Time) + Object, data = eyefix_df_avg)

crossing(Time = seq(-1000, 3000, by = 100), 
         Object = c('Target', 'Competitor', 'Unrelated')) %>% 
  add_predictions(M) %>% 
  ggplot(aes(x = Time, y = pred, group = Object, colour = Object)) +
  geom_line()



# The right way -----------------------------------------------------------
eyefix_df_avg %<>% mutate(Object = factor(Object))
M <- gam(mean_fix ~ Object + s(Time, by =Object), data = eyefix_df_avg)
M <- gam(mean_fix ~ s(Time, Object, bs = 'fs'), data = eyefix_df_avg)

crossing(Time = seq(-1000, 3000, by = 100), 
         Object = c('Target', 'Competitor', 'Unrelated')) 

eyefix_df_avg %>% 
  add_predictions(M) %>% 
  ggplot(aes(x = Time, group = Object, colour = Object)) +
  geom_point(aes(y = mean_fix), size = 0.5, alpha = 0.5) +
  geom_line(aes(y = pred))

# Check fit -------------------------------------------------------------

M <- gam(mean_fix ~ Object + s(Time, by =Object, k = 20), data = eyefix_df_avg)
gam.check(M)

# Check concurvity --------------------------------------------------------------

concurvity(M)
