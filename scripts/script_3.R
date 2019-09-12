
# load packages -----------------------------------------------------------

library(tidyverse)
library(magrittr)
library(mgcv)

# golf data ---------------------------------------------------------------


golf_df <- read_csv('golf_putts.csv') %>% 
  mutate(failure = attempts - success,
         p = success/attempts)

M <- glm(cbind(success, failure) ~ distance,
         family = binomial,
         data = golf_df)

golf_df %>% 
  add_predictions(M, type = 'response') %>% 
  ggplot(aes(x = distance)) +
  geom_point(aes(y = p)) +
  geom_line(aes(y = pred))

# With a gam

Mg <- gam(cbind(success, failure) ~ s(distance, sp = 10),
          family = binomial,
          data = golf_df,
          method = "REML")

golf_df %>% 
  add_predictions(Mg, type = 'response') %>% 
  ggplot(aes(x = distance)) +
  geom_point(aes(y = p)) +
  geom_line(aes(y = pred))

# affairs data model ------------------------------------------------------

affairs_df <- read_csv('affairs.csv') %>% 
  mutate(cheater = affairs > 0,
         gender = factor(gender))

affairs_df %>% 
  group_by(yearsmarried) %>% 
  summarise(ncheat = sum(cheater),
            n = n(), 
            pcheat = ncheat/n) %>% 
  ggplot(aes(x = yearsmarried, y = pcheat)) + geom_point()

M <- gam(cheater ~ gender + s(yearsmarried, by = gender, k = 8, sp = 1),
         data = affairs_df,
         family = binomial,
         method = 'REML')

crossing(yearsmarried = seq(0, 20, by = 0.1), gender = c('male', 'female')) %>% 
  add_predictions(M, type = 'response') %>% 
  ggplot(aes(x = yearsmarried, y = pred, group = gender, colour = gender)) + 
  geom_line()
