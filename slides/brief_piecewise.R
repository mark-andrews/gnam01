## ----setup, include=FALSE, message=FALSE---------------------------------
knitr::opts_chunk$set(echo = FALSE)
library(tidyverse)
library(modelr)
theme_set(theme_classic())
library(rstan)

## ---- out.width='0.75\\textwidth',fig.align='center'---------------------
tent_map <- function(x, a_1, b_1, b_2, x_c){
  (x <= x_c) * (a_1 + b_1 * x) + 
    (x > x_c) * ((a_1 + x_c * (b_1 - b_2)) + b_2 * x)
}
    
a_1 <- 2.0; b_1 <- 1.25; b_2 <- -2.25; x_c <- 0.5

tent_df <- tibble(x = seq(-2, 2, length.out = 50),
                  y = tent_map(x, a_1, b_1, b_2, x_c) + rnorm(length(x), sd = 0.5)
) 

ggplot(tent_df,
       mapping = aes(x = x, y = y)
) + geom_point()


## ---- echo=T-------------------------------------------------------------
M_tent <- nls(y ~ (x <= x_c) * (a_1 + b_1 * x) + 
              (x > x_c) * ((a_1 + x_c * (b_1 - b_2)) + b_2 * x),
              start = list(a_1 = 0, b_1 = 2, b_2 = -2, x_c = 0),
              data = tent_df)


## ------------------------------------------------------------------------
tent_df %>%
  add_predictions(M_tent) %>% 
  ggplot(aes(x = x)) +
  geom_point(aes(y = y), size = 0.5) +
  geom_line(aes(y = pred), colour = 'red')




            