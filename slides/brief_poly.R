## ------------------------------------------------------------------------
library(tidyverse)
library(magrittr)
theme_set(theme_classic())


## ------------------------------------------------------------------------
get_polynomial_design_matrix <- function(K=3, xmin=-10, xmax=10, N=100, rescale = T){
  
  # Produce a matrix of x, and x raised to the power of 0 to K
  # If `rescale, the values of the powers of x are scaled to be between -2 and 2
  
  rescale_f <- function(x, new_min=-1, new_max=1){
    
    new_range <- new_max - new_min
    original_range <- max(x) - min(x)
    
    x <- x * new_range/original_range
    x - (min(x) - new_min)
  }
  
  x <- seq(xmin, xmax, length.out = N)
  Df <- map(x, ~.^seq(0, K)) %>%
    do.call(rbind, .) %>% 
    set_colnames(paste0('degree_', seq(0, K))) %>% 
    as_tibble() %>% 
    mutate(x = degree_1) %>% 
    select(x, everything())
  
  if (rescale){
    Df %>% mutate_at(vars(matches('^degree_[^0]$')), 
                     ~rescale_f(., new_min = -2, new_max = 2))
  } else {
    Df
  }
}

rpolynomial <- function(K = 5){
  beta <- rnorm(K + 1)
  beta <- beta/sum(beta)
  get_polynomial_design_matrix(K = K) %>%
    mutate(y = select(., starts_with('degree')) %>% 
             apply(1, function(x) sum(x*beta))
    ) %>% select(x, y) 
}

rpolynomial_examples <- function(i){
  set.seed(i)
  Df <- imap(rerun(5, rpolynomial(K = 5)), 
             ~mutate(., example = .y)) %>% 
    bind_rows() %>% 
    mutate_at(vars(example), as.character)
  
  p <- Df %>% ggplot(mapping = aes(x = x, y = y, colour = example)) + 
    geom_line() +
    theme(legend.position="none")
  
  p
}



## ------------------------------------------------------------------------
get_polynomial_design_matrix(K=5) %>% 
  gather(degree, y, starts_with('degree')) %>% 
  ggplot(mapping = aes(x = x, y = y, colour = degree)) + geom_line()



## ------------------------------------------------------------------------
rpolynomial_examples(110)


## ------------------------------------------------------------------------
rpolynomial_examples(114)


## ------------------------------------------------------------------------
rpolynomial_examples(116)


## ------------------------------------------------------------------------
rpolynomial_examples(117)


## ---- echo=T, eval=F-----------------------------------------------------
## eyefix_df <- read_csv('funct_theme_pts.csv')


## ------------------------------------------------------------------------
eyefix_df <- read_csv('data/funct_theme_pts.csv')

eyefix_df_avg <- eyefix_df %>% 
  group_by(Time, Object) %>% 
  summarize(mean_fix = mean(meanFix)) %>% 
  ungroup()

eyefix_df_avg_targ <- filter(eyefix_df_avg, Object == 'Target')

eyefix_df_avg %>% 
  ggplot(mapping = aes(x = Time, y = mean_fix, colour = Object)) +
  geom_point() 


