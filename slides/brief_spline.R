
## ------------------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(modelr)
theme_set(theme_classic())


## ------------------------------------------------------------------------
b_spline <- function(x, knots, show_piece = F){
  
  stopifnot(length(knots) == 5)
  
  .b_spline <- function(x){
    if (x >= knots[1] & x < knots[2]) {
      piece <- 1
      u <- (x-knots[1])/(knots[2] - knots[1])
      y <- 1/6 * u^3 
      
    } else if (x >= knots[2] & x < knots[3]) {
      piece <- 2
      u <- (x-knots[2])/(knots[3] - knots[2])
      y <- 1/6 * (1 + 3*u + 3*u^2 - 3*u^3)
      
    } else if (x >= knots[3] & x < knots[4]) {
      piece <- 3
      u <- (x-knots[3])/(knots[4] - knots[3])
      y <- 1/6 * (4 - 6*u^2 + 3*u^3)
      
    } else if (x >= knots[4] & x <= knots[5]) {
      piece <- 4
      u <- (x-knots[4])/(knots[5] - knots[4])
      y <- 1/6 * (1 - 3*u + 3*u^2 - u^3)
    }
    else {
      piece <- 0
      y <- 0 
    } 
    
    if (!show_piece) return(y)
    
    c(y, piece)
  
  }
  
  if (!show_piece){
    tibble(x = x, 
           y = map_dbl(x, .b_spline)
    )
  } else {
    map(x, .b_spline) %>% 
      do.call(rbind, .)%>%
      set_colnames(c('y', 'segment')) %>% 
      as_tibble() %>% 
      mutate(x = x) %>% 
      mutate_at(vars(segment), as.factor) %>% 
      select(x, everything())
  }
  
}


## ------------------------------------------------------------------------
x <- seq(-1, 1, length.out = 1000)
knots <- seq(-0.5, 0.5, length.out = 5)
b_spline(x, knots = knots, show_piece = T) %>%
  ggplot(mapping = aes(x = x, 
                       y = y, 
                       colour = segment)) +
  geom_point(size = 0.5) +
  theme(legend.position="none")


## ------------------------------------------------------------------------
library(splines)


bs(x, knots  = knots) %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  mutate(x=x) %>% 
  gather(k, y, -x) %>% ggplot(mapping = aes(x = x, y = y, group = k)) + geom_line()




## ------------------------------------------------------------------------
rbspline <- function(x, knots){
  tibble(x = x,
         y = bs(x, knots = knots) %*% rnorm(length(knots) + 3) %>% 
           as.vector()
  )
  
}

rbspline_examples <- function(i){
  set.seed(i)
  Df <- imap(rerun(5, rbspline(x, knots)), 
             ~mutate(., example = .y)) %>% 
    bind_rows() %>% 
    mutate_at(vars(example), as.character)
  
  p <- Df %>% ggplot(mapping = aes(x = x, y = y, colour = example)) + 
    geom_line() +
    theme(legend.position="none")
  
  p
}


rbspline_examples(1010)


## ------------------------------------------------------------------------
rbspline_examples(101)


## ------------------------------------------------------------------------
rbspline_examples(1201)


## ------------------------------------------------------------------------
rbspline_examples(1013)


## ------------------------------------------------------------------------
eyefix_df <- read_csv('data/funct_theme_pts.csv')

eyefix_df_avg <- eyefix_df %>% 
  group_by(Time, Object) %>% 
  summarize(mean_fix = mean(meanFix)) %>% 
  ungroup()

eyefix_df_avg_targ <- filter(eyefix_df_avg, Object == 'Target')



## ---- echo=T-------------------------------------------------------------
library(splines)
knots <- seq(-500, 2500, by = 500)
M_bs <- lm(mean_fix ~ bs(Time, knots = knots)*Object, 
           data=eyefix_df_avg)


## ------------------------------------------------------------------------
eyefix_df_avg%>%
  add_predictions(M_bs) %>%
  ggplot(mapping = aes(x = Time, group = Object, colour = Object)) +
  geom_point(aes(y = mean_fix), size = 0.5, alpha = 0.5) +
  geom_line(aes(y = pred))

