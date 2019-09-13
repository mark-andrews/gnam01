golf_df <- read_csv('data/golf_putts.csv')

golf_dat <- within(list(),{
 J <- nrow(golf_df)
 y <- golf_df %>% pull(success)
 n <- golf_df %>% pull(attempts)
 x <- golf_df %>% pull(distance)
 R <- 0.17708333
})

M <- stan(file = 'stan/golf_putt.stan', 
          chains = 4,
          warmup = 1000,
          iter = 2000,
          cores = 2,
          data = golf_dat)

