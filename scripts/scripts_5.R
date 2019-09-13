ggplot(Ovary,
       aes(x = Time, y = follicles, colour = Mare)
) + geom_point() + facet_wrap(~Mare)


fm1 <- gls(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), Ovary,
           correlation = corAR1(form = ~ 1 | Mare))
