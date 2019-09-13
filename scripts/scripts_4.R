library(gamm4)

Mg2 <- gamm4(Reaction ~ s(Days,Subject,bs='fs',k=5), 
             data = sleepstudy)

sleepstudy %>% mutate(p = predict(Mg2$gam)) %>%
  ggplot(aes(x = Days, colour = Subject)) +
  geom_point(aes(y = Reaction)) +
  geom_line(aes(y = p)) +
  facet_wrap(~Subject)

