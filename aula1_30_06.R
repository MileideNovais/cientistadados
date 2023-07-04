datasets::chickwts

df<- chickwts

table(df$feed)

boxplot(df$weight ~ df$feed)

library(dplyr)

df %>%
  group_by(feed) %>%
  
  summarise(crescimento_medio = mean (weight),
            sd = sd(weight))

library(ggplot2)

df %>% ggplot(aes(x = weight)) + 
                geom_histogram(bins = 7, col= 'blue') +
                facet_wrap(~feed)
boxplot(df$weight ~ df$feed)
anova<- aov(df$weight ~ df$feed) 

#post -hoc
anova %>% TukeyHSD() %>% plot()

pairwise.t.test(df$weight,df$feed)



