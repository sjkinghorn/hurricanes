library(rethinking)
library(tidyverse)
data("Hurricanes")
df = Hurricanes

# eda
head(df)
str(df)
summary(df$femininity)
table(df$female)
ggplot( df, aes( x=deaths ) ) + geom_histogram( bins=20 ) + labs(title='Distribution of Deaths')
ggplot(data=df, mapping = aes(x=femininity, y=deaths, color=female)) + geom_point()
ggplot(data=df, mapping = aes(x=femininity, y=damage_norm, color=female)) + geom_point()
ggplot(data=df, mapping = aes(x=factor(female), y=deaths)) + geom_boxplot()
ggplot(data=df, mapping = aes(x=factor(female), y=damage_norm)) + geom_boxplot()
ggplot(data=df, mapping = aes(x=factor(category), y=femininity)) + geom_boxplot()

