library(rethinking)
library(tidyverse)
library(MASS)
data("Hurricanes")
df = Hurricanes

# poisson assumption
mean = mean(df$deaths)
var = var(df$deaths)


# negative binomial regression
model_nb = glm.nb(deaths ~ femininity, data=df)
summary(model_nb)

# poisson regression
model_pois = glm(deaths ~ femininity, family="poisson", data=df)
summary(model_pois)

# compare
pchisq(2*(logLik(model_nb) - logLik(model_pois)), df=1, lower.tail=FALSE)
