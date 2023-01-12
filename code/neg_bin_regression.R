library(rethinking)
library(tidyverse)
library(MASS)
data("Hurricanes")
df = Hurricanes

# poisson assumption
mean(df$deaths)
var(df$deaths)
sprintf("mean = %1.2f, var = %1.2f", mean(df$deaths), var(df$deaths))

# negative binomial regression
model_nb1 = glm.nb(deaths ~ femininity, data=df)
summary(model_nb1)

model_nb2 = glm.nb(deaths ~ femininity + min_pressure + damage_norm, data=df)
summary(model_nb2)
exp(cbind(Coefficient = coef(model_nb2), confint(model_nb2)))

model_nb3 = glm.nb(deaths ~ min_pressure + damage_norm, data=df)
anova(model_nb2, model_nb3)

# poisson regression
model_pois1 = glm(deaths ~ femininity, family="poisson", data=df)
summary(model_pois1)

model_pois2 = glm(deaths ~ femininity + min_pressure + damage_norm, family="poisson", data=df)
summary(model_pois2)
exp(cbind(Coefficient = coef(model_pois2), confint(model_pois2)))

# compare among distributions
anova(model_nb1, model_nb2)

anova(model_pois1, model_pois2)

# compare distributions
pchisq(2*(logLik(model_nb1) - logLik(model_pois1)), df=1, lower.tail=FALSE)

pchisq(2*(logLik(model_nb2) - logLik(model_pois2)), df=1, lower.tail=FALSE)

