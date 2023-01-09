library(rethinking)
library(tidyverse)
data("Hurricanes")
df = Hurricanes

# poisson regression

model_pois = glm(deaths ~ femininity, family="poisson", data=df)
summary(model_pois)

# poisson regression (bayesian)
dat = list(
  Y = df$deaths,
  x = df$femininity
)

model1 <- quap(
  alist(
    Y ~ dpois(mu),
    log(mu) <- a,
    a ~ dnorm(4,1)
  ), data=dat,
)

precis(model1,corr=TRUE)

model2 <- quap(
  alist(
    Y ~ dpois(mu),
    log(mu) <- a+b1*x,
    a~ dnorm(4,1),
    b1 ~ dnorm(0,1)
  ), data=dat,
)

precis(model2)
plot(precis(model2))
post <- extract.samples(model2)
dens(post$b1)

compare(model1, model2)


df$deaths_est <- exp(mean(post$a) + mean(post$b1) * df$femininity)
plot(df$deaths, col=rangi2, xlab="Hurricane", ylab="Death Count")
points(df$deaths_est)
abline(h=mean(df$deaths), lty=2, col="red")