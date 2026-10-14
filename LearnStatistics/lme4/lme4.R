library(lme4)
data(sleepstudy)
head(sleepstudy)

m <- lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
# (1 | Subject) --> each person gets one intercept
summary(m)
equatiomatic::extract_eq(m)

m <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
# (Days | Subject) --> each person gets one intercept and own slope
summary(m)

plot(m)
