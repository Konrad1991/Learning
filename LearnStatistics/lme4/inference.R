library(lme4)
library(lmerTest)
library(emmeans)
library(permutes)
library(robustlmm)
library(ggplot2)

### 1. Parametric linear mixed model
m_lmm <- lmerTest::lmer(
  Reaction ~ Days + (Days | Subject),
  data = sleepstudy
)
summary(m_lmm)
anova(m_lmm)

### 2. emmeans
emm_days <- emmeans(m_lmm, ~ Days)
emm_days
# Since Days is continuous, this tests the linear trend
emtrends(m_lmm, ~ 1, var = "Days")
# For categorical examples, we create a grouped version of `Days`.
sleepstudy$DayGroup <- factor(ifelse(sleepstudy$Days <= 4, "early", "late"))
m_group <- lmerTest::lmer(
  Reaction ~ DayGroup + (1 | Subject),
  data = sleepstudy
)
anova(m_group)
emmeans(m_group, ~ DayGroup)
emmeans(
  m_group,
  pairwise ~ DayGroup,
  adjust = "tukey"
)

### 3. permutes
m_perm <- perm.lmer(
  Reaction ~ DayGroup * Days + (Days | Subject),
  data = sleepstudy,
  np = 1000
)
m_perm

### 4. robustlmm
m_robust <- robustlmm::rlmer(
  Reaction ~ Days + (Days | Subject),
  data = sleepstudy
)
m_robust
summary(m_robust)

### 5. Compare classical and robust estimates
fixef(m_lmm)
fixef(m_robust)
data.frame(
  model = c("lmerTest", "robustlmm"),
  intercept = c(fixef(m_lmm)[1], fixef(m_robust)[1]),
  days = c(fixef(m_lmm)[2], fixef(m_robust)[2])
)

### 6. Residual diagnostics
df_plot <- data.frame(
  fitted = fitted(m_lmm),
  residuals = resid(m_lmm)
)
ggplot(df_plot, aes(fitted, residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = 2)
qqnorm(resid(m_lmm))
qqline(resid(m_lmm))
