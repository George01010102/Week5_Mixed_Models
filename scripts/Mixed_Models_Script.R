library(tidyverse)
library(readr)
library(here)
benzo_data <- read.csv(here("data","benzo_data.csv"))
getwd()
view(benzo_data)


ggplot(benzo_data, aes(x = benzo_um, y = detox_exp)) +
  geom_point(alpha = 0.6) +
  labs(
    x = "Toxin concentration (µM)",
    y = "Detoxification capacity"
  ) +
  geom_smooth(method = "lm", se = TRUE)
#plots data ignoring group structure
#Graph shows positive relationship


ggplot(benzo_data, aes(x = benzo_um, y = detox_exp, colour = group)) +
  geom_point(alpha = 0.6) +
  labs(
    x = "Toxin concentration (µM)",
    y = "Detoxification capacity"
  ) +
  theme(legend.position = "right")
#plots data points colored by experimental batch
#shows that groups differ substantially in baseline detox capacity


ggplot(benzo_data, aes(x = group, y = detox_exp, fill = factor(group))) +
  geom_boxplot() +
  labs(
    x = "Experimental batch",
    y = "Detoxification capacity"
  ) +
  theme(legend.position = "none")
#boxplot shows how detoxification capacity differs between experimental batches
#group 3 has substantially higher detoxification


pooled_model <- lm(detox_exp ~ benzo_um, data = benzo_data)
summary(pooled_model)
#Pooled model, treats all observations as independent. Ignores groups
#This model has standard errors that are artificially small because 430 observations
#are treated as wrongly independent, which inflates estimate confidence


fixed_model <- lm(detox_exp ~ benzo_um + group, data = benzo_data)
summary(fixed_model)
#Includes group as a predictor
#A fixed model works best with <5 groups, with groups having large balanced sample sizes
#this model works if you want to make inferences about these specific batches only

#Limitations of model:
  #Cannot make inferences to future batches
  #uses many dfs when estimating group differences
  #Struggles with unbalanced designs or missing data


library(lmerTest)

mixed_model <- lmer(detox_exp ~ benzo_um + (1 | group), data = benzo_data)
summary(mixed_model)
#Models group to group variation as random
#The notation (1|group) specifies a random intercept - each group has its own baseline value,
#but the slope (effect of toxin) is assumed constant.
#Experimental batch accounts for 67% of the variation not explained by toxin concentration
#(shown by random effects model)
#effect of toxin concentration is 2.03 units increase in detoxification per µM
#increase in toxin (95% CI: 1.69–2.36) (shown by fixed effects)


#Model summary:
#Pooled: Intercept SE is unrealistically small (ignores batch variation)
#Fixed: Smallest SE, but only describes Group 1
#Mixed: Larger intercept SE (correctly reflects batch uncertainty), reasonable slope SE


#Mixed models dont often change conclusions about fixed effects dramatically

#Fixed-effect models are perfectly adequate when:
  #Few groups (< 5) with balanced designs
  #Large sample sizes per group
  #No interest in generalisation beyond these specific groups
  #Complete data with no missing values


library(emmeans)

emmeans(mixed_model, 
        specs = "benzo_um", 
        at = list(benzo_um = c(0, 2.5, 5, 7.5, 10)))
#emmeans can produce mixed models as well as fixed models

library(ggplot2)
library(pbkrtest)
emmeans(mixed_model, 
        specs = "benzo_um", 
        at = list(benzo_um = seq(0, 10, 0.5))) |>
  as.data.frame() |>
  ggplot(aes(x = benzo_um, y = emmean)) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.3) +
  geom_line(linewidth = 1) +
  labs(
    x = "Toxin concentration (µM)",
    y = "Detoxification capacity",
    title = "Population-average predictions"
  )
#Visualises marginal means at different toxin concs



library(ggeffects)

# Population-level predictions (equivalent to emmeans)
ggpredict(mixed_model,
          terms = "benzo_um",
          type = "fixed"
) |>
  plot(show_data = TRUE) +
  labs(
    x = "Toxin concentration (µM)",
    y = "Detoxification capacity"
  )
#Model above shows predictions for individual batches, not grouped
#Mixed models allow us to predict responses for specific groups, this is shown below

ggpredict(mixed_model,
          terms = c("benzo_um", "group"),
          type = "random"
) |>
  plot(show_data = TRUE) +
  facet_wrap(~group) +
  labs(
    x = "Toxin concentration (µM)",
    y = "Detoxification capacity"
  )
#these graphs show that data groups share a similar slope, but a different intercept.
#Mixed models allows for analysis of intercept variation between groups


#We can assess overall model performance using R² values:
library(MuMIn)
r.squaredGLMM(mixed_model)
#R²m (marginal): Variance explained by fixed effects alone (toxin concentration) = 10%
#R²c (conditional): Variance explained by full model (fixed + random effects) = 70%
