egg_hatch_data <- read.csv(here("data","egg_hatch_data.csv"))

egg_hatch_data <- egg_hatch_data |>
  mutate(unhatched_eggs = number_of_eggs - number_of_larvae)

binomial_model <- glm(
  cbind(number_of_larvae, unhatched_eggs) ~ cross,
  family = binomial,
  data = egg_hatch_data
)

summary(binomial_model)


performance::check_overdispersion(binomial_model)

performance::check_model(binomial_model)

#Using Random Effects to Account for Overdispersion
#It is thought that random effects are causing overdispersion in the model

library(lme4)

# Random intercept for replicate only
binomial_mixed_rep <- glmer(
  cbind(number_of_larvae, unhatched_eggs) ~ cross + (1|replicate),
  family = binomial,
  data = egg_hatch_data
)

# Random intercepts for replicate AND female (nested)
binomial_mixed_rep_fem <- glmer(
  cbind(number_of_larvae, unhatched_eggs) ~ cross + (1|replicate/female_num),
  family = binomial,
  data = egg_hatch_data
)

#Compare both models
AIC(binomial_model, binomial_mixed_rep, binomial_mixed_rep_fem)
#binomial_mixed_rep_fem is the best model
#Overdispersion in best model checked
performance::check_overdispersion(binomial_mixed_rep_fem)
#No overdispersion detected

#random effects examined
summary(binomial_mixed_rep_fem)


#visualising results
ggpredict(binomial_mixed_rep_fem, terms = "cross") |>
  plot()
