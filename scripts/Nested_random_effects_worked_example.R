rats <- readRDS(here("data","rats.rds"))

#Sources of variation:
  #1. Measurement error within liver samples
  #2. Heterogeneity between liver sections within a rat
  #3. Individual variation between rats
  #4. Treatment effects

rats |>
  aggregate(Glycogen ~ Rat + Treatment + Liver, data = _, mean) |>
  head(12)

# WRONG - treats this as 2 rats and 3 liver types studied across all rats
rats_wrong <- lmer(Glycogen ~ Treatment + (1|Rat) + (1|Liver), data = rats)

#correct
rats_correct <- lmer(Glycogen ~ Treatment + (1|Rat/Liver), data = rats)
summary(rats_correct)

#Visualising Results
ggpredict(rats_correct, terms = "Treatment") |>
  plot()

#Visulises glycogen levels for individual rats
ggpredict(rats_correct, 
          terms = c("Treatment", "Rat"),
          type = "random") |>
  plot(show_data = TRUE)
