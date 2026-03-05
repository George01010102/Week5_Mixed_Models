library(tidyr)
library(tidyverse)
library(readr)
library(here)

dolphins <- read_csv(here("data","dolphins.csv")) |>
  mutate(direction = factor(direction)) |>
  drop_na()

summarise(dolphins)

#Fitting the Model
library(lmerTest)

dolphmod <- lmer(vt ~ bodymass + direction + (1|animal), data = dolphins)
summary(dolphmod)


#Visualising Predictions
library(emmeans)

# Predictions at mean body mass for each breath direction
emmeans(dolphmod, specs = "direction")

# Visualise the body mass effect
emmeans(dolphmod, 
        specs = "bodymass",
        at = list(bodymass = seq(130, 260, 10))) |>
  as.data.frame() |>
  ggplot(aes(x = bodymass, y = emmean)) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.3) +
  geom_line(linewidth = 1) +
  labs(
    x = "Body mass (kg)",
    y = "Tidal volume (L)"
  )


#Comparing breath directions:
library(ggeffects)

ggpredict(dolphmod, 
          terms = c("bodymass", "direction")) |>
  plot(show_data = TRUE) +
  labs(
    x = "Body mass (kg)",
    y = "Tidal volume (L)"
  )

