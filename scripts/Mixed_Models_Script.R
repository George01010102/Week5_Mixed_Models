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

summarise(benzo_data)
