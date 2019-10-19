# Generate random percent divergence for different primate groups.
# Uses an estimate of 0.2% per million years for cytochrome b from
# Irwin et al. 1991. J. Mol. Evol. 32: 128-144. Standard deviation
# chosen arbitrarily be me to keep the student results clean.
#
# THe overall goal is pedagogical, not complete accuracy.

library(tidyverse)

primates <- read_csv("color_absorbance/primates.csv")

# seq_divergence <- rnorm(9, 0.002, .0002)

# Results used to generate the current percentages in the file.
# 0.002061000 0.002032013 0.002263953 0.001811133 0.001926278 
#0.002143840 0.002259887 0.001945377 0.001863878

# Not necessary to run again once suitable divergences estimated.
primates$percent_divergence <- 
  round((primates$average_age * seq_divergence) * 100, 1)


base_plot <- 
  ggplot(primates,
       aes(x = percent_divergence, y = average_age)) +
  theme_bw() +
  scale_x_continuous(limits = c(2,18),
                     breaks = seq(2,18, 1),
                     expand = c(0.01, 0.1)) +
  scale_y_continuous(limits = c(10,90),
                     breaks = seq(10, 90, 5),
                     expand = c(0.01, 0.2)) +
  labs(x = "Genetic difference (%)",
       y = "Time since divergence (mya)")

ggsave(filename = "base_primate_divergence_plot.png",
       plot = base_plot)

final_plot <- 
  base_plot +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "gray50") + 
  geom_point(size = 3)

ggsave(filename = "final_primate_divergence_plot.png",
       plot = final_plot)

