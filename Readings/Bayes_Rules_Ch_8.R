# Add the path to the broom.mixed submodule
#.libPaths("../dependencies/broom.mixed")

# Load packages
library(bayesrules)
library(tidyverse)
library(rstan)
library(bayesplot)
library(broom.mixed)
library(janitor)

# Load data
data("moma_sample")

# Among the sampled artists, Y= 14 are Gen X or younger:
moma_sample %>% 
    group_by(genx) %>% 
    tally()
# A tibble: 2 x 2

# The evolution in our understanding of π is exhibited in Figure 8.1. 
# Whereas we started out with a vague understanding that under half of 
# displayed artists are Gen X, the data has swayed us toward some certainty 
# that this figure likely falls below 25%.
# Y|π∼Bin(100,π)
# π∼Beta(4,6)
plot_beta_binomial(alpha = 4, beta = 6, y = 14, n = 100)


