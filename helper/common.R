# source me for all analyses 

library(tidyverse)
# library(lme4)
library(knitr) # for kable
# library(ggrepel)
library(ggthemes)
# library(viridis)
# library(cowplot)
# remotes::install_github("jmgirard/agreement")
# library(agreement)
# library(tictoc)
library(GGally)

# Seed for random number generation
set.seed(42)
knitr::opts_chunk$set(cache.extra = knitr::rand_seed, cache = TRUE, 
                      message=FALSE, warning=FALSE, error=FALSE)
options(dplyr.summarise.inform = FALSE)


# .font <- "Source Sans Pro"
# theme_set(theme_bw(base_size = 14, base_family = .font))
theme_set(theme_bw(base_size = 10))
theme_update(panel.grid = element_blank(),
             strip.background = element_blank(),
             legend.key = element_blank(),
             panel.border = element_blank(),
             axis.line = element_line(),
             strip.text = element_text(face = "bold"))

# helper function for scaling to the variance of a particular age group
age_scale <- function (x, age, min = 16, max = 20) {
  (x - mean(x, na.rm=TRUE)) / sd(x[age >= min & age <= max], na.rm=TRUE)
}
