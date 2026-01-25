# Data Preparation
library(tidyverse)
library(haven)
data_star <- read_sav(
  "https://raw.githubusercontent.com/sammerk/did_data/master/STAR.sav"
)

#codebook::label_browser_static(data_star)

write_sav(
  data_star %>%
    select(
      g1tmathss,
      g2tmathss,
      g3tmathss,
      g4tmathss,
      g1freelunch,
      g2freelunch,
      g3freelunch,
      g4nfreelunch,
      g1classsize,
      g2classsize,
      g3classsize
    ),
  "data_star_workshop.sav"
)
