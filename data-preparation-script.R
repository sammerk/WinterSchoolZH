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
      g1tchid,
      g2tchid,
      g3tchid,
      g4tchid,
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
    ) %>%
    mutate(
      classID = ifelse(
        !is.na(g1tchid),
        g1tchid,
        ifelse(
          !is.na(g2tchid),
          g2tchid,
          g3tchid
        )
      )
    ),
  "data_star_workshop.sav"
)
