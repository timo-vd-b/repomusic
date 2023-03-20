

library(tidyverse)
library(spotifyr)
library(compmus)

lose_yourself <- get_tidy_audio_analysis("6GkTKjv1XbFVdI2D8vvDPu")

lose_yourself |>
  tempogram(window_size = 4, hop_size = 1, cyclic = FALSE) |>
  ggplot(aes(x = time, y = bpm, fill = power)) +
  geom_raster() +
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)") +
  theme_classic()







