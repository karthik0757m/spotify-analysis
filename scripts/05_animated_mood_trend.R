library(tidyverse)
library(gganimate)
library(viridis)

spotify_data <- read_csv("output/mood_summary.csv")

animated_plot <- spotify_data |>
  count(release_year, mood) |>
  ggplot(aes(x = mood, y = n, fill = mood)) +
  geom_col(show.legend = FALSE) +
  labs(title = 'Mood Distribution in Year: {closest_state}', x = 'Mood', y = 'Count') +
  scale_fill_viridis(discrete = TRUE) +
  transition_states(release_year, transition_length = 2, state_length = 1) +
  ease_aes('sine-in-out') +
  theme_minimal()

anim_save("output/plots/mood_trend_animation.gif", animation = animated_plot, width = 800, height = 600)
