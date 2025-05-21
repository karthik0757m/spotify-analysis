# Load data
library(ggplot2)
library(viridis)
library(plotly)
spotify_data <- read_csv("output/mood_summary.csv")

# Plot 1: Mood Trend by Year
p1 <- spotify_data |> 
  count(release_year, mood) |> 
  ggplot(aes(x = release_year, y = n, fill = mood)) +
  geom_col(position = "dodge") +
  labs(title = "Mood Trends by Year", x = "Year", y = "Number of Songs") +
  scale_fill_viridis(discrete = TRUE)
  theme_minimal()

ggsave("output/plots/mood_trend_by_year.png", p1, width = 10, height = 6)

# Plot 2: Mood by Genre
p2 <- spotify_data |> 
  count(playlist_genre, mood) |> 
  ggplot(aes(x = reorder(playlist_genre, -n), y = n, fill = mood)) +
  geom_col(position = "dodge") +
  labs(title = "Mood Distribution by Genre", x = "Genre", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis(discrete = TRUE)

ggsave("output/plots/mood_by_genre.png", p2, width = 10, height = 6)
