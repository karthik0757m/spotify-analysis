# Load cleaned data
spotify_data <- read_csv("output/cleaned_spotify.csv")

# Define moods based on valence, energy, danceability
spotify_data <- spotify_data |> 
  mutate(
    mood = case_when(
      valence >= 0.6 & energy >= 0.6 ~ "Happy",
      valence < 0.4 & energy < 0.4 ~ "Sad",
      valence >= 0.6 & danceability >= 0.7 ~ "Party",
      valence < 0.4 & energy >= 0.6 ~ "Angry",
      energy < 0.5 & danceability < 0.5 & valence < 0.5 ~ "Chill",
      TRUE ~ "Mixed"
    )
  )

# Save mood data
write_csv(spotify_data, "output/mood_summary.csv")
