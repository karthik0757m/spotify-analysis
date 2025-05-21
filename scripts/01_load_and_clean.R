# Load packages
library(tidyverse)
library(lubridate)
library(janitor)

# Read dataset
spotify_data <- read_csv("data/spotify_songs.csv") |> 
  clean_names()

# Handle incomplete dates (some are just years)
spotify_data <- spotify_data |> 
  mutate(
    track_album_release_date = if_else(
      nchar(track_album_release_date) == 4,
      paste0(track_album_release_date, "-01-01"),
      track_album_release_date
    ),
    track_album_release_date = ymd(track_album_release_date),
    release_year = year(track_album_release_date),
    release_month = month(track_album_release_date, label = TRUE)
  )

# Save cleaned version
write_csv(spotify_data, "output/cleaned_spotify.csv")

