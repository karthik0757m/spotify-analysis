launch_shiny <- function() {
  library(shiny)
  library(tidyverse)
  library(viridis)
  library(DT)
  library(gganimate)
  library(gifski)
  library(fmsb)
  library(plotly)
  library(superheat)
  library(GGally)
  library(lubridate)
  library(shinythemes)
  
  # Load data
  spotify_data <- read_csv("output/mood_summary.csv")
  
  # Precompute year range
  min_year <- min(spotify_data$release_year, na.rm = TRUE)
  max_year <- max(spotify_data$release_year, na.rm = TRUE)
  
  # Define UI
  ui <- fluidPage(
    theme = shinytheme("cyborg"),
    titlePanel("🎵 Spotify Mood Explorer - Enhanced Edition"),
    sidebarLayout(
      sidebarPanel(
        selectInput("genre", "Select Genre:",
                    choices = unique(spotify_data$playlist_genre),
                    selected = "pop"),
        sliderInput("yearRange", "Select Release Year Range:",
                    min = min_year, max = max_year,
                    value = c(min_year, max_year), sep = ""),
        sliderInput("popularityRange", "Track Popularity:",
                    min = 0, max = 100, value = c(0, 100)),
        checkboxGroupInput("moods", "Select Moods:",
                           choices = unique(spotify_data$mood),
                           selected = unique(spotify_data$mood)),
        downloadButton("downloadFiltered", "Download Filtered CSV")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Mood Distribution", plotOutput("moodPlot")),
          tabPanel("Mood Over Time", plotOutput("moodTrend")),
          tabPanel("Genre Comparison", plotOutput("genreCompare")),
          tabPanel("Exceptional Tracks", DTOutput("outliersTable")),
          tabPanel("Animated Trend", imageOutput("animatedGif")),
          tabPanel("Radar Chart", plotOutput("radarPlot")),
          tabPanel("Valence-Energy Map", plotlyOutput("valenceEnergyPlot")),
          tabPanel("Feature Correlation", plotOutput("correlationHeatmap")),
          tabPanel("Streamgraph", plotOutput("genreStreamgraph")),
          tabPanel("Parallel Coordinates", plotlyOutput("parallelPlot"))
        )
      )
    )
  )
  
  server <- function(input, output) {
    # Reactive filtered dataset
    filtered_data <- reactive({
      spotify_data |>
        filter(playlist_genre == input$genre,
               release_year >= input$yearRange[1],
               release_year <= input$yearRange[2],
               track_popularity >= input$popularityRange[1],
               track_popularity <= input$popularityRange[2],
               mood %in% input$moods)
    })
    
    output$moodPlot <- renderPlot({
      filtered_data() |>
        count(mood) |>
        ggplot(aes(x = mood, y = n, fill = mood)) +
        geom_col() +
        labs(title = paste("Mood Distribution for", input$genre),
             x = "Mood", y = "Number of Songs") +
        scale_fill_viridis(discrete = TRUE) +
        theme_minimal()
    })
    
    output$moodTrend <- renderPlot({
      filtered_data() |>
        count(release_year, mood) |>
        ggplot(aes(x = release_year, y = n, color = mood)) +
        geom_line(size = 1.2) +
        labs(title = "Mood Trend Over Time", x = "Year", y = "Count") +
        theme_minimal()
    })
    
    output$genreCompare <- renderPlot({
      data <- spotify_data |>
        filter(release_year >= input$yearRange[1],
               release_year <= input$yearRange[2],
               track_popularity >= input$popularityRange[1],
               track_popularity <= input$popularityRange[2],
               mood %in% input$moods)
      
      validate(
        need(nrow(data) > 0, "No data available for the selected filters.")
      )
      
      data |>
        count(playlist_genre, mood) |>
        ggplot(aes(x = playlist_genre, y = n, fill = mood)) +
        geom_col(position = "stack") +
        labs(title = "Mood Distribution Across Genres", x = "Genre", y = "Count") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_viridis(discrete = TRUE) +
        theme_minimal()
    })
    
    output$outliersTable <- renderDT({
      filtered_data() |>
        filter(track_popularity > 90) |>
        arrange(desc(track_popularity))|>
        select(track_name, track_artist, track_popularity, mood, playlist_genre, playlist_subgenre, release_year)
    })
    
    output$animatedGif <- renderImage({
      list(src = "output/plots/mood_trend_animation.gif",
           contentType = 'image/gif',
           width = 800,
           height = 600,
           alt = "Animated Mood Trend")
    }, deleteFile = FALSE)
    
    output$downloadFiltered <- downloadHandler(
      filename = function() {
        paste0("spotify_filtered_", input$genre, ".csv")
      },
      content = function(file) {
        write_csv(filtered_data(), file)
      }
    )
    
    output$radarPlot <- renderPlot({
      radar_data <- filtered_data() |>
        group_by(mood) |>
        summarise(across(c(danceability, energy, valence, acousticness, instrumentalness, speechiness), mean, na.rm = TRUE)) |>
        column_to_rownames("mood")
      
      max_vals <- apply(radar_data, 2, max)
      min_vals <- apply(radar_data, 2, min)
      radar_ready <- rbind(max_vals, min_vals, radar_data)
      
      radarchart(radar_ready,
                 axistype = 1,
                 pcol = viridis::viridis(nrow(radar_data)),
                 plwd = 2,
                 plty = 1,
                 title = paste("Mood Fingerprints in", input$genre),
                 cglcol = "grey", cglty = 1,
                 axislabcol = "black", caxislabels = NULL,
                 vlcex = 0.8)
      legend("topright", legend = rownames(radar_data), col = viridis::viridis(nrow(radar_data)), lty = 1, lwd = 2, bty = "n")
    })
    
    output$valenceEnergyPlot <- renderPlotly({
      df <- filtered_data()
      validate(
        need(nrow(df) > 0, "No tracks found for selected filters.")
      )
      
      plot_ly(df, x = ~valence, y = ~energy, type = 'scatter', mode = 'markers',
              text = ~paste("🎵", track_name, "<br>🎤", track_artist),
              color = ~mood,
              colors = viridis::viridis(length(unique(df$mood)))) |>
        layout(title = "Valence vs Energy Map",
               xaxis = list(title = "Valence"),
               yaxis = list(title = "Energy"))
    })
    
    output$correlationHeatmap <- renderPlot({
      corr_data <- filtered_data() |>
        select(danceability, energy, valence, acousticness, instrumentalness, speechiness, track_popularity)
      corr <- cor(corr_data, use = "complete.obs")
      superheat(corr, scale = FALSE, heat.col.scheme = "viridis")
    })
    
    output$genreStreamgraph <- renderPlot({
      genre_time <- spotify_data |>
        count(release_year, playlist_genre) |>
        ggplot(aes(x = release_year, y = n, fill = playlist_genre)) +
        geom_area(alpha = 0.8 , size = 0.5, colour = "white") +
        labs(title = "Genre Streamgraph Over Time", x = "Year", y = "Track Count") +
        theme_minimal()
      genre_time
    })
    
    output$parallelPlot <- renderPlotly({
      df <- filtered_data() |>
        select(track_name, mood, danceability, energy, valence, acousticness, instrumentalness, speechiness)
      plot_ly(type = 'parcoords',
              line = list(color = as.numeric(factor(df$mood)), colorscale = 'Viridis'),
              dimensions = list(
                list(label = 'Danceability', values = df$danceability),
                list(label = 'Energy', values = df$energy),
                list(label = 'Valence', values = df$valence),
                list(label = 'Acousticness', values = df$acousticness),
                list(label = 'Instrumentalness', values = df$instrumentalness),
                list(label = 'Speechiness', values = df$speechiness)
              ))
    })
  }
  
  shinyApp(ui = ui, server = server)
  
}

# Auto-launch if sourced
launch_shiny()
