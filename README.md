# R-Projects
---
title: "Untitled"
output: html_document
date: "2024-04-16"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# R Markdown

```{r}
library(shiny)
library(dplyr)
library(ggplot2)
library(shinythemes)

# Load data from CSV file
movies <- read.csv("C://Users//HP//Downloads//movies.csv")

# UI
ui <- fluidPage(
  # Custom CSS for background image and white text color
  tags$head(
    tags$style(HTML("
      body {
        background-image: url('https://editor.analyticsvidhya.com/uploads/76889recommender-system-for-movie-recommendation.jpg');
        background-size: cover;
        background-repeat: no-repeat;
        background-position: center;
        background-color: #f0f0f0; /* Fallback color if the image is not available */
        color: dark grey; /* Black text color */
      }
      h1 {
        color: white; /* White text color for the 'Movie Recommendation' heading */
      }
    "))
  ),
  titlePanel("Movie Recommendation System Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("movie", "Select a Movie:", choices = unique(movies$title)),
      numericInput("num_recommendations", "Number of Recommendations:", value = 5, min = 1, max = 10),
      actionButton("recommend", label = "Get Recommendations", icon = icon("play")),
      h3(HTML("Objectives: <i class='fas fa-list'></i>")),
      tags$ol(
        tags$li("1. Provide movie recommendations based on user selection."),
        tags$li("2. Display distribution of ratings using a histogram."),
        tags$li("3. Show distribution of genres using a bar plot."),
        tags$li("4. Visualize ratings by genre using a box plot."),
        tags$li("5. Present a column chart for rating vs. tag frequency."),
        tags$li("6. Allow users to explore different movies and genres."),
        tags$li("7. Enable users to easily compare ratings across genres."),
        tags$li("8. Provide an intuitive and visually appealing interface.")
      ),
      h3(HTML("Conclusion: <i class='fas fa-check'></i>")),
      p("This dashboard offers a user-friendly way to explore movies and genres, providing personalized recommendations and insightful visualizations.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Recommendations", verbatimTextOutput("recommendations")),
        tabPanel("Rating Histogram", plotOutput("rating_histogram")),
        tabPanel("Genre Bar Plot", plotOutput("genre_bar_plot")),
        tabPanel("Ratings by Genre Box Plot", plotOutput("genre_box_plot")),
        tabPanel("Rating vs. Tag Frequency Column Chart", plotOutput("tag_column_chart"))
      )
    )
  ),
  theme = shinythemes::shinytheme("cerulean") +
    theme(
      text = element_text(color = "black"),  # Black text color
      plot.title = element_text(color = "black"),  # Black text color
      plot.background = element_rect(fill = "rgba(0, 0, 0, 0)"),  # Transparent plot background
      panel.background = element_rect(fill = "rgba(0, 0, 0, 0)")  # Transparent panel background
    )
)

# Server logic
server <- function(input, output, session) {
  observeEvent(input$recommend, {
    selected_movie <- movies %>% filter(title == input$movie)
    if (nrow(selected_movie) == 0) {
      output$recommendations <- renderPrint("Movie not found.")
      return()
    }
    
    selected_movie_genre <- selected_movie$genres
    recommended_movies <- movies %>% filter(genres == selected_movie_genre & title != input$movie) %>%
      arrange(desc(rating))
    
    output$recommendations <- renderPrint({
      recommended_movies$title
    })
  })
  
  output$rating_histogram <- renderPlot({
    ggplot(movies, aes(x = rating)) +
      geom_histogram(binwidth = 0.5, fill = "#6699ff", color = "#003366") +
      labs(title = "Distribution of Ratings", x = "Rating", y = "Frequency") +
      theme_minimal()
  })
  
  output$genre_bar_plot <- renderPlot({
    genres <- unlist(strsplit(movies$genres, "\\|"))
    genre_counts <- table(genres)
    genre_df <- data.frame(genre = names(genre_counts), count = as.numeric(genre_counts))
    
    ggplot(genre_df, aes(x = genre, y = count)) +
      geom_bar(stat = "identity", fill = "#6699ff", color = "#003366") +
      labs(title = "Distribution of Genres", x = "Genre", y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$genre_box_plot <- renderPlot({
    ggplot(movies, aes(x = genres, y = rating, fill = genres)) +
      geom_boxplot() +
      labs(title = "Ratings by Genre", x = "Genre", y = "Rating") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$tag_column_chart <- renderPlot({
    tags <- unlist(strsplit(movies$tag, "\\|"))
    tag_counts <- table(tags)
    tag_df <- data.frame(tag = names(tag_counts), count = as.numeric(tag_counts))
    merged_data <- merge(movies, tag_df, by.x = "tag", by.y = "tag", all.x = TRUE)
    
    ggplot(merged_data, aes(x = count, y = rating)) +
      geom_bar(stat = "identity", fill = "#6699ff", color = "#003366") +
      labs(title = "Rating vs. Tag Frequency Column Chart", x = "Tag Frequency", y = "Rating") +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)

```





