library(shiny)
library(shinydashboard)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(DT)
library(tm)

# Load and clean data
bbc_data <- read_csv("bbcsports.csv")

# Rename columns to match expected structure
bbc_data <- bbc_data %>%
  rename(content = text, category = label) %>%
  filter(!is.na(content), !is.na(category)) %>%
  mutate(id = row_number())

# Tokenization for word frequency and TF-IDF
tokens <- bbc_data %>%
  unnest_tokens(word, content) %>%
  anti_join(stop_words, by = "word") %>%
  count(id, category, word, sort = TRUE) %>%
  ungroup()

# TF-IDF calculation
tfidf <- tokens %>%
  bind_tf_idf(word, id, n) %>%
  arrange(desc(tf_idf))

# Top words by category for word cloud
top_words <- tokens %>%
  group_by(category, word) %>%
  summarise(freq = sum(n), .groups = "drop")

# UI
ui <- dashboardPage(
  dashboardHeader(title = "BBC Sports Explorer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Word Cloud", tabName = "wordcloud", icon = icon("cloud")),
      menuItem("TF-IDF", tabName = "tfidf", icon = icon("chart-bar")),
      menuItem("Browse Articles", tabName = "table", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      # Overview tab
      tabItem(tabName = "overview",
              fluidRow(
                box(title = "Articles per Category", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("categoryPlot"))
              )
      ),
      
      # Word Cloud tab
      tabItem(tabName = "wordcloud",
              fluidRow(
                box(title = "Select a Category", width = 4, 
                    selectInput("selected_category", "Category:", choices = unique(bbc_data$category))),
                box(title = "Word Cloud", width = 8, 
                    plotOutput("wordCloudPlot"))
              )
      ),
      
      # TF-IDF tab
      tabItem(tabName = "tfidf",
              fluidRow(
                box(title = "Top TF-IDF Words by Category", width = 12,
                    plotOutput("tfidfPlot"))
              )
      ),
      
      # Data Table tab
      tabItem(tabName = "table",
              fluidRow(
                box(title = "BBC Articles", width = 12,
                    DTOutput("dataTable"))
              )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Plot category distribution
  output$categoryPlot <- renderPlot({
    bbc_data %>%
      count(category) %>%
      ggplot(aes(x = reorder(category, n), y = n, fill = category)) +
      geom_col() +
      coord_flip() +
      labs(x = "Category", y = "Article Count", title = "Articles by Category") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # Render word cloud
  output$wordCloudPlot <- renderPlot({
    words <- top_words %>%
      filter(category == input$selected_category)
    
    wordcloud(words = words$word, freq = words$freq, max.words = 100, colors = brewer.pal(8, "Dark2"))
  })
  
  # Render TF-IDF plot
  output$tfidfPlot <- renderPlot({
    tfidf %>%
      group_by(category) %>%
      slice_max(tf_idf, n = 10) %>%
      ungroup() %>%
      mutate(word = reorder_within(word, tf_idf, category)) %>%
      ggplot(aes(word, tf_idf, fill = category)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ category, scales = "free") +
      scale_x_reordered() +
      coord_flip() +
      labs(x = "Word", y = "TF-IDF", title = "Top TF-IDF Words per Category") +
      theme_minimal()
  })
  
  # Render article table
  output$dataTable <- renderDT({
    datatable(bbc_data %>% select(title, category), options = list(pageLength = 10))
  })
}

shinyApp(ui = ui, server = server)
