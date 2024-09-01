library(shiny)
library(dplyr)
library(DT)
library(shinycssloaders)
library(mongolite)
library(data.table)

# Access the environment variable directly
url <- Sys.getenv("url")

mongo <- mongo(
  collection= 'soccer',
  url = url
)

# Determine the port from the environment variable
port <- as.numeric(Sys.getenv("PORT", 3838))

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f0f8ff;
        color: #333;
        font-family: Arial, sans-serif;
      }
      .title {
        text-align: center;
        font-weight: bold;
        color: #2c3e50;
        margin-bottom: 20px;
      }
      .action-button {
        background-color: #2980b9;
        color: white;
        font-size: 16px;
        border-radius: 5px;
        padding: 10px 20px;
      }
      .action-button:hover {
        background-color: #3498db;
        color: white;
      }
      #loading_message {
        font-size: 18px;
        font-weight: bold;
        color: #e74c3c;
        text-align: center;
        margin-top: 20px;
      }
      h2 {
        color: #27ae60;
        text-align: center;
        animation: blink 1s infinite;
      }
      @keyframes blink {
        50% { opacity: 0; }
      }
    "))
  ),
  
  titlePanel("World Cup Contest Simulation", windowTitle = "World Cup Contest"),
  
  sidebarLayout(
    sidebarPanel(
      tags$div(class = "title", "Contest Setup"),
      actionButton("run", "Run Contest", class = "action-button"),
      textOutput("loading_message"),
      textOutput("winner_output")
    ),
    mainPanel(
      tags$div(class = "title", "Initial Contest Teams"),
      DTOutput("contest_table"),
      uiOutput("winner_section")
    )
  )
)

server <- function(input, output, session) {
  
  # Load data once
  data <- mongo$find()
  data <- data %>%
    filter(tournament == 'World Cup') %>%
    sample_n(size = min(nrow(data), 1000))
  
  # Extract unique teams
  home <- unique(data['home_team'])
  away <- unique(data['away_team'])
  
  home <- home %>%
    rename(country = home_team)
  
  away <- away %>%
    rename(country = away_team)
  
  countries <- unique(rbind(home, away)$country)
  
  # Initial random selection of 32 teams
  sample_countries <- sample(countries, size = 32, replace = FALSE)
  home_team <- sample_countries[1:16]
  away_team <- sample_countries[17:32]
  
  contest <- data.frame(A = home_team, B = away_team)
  
  # Display the initial teams
  output$contest_table <- renderDT({
    datatable(contest, options = list(dom = 't', paging = FALSE), rownames = FALSE)
  })
  
  observeEvent(input$run, {
    progress <- shiny::Progress$new()
    progress$set(message = "Matches are being played now", value = 0)
    on.exit(progress$close())
    
    # Prepare the data for modeling
    data$winner <- ifelse(data$home_score > data$away_score, 1, 0)
    
    world_cup <- data %>%
      select(-date, -neutral, -home_score, -away_score, -tournament, -country) %>%
      rename(A = home_team, B = away_team)
    
    world_cup2 <- world_cup %>%
      select(B, A, winner) %>%
      mutate(winner = ifelse(winner == 0, 1, 0))
    
    world <- rbind(world_cup, world_cup2)
    
    # Train the model
    model <- glm(winner ~ A + B, world, family = binomial)
    
    # Predict the outcomes and simulate the contest
    while (nrow(contest) > 1) {
      contest$A <- factor(contest$A, levels = levels(world$A))
      contest$B <- factor(contest$B, levels = levels(world$B))
      
      predictions <- predict(model, contest, type = "response")
      contest$predictions <- ifelse(predictions >= .5, 1, 0)
      contest <- contest %>%
        filter(predictions == 1)
      
      half_rows <- floor(nrow(contest) / 2)
      shuffled_indices <- sample(seq_len(nrow(contest)))
      indices1 <- shuffled_indices[1:half_rows]
      indices2 <- shuffled_indices[(half_rows + 1):nrow(contest)]
      
      winners1 <- contest$A[indices1]
      winners2 <- contest$A[indices2]
      
      if (length(winners1) > length(winners2)) {
        winners1 <- winners1[1:length(winners2)]
      } else if (length(winners2) > length(winners1)) {
        winners2 <- winners2[1:length(winners1)]
      }
      
      contest <- data.frame(A = winners1, B = winners2)
      
      progress$inc(0.25, detail = paste("Round", (4 - floor(nrow(contest) / 2)), "complete..."))
    }
    
    winner <- contest$A[1]
    output$winner_output <- renderText({paste("The winner is:", winner)})
    
    output$winner_section <- renderUI({
      tags$h2(paste("🎉 The winner is:", winner, "🎉"))
    })
    
    # Clear unused variables and trigger garbage collection
    rm(contest, model)
    gc()
    
    output$loading_message <- renderText({""})
  })
}

shinyApp(ui = ui, server = server, options = list(host = "0.0.0.0", port = port))


