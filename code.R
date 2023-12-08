library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(randomForest)

# Load data from Excel file
excel_file <- "D:\\Icc world cup\\Book1.xlsx"
cricket_data <- read_excel(excel_file)

# Define the UI
ui <- fluidPage(
  titlePanel("Cricket Team Data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x_variable", "Select X-Axis Variable:", choices = names(cricket_data)),
      selectInput("y_variable", "Select Y-Axis Variable:", choices = names(cricket_data)),
      selectInput("plot_type", "Select Plot Type:", choices = c("Bar Chart", "Scatterplot"))
    ),
    mainPanel(
      plotOutput("plot"),
      verbatimTextOutput("mse_output"),
      tableOutput("team_rankings_table")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  output$plot <- renderPlot({
    x_var <- cricket_data[[input$x_variable]]
    y_var <- cricket_data[[input$y_variable]]
    
    if (input$plot_type == "Bar Chart") {
      ggplot(cricket_data, aes(x = x_var, y = y_var)) +
        geom_bar(stat = "identity", fill = "blue") +
        labs(title = "Bar Chart", x = input$x_variable, y = input$y_variable)
    } else if (input$plot_type == "Scatterplot") {
      ggplot(cricket_data, aes(x = x_var, y = y_var, label = cricket_data$Team_name)) +
        geom_point() +
        geom_text(hjust = -0.2, vjust = -0.5) +
        labs(title = "Scatterplot", x = input$x_variable, y = input$y_variable)
    }
  })
  
  output$mse_output <- renderPrint({
    # Feature selection (assuming these are the important features)
    selected_features <- c("Win_percentage_ODI", "WC_match_won", "WC_match_loss", "Team_ranking")
    
    # Subset the data with selected features
    selected_data <- cricket_data[selected_features]
    
    # Split the data into training and testing sets
    set.seed(123) # For reproducibility
    train_indices <- sample(1:nrow(selected_data), 0.7 * nrow(selected_data))
    train_data <- selected_data[train_indices, ]
    test_data <- selected_data[-train_indices, ]
    
    # Perform hyperparameter tuning using random search
    ntree_values <- c(500, 1000, 1500)
    mtry_values <- c(1, 2, 3)  # Adjust this based on the number of features
    
    best_mse <- Inf
    best_ntree <- NULL
    best_mtry <- NULL
    
    for (ntree in ntree_values) {
      for (mtry in mtry_values) {
        rf_model <- randomForest(Team_ranking ~ ., data = train_data, ntree = ntree, mtry = mtry)
        predictions <- predict(rf_model, test_data)
        mse <- mean((test_data$Team_ranking - predictions)^2)
        
        if (mse < best_mse) {
          best_mse <- mse
          best_ntree <- ntree
          best_mtry <- mtry
        }
      }
    }
    
    rf_model <- randomForest(Team_ranking ~ ., data = train_data, ntree = best_ntree, mtry = best_mtry)
    predictions <- predict(rf_model, test_data)
    
    mse <- mean((test_data$Team_ranking - predictions)^2)
    paste("Mean Squared Error:", mse)
  })
  
  output$team_rankings_table <- renderTable({
    # Create a table with team names, original rankings, and predicted rankings
    team_rankings <- data.frame(
      Team_name = cricket_data$Team_name[-train_indices],
      Original_Ranking = test_data$Team_ranking,
      Predicted_Ranking = predictions
    )
    team_rankings
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
