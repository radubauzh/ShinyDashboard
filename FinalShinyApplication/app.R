library(shiny)
library(leaflet)
library(data.table)
library(MyFinalPackage)

# Load and merge data
customers <- fread("data_customer.csv")
personal <- fread("data_personal.csv")
merged_data <- merge(customers, personal, by = "CustomerId")

# Assuming Exited and Gender were meant to be used in analysis/modeling previously
merged_data$Exited <- as.factor(merged_data$Exited)
merged_data$Gender <- as.factor(merged_data$Gender)

churn_model <- glm(Exited ~ CreditScore + Gender + Age + Tenure + Balance + NumOfProducts + HasCrCard + IsActiveMember + EstimatedSalary, data = merged_data, family = "binomial")
merged_data$ChurnProbability <- predict(churn_model, newdata = merged_data, type = "response")

top_customers <- merged_data[order(-ChurnProbability)][1:100, ]

# Shiny UI
ui <- fluidPage(
  titlePanel("Customer Churn Dashboard"),
  sidebarLayout(
    sidebarPanel(
      textInput("customerId", "Enter Customer ID:"),
      actionButton("submit", "Get Churn Probability"),
      textOutput("churnProb")
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet(data = top_customers) %>%
      addTiles() %>%
      addMarkers(~zip_longitude, ~zip_latitude, popup = ~paste("Customer ID:", CustomerId, "<br>Churn Probability:", ChurnProbability))
  })
  
  observeEvent(input$submit, {
    req(input$customerId)
    customer_id <- input$customerId
    tryCatch({
      churn_probability <- getChurnProbability(top_customers, customer_id) # Use the function from MyFinalPackage
      output$churnProb <- renderText({
        paste("Churn Probability for Customer", customer_id, ":", churn_probability)
      })
    }, error = function(e) {
      output$churnProb <- renderText(e$message)
    })
  })
}

shinyApp(ui = ui, server = server)
