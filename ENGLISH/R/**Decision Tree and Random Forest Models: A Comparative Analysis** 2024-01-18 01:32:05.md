```r
# Load the required libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(tidyr)
library(tidyverse)
library(shiny)
library(data.table)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
library(pROC)
library(plotly)
library(tidymodels)

# Load the dataset
data <- read.csv("data.csv")

# Preprocess the dataset
data <- data %>%
  mutate(
    date = ymd(date),
    category = factor(category),
    value = as.numeric(value)
  ) %>%
  na.omit()

# Create a training and testing set
set.seed(123)
train_index <- sample(1:nrow(data), 0.75 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Build a decision tree model
tree_model <- rpart(
  value ~ .,
  data = train_data,
  method = "class"
)

# Plot the decision tree
plot(tree_model, type = "tree")

# Evaluate the decision tree model
pred_train <- predict(tree_model, train_data)
pred_test <- predict(tree_model, test_data)

accuracy_train <- mean(pred_train == train_data$value)
accuracy_test <- mean(pred_test == test_data$value)

print(paste("Training accuracy:", accuracy_train))
print(paste("Testing accuracy:", accuracy_test))

# Build a random forest model
rf_model <- randomForest(
  value ~ .,
  data = train_data,
  ntree = 100
)

# Evaluate the random forest model
pred_train <- predict(rf_model, train_data)
pred_test <- predict(rf_model, test_data)

accuracy_train <- mean(pred_train == train_data$value)
accuracy_test <- mean(pred_test == test_data$value)

print(paste("Training accuracy:", accuracy_train))
print(paste("Testing accuracy:", accuracy_test))

# Create a receiver operating characteristic (ROC) curve for the random forest model
roc_curve <- roc(test_data$value, pred_test)
plot(roc_curve, print.auc = TRUE)

# Create a Shiny app
ui <- fluidPage(
  titlePanel("Decision Tree and Random Forest Models"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "model",
        "Model",
        choices = list("Decision Tree" = "tree", "Random Forest" = "rf")
      ),
      sliderInput(
        "threshold",
        "Threshold",
        min = 0,
        max = 1,
        value = 0.5
      )
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    if (input$model == "tree") {
      plot(tree_model, type = "tree")
    } else {
      plot(roc_curve)
    }
  })
}

shinyApp(ui, server)
```

This code performs the following tasks:

1. Loads the required libraries.
2. Loads the dataset.
3. Preprocesses the dataset.
4. Creates a training and testing set.
5. Builds a decision tree model.
6. Plots the decision tree.
7. Evaluates the decision tree model.
8. Builds a random forest model.
9. Evaluates the random forest model.
10. Creates a receiver operating characteristic (ROC) curve for the random forest model.
11. Creates a Shiny app that allows users to select the model they want to view and the threshold for the ROC curve.