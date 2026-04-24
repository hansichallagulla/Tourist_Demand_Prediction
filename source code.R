install.packages(c("tidyverse","caret","randomForest","factoextra","cluster"))
library(tidyverse)
library(caret)
library(randomForest)
library(factoextra)
library(cluster)
data <- read.csv(file.choose())
str(data)
summary(data)

data <- na.omit(data)

data$Category <- as.factor(data$Category)
data$Country <- as.factor(data$Country)
data$Season <- as.factor(data$Season)

#Dimension Tables
dim_location <- data %>%
  select(Location, Country) %>%
  distinct() %>%
  mutate(Location_ID = row_number())

dim_category <- data %>%
  select(Category) %>%
  distinct() %>%
  mutate(Category_ID = row_number())

#Fact Table
fact_table <- data %>%
  left_join(dim_location, by = c("Location","Country")) %>%
  left_join(dim_category, by = "Category") %>%
  select(Location_ID, Category_ID, Visitors, Revenue, Rating, Accommodation_Available)

#Correlation
cor(fact_table[, c("Visitors","Revenue","Rating","Accommodation_Available")])

#Clustering
cluster_data <- fact_table[, c("Visitors","Revenue","Rating")]
cluster_data <- scale(cluster_data)

set.seed(123)
k_model <- kmeans(cluster_data, centers = 4)

fact_table$Cluster <- k_model$cluster

fviz_cluster(k_model, data = cluster_data)

#Classification
fact_table <- fact_table %>%
  left_join(dim_category, by = "Category_ID")

set.seed(123)
trainIndex <- createDataPartition(fact_table$Category, p = 0.8, list = FALSE)

train <- fact_table[trainIndex,]
test <- fact_table[-trainIndex,]

fact_table$Category <- ifelse(fact_table$Rating > 4.2 & fact_table$Accommodation_Available == 1, "Luxury",
                              ifelse(fact_table$Rating > 3.5, "Premium",
                                     ifelse(fact_table$Visitors > 120000, "Popular",
                                            ifelse(fact_table$Revenue > 100000, "Economic", "Budget"))))

fact_table$Category <- as.factor(fact_table$Category)

set.seed(123)
trainIndex <- createDataPartition(fact_table$Category, p = 0.8, list = FALSE)

train <- fact_table[trainIndex,]
test <- fact_table[-trainIndex,]

model_rf <- randomForest(Category ~ Visitors + Revenue + Rating + Accommodation_Available,
                         data = train,
                         ntree = 200)

pred <- predict(model_rf, test)
confusionMatrix(pred, test$Category)

#Train
model_rf <- randomForest(Category ~ Visitors + Revenue + Rating + Accommodation_Available,
                         data = train,
                         ntree = 200)

#Evaluate
pred <- predict(model_rf, test)
confusionMatrix(pred, test$Category)

#Regression
model_lm <- lm(Visitors ~ Revenue + Rating + Accommodation_Available, data = fact_table)
summary(model_lm)

#Future Prediction
future <- data.frame(
  Revenue = 80000,
  Rating = 4.5,
  Accommodation_Available = 1
)
predict(model_lm, future)

ggplot(fact_table, aes(x = factor(Cluster), y = Visitors)) +
  geom_boxplot(fill = "orange") +
  ggtitle("Tourist Demand by Cluster")

importance(model_rf)
varImpPlot(model_rf)
fact_table$Category <- ifelse(fact_table$Rating > 4.2 & fact_table$Accommodation_Available == 1, "Luxury",
                              ifelse(fact_table$Rating > 3.5, "Premium",
                                     ifelse(fact_table$Visitors > 120000, "Popular",
                                            ifelse(fact_table$Revenue > 100000, "Economic", "Budget"))))

fact_table$Category <- as.factor(fact_table$Category)

#Classification
set.seed(123)
trainIndex <- createDataPartition(fact_table$Category, p = 0.8, list = FALSE)

train <- fact_table[trainIndex,]
test <- fact_table[-trainIndex,]

#Train
model_rf <- randomForest(Category ~ Visitors + Revenue + Rating + Accommodation_Available,
                         data = train,
                         ntree = 200)

#Evaluate
pred <- predict(model_rf, test)
confusionMatrix(pred, test$Category)

#Regression
model_lm <- lm(Visitors ~ Revenue + Rating + Accommodation_Available, data = fact_table)
summary(model_lm)

#Future Prediction
future <- data.frame(
  Revenue = 80000,
  Rating = 4.5,
  Accommodation_Available = 1
)
predict(model_lm, future)

ggplot(fact_table, aes(x = factor(Cluster), y = Visitors)) +
  geom_boxplot(fill = "orange") +
  ggtitle("Tourist Demand by Cluster")

importance(model_rf)
varImpPlot(model_rf)

write.csv(fact_table, "fact_table_final.csv", row.names = FALSE)
write.csv(dim_location, "dim_location.csv", row.names = FALSE)
write.csv(dim_category, "dim_category.csv", row.names = FALSE)

#Visitors by Category
ggplot(fact_table, aes(x = Category, y = Visitors)) +
  geom_boxplot(fill = "orange") +
  ggtitle("Tourist Demand by Category")

#Revenue vs Visitors
ggplot(fact_table, aes(x = Visitors, y = Revenue)) +
  geom_point(alpha = 0.5) +
  ggtitle("Revenue vs Visitors")

#Cluster Analysis
ggplot(fact_table, aes(x = factor(Cluster), y = Visitors)) +
  geom_boxplot(fill = "blue") +
  ggtitle("Visitor Distribution by Cluster")

#Accomodation Impact
ggplot(fact_table, aes(x = factor(Accommodation_Available), y = Visitors)) +
  geom_boxplot(fill = "green") +
  ggtitle("Impact of Accommodation on Visitors")

#Ratings vs Visitors
ggplot(fact_table, aes(x = Rating, y = Visitors)) +
  geom_point(alpha = 0.5) +
  ggtitle("Rating vs Visitors")

ggsave("plot1.png")

install.packages("shiny")
library(shiny)

# ---------------- SHINY UI ----------------
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      
      body {
        background-color: #0b0b0f;
        color: white;
        font-family: 'Segoe UI', sans-serif;
      }

      h3 {
        color: #bb86fc;
      }

      .panel-box {
        background: linear-gradient(145deg, #2b0f3a, #1f1b4d);
        padding: 20px;
        border-radius: 15px;
        margin-bottom: 20px;
      }

      /* FIXED prediction box (same as panels) */
      .prediction-box {
        background: linear-gradient(145deg, #2b0f3a, #1f1b4d);
        padding: 15px;
        border-radius: 12px;
        font-size: 18px;
        text-align: center;
        color: #ffffff;
      }

      /* FIXED labels (Year, Revenue, etc.) */
      .control-label {
        color: #BB86FC !important;
        font-weight: 500;
      }

      .form-control {
        background-color: #1a1a2e !important;
        color: white !important;
        border: 1px solid #444 !important;
      }

      select {
        background-color: #1a1a2e !important;
        color: white !important;
        border: 1px solid #444 !important;
        appearance: none;
        -webkit-appearance: none;
        -moz-appearance: none;
      }

      select:focus {
        background-color: #1a1a2e !important;
        color: white !important;
      }

      select option {
        background-color: #1a1a2e;
        color: white;
      }
      
      .selectize-input {
  background-color: #1a1a2e !important;
  color: white !important;
  border: 1px solid #444 !important;
}

.selectize-dropdown {
  background-color: #1a1a2e !important;
  color: white !important;
  border: 1px solid #444 !important;
}

.selectize-dropdown .option {
  background-color: #1a1a2e;
  color: white;
}

.selectize-dropdown .option.active {
  background-color: #2b0f3a !important;
}

    "))
  ),
  
  titlePanel("Future Tourist Demand Dashboard"),
  
  # -------- INPUT ROW --------
  div(class = "panel-box",
      
      fluidRow(
        column(3, numericInput("year", "Year", 2026, min = 2025)),
        column(3, numericInput("revenue", "Revenue", 80000, min = 0)),
        column(3, numericInput("rating", "Rating", 4.5, min = 1, max = 5)),
        column(3, selectizeInput("acc", "Accommodation",
                                 choices = c("Yes" = 1, "No" = 0)))
      )
  ),
  
  # -------- PREDICTION --------
  div(class = "panel-box",
      h3("Predicted Tourist Demand"),
      div(class = "prediction-box", textOutput("prediction")),
      textOutput("warning")
  ),
  
  # -------- GRAPHS --------
  div(class = "panel-box",
      h3("Visual Insights"),
      
      fluidRow(
        column(4, plotOutput("clusterPlot", height = "250px")),
        column(4, plotOutput("revenuePlot", height = "250px")),
        column(4, plotOutput("ratingPlot", height = "250px"))
      )
  )
)

# ---------------- SHINY SERVER ----------------
server <- function(input, output) {
  
  output$prediction <- renderText({
    
    if(input$revenue < 1000){
      return("Revenue too low for realistic prediction")
    }
    
    new_data <- data.frame(
      Revenue = input$revenue,
      Rating = input$rating,
      Accommodation_Available = as.numeric(input$acc)
    )
    
    pred <- predict(model_lm, new_data)
    
    paste("Predicted Visitors in", input$year, ":", round(pred))
  })
  
  output$warning <- renderText({
    if(input$revenue < 1000){
      return("Tip: Increase revenue input")
    } else {
      return("")
    }
  })
  
  empty_plot <- function() {
    ggplot() +
      theme_void() +
      theme(
        plot.background = element_rect(fill = "#0b0b0f", color = NA)
      )
  }
  
  output$clusterPlot <- renderPlot({
    ggplot(fact_table, aes(x = factor(Cluster), y = Visitors)) +
      geom_boxplot(fill = "#1f77b4") +
      theme_dark() +
      theme(
        plot.background = element_rect(fill = "#0b0b0f"),
        panel.background = element_rect(fill = "#1a1a2e"),
        text = element_text(color = "white")
      )
  })
  
  output$revenuePlot <- renderPlot({
    
    if(input$revenue < 1000){
      return(empty_plot())
    }
    
    user_pred <- predict(model_lm, data.frame(
      Revenue = input$revenue,
      Rating = input$rating,
      Accommodation_Available = as.numeric(input$acc)
    ))
    
    ggplot(fact_table, aes(x = Revenue, y = Visitors)) +
      geom_point(alpha = 0.5, color = "#bb86fc") +
      geom_point(aes(x = input$revenue, y = user_pred), 
                 color = "#ff4ecd", size = 3) +
      theme_dark() +
      theme(
        plot.background = element_rect(fill = "#0b0b0f"),
        panel.background = element_rect(fill = "#1a1a2e"),
        text = element_text(color = "white")
      )
  })
  
  output$ratingPlot <- renderPlot({
    
    if(input$revenue < 1000){
      return(empty_plot())
    }
    
    user_pred <- predict(model_lm, data.frame(
      Revenue = input$revenue,
      Rating = input$rating,
      Accommodation_Available = as.numeric(input$acc)
    ))
    
    ggplot(fact_table, aes(x = Rating, y = Visitors)) +
      geom_point(alpha = 0.5, color = "#bb86fc") +
      geom_point(aes(x = input$rating, y = user_pred), 
                 color = "#ff4ecd", size = 3) +
      theme_dark() +
      theme(
        plot.background = element_rect(fill = "#0b0b0f"),
        panel.background = element_rect(fill = "#1a1a2e"),
        text = element_text(color = "white")
      )
  })
}

shinyApp(ui = ui, server = server)