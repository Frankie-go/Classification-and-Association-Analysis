#Group4:  35780959,  36497215   35621168,  36390968
# code part 2:Classification (Data provided by Group4_01)

#steps:
# Load necessary packages
# Load data
# Feature engineering
# Data preprocessing

# Stratified Sampling to Split Dataset
# Classification Model Construction
# Model Evaluation
# Model Performance Comparison
# Visualization

# Load the necessary packages
install.packages(c("dplyr", "randomForest", "e1071", "caret",
                   "nnet", "ROSE", "ggplot2", "reshape2"))

library(dplyr)
library(randomForest)
library(e1071)
library(caret)
library(nnet)
library(ROSE)
library(ggplot2)
library(reshape2)


# 1. Data Loading
setwd("/Users/mac/Desktop/GJ")
data <- read.csv("/Users/mac/Desktop/GJ/output_table.csv")
# Clean Column Names
colnames(data) <- gsub("\\.+", "_", tolower(colnames(data)))

# 2. Feature Engineering 
data$continent <- case_when(
  data$country %in% c("Angola", "Burkina Faso", "Egypt", "Kenya", "Madagascar", "Malawi") ~ "Africa",
  data$country %in% c("Argentina", "Bolivia", "Chile", "Colombia", "Ecuador", "Peru", "Venezuela") ~ "South America",
  TRUE ~ "Other"
)

data$economic_status <- case_when(
  data$country %in% c("Argentina", "Chile", "Colombia", "Peru") ~ "High Developeing",
  data$country %in% c("Bolivia", "Ecuador", "Egypt", "Venezuela") ~ "Medium Developing",
  data$country %in% c("Angola", "Burkina Faso", "Kenya" ,"Madagascar", "Malawi") ~ "Lower Developing",
  TRUE ~ "Other"
)

data$disaster_type <- case_when(
  data$event %in% c("DROUGHT", "FLOOD", "RIVERINE FLOOD", "EARTHQUAKE", "LANDSLIDE",
                    "HAILSTORM", "TORNADO", "LIGHTNING", "CYCLONE", "AVALANCHE",
                    "SNOWSTORM", "FOREST FIRE", "HEATWAVE", "MUDSLIDE", "TSUNAMI",
                    "SURGE", "STRONG WIND", "STORM", "RAINS", "ELECTRICSTORM",
                    "FLASH FLOOD", "HEAVY RAIN", "STORMY RAIN") ~ "Natural",
  data$event %in% c("FIRE", "AVIATION ACCIDENT", "PLANE CRASH", "ROAD ACCIDENT",
                    "TRAINS ACCEDNTS", "NAVIGATION ACCIDENT", "CONSTRUCTION COLLAPSE",
                    "STRUCTURAL COLLAPSE", "POLLUTION", "POLLUTION MARINE", "EXPLOSION",
                    "MATERIALS PELIGROSOS", "TERROR ATTACK", "INDUSTRIAL ACCIDENT") ~ "Human-made",
  TRUE ~ "Other"
)

data$disaster_type <- as.factor(data$disaster_type)

# 3. Data Preprocessing: Feature Normalization
numeric_features <- c("deaths", "injured", "houses_affected", "affected")
data[numeric_features] <- scale(data[numeric_features])

# Convert Target Variable to Factor Type
data$disaster_type <- as.factor(data$disaster_type)
data$economic_status <- as.factor(data$economic_status)
data$continent <- as.factor(data$continent)

# 4. Stratified Sampling to Split Dataset

# Disaster Type 
trainIndex_disaster <- createDataPartition(data$disaster_type, p = 0.8, list = FALSE)
trainData_disaster <- data[trainIndex_disaster, ]
testData_disaster <- data[-trainIndex_disaster, ]

# Economic Status 
trainIndex_economy <- createDataPartition(data$economic_status, p = 0.8, list = FALSE)
trainData_economy <- data[trainIndex_economy, ]
testData_economy <- data[-trainIndex_economy, ]

# Continent 
trainIndex_continent <- createDataPartition(data$continent, p = 0.8, list = FALSE)
trainData_continent <- data[trainIndex_continent, ]
testData_continent <- data[-trainIndex_continent, ]


# Use ROSE Only for the Continent Status Task
trainData_continent <- ovun.sample(continent ~ deaths + injured + houses_affected,
                                   data = trainData_economy, method = "both")$data
# Imbalance Handling for Other Tasks
trainData_economy <- trainData_economy
trainData_disaster <- trainData_disaster  

# 5. Classification Model Construction 

run_models <- function(target_var, train_data, test_data, task_name) {
  # Random Forest
  rf_model <- randomForest(as.formula(paste(target_var, "~ deaths + injured + houses_affected  ")),
                           data = train_data, ntree = 500, mtry = 2)
  rf_pred <- predict(rf_model, newdata = test_data)
  rf_cm <- confusionMatrix(rf_pred, test_data[[target_var]])
  
  # SVM
  svm_model <- svm(as.formula(paste(target_var, "~ deaths + injured + houses_affected")),
                   data = train_data, kernel = "radial", cost = 1, gamma = 0.1)
  svm_pred <- predict(svm_model, newdata = test_data)
  svm_cm <- confusionMatrix(svm_pred, test_data[[target_var]])
  
  # Neural Network
  nn_model <- nnet(as.formula(paste(target_var, "~ deaths + injured + houses_affected  ")),
                   data = train_data, size = 5, decay = 0.1, maxit = 200)
  nn_pred <- predict(nn_model, newdata = test_data, type = "class")
  nn_cm <- confusionMatrix(as.factor(nn_pred), test_data[[target_var]])
  
  # Save Model
  saveRDS(rf_model, paste0(task_name, "_rf_model.rds"))
  saveRDS(svm_model, paste0(task_name, "_svm_model.rds"))
  saveRDS(nn_model, paste0(task_name, "_nn_model.rds"))
  
  return(list(
    RandomForest = rf_cm,
    SVM = svm_cm,
    NeuralNetwork = nn_cm,
    rf_model = rf_model,  # Return Model Object
    svm_model = svm_model, 
    nn_model = nn_model
  ))
}

# 6. Model Evaluation 

# 6.1 Disaster Type Classification
disaster_results <- run_models("disaster_type", trainData_disaster, testData_disaster, "disaster")

# 6.2 Economic Status Classification
economy_results <- run_models("economic_status", trainData_economy, testData_economy, "economy")

# 6.3 Continent Classification
continent_results <- run_models("continent", trainData_continent, testData_continent, "continent")

# 7. Model Performance Comparison 

comparison <- data.frame(
  Task = c("Disaster Type", "Disaster Type", "Disaster Type",
           "Economic Status", "Economic Status", "Economic Status",
           "Continent", "Continent", "Continent"),
  Model = c("Random Forest", "SVM", "Neural Network",
            "Random Forest", "SVM", "Neural Network",
            "Random Forest", "SVM", "Neural Network"),
  Accuracy = c(
    disaster_results$RandomForest$overall["Accuracy"], disaster_results$SVM$overall["Accuracy"], disaster_results$NeuralNetwork$overall["Accuracy"],
    economy_results$RandomForest$overall["Accuracy"], economy_results$SVM$overall["Accuracy"], economy_results$NeuralNetwork$overall["Accuracy"],
    continent_results$RandomForest$overall["Accuracy"], continent_results$SVM$overall["Accuracy"], continent_results$NeuralNetwork$overall["Accuracy"]
  )
)

# Save Comparison Results
write.csv(comparison, "Model Performance Comparison.csv", row.names = FALSE)
print("Model Performance Comparison has been saved as 'Model Performance Comparison'.csv'")

# 8. Visualization 

# Function for Visualizing Confusion Matrix
plot_confusion_matrix <- function(cm, title) {
  cm_data <- as.table(cm)
  cm_melt <- melt(cm_data)
  
  # To ensure the column names are correct
  colnames(cm_melt) <- c("Actual", "Predicted", "Count")
  
  ggplot(cm_melt, aes(x = Actual, y = Predicted, fill = Count)) +
    geom_tile() +
    geom_text(aes(label = Count), color = "white",size = 8) +
    scale_fill_gradient(low = "white", high = "blue") +
    theme_minimal() +
    labs(title = title, x = "Actual", y = "Predicted") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(size = 20),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 15))
}

# Visualize the Confusion Matrix for Each Model
plot_confusion_matrix(disaster_results$RandomForest, "Disaster Type - Random Forest")
plot_confusion_matrix(disaster_results$SVM, "Disaster Type - SVM")
plot_confusion_matrix(disaster_results$NeuralNetwork, "Disaster Type - Neural Network")

plot_confusion_matrix(economy_results$RandomForest, "Economic Status - Random Forest")
plot_confusion_matrix(economy_results$SVM, "Economic Status - SVM")
plot_confusion_matrix(economy_results$NeuralNetwork, "Economic Status - Neural Network")

plot_confusion_matrix(continent_results$RandomForest, "Continent - Random Forest")
plot_confusion_matrix(continent_results$SVM, "Continent - SVM")
plot_confusion_matrix(continent_results$NeuralNetwork, "Continent - Neural Network")

# Visualize Feature Importance
plot_feature_importance <- function(model, task_name) {
  importance_data <- data.frame(Feature = rownames(importance(model)), Importance = importance(model)[, 1])
  importance_data <- importance_data[order(importance_data$Importance, decreasing = TRUE), ]
  
  ggplot(importance_data, aes(x = reorder(Feature, Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = "purple") +
    coord_flip() +
    labs(title = paste("Feature Importance -", task_name), x = "Feature", y = "Importance") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(size = 20),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 15))
}

# Visualize Feature Importance Using the Returned Model Object
plot_feature_importance(disaster_results$rf_model, "Disaster Type")
plot_feature_importance(economy_results$rf_model, "Economic Status")
plot_feature_importance(continent_results$rf_model, "Continent")

# Plot a Bar Chart for Model Accuracy Comparison
plot_accuracy_comparison <- function(comparison_data) {
  ggplot(comparison_data, aes(x = Task, y = Accuracy, fill = Model)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Model Accuracy Comparison", x = "Task", y = "Accuracy") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(size = 20),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 15))
  
}

# Plot Using the Previously Saved Comparison Results
plot_accuracy_comparison(comparison)

