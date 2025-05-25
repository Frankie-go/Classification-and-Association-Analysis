#Group4:  35780959,  36497215   35621168,  36390968
# code part 1:PCA,Regression and Correlation matrix



#Code steps:
#1. Import data and load packages
#2. Delete specified columns
#3. Filter time range
#4. Delete rows with missing value ratio exceeding 80%.
#5. Manually delete outliers (singular values) based on scatter plots.
#6. Fill missing values
#7. Use random forest (missForest) to fill missing values ​​in numerical columns.
#8. Merge related columns
#9. Standardize data
#10. Center and standardize numerical data.
#11. Principal component analysis (PCA)
#12 Regression analysis
#Principal component random forest regression
#Principal component linear regression analysis.
#13 Save


# Install necessary packages 
install.packages(c(
  "ggplot2",
  "lubridate",
  "corrplot",
  "pheatmap",
  "plotly",
  "tidyverse",
  "stats",
  "cowplot",
  "missForest",
  "Metrics",
  "caret"
))

# Load the required R packages
library(ggplot2)
library(lubridate)
library(corrplot)
library(pheatmap)
library(plotly)
library(tidyverse)
library(stats)
library(cowplot)
library(missForest)
library(Metrics)
library(caret)
library(dplyr)
library(tidyr)
library(randomForest)

# Setting the working directory
setwd("/Users/mac/Desktop/GJ")

# Reading data
data <- read.csv("output_table.csv")

# Part 1: Calculate the correlation matrix of the original data
# Extract numerical variables and calculate the correlation matrix
correlation_matrix1 <- cor(data[sapply(data, is.numeric)])
print(correlation_matrix1)
# Draw a correlation matrix
heatmap1 <- pheatmap(
  correlation_matrix1, 
  display_numbers = TRUE,          
  number_format = '%.1f',          #
  color = colorRampPalette(c("purple", "white", "violet"))(100) 
)


# Part 2: Log transform data
# Log transform the 'Deaths' and 'Injured' columns, adding 1 to avoid log(0)
data$Deaths <- log(data$Deaths + 1)
data$Injured <- log(data$Injured + 1)

# Part 3:log-transformed correlation matrix
correlation_matrix2 <- cor(data[sapply(data, is.numeric)])
print(correlation_matrix2)

# Plot the log-transformed correlation matrix
heatmap2 <- pheatmap(
  correlation_matrix2, 
  display_numbers = TRUE,          
  number_format = '%.1f',          
  color = colorRampPalette(c("purple", "white", "violet"))(100) 
)



# Importing country datasets
angola_data <- read.csv("Angola.csv")
argentina_data <- read.csv("Argentina.csv")
bolivia_data <- read.csv("Bolivia.csv")
burkina_faso_data <- read.csv("Burkina Faso.csv")
chile_data <- read.csv("Chile.csv")
colombia_data <- read.csv("Colombia.csv")
ecuador_data <- read.csv("Ecuador.csv")
egypt_data <- read.csv("Egypt.csv")
kenya_data <- read.csv("Kenya.csv")
madagascar_data <- read.csv("Madagascar.csv")
malawi_data <- read.csv("Malawi.csv")
peru_data <- read.csv("Peru.csv")
venezuela_data <- read.csv("Venezuela.csv")

# Delete the Total row for each dataset
angola_data <- head(angola_data, -1)
argentina_data <- head(argentina_data, -1)
bolivia_data <- head(bolivia_data, -1)
burkina_faso_data <- head(burkina_faso_data, -1)
chile_data <- head(chile_data, -1)
colombia_data <- head(colombia_data, -1)
ecuador_data <- head(ecuador_data, -1)
egypt_data <- head(egypt_data, -1)
kenya_data <- head(kenya_data, -1)
madagascar_data <- head(madagascar_data, -1)
malawi_data <- head(malawi_data, -1)
peru_data <- head(peru_data, -1)
venezuela_data <- head(venezuela_data, -1)

# Merge all datasets into one data frame
df1 <- bind_rows(
  angola_data, argentina_data, bolivia_data, burkina_faso_data,
  chile_data, colombia_data, ecuador_data, egypt_data, kenya_data,
  madagascar_data, malawi_data, peru_data, venezuela_data
)

# Replace spaces and "-" with underscores
names(df1) <- gsub(" ", "_", names(df1))
names(df1) <- gsub("-", "_", names(df1))

# View data types and dimensions
sapply(df1, class)
dim(df1)

# Data status description function
summary_stats <- function(df1) {
  summary <- data.frame(
    Column = names(df1),
    Mean = sapply(df1, function(x) if (is.numeric(x)) mean(x, na.rm = TRUE) else NA),
    Median = sapply(df1, function(x) if (is.numeric(x)) median(x, na.rm = TRUE) else NA),
    Variance = sapply(df1, function(x) if (is.numeric(x)) var(x, na.rm = TRUE) else NA),
    Missing = sapply(df1, function(x) sum(is.na(x))),
    Unique = sapply(df1, function(x) length(unique(x))),
    Class = sapply(df1, class)
  )
  return(summary)
}

# Run the describe function to view the data status
stats <- summary_stats(df1)
print(stats)

# Remove the column named "missing"
df1 <- df1 %>% select(-Missing)

# Filter data from 1995 to 2023
df1$Year <- as.numeric(substr(df1$Year...Month, 1, 4))
filtered_df1 <- subset(df1, Year >= 1995 & Year <= 2023)
filtered_df1 <- filtered_df1[, !(names(filtered_df1) %in% c("Year...Month" ))]

# Remove rows with more than 20% missingness
df <- filtered_df1 %>% filter(rowSums(is.na(.)) / ncol(.) < 0.8) 








# View the basic information of the columns of the data frame df
stats <- summary_stats(df)
print(stats)




# 5. Remove outliers from each column 
#——manually delete singular values within 1% of the total based on the scatter plot

# Check the DataCards column
df <- df %>% filter(is.na(DataCards) | DataCards < 450)
ggplot(df, aes(x = 1:nrow(df), y = DataCards)) +
  geom_point(alpha = 0.6, color = "blue") +
  labs(title = "Scatter Plot of DataCards",
       x = "Index",
       y = "DataCards") +
  theme_minimal()

# Check the Deaths column and deal with outliers (2)
ggplot(df, aes(x = 1:nrow(df), y = Deaths)) +
  geom_point(alpha = 0.6, color = "red") +
  labs(title = "Scatter Plot of Deaths",
       x = "Index",
       y = "Deaths") +
  theme_minimal()

# Filter outliers in the Deaths column
df <- df %>% filter(is.na(Deaths) | Deaths < 400)

# Check the Deaths column and deal with outliers (3)
ggplot(df, aes(x = 1:nrow(df), y = Deaths)) +
  geom_point(alpha = 0.6, color = "red") +
  labs(title = "Scatter Plot of Deaths",
       x = "Index",
       y = "Deaths") +
  theme_minimal()


# Check the Injured column and handle outliers
ggplot(df, aes(x = 1:nrow(df), y = Injured)) +
  geom_point(alpha = 0.6, color = "green") +
  labs(title = "Scatter Plot of Injured",
       x = "Index",
       y = "Injured") +
  theme_minimal()

# Filter outliers in the Injured column (15 outliers)
df <- df %>% filter(is.na(Injured) | Injured <= 5000)

# Re-plot the scatter plot of the Injured column
ggplot(df, aes(x = 1:nrow(df), y = Injured)) +
  geom_point(alpha = 0.6, color = "green") +
  labs(title = "Scatter Plot of Injured (After Cleaning)",
       x = "Index",
       y = "Injured") +
  theme_minimal()

# Check the Houses.Destroyed column and handle outliers
ggplot(df, aes(x = 1:nrow(df), y = Houses.Destroyed)) +
  geom_point(alpha = 0.6, color = "purple") +
  labs(title = "Scatter Plot of Houses.Destroyed",
       x = "Index",
       y = "Houses.Destroyed") +
  theme_minimal()

# Filter outliers in the Houses.Destroyed column (3 out of 3)
df <- df %>% filter(is.na(Houses.Destroyed) | Houses.Destroyed <= 7000)

# Re-plot the scatter plot for the Houses.Destroyed column
ggplot(df, aes(x = 1:nrow(df), y = Houses.Destroyed)) +
  geom_point(alpha = 0.6, color = "purple") +
  labs(title = "Scatter Plot of Houses.Destroyed (After Cleaning)",
       x = "Index",
       y = "Houses.Destroyed") +
  theme_minimal()

# Check the Houses.Damaged column and handle outliers
ggplot(df, aes(x = 1:nrow(df), y = Houses.Damaged)) +
  geom_point(alpha = 0.6, color = "orange") +
  labs(title = "Scatter Plot of Houses.Damaged",
       x = "Index",
       y = "Houses.Damaged") +
  theme_minimal()

# Filter outliers in the Houses.Damaged column (16 outliers)
df <- df %>% filter(is.na(Houses.Damaged) | Houses.Damaged <= 17500)

# Re-plot the scatter plot of the Houses.Damaged column
ggplot(df, aes(x = 1:nrow(df), y = Houses.Damaged)) +
  geom_point(alpha = 0.6, color = "orange") +
  labs(title = "Scatter Plot of Houses.Damaged (After Cleaning)",
       x = "Index",
       y = "Houses.Damaged") +
  theme_minimal()

# Check the Directly.affected column and handle outliers (18)
ggplot(df, aes(x = 1:nrow(df), y = Directly.affected)) +
  geom_point(alpha = 0.6, color = "cyan") +
  labs(title = "Scatter Plot of Directly.affected",
       x = "Index",
       y = "Directly.affected") +
  theme_minimal()

# Filter outliers in the Directly.affected column
df <- df %>% filter(is.na(Directly.affected) | Directly.affected <= 100000)

# Re-plot the scatter plot of the Directly.affected column
ggplot(df, aes(x = 1:nrow(df), y = Directly.affected)) +
  geom_point(alpha = 0.6, color = "cyan") +
  labs(title = "Scatter Plot of Directly.affected (After Cleaning)",
       x = "Index",
       y = "Directly.affected") +
  theme_minimal()

# Check the Relocated column and handle outliers (10)
ggplot(df, aes(x = 1:nrow(df), y = Relocated)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  labs(title = "Scatter Plot of Relocated",
       x = "Index",
       y = "Relocated") +
  theme_minimal()

# Filter outliers in the Relocated column
df <- df %>% filter(is.na(Relocated) | Relocated <= 500)

# Redraw the scatter plot of the Relocated column
ggplot(df, aes(x = 1:nrow(df), y = Relocated)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  labs(title = "Scatter Plot of Relocated (After Cleaning)",
       x = "Index",
       y = "Relocated") +
  theme_minimal()

# Check the Evacuated column and handle outliers
ggplot(df, aes(x = 1:nrow(df), y = Evacuated)) +
  geom_point(alpha = 0.6, color = "darkred") +
  labs(title = "Scatter Plot of Evacuated",
       x = "Index",
       y = "Evacuated") +
  theme_minimal()

# Filter outliers in the Evacuated column (21 outliers)
df <- df %>% filter(is.na(Evacuated) | Evacuated <= 5000)

# Re-plot the scatter plot of the Evacuated column
ggplot(df, aes(x = 1:nrow(df), y = Evacuated)) +
  geom_point(alpha = 0.6, color = "darkred") +
  labs(title = "Scatter Plot of Evacuated (After Cleaning)",
       x = "Index",
       y = "Evacuated") +
  theme_minimal()






# 6. Filling missing values using random forest(PCA,Regression，classification)


# Copy the dataset to handle missing values
df_cleaned_no_na <- df

# Build a random forest model to fill missing values (for numerical columns)
numeric_cols <- sapply(df_cleaned_no_na, is.numeric)  # Filtering Numeric Columns
df_numeric <- df_cleaned_no_na[, numeric_cols]

# Fill missing values using random forest
filled_numeric <- missForest(df_numeric, maxiter = 5)  # Fill missing values using missForest

# Replace the filled value column
df_cleaned_no_na[, numeric_cols] <- filled_numeric$ximp

# View the completed dataset
head(df_cleaned_no_na)

# Check whether all missing values have been filled
cat("Number of missing values：", sum(is.na(df_cleaned_no_na)), "\n")

#This CSV prepares data for classification
write.csv(df_cleaned_no_na, file = "C:/Users/mac/Desktop/output_table.csv", row.names = FALSE)


# 7. Merge related columns
df_cleaned_no_na <- df_cleaned_no_na %>%
  mutate(
    Houses_Affected = `Houses.Destroyed` + `Houses.Damaged`,
    Affected = `Directly.affected` + `Indirectly.Affected`,
    Relocation = `Relocated` + `Evacuated`
  ) %>%
  select(-`Houses.Destroyed`, -`Houses.Damaged`, -`Directly.affected`, -`Indirectly.Affected`, -`Relocated`, -`Evacuated`,-`Year`)  # 删除原始列\Year列

#  Select numeric columns and calculate the correlation matrix
df_numeric <- df_cleaned_no_na %>% select_if(is.numeric)
cor_matrix <- cor(df_numeric)
cor_matrix



# 8. Data Standardization
preProc <- preProcess(df_cleaned_no_na[, !(names(df_cleaned_no_na) %in% c("Country", "Event", "Deaths"))], method = c("center", "scale"))
df_scaled <- predict(preProc, df_cleaned_no_na[, !(names(df_cleaned_no_na) %in% c("Country", "Event", "Deaths"))])

# Add target variable
df_scaled <- cbind(df_scaled, Deaths = df_cleaned_no_na$Deaths)


#9.Principal component analysis

# 1. Extract feature columns (excluding the target variable Deaths)
pca_data <- df_scaled[, !(names(df_scaled) %in% c("Deaths"))]

# 2. Run PCA
pca_model <- prcomp(pca_data, center = TRUE, scale. = TRUE)  
# The data has been standardized, and this step also ensures centralization and standardization

# 3. View principal component results
summary(pca_model)  
# View the proportion of variance explained by the principal components

pca_model$rotation

# 4. Extracting principal components
pca_scores <- as.data.frame(pca_model$x) 
# Principal component scores
head(pca_scores)

# 5. Extracting the proportion of variance explained
explained_variance <- pca_model$sdev^2 / sum(pca_model$sdev^2)
cumulative_variance <- cumsum(explained_variance)

# 6. Plot the proportion of variance explained
variance_df <- data.frame(
  Principal_Component = paste0("PC", 1:length(explained_variance)),
  Explained_Variance = explained_variance,
  Cumulative_Variance = cumulative_variance
)

#Draw a graph of PCA
p <- ggplot(variance_df, aes(x = Principal_Component, y = Explained_Variance)) +
  geom_bar(aes(fill = "Proportion of Variance"), stat = "identity", alpha = 0.7) +
  geom_line(aes(y = Cumulative_Variance, color = "Cumulative Proportion", group = 1), size = 1) +
  geom_point(aes(y = Cumulative_Variance, color = "Cumulative Proportion"), size = 2) +
  labs(
    title = "Explained Variance by Principal Components",
    x = "Principal Component",
    y = "Proportion of Variance Explained"
  ) +
  # colour
  scale_fill_manual(values = c("Proportion of Variance" = "purple")) +  
  scale_color_manual(values = c("Cumulative Proportion" = "pink")) +  
  theme_minimal() +
  theme(
    legend.title = element_blank(),  # Remove legend title
    legend.position = "right",       # Place the legend on the right
    legend.box.margin = margin(0, 10, 0, 0)  # 
  )
print(p)

# Save the map as an image with a transparent background
ggsave("p.png", plot = p, bg = "transparent", width = 8, height = 5, dpi = 300)

#10PCA regression
pca_top4 <- pca_scores[, 1:4]  # Extract the first 4 principal components
pca_top4 <- cbind(pca_top4, Deaths = df_scaled$Deaths)  # Add target variable Deaths



# Building a Random Forest Model
set.seed(123)  # Setting the random seed
rf_model_pca <- randomForest(Deaths ~ ., data = pca_top4)  # Modeling using the first 4 PC

rf_model_pca

# Generate forecasts
rf_pred_pca <- predict(rf_model_pca, newdata = pca_top4)

# Calculate R-squared and MSE
rf_r2_pca <- 1 - sum((pca_top4$Deaths - rf_pred_pca)^2) / sum((pca_top4$Deaths - mean(pca_top4$Deaths))^2)  # 计算 R-squared
rf_mse_pca <- mean((pca_top4$Deaths - rf_pred_pca)^2)  # calculate MSE

# Output
cat("Random Forest with Top 4 Principal Components:\n")
cat("R-squared: ", rf_r2_pca, "\n")
cat("MSE: ", rf_mse_pca, "\n")

# Visualizing actual and predicted values
p_rf <- ggplot(data = NULL, aes(x = pca_top4$Deaths, y = rf_pred_pca)) +
  geom_point(alpha = 0.6, color = "purple") +  # scatter polt
  geom_abline(slope = 1, intercept = 0, color = "pink", linetype = "dashed") +  # line y=x
  labs(
    title = "Random Forest Regression with Principal Components: Actual vs Predicted",
    x = "Actual Deaths",
    y = "Predicted Deaths"
  ) +
  theme_minimal()
print(p_rf)
# save file
ggsave("rf_actual_vs_predicted.png", plot = p_rf, bg = "transparent", width = 8, height = 5, dpi = 300)

# Lnear regression
install.packages("caret")
library(caret)
# Data preparing
# Extract PC1-PC4 and target variable Deaths
linear_pca_data <- cbind(pca_scores[, 1:4], Deaths = df_scaled$Deaths)

# Divide training set and test set
set.seed(123)  # Set the random seed
trainIndex <- createDataPartition(linear_pca_data$Deaths, p = 0.8, list = FALSE)
trainData_lm <- linear_pca_data[trainIndex, ]
testData_lm <- linear_pca_data[-trainIndex, ]

# Building a linear regression model
lm_model <- lm(Deaths ~ PC1 + PC2 + PC3 + PC4, data = trainData_lm)

# View Model Summary
summary(lm_model)

# Model prediction
lm_pred <- predict(lm_model, newdata = testData_lm)

# Model Evaluation
# Calculate MSE
lm_mse <- mean((testData_lm$Deaths - lm_pred)^2)

# Calculate R-squared
lm_r2 <- 1 - sum((testData_lm$Deaths - lm_pred)^2) / sum((testData_lm$Deaths - mean(testData_lm$Deaths))^2)

# Output evaluation results
cat("Linear Regression with Principal Components:\n")
cat("MSE: ", lm_mse, "\n")
cat("R-squared: ", lm_r2, "\n")



# visualizasion
p2 <- ggplot(data = NULL, aes(x = testData_lm$Deaths, y = lm_pred)) +
  geom_point(alpha = 0.6, color = "purple") +
  geom_abline(slope = 1, intercept = 0, color = "pink", linetype = "dashed") +
  labs(
    title = "Linear Regression with Principal Components: Actual vs Predicted",
    x = "Actual Deaths",
    y = "Predicted Deaths"
  ) +
  theme_minimal()
print(p2)
# save
ggsave("p2.png", plot = p2, bg = "transparent", width = 6, height = 5, dpi = 300)