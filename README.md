# Classification-and-Association-Analysis

# 🌍 Disaster Impact Analysis using Machine Learning

## 📌 Project Overview
This project analyzes historical disaster data (from UNDRR DesInventar) for 13 countries using a full ML pipeline: data cleaning, feature engineering, dimensionality reduction (PCA), regression, and classification.

---

## 🧹 Data Processing
- Cleaned records from 1995–2023 across 13 countries.
- Removed outliers via scatter plot analysis.
- Handled missing data using **missForest** (random forest imputation).
- Log-transformed skewed columns like `Deaths` and `Injured`.
- Created new engineered features: `Houses_Affected`, `Relocation`, and `Affected`.

---

## 🔬 PCA (Principal Component Analysis)
- Applied PCA to reduce dimensionality (from 13 to 4 PCs), explaining ~96% variance.
- Used top 4 PCs for regression and classification tasks.

---

## 📈 Regression Modeling
Two models to predict death counts:
- **Linear Regression**  
- **Random Forest Regression** (performed better due to capturing nonlinearities)

---

## 🤖 Classification Tasks
Three classification tasks using:
- **Random Forest**, **SVM**, **Neural Network**
- Target variables:
  - `Disaster Type` (Natural vs Human-Made)
  - `Economic Status` (High, Medium, Low Developing)
  - `Continent` (Africa, South America)

### 🔍 Results Summary
| Task            | Best Model      | Accuracy |
|-----------------|------------------|----------|
| Continent       | Random Forest     | 94.26%   |
| Economic Status | SVM               | 60.95%   |
| Disaster Type   | Neural Network    | 49.67%   |

---

## 📊 Key Takeaways
- PCA helped mitigate multicollinearity and reduce complexity.
- Random forest models performed best overall.
- Disaster impact correlates with economic status and geographic region.
- ML can support disaster preparedness by revealing critical patterns.

---

## 🛠️ Tech Stack
- **R**, **ggplot2**, **randomForest**, **caret**, **nnet**, **missForest**, **pheatmap**

---

## 📂 Structure
- `Group4_1.R`: Data cleaning, PCA, regression
- `Group4_2.R`: Feature engineering, classification, evaluation
- `output_table.csv`: Cleaned dataset for modeling
- `Model Performance Comparison.csv`: Accuracy of models

