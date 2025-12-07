# 🎵 Spotify Song Popularity Analysis

## 📌 Project Overview
This project explores the factors that influence song popularity on Spotify using a dataset of top 2,000 tracks (1956-2019). Using **R**, the project performs Exploratory Data Analysis (EDA), Hypothesis Testing, and Multiple Linear Regression to build a predictive model.

## ❓ Business Problem
Understanding what makes a song popular is crucial for record labels, artists, and streaming platforms. This analysis aims to identify the key audio features (e.g., Energy, Danceability) that correlate with higher popularity scores.

## 🛠️ Technologies Used
*   **Language:** R
*   **Libraries:** `tidyverse`, `ggplot2`, `car` (for VIF), `lmtest` (for Breusch-Pagan)
*   **Techniques:** Hypothesis Testing (T-test, ANOVA), Linear Regression, Assumption Checking.

## 📊 Key Findings & Methodology

### 1. Exploratory Data Analysis (EDA)
*   **Hypothesis Testing:** Conducted Welch Two-Sample t-tests and ANOVA.
*   **Result:** Found statistically significant differences in popularity across genres (p < 0.05), but no significant difference between "Pop" and "Rock" specifically.

### 2. Modeling Approach
We iterated through three models to predict `Popularity`:
*   **Model 1 (Simple Linear):** Used `Total.Followers` as the sole predictor. (R² = 0.11).
*   **Model 2 (Multiple Linear - Champion):** Included all audio features. Achieved the best performance (**R² = 0.47**, **RMSE = 9.93**).
*   **Model 3 (Interaction Terms):** Added interactions like `Energy * Danceability`. Did not improve performance over Model 2.

### 3. Conclusion
The final model explains **47%** of the variance in song popularity. While features like `Total.Followers` and `Year` are strong predictors, the audio features alone (like tempo or loudness) only tell part of the story. Future work would involve using non-linear models like **Random Forest** or **XGBoost** to capture complex relationships.

## 📂 Project Structure
*   `scripts/`: Contains the R code for data cleaning, EDA, and modeling.
*   `reports/`: Full PDF report detailing the statistical tests and diagnostics.
*   `data/`: Source data from Kaggle.

---
*Author: LE Minh Quan*
