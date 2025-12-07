# Import library 
library(GGally)
library(corrplot) 
library(ggplot2) 
library(dplyr) 
library(car)
library(stats)
library(tidyverse)
library(stringr)
library(lmtest)
library(gridExtra)
# Import data
data<-read.csv(file.choose(),header = TRUE,dec = ".")
# Take a view
str(data)
#Check for missing value
total_missing <- sum(is.na(data))
print(paste0("Total missing values in the dataset: ", total_missing))
#Rename "Duration", "BPM", "Loudness" 

data <- rename(data,
               "Duration" = "Length..Duration.",  
               "BPM" = "Beats.Per.Minute..BPM.", 
               "Loudness_db" = "Loudness..dB.") 
#Convert "Duration" column to integer
data$Duration <- as.integer(data$Duration)
#drop the missing values
data <- na.omit(data)

#Convert "Top Genre" column
top_genre_counts <- table(data$Top.Genre) %>%
  sort(decreasing = TRUE)

print(top_genre_counts)
#Group the genres into 7 most popular, and 1 for the others
data <- mutate(data,  
             Top.Genre = case_when( 
               str_detect(Top.Genre, "adult standards") ~ "adult.standards",
               str_detect(Top.Genre, "soul") ~ "soul",
               str_detect(Top.Genre, "alternative") ~ "alternative",  
               str_detect(Top.Genre, "dance") ~ "dance", 
               str_detect(Top.Genre, "indie") ~ "indie", 
               str_detect(Top.Genre, "hip hop|rap") ~ "hiphop.rap",  
               str_detect(Top.Genre, "rock|prog") ~ "rock",  
               str_detect(Top.Genre, "pop") ~ "pop",
               TRUE ~ "others"
             )
)

#Create dummies variables for "Top Genre" by choosing "others" as the reference level
data$Top.Genre <- relevel(factor(data$Top.Genre), ref = "others")

# Create the model matrix
genre_dummies <- model.matrix(~ Top.Genre, data = data)[,-1]

# Add dummy variables to the main data frame
data <- cbind(data, genre_dummies)

data <- data[data$Top.Genre != "others", ]

#Create new column for years since release to see impact in model
data <- data %>%
  mutate(Years_since_release = 2024 - Year)

#Merge the artist names with their total number followers in another dataset
followers<-read.csv(file.choose(),header = TRUE,dec = ".")

data <- merge(data, followers, by = "Artist", all.x = TRUE)

#Export the data after cleaning
write.csv(data, file = "data_final.csv", row.names = FALSE)

# Create the distribution plot
ggplot(data, aes(x = Popularity)) +
  geom_histogram(bins = 30, color = "blue", alpha = 0.5) +  
  labs(title = "Distribution of Popularity Scores", x = "Popularity") +
  theme_classic()  

#Create the QQ plot
qqnorm(data$Popularity) 
qqline(data$Popularity)

# Perform the Shapiro-Wilk test
shapiro_test_result <- shapiro.test(data$Popularity)

# Print the test results
print(shapiro_test_result)

# Set the sample size
sample_size <- 100

# Take a random sample of Popularity values
data_sample <- data[sample(nrow(data), size = sample_size, replace = FALSE), ]

# Perform the Shapiro-Wilk test on the sample
shapiro_test_result <- shapiro.test(data_sample$Popularity)

# Print the test results
print(shapiro_test_result)

data <- data_sample

pop_popularity <- data[data$Top.Genre == "pop", "Popularity"]  
rock_popularity <- data[data$Top.Genre == "rock", "Popularity"]   
alternative_popularity <- data[data$Top_Genre == "alternative", "Popularity"]  
dance_popularity <- data[data$Top_Genre == "dance", "Popularity"]    
indie_popularity <- data[data$Top_Genre == "indie", "Popularity"]
# Perform Welch's t-test
# Null hypothesis: The mean popularity of pop songs is equal to the mean popularity of rock songs.
# Alternative hypothesis: The mean popularity of pop songs is not equal to the mean popularity of rock songs.
t_test_result <- t.test(pop_popularity, rock_popularity)
t_test_result

# Perform ANOVA

# Null hypothesis: The mean popularity is equal across different genres.
# Alternative hypothesis: The mean popularity is not equal across different genres.

anova_result <- aov(Popularity ~ Top.Genre, data = data)
summary(anova_result)

# Check the assumptions of ANOVA

# 1. Independence assumption
# ANOVA assumes that the observations are independent. 

# 2. Normality assumption
# ANOVA assumes that the residuals are normally distributed.
# We can check this assumption by examining the Q-Q plot of the residuals.
qqnorm(anova_result$residuals)
qqline(anova_result$residuals)
shapiro.test(anova_result$residuals)
# 3. Homogeneity of variances assumption
# ANOVA assumes that the variances of the residuals are equal across groups.
# We can check this assumption by examining the plot of residuals vs. fitted values.
plot(anova_result$fitted.values, anova_result$residuals, xlab = "Fitted Values", ylab = "Residuals")
bptest(anova_result)
# 4. Independence of errors assumption
# ANOVA assumes that the errors are independent of each other.


data <- data %>% select(-Artist, -Index, -Title, -Top.Genre, -X, -Year)

numeric_cols <- names(data) 

# Reshape the data to long format
data_long <- data %>% pivot_longer(cols = -Popularity, names_to = "variable", values_to = "value")

# Create the scatterplot
ggplot(data_long, aes(x = value, y = Popularity)) +
  geom_point(aes(color = variable), alpha = 1) +
  facet_wrap(~ variable, scales = "free_x") +
  labs(title = "Popularity vs. Variable", x = "Variable", y = "Popularity") +
  theme_bw() +
  theme(strip.text.x = element_text(angle = 0))

# Calculate correlation coefficients
correlations <- sapply(data[numeric_cols], function(x) cor(x, data$Popularity, use = "pairwise.complete.obs"))

# Sort correlations in descending order
sorted_correlations <- sort(correlations, decreasing = TRUE)

# Print sorted correlations
print(sorted_correlations)

# Create the simple linear regression model
model_1 <- lm(Popularity ~ Total.Followers, data = data)

# Print the summary of the model
summary(model_1)

# Check the "linearity" assumption by using scatterplot 
ggplot(data, aes(x = Total.Followers, y = Popularity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  labs(title = "Popularity vs. Total.Followers", x = "Total.Followers", y = "Popularity") +
  theme_bw()

# Check the "normality" assumption by using histogram and Q-Q plot of residuals
# Calculate the residuals
residuals <- model_1$residuals

# Create the histogram
ggplot() +
  geom_histogram(aes(x = residuals), bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency") +
  theme_bw()

# Create the Q-Q plot
ggplot() +
  geom_qq(aes(sample = residuals)) +
  geom_qq_line(aes(sample = residuals), color = "red", linetype = "dashed") +
  labs(title = "Q-Q Plot of Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_bw()

# Perform the Shapiro-Wilk test
shapiro.test(residuals)

#Check the 'homoscedasticity' assumption by using scatterplot of the residuals versus the fitted values
# Calculate the fitted values
fitted_values <- model_1$fitted.values

# Create the scatterplot
ggplot() +
  geom_point(aes(x = fitted_values, y = residuals)) +
  labs(title = "Residuals vs. Fitted Values", x = "Fitted Values", y = "Residuals") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_bw()

# Perform the Breusch-Pagan test
bptest(model_1)

# Create the multiple linear regression model
model_2 <- lm(Popularity ~ ., data = data)

# Print the summary of the model
summary(model_2)

# Check the alias of the model
alias_info <- alias(model_2)

# Print the alias information
print(alias_info)

# Create the multiple linear regression model without Top.Genresoul
model_2 <- lm(Popularity ~ . -Top.Genresoul, data = data)

# Print the summary of the model
summary(model_2)

# Compute the VIF for each predictor in the model
vif_values <- car::vif(model_2)

# Print the VIF values
print(vif_values)

# Check the "linearity" assumption by using scatterplot grid
# Reshape the data to long format
data_long <- data %>% pivot_longer(cols = -Popularity, names_to = "variable", values_to = "value")

# Create the scatterplot
ggplot(data_long, aes(x = value, y = Popularity)) +
  geom_point(aes(color = variable), alpha = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") + 
  facet_wrap(~ variable, scales = "free_x") +
  labs(title = "Popularity vs. Variable", x = "Variable", y = "Popularity") +
  theme_bw() +
  theme(strip.text.x = element_text(angle = 0))

# Check the 'homoscedasticity' assumption by using scatterplot of the residuals versus the fitted values
# Calculate the residuals and fitted values
residuals_2 <- resid(model_2)
fitted_values_2 <- fitted(model_2)

# Create the residuals vs fitted values plot
ggplot() +
  geom_point(aes(x = fitted_values_2, y = residuals_2)) +
  labs(title = "Residuals vs. Fitted Values", x = "Fitted Values", y = "Residuals") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_bw()

# Perform the Breusch-Pagan test
bptest(model_2)

# Create a histogram of the residuals
ggplot() +
  geom_histogram(aes(x = residuals_2), bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency") +
  theme_bw()

# Create a Q-Q plot of the residuals
ggplot() +
  geom_qq(aes(sample = residuals_2)) +
  geom_qq_line(aes(sample = residuals_2), color = "red", linetype = "dashed") +
  labs(title = "Q-Q Plot of Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_bw()

shapiro.test(residuals_2)

# Create a copy of the data
data_transformed <- data

# Create interaction features
data_transformed$Energy_Danceability <- data$Energy * data$Danceability
data_transformed$BPM_Loudness <- data$BPM * data$Loudness_db
data_transformed$Acousticness_Speechiness <- data$Acousticness * data$Speechiness
data_transformed <- data_transformed %>% select(-Energy, -Danceability, -BPM, -Loudness_db, -Acousticness, -Speechiness, -Top.Genresoul)
# Fit the model using the transformed data
formula <- as.formula(paste("Popularity ~", paste(names(data_transformed)[-which(names(data_transformed) == "Popularity")], collapse = " + ")))
model_3 <- lm(formula, data = data_transformed)
summary(model_3)

# Compute the VIF for each predictor in the model
vif_values <- car::vif(model_3)

# Print the VIF values
print(vif_values)

# Check the "linearity" assumption by using scatterplot grid
data_long <- data_transformed %>% pivot_longer(cols = -Popularity, names_to = "variable", values_to = "value")

# Create the scatterplot
ggplot(data_long, aes(x = value, y = Popularity)) +
  geom_point(aes(color = variable), alpha = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") + 
  facet_wrap(~ variable, scales = "free_x") +
  labs(title = "Popularity vs. Variable", x = "Variable", y = "Popularity") +
  theme_bw() +
  theme(strip.text.x = element_text(angle = 0))

# Check the "normality" assumption by using histogram and Q-Q plot of residuals
# Calculate the residuals
residuals_3 <- resid(model_3)

# Create the histogram of the residuals
ggplot() +
  geom_histogram(aes(x = residuals_3), bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency") +
  theme_bw()

# Create the Q-Q plot of the residuals
ggplot() +
  geom_qq(aes(sample = residuals_3)) +
  geom_qq_line(aes(sample = residuals_3), color = "red", linetype = "dashed") +
  labs(title = "Q-Q Plot of Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_bw()

# Perform the Shapiro-Wilk test
shapiro.test(residuals_3)

# Check the 'homoscedasticity' assumption by using scatterplot of the residuals versus the fitted values
# Calculate the fitted values
fitted_values_3 <- fitted(model_3)

# Create the scatterplot
ggplot() +
  geom_point(aes(x = fitted_values_3, y = residuals_3)) +
  labs(title = "Residuals vs. Fitted Values", x = "Fitted Values", y = "Residuals") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_bw()

# Perform the Breusch-Pagan test
bptest(model_3)

#Evaluation model 1
# Predict the response on the data
predictions_1 <- predict(model_1, newdata = data)

# Calculate the residuals
residuals_1 <- data$Popularity - predictions

# Calculate RMSE
rmse_1 <- sqrt(mean(residuals_1^2))
print(paste("RMSE: ", rmse_1))

# Calculate MAE
mae_1 <- mean(abs(residuals_1))
print(paste("MAE: ", mae_1))

#Evaluation model 2
# Predict the response on the data
predictions_2 <- predict(model_2, newdata = data)

# Calculate the residuals
residuals_2 <- data$Popularity - predictions

# Calculate RMSE
rmse_2 <- sqrt(mean(residuals_2^2))
print(paste("RMSE: ", rmse_2))

# Calculate MAE
mae_2 <- mean(abs(residuals_2))
print(paste("MAE: ", mae_2))

#Evaluation model 3
# Predict the response on the data
predictions_3 <- predict(model_3, newdata = data_transformed)

# Calculate the residuals
residuals_3 <- data_transformed$Popularity - predictions_3

# Calculate RMSE
rmse_3 <- sqrt(mean(residuals_3^2))
print(paste("RMSE: ", rmse_3))

# Calculate MAE
mae_3 <- mean(abs(residuals_3))
print(paste("MAE: ", mae_3))