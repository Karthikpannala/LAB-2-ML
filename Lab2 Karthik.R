# 'student.csv' should be in the current working directory, make sure.
library(tidyverse)

# Set working directory
setwd(""C:\Users\karth\OneDrive\Desktop\"")

# Load student dataset using base R read.csv function
students <- read.csv("Students lab2.csv")

# Data preprocessing
students <- students %>%
  mutate(
    passed = ifelse(final_result == "Pass", 1, 0),
    pass = as.factor(passed),
    credits = as.factor(studied_credits),
    imd_band = factor(imd_band, levels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", "50-60%", "60-70%", "70-80%", "80-90%", "90-100%")),
    imd_band = as.integer(imd_band)
  )

# Creat training and test sets
set.seed(20230712)  # Setting seed for reproducibility
sample_count <- floor(0.8 * nrow(students))
training <- sample(seq_len(nrow(students)), size = sample_count)

train_set <- students[training,]
test_set <- students[-training,]

# Building a logistic regression model with glm (Generalized Linear Model) in base R
logit_model <- glm( pass ~ credits + imd_band, family = binomial(link = "logit"), data = train_set)

# Display Model Summary
summary(logit_model)

# Predicting on test data
test_predictions <- predict(logit_model, test_set, type = "response")
predicted_outcome <- ifelse(test_predictions > 0.5, 1, 0)

# Calculating the Accuracy
true_outcomes <- as.numeric(test_set$pass) - 1  # Adjusting factor levels from 1 to 0 and 1 for comparison
model_accuracy <- mean(predicted_outcome == true_outcomes)
print(paste("Model Accuracy:", model_accuracy))
