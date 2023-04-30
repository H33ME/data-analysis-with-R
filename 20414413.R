# start by setting the working directory
setwd('~/Data-Analytic-with-R')

# load the required library
library(tidyverse)
install.packages(c('caret', 'e1071'))
library(caret)
library(e1071)
library(janitor)
# load the data
insurance_data <- read_csv('./insurance.csv')

# structure of data
str(insurance_data)

# statistical summary
summary(insurance_data)
# clean the data
# refactoring variables function
factorize_function <- function(dataframe) {
  lst <- list()
  for (i in seq_along(dataframe)) {
    if (is.character(dataframe[[i]])) {
      lst[[i]] <- as.factor(dataframe[[i]])
    } else{
      lst[[i]] <- dataframe[[i]]
    }
  }
  new_dataframe <- lst %>% as_tibble(.name_repair = 'unique')
  colnames(new_dataframe) <- colnames(dataframe)
  return(new_dataframe)
}
# outliers in age and familysize variables
age_outliers <- boxplot.stats(insurance_data$age)$out
fam_outliers <- boxplot.stats(insurance_data$familysize)$out

clean_insurance_data <- insurance_data %>%
  # filter out outliers
  filter(
    !age %in% age_outliers,
    !familysize %in% fam_outliers,
    gender %in% c('Female', 'Male'),
    married %in% c('Yes', 'No')
  ) %>%
  # remove missing values
  remove_missing() %>%
  # clean the column names
  clean_names() %>%
  # factor character variables
  factorize_function()

# bar plot for insurance status
clean_insurance_data %>%
  ggplot(aes(x = status, fill = gender)) +
  geom_bar(position = 'dodge') +
  labs(title = 'barplot showing how insurance status is distributed in male and female',
       x = 'Insurance status', y = 'Count')

# piechart for insurance status with respect to education
clean_insurance_data %>% 
  group_by(education) %>% 
  count(education, status, sort = T) %>% 
  ggplot(aes(x = '', y = n, fill = education))+
  geom_bar(stat = 'identity', width =1)+
  coord_polar('y', start = 0)+
  geom_text(aes(label=n), position = position_stack(vjust = 0.5)) +
  facet_wrap(~status)+
  labs(title = 'a piechart showing how insurance status depends on education')+
  theme_void()
# supervised machine learning 
# create training and testing data sets
seed<- 20414413
set.seed(seed)
training_indices <- createDataPartition(clean_insurance_data$status, p = 0.7, list = FALSE)
training_data <- clean_insurance_data[training_indices, ]
testing_data <- clean_insurance_data[-training_indices, ]

# define the formula 
formula <- as.formula("status ~ age+familysize+healthy+gender+education+married+selfemployed +region+race")

# set up the control parameters for cross-validation
control <- trainControl(method = "cv", number = 5)

# train three different machine learning models using cross-validation
# logistic regression
logistic_model <- train(formula, data = training_data, method = "glm", family = "binomial", trControl = control)

# support vector machines
svm_model <- train(formula, data = training_data, method = "svmRadial", trControl = control)

# decision tree
dt_model <- train(formula, data = training_data, method = "rpart", trControl = control)
# make predictions on the test data using the trained models
logistic_predictions <- predict(logistic_model, newdata = testing_data)
svm_predictions <- predict(svm_model, newdata = testing_data)
dt_predictions <- predict(dt_model, newdata = testing_data)

# evaluate the models' performance
logistic_accuracy <- confusionMatrix(logistic_predictions, testing_data$status)$overall["Accuracy"]
svm_accuracy <- confusionMatrix(svm_predictions, testing_data$status)$overall["Accuracy"]
dt_accuracy <- confusionMatrix(dt_predictions, testing_data$status)$overall["Accuracy"]

# print the accuracy of each model
cat("Logistic regression accuracy:", logistic_accuracy, "\n")
cat("SVM accuracy:", svm_accuracy, "\n")
cat("Decision tree accuracy:", dt_accuracy, "\n")
