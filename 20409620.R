# load the library required
library(tidyverse)
library(caret)
library(e1071)

#load the data
insurance_df <- read_csv('./insurance.csv')

# cleaning the data
# extract outliers
# Calculate the interquartile range (IQR)
Q1_age <- quantile(insurance_df$age, 0.25, na.rm = T)
Q3_age <- quantile(insurance_df$age, 0.75, na.rm = T)
IQR_age <- Q3_age - Q1_age
Q1_famsize <- quantile(insurance_df$familysize, .25, na.rm = T)
Q3_famsize <- quantile(insurance_df$familysize, .75, na.rm = T)
IQR_famsize <- Q3_famsize - Q1_famsize

# Define the outlier cutoff values
cutoff_low_age <- Q1_age - 1.5 * IQR_age
cutoff_high_age <- Q3_age + 1.5 * IQR_age
cutoff_low_famsize <- Q1_famsize - 1.5 * IQR_famsize
cutoff_high_famsize <- Q3_famsize + 1.5 * IQR_famsize
# Extract the outliers
outliers_age <-
  insurance_df[insurance_df$age < cutoff_low_age |
                 insurance_df$age > cutoff_high_age,]
outliers_famsize <-
  insurance_df[insurance_df$familysize < cutoff_low_famsize |
                 insurance_df$familysize > cutoff_high_famsize, ]

clean_insurance_df <- insurance_df %>%
  # remove any outliers and duplicates
  filter(
    !age %in% outliers_age$age,
    !familysize %in% outliers_famsize$familysize,
    !married %in% c('married', 'single')
  ) %>%
  mutate(
    status = as.factor(status),
    healthy = as.factor(healthy),
    gender = as.factor(gender),
    education = as.factor(education),
    married = as.factor(married),
    selfemployed = as.factor(selfemployed),
    region = as.factor(region),
    race = as.factor(race)
  ) %>%
  # remove missing values
  remove_missing()

# barplot for insurance status and healthy status
clean_insurance_df %>% 
  ggplot(aes(x = status, fill = healthy))+
  geom_bar(position = 'dodge')+
  labs(title = 'Barplot for insurance status and health status',
       x = 'Insurance Status')
# facetted bar plot for status, selfemployed and race
clean_insurance_df %>%
  group_by(race) %>% 
  count(race, status, selfemployed, sort = T) %>% 
  ggplot(aes(x =status,y = n, fill = race))+
  geom_bar(position = 'dodge', stat='identity')+
  labs(title = 'A faceted bar plot for status race and self employed',
       x = 'Insurance Status', y = 'Counts')+                                                                                                              
  facet_grid( ~ selfemployed)

# machine learning
# split data into training and testing datasets
seed<- 20409620
set.seed(seed)
trainIndex <- createDataPartition(clean_insurance_df$status, p = 0.7, list = FALSE)
trainData <- clean_insurance_df[trainIndex, ]
testData <- clean_insurance_df[-trainIndex, ]

# train data using three different machine learning algorithms
# logistic regression
formula<- Formula::as.Formula("status ~ age+familysize+healthy+gender+education+married+selfemployed +region+race")
control<- trainControl(method = 'cv', number = 10)
model_logit <- train(formula, data = trainData, method = "glm", family = "binomial", trControl=control)

# decision tree
model_tree <- train(formula, data = trainData, method = "rpart", trControl = control)

# support vector machine
model_svm <- train(formula, data = trainData, method = "svmRadial", trControl = control)

# test accuracy of each model using the test dataset
pred_logit <- predict(model_logit, testData)
pred_tree <- predict(model_tree, testData)
pred_svm <- predict(model_svm, testData)

# evaluate accuracy of each model
confusionMatrix(pred_logit, testData$status)$overall['Accuracy']
confusionMatrix(pred_tree, testData$status)$overall['Accuracy']
confusionMatrix(pred_svm, testData$status)$overall['Accuracy']
