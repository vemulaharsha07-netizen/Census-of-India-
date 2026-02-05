###Resources###
##Class Notes, HW-4, HW-5, Graphing, geeksforgeek, Other R tutorials websites like
##Codeacademy, Quick R 




#### Load required libraries

library(readxl)
library(dplyr)
library(caret)
library(randomForest)
library(glmnet)

#### Set seed for reproducibility
set.seed(123)

#### Load the dataset
dataset <- read_excel("E:\\R Programming\\Census_India_New.xlsx")
View(dataset)

######## Data preparation #########
# Address outliers 
dataset <- dataset %>%
  filter(!is.na(Population))

#### Data partitioning 
train_index <- createDataPartition(dataset$Population, p = 0.8, list = FALSE)
train_data <- dataset[train_index, ]
test_data <- dataset[-train_index, ]


######## Model building #########
# Model 1: Linear Regression
lm_model <- lm(Population ~ ., data = train_data)
summary(lm_model)
library(coefplot)
coefplot(lm_model)


#### install.packages("rpart.plot")

library(rpart.plot)
tree_model <- rpart(Population ~ ., data = train_data)
summary(tree_model)
rpart.plot(tree_model)



test_data$State <- factor(test_data$State, levels = levels(train_data$State))


########## Model predictions  ########


tree_predictions <- predict(tree_model, newdata = test_data)
summary(tree_predictions)
View(tree_predictions)





#### Model evalluation #######
tree_correlation <- cor(tree_predictions, test_data$Population)
tree_mse <- mean((tree_predictions - test_data$Population)^2)
cat("Decision Tree MSE: ", tree_mse, "\n")


##### Linear Regression evaluation (for comparison) ###########
lm_model <- lm(Population ~ ., data = train_data)
lm_predictions <- predict(lm_model, newdata = test_data)
lm_mse <- mean((lm_predictions - test_data$Population)^2)

###############Linear regression mse###############
# Calculate correlation between predictions and true values
library(caret)

#### Define hyperparameter grid
hyperparameters <- expand.grid(
  cp = c(0.01, 0.001, 0.0001)
)

##### Performming grid search
model <- train(
  Population ~ .,
  data = train_data,
  method = "rpart",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = hyperparameters
)

#### Extract best model
best_model <- model$finalModel

####Check column names in test_data
colnames(test_data)



### Calculate MSE
tree_mse <- mean((tree_predictions - test_data$Population)^2)

cat("Decision Tree MSE after tuning: ", tree_mse, "\n")

###################################################################
### Check for missing values in predictions or true values
if (any(is.na(lm_predictions)) || any(is.na(test_data$Population))) {
  cat("Missing values found. Imputing missing values...\n")
  
  ### Impute missing values
  lm_predictions[is.na(lm_predictions)] <- mean(lm_predictions, na.rm = TRUE)
  test_data$Population[is.na(test_data$Population)] <- mean(test_data$Population, na.rm = TRUE)
}

####Calculate MSE
lm_mse <- mean((lm_predictions - test_data$Population)^2)

#### Check if MSE is NaN and replace it with -1
if (is.nan(lm_mse)) {
  lm_mse <- 1
}

cat("Linear Regression MSE: ", lm_mse, "\n")


cat("Decision Tree MSE: ", tree_mse, "\n")
cat("Linear Regression MSE: ", lm_mse, "\n")


########################## Code to remove ouliers (Reference) ######
remove_outliers <- function(x) {
  qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  H <- 1.5 * IQR(x, na.rm = TRUE)
  x[(x < (qnt[1] - H) | (x > (qnt[2] + H)))] <- NA
  return(x)
}

#### Set scipen option to 999
options(scipen = 999)



#######Model evaluation###########

#### Decision Tree evaluation  ##########
tree_predictions <- predict(tree_model, newdata = test_data)
if (any(is.na(tree_predictions)) || any(is.na(test_data$Population))) {
  cat("Error: Missing values found in predictions or test_data$Population for Decision Tree\n")
} else {
  tree_mse <- mean((tree_predictions - test_data$Population)^2)
  cat("Decision Tree MSE: ", tree_mse, "\n")
}

######### Linear Regression evaluation #########
lm_predictions <- predict(lm_model, newdata = test_data)
if (any(is.na(lm_predictions)) || any(is.na(test_data$Population))) {
  cat("Error: Missing values found in predictions or test_data$Population for Linear Regression\n")
} else {
  lm_mse <- mean((lm_predictions - test_data$Population)^2)
  cat("Linear Regression MSE: ", lm_mse, "\n")
  
  # Compare model performance
  if (!is.na(tree_mse) && !is.na(lm_mse)) {
    if (tree_mse < lm_mse) {
      cat("Decision Tree outperforms Linear Regression\n")
    } else {
      cat("Linear Regression outperforms Decision Tree\n")
    }
  } else {
    cat("Error: Unable to compare model performance due to missing values in MSE\n")
  }
}
#######################test##################
lm_predictions <- predict(lm_model, newdata = test_data)

# Check for missing values
if (any(is.na(lm_predictions)) || any(is.na(test_data$Population))) {
  cat("Missing values found. Imputing missing values...\n")
  
  # Impute missing values
  lm_predictions[is.na(lm_predictions)] <- mean(lm_predictions, na.rm = TRUE)
  test_data$Population[is.na(test_data$Population)] <- mean(test_data$Population, na.rm = TRUE)
}

########## Calculate MSE
lm_mse <- mean((lm_predictions - test_data$Population)^2)
cat("Linear Regression MSE: ", lm_mse, "\n")

###### Compare model performance
if (!is.na(tree_mse) && !is.na(lm_mse)) {
  if (tree_mse < lm_mse) {
    cat("Decision Tree outperforms Linear Regression\n")
  } else {
    cat("Linear Regression outperforms Decision Tree\n")
  }
} else {
  cat("Error: Unable to compare model performance due to missing values in MSE\n")
}


##############################################

## ??name what packages need to be installed will gets installed

#############################################
####### Decision Tree evaluation#############
tree_predictions <- predict(tree_model, newdata = test_data)
if (any(is.na(tree_predictions)) || any(is.na(test_data$Population))) {
  cat("Error: Missing values found in predictions or test_data$Population for Decision Tree\n")
} else {
  tree_mse <- mean((tree_predictions - test_data$Population)^2)
  tree_aic <- AIC(tree_model)
  cat("Decision Tree MSE: ", tree_mse, "\n")
  cat("Decision Tree AIC: ", tree_aic, "\n")
}


##########Decision Tree evaluation  ###########
tree_predictions <- predict(tree_model, newdata = test_data)

if (any(is.na(tree_predictions)) || any(is.na(test_data$Population))) {
  cat("Error: Missing values found in predictions or test_data$Population for Decision Tree\n")
} else {
  tree_mse <- mean((tree_predictions - test_data$Population)^2)
  cat("Decision Tree MSE: ", tree_mse, "\n")
}

# Linear Regression evaluation
lm_predictions <- predict(lm_model, newdata = test_data)
if (any(is.na(lm_predictions)) || any(is.na(test_data$Population))) {
  cat("Error: Missing values found in predictions or test_data$Population for Linear Regression\n")
} else {
  lm_mse <- mean((lm_predictions - test_data$Population)^2)
  lm_aic <- AIC(lm_model)
  cat("Linear Regression MSE: ", lm_mse, "\n")
  cat("Linear Regression AIC: ", lm_aic, "\n")
  
##### Compare model performance based on AIC  ###########
  if (tree_aic < lm_aic) {
    cat("Decision Tree has lower AIC, suggesting it might be a better model\n")
  } else {
    cat("Linear Regression has lower AIC, suggesting it might be a better model\n")
  }
}


######### Calculate Mean Absolute Percentage Error (MAPE) ###########
mape <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual)) * 100
}

############ Decision Tree evaluation  ###############
tree_mape <- mape(test_data$Population, tree_predictions)
cat("Decision Tree MAPE: ", tree_mape, "%\n")


###############Train your model (e.g., Random Forest) #############
library(randomForest)

#####Included every libraries as per prof. instruction.

######## Replace 'rf_model' with your chosen model object  ########
rf_model <- randomForest(Population ~ ., data = train_data)


########### Calculate Mean Absolute Percentage Error (MAPE)  ########
mape <- mean(ape, na.rm = TRUE) * 100


 

######## Calculate AIC for linear regression  #############
lm_aic <- AIC(lm_model)
cat("Linear Regression AIC: ", lm_aic, "\n")

######### Model predictions ###########
tree_predictions <- predict(tree_model, newdata = test_data)
lm_predictions <- predict(lm_model, newdata = test_data)

###### Model evaluation #######

######### Decision Tree evaluation  ##########
if (any(is.na(tree_predictions)) || any(is.na(test_data$Population))) {
  cat("Error: Missing values found in predictions or test_data$Population for Decision Tree\n")
} else {
  tree_mse <- mean((tree_predictions - test_data$Population)^2)
  tree_mape <- mape(test_data$Population, tree_predictions)
  cat("Decision Tree MSE: ", tree_mse, "\n")
  cat("Decision Tree MAPE: ", tree_mape, "%\n")
}


######## Load required libraries ##########
library(caTools)
library(ggplot2)
library(dplyr)
library(coefplot)

########Make the example reproducible ######
set.seed(1)

#### Data partitioning #######
sample <- sample.split(dataset$Population, SplitRatio = 0.7)
train_data <- subset(dataset, sample == TRUE)
test_data <- subset(dataset, sample == FALSE)

########### Modeling #########
lm_model1 <- lm(Population ~ ., data = train_data)
lm_model2 <- lm(Population ~ ., data = train_data[, -c(1:2)])


######## Model Evaluation #######
summary(lm_model1)
summary(lm_model2)


########### Coefficient plots #######
coefplot(lm_model1)
coefplot(lm_model2)


############ Model Prediction ######

#### Remove unused levels from the 'State' factor variable in the test data ##
test_data$State <- factor(test_data$State)

test_data$State <- droplevels(test_data$State)

##### Convert 'State' variable to factor #######
test_data$State <- factor(test_data$State)

##### Relevel the 'State' factor variable in the test data to match the training data ###
test_data$State <- factor(test_data$State, levels = levels(train_data$State))

lm_pred1 <- predict(lm_model1, newdata = test_data)
lm_pred1 <- predict(lm_model1, newdata = test_data[, ])
View(lm_pred1)
lm_pred2 <- predict(lm_model2, newdata = test_data[, ])

### Model Evaluation #####
cor(lm_pred1, test_data$Population)^2
cor(lm_pred2, test_data$Population)^2

##### Error Measures
SSE1 <- sum((lm_pred1 - test_data$Population)^2)
MSE1 <- mean((lm_pred1 - test_data$Population)^2)
MAE1 <- mean(abs(lm_pred1 - test_data$Population))
str(MAE1)

SSE2 <- sum((lm_pred2 - test_data$Population)^2)
MSE2 <- mean((lm_pred2 - test_data$Population)^2)
MAE2 <- mean(abs(lm_pred2 - test_data$Population))


error_df <- data.frame(
  MSE = c(MSE1),
  MAE = c(MAE1)
)

rownames(error_df) <- c("Model 1", "Model 2")
error_df


### Decision Tree Model #####
library(rpart)
library(rpart.plot)
tree_model <- rpart(Population ~ ., data = train_data)
rpart.plot(tree_model)
##Visualize the Decision Tree ####
rpart.plot(tree_model, box.palette = c("white", "skyblue"), shadow.col = "gray", nn = TRUE)
tree_predictions <- predict(tree_model, newdata = test_data[,])

cor(tree_predictions, test_data$Population)^2

### Error Measures ###
SSE_tree <- sum((tree_predictions - test_data$Population)^2)
MSE_tree <- mean((tree_predictions - test_data$Population)^2)
MAE_tree <- mean(abs(tree_predictions - test_data$Population))

error_df_tree <- data.frame(
  SSE = SSE_tree,
  MSE = MSE_tree,
  MAE = MAE_tree
)



#########################Random forest ###############################

install.packages(c("readxl", "caret", "randomForest"))
library(readxl)
library(caret)
library(randomForest)


###### Train a Random Forest model ######
model <- train(Population ~ ., data = train_data, method = "rf")


##### Calculate accuracy #####
accuracy <- confusionMatrix(predictions, test_data$Population)$overall["Accuracy"]
print(paste("Test Accuracy:", accuracy))



install.packages(c("readxl", "randomForest"))
library(readxl)
library(randomForest)

# Load the dataset
data <- read_excel("E:/R Programming/Census_India_New.xlsx")

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_indices <- sample(nrow(data), 0.8 * nrow(data))  # 80% for training
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

####Define predictor variables and target variable####
##Check if predictor variables exist in the dataset #####
existing_variables <- colnames(train_data)
missing_variables <- setdiff(predictor_variables, existing_variables)

#### Adjust predictor variables if necessary
predictor_variables <- setdiff(predictor_variables, missing_variables)
data$Population <- as.numeric(as.character(data$Population))

####Train the Random Forest model
model <- randomForest(x = train_data[, predictor_variables], y = train_data[[target_variable]])

##### Make predictions on the test set
predictions <- predict(model, newdata = test_data[, predictor_variables])

###### Calculate accuracy
accuracy <- sum(predictions == test_data[[target_variable]]) / length(test_data[[target_variable]])
print(paste("Test Accuracy:", accuracy))

######## Graphing #######

# Load required libraries
library(ggplot2)
library(readxl)



## Scatter plot for Population vs Literacy
ggplot(data, aes(x = Literacy, y = Population)) +
  geom_point() +
  labs(title = "Population vs Literacy",
       x = "Literacy",
       y = "Population")

#### Scatter plot for Population vs Growth
ggplot(data, aes(x = Growth, y = State)) +
  geom_point() +
  labs(title = "Growth vs State",
       x = "Growth",
       y = "State")

#### Scatter plot for Literacy vs Growth
ggplot(data, aes(x = Literacy, y = Growth)) +
  geom_point() +
  labs(title = "Literacy vs Growth",
       x = "Literacy",
       y = "Growth")

install.packages('ggplot2')
library(ggplot2)
install.packages('tidyverse')
library(tidyverse)


ggplot(data = CensusofIndia_2) + geom_density(aes(x = Literacy))

ggplot(data = Census_India, aes(x = Literacy)) + geom_density()  
ggplot(data = Census_India, aes(x = SexRatio)) + geom_density(color = 'Red')      
ggplot(data = CensusofIndia_2, aes(x = Growth...3)) + geom_density(color = 'purple')
ggplot(data = dataset, aes(x = Literacy)) + geom_density()

ggplot(dataset, aes(x = Literacy, y = State)) + geom_col()
ggplot(CensusofIndia_2, aes(x = State, y = Literacy)) + geom_point(color = 'blue')


library(ggplot2)
library(readxl)

#### Read the dataset
data <- read_excel("E:/R Programming/Census_India_New.xlsx")


#### Continuous vs Continuous: Scatterplot of Population vs. Literacy
ggplot(data, aes(x = Population, y = Literacy)) +
  geom_point(color = "blue") +
  labs(title = "Population vs. Literacy",
       x = "Population",
       y = "Literacy")

#### Continuous vs Categorical: Bar chart of State vs. Population
ggplot(data, aes(x = State, y = Population)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Population by State",
       x = "State",
       y = "Population") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### Categorical vs Categorical: Stacked bar chart of State vs. Ranking
ggplot(data, aes(x = State, fill = Ranking)) +
  geom_bar() +
  labs(title = "Ranking by State",
       x = "State",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("1" = "green", "2" = "orange", "3" = "red"))

#### Continuous vs Continuous: Scatterplot of Growth vs. SexRatio
ggplot(data, aes(x = Growth, y = SexRatio)) +
  geom_point(color = "blue") +
  labs(title = "Growth vs. SexRatio",
       x = "Growth",
       y = "SexRatio")

#### Continuous vs Categorical: Boxplot of State vs. Literacy
ggplot(data, aes(x = State, y = Literacy)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Literacy by State",
       x = "State",
       y = "Literacy") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Categorical vs Categorical: Stacked bar chart of State vs. Ranking (alternate representation)
ggplot(data, aes(x = Ranking, fill = State)) +
  geom_bar(position = "fill") +
  labs(title = "Ranking Distribution by State",
       x = "Ranking",
       y = "Proportion") +
  scale_fill_brewer(palette = "Set3")

ggplot(data, aes(x = State, y = Literacy)) + geom_point(color = 'Orange') 
ggplot(data, aes(x = Literacy)) + geom_boxplot(color = 'blue')  
ggplot(data, aes(y = Literacy)) + geom_boxplot(color = 'Green')  
ggplot(data, aes(x = State, y = Literacy )) + geom_boxplot(color = 'Orange')
ggplot(data, aes(x = State, y = Literacy)) + geom_point(color = 'Orange') 
ggplot(data, aes(x = Literacy, y = State)) + geom_point(color = 'Orange') 



####################2###############
library(ggplot2)
library(readxl)

###Read the dataset
data <- read_excel("E:/R Programming/Census_India_New.xlsx")

### Generate graphs using different variables

### Continuous vs Continuous: Scatterplot of Population vs. Literacy
ggplot(data, aes(x = Population, y = Literacy)) +
  geom_point(color = "blue") +
  labs(title = "Population vs. Literacy",
       x = "Population",
       y = "Literacy")

### Continuous vs Categorical: Bar chart of State vs. Population
ggplot(data, aes(x = Population, y = State)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Population by State",
       x = "Population",
       y = "State") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Categorical vs Categorical: Stacked bar chart of State vs. Ranking
ggplot(data, aes(x = State, fill = Ranking)) +
  geom_bar() +
  labs(title = "Ranking by State",
       x = "State",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("1" = "green", "2" = "orange", "3" = "red")) 

### Continuous vs Continuous: Scatterplot of Growth vs. SexRatio
ggplot(data, aes(x = Growth, y = SexRatio)) +
  geom_point(color = "blue") +
  labs(title = "Growth vs. Sex Ratio",
       x = "Growth",
       y = "Sex Ratio")

### Continuous vs Categorical: Boxplot of State vs. Literacy
ggplot(data, aes(x = State, y = Literacy)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Literacy by State",
       x = "State",
       y = "Literacy") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### Categorical vs Categorical: Stacked bar chart of State vs. Ranking (alternate representation)
ggplot(data, aes(x = Ranking, fill = State)) +
  geom_bar(position = "fill") +
  labs(title = "Ranking Distribution by State",
       x = "Ranking",
       y = "Proportion") +
  scale_fill_brewer(palette = "Set3")


library(caret)

### Set seed for reproducibility
set.seed(123)

#### Generate synthetic data ##from our class lessons and notes
data <- data.frame(
  feature1 = rnorm(1000),
  feature2 = rnorm(1000),
  target = factor(sample(0:1, 1000, replace = TRUE))
)

### Split the data into training and testing sets
trainIndex <- createDataPartition(data$target, p = 0.8, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

##### Train Logistic Regression model #####
log_model <- glm(target ~ ., data = train_data, family = "binomial")
View(log_model)
summary(log_model)
str(log_model)
####Train Decision Tree model
tree_model <- train(target ~ ., data = train_data, method = "rpart")
str(tree_model)
#### Predictions
log_predictions <- predict(log_model, newdata = test_data, type = "response")
tree_predictions <- predict(tree_model, newdata = test_data)

#### Calculate accuracy
log_accuracy <- confusionMatrix(table(ifelse(log_predictions > 0.5, 1, 0), test_data$target))$overall['Accuracy']
tree_accuracy <- confusionMatrix(table(tree_predictions, test_data$target))$overall['Accuracy']

log_accuracy
tree_accuracy
print("Logistic Regression Model:")
print(log_conf_matrix)
print(paste("Accuracy:", round(log_accuracy, 4)))

print("Decision Tree Model:")
print(tree_conf_matrix)
print(paste("Accuracy:", round(tree_accuracy, 4)))



###########

data <- read_excel("E:/R Programming/Census_India_New.xlsx")



### Filter out rows with missing values in Growth column
dataset<- dataset[complete.cases(dataset$Growth), ]

#### Sort the dataset by Growth in descending order to find the state with the highest growth rate
highest_growth_state <- dataset %>%
  arrange(desc(Growth)) %>%
  slice(1)  

cat("State with the highest growth rate:", highest_growth_state$State, "\n")

###### Now can conduct further analysis and modeling specific to the state with the highest growth rate
highest_growth_state_data <- dataset %>%
  filter(State == highest_growth_state$State)



#####regioanal comaprision
# Filter out rows with missing values in Growth column
dataset <- dataset[complete.cases(dataset$Growth), ]

#### Plot regional comparison graph (bar plot)
ggplot(data, aes(x = State, y = Growth, fill = State)) +
  geom_bar(stat = "identity") +
  labs(title = "Regional Comparison of Growth Rates",
       x = "State",
       y = "Growth Rate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d()  






#### Load the dataset
dataset <- read_excel("E:/R Programming/Census_India_New.xlsx")

#### Calculate overall ranking by summing ranks of all parameters for each state
dataset$Overall_Ranking <- rowSums(dataset[, c("Population", "Literacy", "Growth", "SexRatio")], na.rm = TRUE)

##Sort dataset by overall ranking in descending order
sorted_dataset <- dataset[order(-dataset$Overall_Ranking), ]

### Plot state-wise ranking graph (bar plot)
ggplot(sorted_dataset, aes(x = State, y = Overall_Ranking, fill = State)) +
  geom_bar(stat = "identity") +
  labs(title = "State-wise Overall Ranking",
       x = "State",
       y = "Overall Ranking") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d()

#### Convert scientific notation to numbers
number_in_scientific_notation <- "5.0e+1"
regular_number <- as.numeric(number_in_scientific_notation)

#### Print the result
print(regular_number)
