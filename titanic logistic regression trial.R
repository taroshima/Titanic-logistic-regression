library("dplyr")
library("tidyr")
library("plotly")
library("ggplot2")
library("shiny")
library("caret")


titanic_train <- read.csv("titanic_train.csv")
str(titanic_train)

titanic_train$Cabin[titanic_train$Cabin == ""] <- NA

null_counts <- colSums(is.na(titanic_train))
null_counts_df <- data.frame(Column = names(null_counts), Null_Count = null_counts)

#bar plot for null values
ggplot(null_counts_df, aes(x = reorder(Column, Null_Count), y = Null_Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Number of Null Values in Each Column", x = "Column", y = "Number of Null Values") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(titanic_train, aes(x = factor(Survived))) +
  geom_bar(fill = "blue") +
  labs(title = "Count of Survived Passengers", x = "Survived", y = "Count") +
  scale_x_discrete(labels = c("No", "Yes"))


ggplot(titanic_train, aes(x = factor(Survived), fill = Sex)) +
  geom_bar(position = "dodge") +
  labs(title = "Count of Survived Passengers by Sex", x = "Survived", y = "Count") +
  scale_x_discrete(labels = c("No", "Yes")) +  
  scale_fill_manual(values = c("#1f77b4", "#d62728"))


ggplot(titanic_train, aes(x = factor(Survived), fill = factor(Pclass))) +
  geom_bar(position = "dodge") +
  labs(title = "Count of Survived Passengers by Pclass", x = "Survived", y = "Count") +
  scale_x_discrete(labels = c("No", "Yes")) +  
  scale_fill_manual(values =c("red","blue","green") )


ggplot(titanic_train, aes(x = Age)) +
  geom_histogram(binwidth = 3, fill = "darkred", color = "black") +
  geom_density(color = "darkred") +
  labs(title = "Distribution of Age", x = "Age", y = "Frequency") +
  theme_minimal()



#Filling in the null values
median_age <- aggregate(Age ~ Pclass, data = titanic_train, FUN = median)

p <- ggplot(titanic_train, aes(x = factor(Pclass), y = Age)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(title = "Distribution of Age by Pclass", x = "Pclass", y = "Age") +
  scale_x_discrete(labels = c("1st Class", "2nd Class", "3rd Class")) +  
  theme_minimal()

p + geom_text(data = median_age, aes(x = factor(Pclass), y = Age, label = paste("Median:", round(Age, 1))),vjust = -0.5)

median_age <- titanic_train %>%
  group_by(Pclass) %>%
  summarize(Median_Age = median(Age, na.rm = TRUE))

# Fill NA values in 'Age' column with median age for each 'Pclass'
titanic_train <- titanic_train %>%
  left_join(median_age, by = "Pclass") %>%
  mutate(Age = ifelse(is.na(Age), Median_Age, Age)) %>%
  select(-Median_Age)

titanic_train <- titanic_train %>%
  select(-Cabin)


#Building the logistic regression model

# Converting columns to dummy variables in order for it be of use during the regression
sex_dummies <- as.data.frame(model.matrix(~ Sex - 1, data = titanic_train))
embark_dummies <- as.data.frame(model.matrix(~ Embarked - 1, data = titanic_train))

titanic_train <- titanic_train %>%
  select(-c(Sex, Embarked, Name, Ticket)) 

titanic_train <- cbind(titanic_train, sex_dummies, embark_dummies)



set.seed(101)  # Set random seed for reproducing the test , train split
train_indices <- createDataPartition(titanic_train$Survived, p = 0.7, list = FALSE)
train_data <- titanic_train[train_indices, ]
test_data <- titanic_train[-train_indices, ]

# Fit logistic regression model
logmodel <- glm(Survived ~ ., data = train_data, family = binomial)

# Make predictions on the test set
predictions <- predict(logmodel, newdata = test_data, type = "response")









confusion_matrix <- table(predictions = ifelse(predictions > 0.5, 1, 0), actual = test_data$Survived)
confusion_matrix


accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy



precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
precision
recall
