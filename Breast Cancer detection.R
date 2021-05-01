# Breast Cancer detection using Logistic Regression

# Importing the dataset

library(readr)
dataset = read_csv("breast_cancer.csv")
dataset$Class=ifelse(dataset$Class == "2", 0, ifelse(dataset$Class == "4", 1, 3))
dataset=dataset[-1]
dataset[10]

# Splitting the dataset into the Training set and Test set
library(caTools)
split = sample.split(dataset$Class, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
# Fitting Logistic Regression to the Training set
classifier = glm(formula = Class ~.,family='binomial',data=dataset)

# Predicting the Test set results
prob_pred = predict(classifier,training_set ,type = 'response')
prob_pred
y_pred = ifelse(prob_pred>0.5,1,0)
y_pred
# Making the Confusion Matrix
y_test=test_set$Class
y_test
cm = table(y_test, y_pred)
cm
