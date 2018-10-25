data <- read.csv("C:/Users/Jason/Desktop/Udemy Data Science A-Z/Modelling/Logistic Regression/Email-Offer.csv")
data$TookAction <- as.factor(data$TookAction)

#train/test split
library(caTools)
split <- sample.split(data$TookAction, SplitRatio = .75)
tookaction_train <- subset(data, split==T)
tookaction_test <- subset(data, split==F)

#logistic regression model
model <- glm(TookAction ~ ., data = tookaction_train, family = binomial)
summary(model)

probs <- predict(model, type = "response")

#confusion matrix
model_pred <- ifelse(probs > 0.5, "Took Action", "No Action")
train_action = tookaction_train$TookAction
table(model_pred, train_action)

#plot the line
library(ggplot2)
ggplot(tookaction_train, aes(x = Age, y = TookAction)) + 
  geom_point() +
  theme_bw() +
  stat_smooth(method = 'glm', family = 'binomial', se = FALSE)

#try model on our test set
new_probs <- predict(model, newdata = tookaction_test, type = "response")
model_pred_test <- ifelse(new_probs > 0.5, "Took Action", "No Action")
test_action = tookaction_test$TookAction
table(model_pred_test, test_action)

ggplot(tookaction_test, aes(x = Age, y = TookAction)) + 
  geom_point() +
  theme_bw() +
  stat_smooth(method = 'glm', family = 'binomial', se = FALSE)