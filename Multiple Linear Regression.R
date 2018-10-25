data <- read.csv("C:/Users/Jason/Desktop/Udemy Data Science A-Z/Modelling/Multiple Linear Regression/50-Startups.csv")
library(knitr)
library(fastDummies)
library(ggplot2)

#make factor column into two new dummy variable columns for multiple linear regression
data <- dummy_cols(data)

#EDA
ggplot(data, aes(x = Profit, y = Administration, color = State))+geom_point()+theme_bw()

ggplot(data, aes(x = Profit, y = Marketing.Spend, color = State))+geom_point()+theme_bw()

ggplot(data, aes(x = Profit, y = R.D.Spend, color = State))+geom_point()+theme_bw()

#change columnn names so that there are no spaces
names(data) <- gsub(" ", "_", names(data))


#Building the model Using Backward Elimination method with significance Level P-Value = 0.05
library(caret)
multi_model <- lm(Profit ~ R.D.Spend + Administration + Marketing.Spend + State_New_York, data = data)
summary(multi_model)

# largest p value comes from Administration (.651) and is higher than our significance level - take out first
multi_model1 <- lm(Profit ~ R.D.Spend + Marketing.Spend + State_New_York, data = data)
summary(multi_model1)

#next largest p value is State_New_York (.577) and is higher than our significance level - next to take out
multi_model2 <- lm(Profit ~ R.D.Spend + Marketing.Spend, data = data)
summary(multi_model2)

#next largest p value is Marketing.Spend (.06 and is higher than our significance level,
#although is mostly linear and barely above our p value threshold
#model ends up becoming simple linear regression because of the significance levels
multi_model3 <- lm(Profit ~ R.D.Spend, data = data)
summary(multi_model3)

#however, since adjusted R-squared is highest with the Marketing.Spend variable, we will include it in our final regression model
multi_model_final <- lm(Profit ~ R.D.Spend + Marketing.Spend, data = data)
summary(multi_model_final)
