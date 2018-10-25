library(ggplot2)

data <- read.csv("C:/Users/Jason/Desktop/Udemy Data Science A-Z/Modelling/Simple Linear Regression/SalaryData.csv")
ggplot(model, aes(x = YearsExperience, y = Salary)) + 
  geom_point() + 
  geom_smooth(method = "lm")

model <- lm(Salary ~ YearsExperience, data = data)
summary(model)

predict_salary <- predict(model, interval = "prediction")
new_df <- cbind(data, predict_salary)

ggplot(new_df, aes(x = YearsExperience, y = Salary))+
  geom_point()+
  geom_smooth(method = lm, se = T)