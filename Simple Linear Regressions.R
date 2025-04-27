data <- read_csv("Life-Expectancy-Data-Updated_csv")

data1 <- data %>% select(-Country, -Year, -Region)
library(dplyr)
library(ggplot2)

model1 <- lm(Life_expectancy ~., data = data1)
summary(model1)


# 1. Infant_deaths
model1 <- lm(Life_expectancy ~ Infant_deaths, data = data)
summary(model1)
ggplot(data, aes(x = Infant_deaths, y = Life_expectancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Life Expectancy vs Infant Deaths")

# 2. Under_five_deaths
model2 <- lm(Life_expectancy ~ Under_five_deaths, data = data)
summary(model2)
ggplot(data, aes(x = Under_five_deaths, y = Life_expectancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Life Expectancy vs Under Five Deaths")

# 3. Adult_mortality
model3 <- lm(Life_expectancy ~ Adult_mortality, data = data)
summary(model3)
ggplot(data, aes(x = Adult_mortality, y = Life_expectancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Life Expectancy vs Adult Mortality")

# 4. Alcohol_consumption
model4 <- lm(Life_expectancy ~ Alcohol_consumption, data = data)
summary(model4)
ggplot(data, aes(x = Alcohol_consumption, y = Life_expectancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Life Expectancy vs Alcohol Consumption")

# 5. Hepatitis_B
model5 <- lm(Life_expectancy ~ Hepatitis_B, data = data)
summary(model5)
ggplot(data, aes(x = Hepatitis_B, y = Life_expectancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Life Expectancy vs Hepatitis B")

# 6. Measles
model6 <- lm(Life_expectancy ~ Measles, data = data)
summary(model6)
ggplot(data, aes(x = Measles, y = Life_expectancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Life Expectancy vs Measles")

# 7. BMI
model7 <- lm(Life_expectancy ~ BMI, data = data)
summary(model7)
ggplot(data, aes(x = BMI, y = Life_expectancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Life Expectancy vs BMI")

# 8. Polio
model8 <- lm(Life_expectancy ~ Polio, data = data)
summary(model8)
ggplot(data, aes(x = Polio, y = Life_expectancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Life Expectancy vs Polio")

# 9. Diphtheria
model9 <- lm(Life_expectancy ~ Diphtheria, data = data)
summary(model9)
ggplot(data, aes(x = Diphtheria, y = Life_expectancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Life Expectancy vs Diphtheria")

# 10. Incidents_HIV
model10 <- lm(Life_expectancy ~ Incidents_HIV, data = data)
summary(model10)
ggplot(data, aes(x = Incidents_HIV, y = Life_expectancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Life Expectancy vs HIV Incidents")

# 11. GDP_per_capita
model11 <- lm(Life_expectancy ~ GDP_per_capita, data = data)
summary(model11)
ggplot(data, aes(x = GDP_per_capita, y = Life_expectancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Life Expectancy vs GDP per Capita")

# 12. Population_mln
model12 <- lm(Life_expectancy ~ Population_mln, data = data)
summary(model12)
ggplot(data, aes(x = Population_mln, y = Life_expectancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Life Expectancy vs Population (Millions)")

# 13. Thinness_ten_nineteen_years
model13 <- lm(Life_expectancy ~ Thinness_ten_nineteen_years, data = data)
summary(model13)
ggplot(data, aes(x = Thinness_ten_nineteen_years, y = Life_expectancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Life Expectancy vs Thinness (10-19 Years)")

# 14. Thinness_five_nine_years
model14 <- lm(Life_expectancy ~ Thinness_five_nine_years, data = data)
summary(model14)
ggplot(data, aes(x = Thinness_five_nine_years, y = Life_expectancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Life Expectancy vs Thinness (5-9 Years)")

# 15. Schooling
model15 <- lm(Life_expectancy ~ Schooling, data = data)
summary(model15)
ggplot(data, aes(x = Schooling, y = Life_expectancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Life Expectancy vs Schooling")

# 16. Economy_status_Developed
model16 <- lm(Life_expectancy ~ Economy_status_Developed, data = data)
summary(model16)
ggplot(data, aes(x = Economy_status_Developed, y = Life_expectancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Life Expectancy vs Developed Status")

# 17. Economy_status_Developing
model17 <- lm(Life_expectancy ~ Economy_status_Developing, data = data)
summary(model17)
ggplot(data, aes(x = Economy_status_Developing, y = Life_expectancy)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Life Expectancy vs Developing Status")
