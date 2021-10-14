# QUESTION 1 
# (a) Calculate the ??2 test statistic by hand (even better if you can do "by hand" in R).

Fo_1 <- 14
Fo_2 <- 6
Fo_3 <- 7
Fo_4 <- 7
Fo_5 <- 7
Fo_6 <- 1
Fe_1 <- ((27/42) * 21)
Fe_2 <- ((27/42) * 13)
Fe_3 <- ((27/42) * 8)
Fe_4 <- ((15/42) * 21)
Fe_5 <- ((15/42) * 13)
Fe_6 <- ((15/42) * 8)
chisq <- (
  ((Fo_1 - Fe_1)^2/Fe_1) + ((Fo_2 - Fe_2)^2/Fe_2) +
    ((Fo_3 - Fe_3)^2/Fe_3) +  ((Fo_4 - Fe_4)^2/Fe_4) +
    ((Fo_5 - Fe_5)^2/Fe_5) + ((Fo_6 - Fe_6)^2/Fe_6)
)
chisq

# (b) Now calculate the p-value from the test statistic you just created (in R).2 What do you
# conclude if ?? = .1?


pchisq <- pchisq(3.791168, df = 2, lower.tail = FALSE)
pchisq

# As the value is 0,15 which is larger than the alpha We can Reject the null hypothesis 

# (c) Calculate the standardized residuals for each cell and put them in the table below.

library(tidyverse)
getwd()
setwd("C:/ASDS Git/Applied Stats I/StatsI_Fall2021")
mydata = read.table("MyData .csv", header=TRUE, sep=',')
datatable <- matrix(c(14, 6, 7, 7, 7, 1), nrow=3, ncol=2)
datatable

chisq <- chisq.test(datatable, correct = FALSE)
chisq
chisq$stdres # when they were stopped there was the least difference between expected and observed


# d) How might the standardized residuals help you interpret the results?

# QUESTION 2

# a) State a null and alternative (two-tailed) hypothesis.

# H0 : No effect of the reservation policy on the number of new or repaired drinking water facilities in the villages. 

# (b) Run a bivariate regression to test this hypothesis in R (include your code!)

Economics <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")

view(Economics)
lm1 <- lm(Economics$irrigation~Economics$reserved)
summary(lm1)

# The p-value is higher than .05, we fail to reject the null hypothesis. 

# (c) Interpret the coefficient estimate for reservation policy.

# The coefficient estimate is (-0.36), as it's value is negative we conclude that 
# the reservation policy has a negative relationship with the irrigation repair systems. 
 
# QUESTION 3

# (c) Interpret the coefficient estimate for reservation policy.

Fruitfly <- read.csv("https://www.zoology.ubc.ca/~bio501/R/data/fruitflies.csv")
view(Fruitfly)

# 2. Plot lifespan vs thorax. Does it look like there is a linear relationship? Provide the
# plot. What is the correlation coefficient between these two variables?

# One way of doing this:
scatter.smooth(Fruitfly$longevity.days, Fruitfly$thorax.mm, xlab = 'Longevity', ylab = 'Thorax', main = 'Scatterplot of linear relationship', col = c("blue", "red", "orange", "black"))
# Plotting with ggplot: 
ggplot(aes(longevity.days, thorax.mm), data = Fruitfly) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  ggtitle("Scatterplot of linear relationship") +
  labs(y="Thorax")+
  labs(x = "Longevity")

cor.test(Fruitfly$longevity.days, Fruitfly$thorax.mm)

# 0.63 strong positive correlation 

# 3. Regress lifespan on thorax. Interpret the slope of the fitted model.

lm <- lm(Fruitfly$longevity.days ~ Fruitfly$thorax.mm, data = Fruitfly)
lm 
# 144.33, This value suggests that for every 0.1 increase in Thorax, Longevity increases with 
# 14,43 increase in days. 
summary(lm)

# 4. Test for a significant linear relationship between lifespan and thorax. Provide and
# interpret your results of your test.

# Based on the results of the test we can reject the null hypothesis (no linear relationship between lifespan and thorax)

#5. Provide the 90% confidence interval for the slope of the fitted model.

#  Use the formula of confidence interval.

lower_interval <- (144.33 - ((15.77*1.645)))
lower_interval
upper_interval <- (144.33 + ((15.77*1.645)))
upper_interval

# Use the function confint() in R 

confint(lm)
confint(lm, level = 0.90)

# 6. Use the predict() function in R to (1) predict an individual fruitfly's lifespan when
# thorax=0.8 and (2) the average lifespan of fruitflies when thorax=0.8 by the fitted
# model. This requires that you compute prediction and confidence intervals. What
# are the expected values of lifespan? What are the prediction and confidence intervals
# around the expected values?

# lm is the regression line --> we want to use the regression line to make prediction off
Fruitfly_ <- lm(longevity.days ~ thorax.mm, data = Fruitfly)
new_df <- data.frame (thorax.mm = 0.8)

predict(Fruitfly_, newdata = new_df)
# 54.4 days it would live 

predict(Fruitfly_, newdata = new_df, interval = 'confidence')

# 7. For a sequence of thorax values, draw a plot with their fitted values for lifespan, as
# well as the prediction intervals and confidence intervals.
Fruitfly <- read.csv("https://www.zoology.ubc.ca/~bio501/R/data/fruitflies.csv")
view(Fruitfly)


Fruitfly_lm <- lm(longevity.days ~ thorax.mm, data = Fruitfly)

my_conf <- ggplot(Fruitfly, aes(x=Fruitfly$thorax.mm, y=Fruitfly$longevity.days)) +
  geom_point() +
  geom_smooth(method=lm, color="red", fill="#69b3a2", se=TRUE)

my_predict <- predict(Fruitfly_lm, interval = "prediction")
my_df <- cbind(Fruitfly, my_predict)

ggplot(my_df, aes(thorax.mm, longevity.days)) +
  geom_point() +
  geom_line(aes(y=lwr), color = "red", linetype = "dashed") +
  geom_line(aes(y=upr), color = "red", linetype = "dashed") +
  geom_smooth(method="lm", formula = y ~ x)



