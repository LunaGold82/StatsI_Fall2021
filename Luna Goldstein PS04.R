install.packages('car')
library(car)
data(Prestige)
help(Prestige)

# Create a new variable professional by recoding the variable type so that professionals
# are coded as 1, and blue and white collar workers are coded as 0 (Hint: ifelse.)
Prestige$professional <- ifelse(Prestige$type == 'prof', 1, 0)

# Run a linear model with prestige as an outcome and income, professional, and the
# interaction of the two as predictors (Note: this is a continuous × dummy interaction.)
model1 <- lm(prestige ~ income + professional + income:professional, data = Prestige)
model1

# What is the effect of a $1,000 increase in income on prestige score for professional
# occupations? In other words, we are interested in the marginal effect of income when
# the variable professional takes the value of 1. Calculate the change in ^y associated
# with a $1,000 increase in income based on your answer for (c).
effect1 <- 21.142259 + 0.003171*0 + 37.781280 * 1 + (-0.002326 * 0 * 1)
effect1
effect2 <- 21.142259 + 0.003171 * 1000 + 37.781280 * 1 + (-0.002326 * 1000 * 1)
effect2
effect3 <- effect2 - effect1
effect3

# What is the effect of changing one's occupations from non-professional to professional
# when her income is $6,000? We are interested in the marginal effect of professional
# jobs when the variable income takes the value of 6, 000. Calculate the change in ^y
# based on your answer for (c).
effect4 <- 21.142259 + 0.003171 * 6000 + 37.781280 * 1 + (-0.002326 * 6000 * 1)
effect4
effect5 <-21.142259 + 0.003171 * 6000 + 37.781280 * 0 + (-0.002326 * 6000 * 0)
effect5
effect6 <- effect4 - effect5 
effect6

