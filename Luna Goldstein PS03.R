install.packages("tidyverse")
library(ggplot2)
options(scipen = 999) 
incumbents <- read.csv("c:/ASDS Git/Applied Stats I/StatsI_Fall2021/datasets/incumbents_subset.csv")

# (1)
# Run a regression where the outcome variable is voteshare and the explanatory variable
# is difflog.
campaign <- lm(incumbents$voteshare ~ incumbents$difflog)
summary(campaign)

# Make a scatterplot of the two variables and add the regression line.
plot(incumbents$difflog, incumbents$voteshare,
     main = "Scatter Plot of Two variables",
     xlab = "Explanatory Variable on X axis", # Voteshare
     ylab = "Outcome Variable on y axis") # difflog
abline(lm(incumbents$voteshare ~ incumbents$difflog), col = "red")

ggplot(incumbents, aes(x=difflog, y=voteshare)) + geom_point() + geom_smooth(formula = y ~ x,method=lm, se=FALSE, fullrange=TRUE) + labs(title="Scatter Plot of Two variables",
                                                                      x="Difflog", y = "Voteshare")
# Save the residuals of the model in a separate object.
campaign_resid <- resid(campaign)
campaign_resid

# Write the prediction equation.
lm(formula = voteshare ~ difflog, data = incumbents)
# B0 = 0,57903
# B1 = 0,04167
# X = Difflog
# Y = Voteshare 
# Voteshare = 0,579 + 0,042 * difflog 

# (2)
# Run a regression where the outcome variable is presvote and the explanatory variable
# is difflog
spending <- lm(incumbents$presvote ~ incumbents$difflog)
summary(spending)

# Make a scatterplot of the two variables and add the regression line.
plot(incumbents$difflog, incumbents$presvote,
     main = "Scatter Plot of Two variables",
     xlab = "Explanatory Variable on X axis", # presvote
     ylab = "Outcome Variable on y axis") # difflog
abline(lm(incumbents$presvote ~ incumbents$difflog), col = "red")

ggplot(incumbents, aes(x=difflog, y=presvote)) + geom_point() + geom_smooth(formula = y ~ x,method=lm, se=FALSE, fullrange=TRUE) + labs(title="Scatter Plot of Two variables",
                                                                                                                         x="Difflog", y = "Presvote")
# Save the residuals of the model in a separate object.
spending_resid <- resid(spending)
spending_resid

# Write the prediction equation.
lm(formula = presvote ~ difflog, data = incumbents)
# B0 = 0,57903
# B1 = 0,04167
# X = Difflog
# Y = presvote 
# Presvote = 0,508 + 0,024 * difflog 

# (3)
# Run a regression where the outcome variable is voteshare and the explanatory variable
# is presvote
pres_vote <- lm(incumbents$voteshare ~ incumbents$presvote)
summary(pres_vote)

# Make a scatterplot of the two variables and add the regression line.
plot(incumbents$presvote, incumbents$voteshare,
     main = "Scatter Plot of Two variables",
     xlab = "Explanatory Variable on X axis", # Voteshare
     ylab = "Outcome Variable on y axis") # presvote
abline(lm(incumbents$voteshare ~ incumbents$presvote), col = "red")

ggplot(incumbents, aes(x=presvote, y=voteshare)) + geom_point() + geom_smooth(formula = y ~ x,method=lm, se=FALSE, fullrange=TRUE) + labs(title="Scatter Plot of Two variables",
                                                                                                                         x="Presvote", y = "Voteshare")
# Save the residuals of the model in a separate object.
pres_vote_resid <- resid(pres_vote)
pres_vote_resid

# Write the prediction equation.
lm(formula = voteshare ~ presvote, data = incumbents)
# B0 = 0,4413 
# B1 = 0,3880 
# X = Voteshare
# Y = Presvote
# Voteshare = 0,508 + 0,024 * Presvote

# (4)
# Run a regression where the outcome variable is the residuals from Question 1 and the
# explanatory variable is the residuals from Question 2
ques_4 <- lm(campaign_resid ~ spending_resid)
summary(ques_4)

# Make a scatterplot of the two residuals and add the regression line.
ggplot(incumbents, aes(x=spending_resid, y=campaign_resid)) + geom_point() + geom_smooth(formula = y ~ x,method=lm, se=FALSE, fullrange=TRUE) + labs(title="Scatter Plot of Two residuals",
                                                                                                                         x="unexplained variation in Difflog", y = "unexplained variation in voteshare")
# Write the prediction equation
lm(formula = campaign_resid ~ spending_resid, data = incumbents)
# B0 = -4.860e-18
# B1 = 2.569e-01
# X = spending_resid
# Y = campaign_resid
# campaign_resid = (-4.860e-18) + (2.569e-01) * spending_resid

# (5)
#  Run a regression where the outcome variable is the incumbent's voteshare and the
# explanatory variables are difflog and presvote.
model <- lm(voteshare ~ difflog + presvote, data = incumbents)
summary(model)

# Write the prediction equation.
lm(formula = voteshare ~ difflog + presvote, data = incumbents)
# B0 = 0.44864 
# B1 = 0.03554 
# B2 = 0.25688
# X1 = Difflog
# X2 = Presvote
# Y = Voteshare 
# Voteshare = 0.44864 + 0.03554 * Difflog  + 0.25688 * Presvote

# What is it in this output that is identical to the output in Question 4? Why do you
# think this is the case?

# The it is the same value as the unexplained variance, meaning that when you add a variable it explains it. 
