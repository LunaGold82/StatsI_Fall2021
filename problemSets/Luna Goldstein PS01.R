#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set working directory
setwd("c:/ASDS Git/Applied Stats I/StatsI_Fall2021/problemSets/PS01")

#####################
# Problem 1
#####################

# Part 1:

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

t.test(y, conf.level = 0.9)

# part 2: 

t.test(y, mu = 100, alternative = "greater")

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/expenditure.txt", header=T)

#part 1 
str (expenditure)
lines(expenditure$Y)
lines(expenditure$X1)
lines(expenditure$X2)
lines(expenditure$X3)
plot(expenditure, ylim=range(expenditure$Y, expenditure$X1,expenditure$X2,expenditure$X3), col='red', main = "Expenditure of states in US", lower.panel = NULL)

# part 2 
# option a 

plot(expenditure$Region, expenditure$Y,
     pch = 16, frame = TRUE, 
     xlab = "Region", ylab = "Y", col = "red")

# option b 
install.packages("ggplot2")
library(ggplot2)
data=as.data.frame(expenditure[,c(2,6)])
data$Region = as.factor(data$Region)
mode(data$Region)

#create a box plot graph comparing expenditure and region
ggplot(aes(y = Y, x = Region, fill=Region), data = data)+ geom_boxplot()+ggtitle("Box plots of Expenditure by Region")

# Part 3 
scatter.smooth( expenditure$X1, expenditure$Y, xlab = 'Personal Income', ylab = 'expenditure', main = 'Income and expenditure', col = c("blue", "red", "orange", "black"), pch = c(0,1,2,3))

