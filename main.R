# -------------------------------------------------------------------------
# Import data and set up
# -------------------------------------------------------------------------

data <- read.csv("/home/sherwin/Downloads/Ecommerce_ Customers")
View(data)

str(data)
summary(data)

# -------------------------------------------------------------------------
# Create plots and search for insights
# -------------------------------------------------------------------------

library(tidyverse)
library(ggplot2)

# correlation b/w website and yearly amount spent

ggplot(data,aes(x=Time.on.Website,y=Yearly.Amount.Spent)) +
  geom_point(color="orange") +
  ggtitle("Time on Website against Yearly Amount Spent") +
  xlab("Time on webiste") +
  ylab("Yearly amount spent")


# avg session length vs yearly amount spent

ggplot(data,aes(x=Avg..Session.Length,y=Yearly.Amount.Spent)) +
  geom_point(color="green") +
  ggtitle("Session Length against Yearly Amount Spent") +
  xlab("Average Session Length") +
  ylab("Amount Spent")

## pairplot of all continuous variables 

pairs(data[c("Avg..Session.Length","Time.on.App","Time.on.Website","Length.of.Membership",
             "Yearly.Amount.Spent")],
      col="blue",
      pch=16,
      main = "Pairplot of all continous variables"
      )


# -------------------------------------------------------------------------
# Exploring Selected Variables 
# -------------------------------------------------------------------------

# is the varibale normally distributed

hist(data$Length.of.Membership)

# for gggplot here is the code

ggplot(data,aes(x=Length.of.Membership)) +
  geom_histogram(
    color="black",
    fill="orange",
    binwidth = 0.5
  )

# checking outliers for the choosen variable

boxplot(data$Length.of.Membership)

# code for ggplot

ggplot(data,aes(y=Length.of.Membership)) +
  geom_boxplot(fill="orange")


# -------------------------------------------------------------------------
# Fitting a Linear Model
# -------------------------------------------------------------------------

attach(data)
lm.fit1 <- lm(Yearly.Amount.Spent~Length.of.Membership)

summary(lm.fit1)

plot(Yearly.Amount.Spent~Length.of.Membership)

abline(lm.fit1,col="red")


# -------------------------------------------------------------------------
# Residual Analysis
# -------------------------------------------------------------------------


hist(residuals(lm.fit1))

qqnorm(residuals(lm.fit1))
qqline(residuals(lm.fit1),col="red")

shapiro.test(residuals(lm.fit1))


# -------------------------------------------------------------------------
# Evaluation of the Model
# -------------------------------------------------------------------------

set.seed(1)

row.number <- sample(1:nrow(data),0.8*nrow(data))

train <- data[row.number,]

test <- data[-row.number,]

# estimate the linear fit with the training set

lm.fit0.8 <- lm(Yearly.Amount.Spent~Length.of.Membership,data=train)

summary(lm.fit0.8)

# predict in the test dataset

prediction0.8 <- predict(lm.fit0.8, newdata = test)


err0.8 <- prediction0.8 - test$Yearly.Amount.Spent

# root mean square error

rmse <- sqrt(mean(err0.8^2))

# mean absolute percentage error

mape <- mean(abs(err0.8/test$Yearly.Amount.Spent))


c(RMSE=rmse,mape=mape,R2=summary(lm.fit0.8)$r.squared)


# -------------------------------------------------------------------------
# Multiple regression
# -------------------------------------------------------------------------

attach(data)

lm.fit <- lm(Yearly.Amount.Spent~Avg..Session.Length + 
               Time.on.App + Time.on.Website + Length.of.Membership)


summary(lm.fit)

# estimate the linear fit with the training set

multi.lm.fit0.8 <- lm(Yearly.Amount.Spent~Avg..Session.Length + 
                  Time.on.App + Time.on.Website + Length.of.Membership,data=train)

summary(lm.fit0.8)

# predict in the test dataset

prediction0.8 <- predict(multi.lm.fit0.8, newdata = test)


err0.8 <- prediction0.8 - test$Yearly.Amount.Spent

# root mean square error

rmse <- sqrt(mean(err0.8^2))

# mean absolute percentage error

mape <- mean(abs(err0.8/test$Yearly.Amount.Spent))


c(RMSE=rmse,mape=mape,R2=summary(lm.fit0.8)$r.squared)
