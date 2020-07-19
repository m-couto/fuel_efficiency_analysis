# Course project


# Questions:
    # Is an automatic or manual transmission better for MPG ?
    # Quantify the MPG difference between automatic and manual transmissions


library(datasets)
library(ggplot2)
library(dplyr)

head(mtcars)

# Variables:
    # mpg: Miles/(US) gallon
    # cyl: Number of cylinders (4, 6, 8)
    # disp: Displacement (cu.in.)
    # hp: Gross horsepower
    # drat: Rear axle ratio
    # wt: Weight (1000 lbs)
    # qsec: 1/4 mile time
    # vs: Engine (0 = V-shaped, 1 = straight)
    # am: Transmission (0 = automatic, 1 = manual)
    # gear: Number of forward gears
    # carb: Number of carburetors


# Exploratory data analysis

plot(mtcars$mpg)


# mpg ~ am and cyl
mtcars$am <- factor(mtcars$am)

ggplot(mtcars, aes(y=mpg, color=am)) + geom_boxplot() +
    theme(axis.text.x = element_blank())

ggplot(mtcars, aes(y=mpg, color=am)) + geom_boxplot() +
    facet_grid(. ~ cyl) + ylab('Miles per galon') +
    ggtitle('Miles per galon per type of transmission and number of cylinders') +
    theme(axis.text.x = element_blank()) +
    scale_color_discrete(name = 'Transmission type',
                        labels = c('automatic', 'manual'))

mtcars$cyl <- factor(mtcars$cyl)
plot(mtcars$mpg ~ mtcars$cyl)


# mpg ~ wt
hist(mtcars$wt)

ggplot(mtcars, aes(x=wt)) + geom_histogram(aes(color=am), fill='white')

ggplot(mtcars, aes(y=mpg, x=wt, color=am)) + geom_point() +
    xlab('Weight (1000 lbs)') + ylab('Miles per galon') +
    ggtitle('Miles per galon per type of transmission and weight') +
    scale_color_discrete(name = 'Transmission type', labels = c('automatic', 'manual'))


# mpg ~ qsec
hist(mtcars$qsec)

ggplot(mtcars, aes(y=mpg, x=qsec, color=am)) + geom_point() +
    xlab('Quarter of mile time') + ylab('Miles per galon') +
    ggtitle('Miles per galon per type of transmission and 1/4 mile time') +
    scale_color_discrete(name = 'Transmission type', labels = c('automatic', 'manual'))





# Regression models

# Model 1: mpg ~ am
fit1 <- lm(mpg ~ am - 1, data=mtcars)
summary(fit1)$coef

# 17.14737 is the predicted average mpg for automatic transmission cars
# 24.39231 is the predicted average mpg for manual transmission cars

# Confirmation:
tab <- mtcars %>% group_by(am) %>% summarise(mn = mean(mpg))
as.data.frame(tab)

lapply(split(mtcars$mpg, mtcars$am), mean)

# Therefore, the predicted difference in mpg mean between manual and
# transmission cars is
summary(fit1)$coef[2,1] - summary(fit1)$coef[1,1]




# Model 2: mpg ~ am
fit2 <- lm(mpg ~ am, data=mtcars)
summary(fit2)$coef

# Therefore, the predicted difference in mpg mean between manual and
# transmission cars is
summary(fit2)$coef[2,1]

# Small p-values mean:
    # the expected mpg for automatic transmission cars (am=0) is not 0
    # the expected mpg is different for automatic transmission cars (am=0)
        # and manual transmission cars (am=1)


# Residuals:
e <- resid(fit2)
plot(e, pch=21, col='black', bg='lightblue', frame=FALSE)
abline(h=0, lwd=2)

    # no discernable pattern

# std error:
summary(fit2)$sigma



# Model 3: mpg ~ am + cyl

fit3 <- lm(mpg ~ am + cyl, data=mtcars)
summary(fit3)$coef



# Best model:

# load the mtcars data starting regression model
fit <- lm(mpg ~ . , data = mtcars)

# step-wise search
step(fit)

# Therefore, the best model that captures most of the variability in the data
# is simply mpg ~ wt + qsec + am.


bestfit <- lm(mpg ~ wt + qsec + am, data=mtcars)

# coefficients
summary(bestfit)$coef

    # 2.935837: difference in mpg mean between manual transmission (am=1)
        # and automatic transmission (am=0) holding weight and qsec constant
    # -3.916504: average effect on mpg of a 1000lbs increase in weight
        # holding qsec and am constant
    # 1.225886: average effect on mpg of a 1-unit increase on the time for
        # travelling 1/4 mile, holding weight and am constant


# Residuals plot
e <- resid(bestfit)
plot(e, pch=21, col='black', bg='lightblue', frame=FALSE,
     xlab='cars', ylab='residuals', main='Residual plot')
abline(h=0, lwd=2)

    # there is no discernable pattern here
    # so there doesn't seem to be a problem here


# std error:
summary(bestfit)$sigma


# Analysis of variance
anova(bestfit)



# Diagnostics:

# dfbetas: difference in coeffs when including vs excluding the data point
dfbetaMat <- dfbetas(bestfit)

index <- c()
title <- c('intercept', 'weight', 'qsec', 'manual transmission')

par(mfrow=c(2,2))

for (i in 1:4){
    plot(dfbetaMat[,i], xlab='cars', ylab='dfbeta values', main = paste('dfbeta values for', title[i] , 'term'))
    index <- c( index, which.max(dfbetaMat[,i]) )
}

par(mfrow=c(1,1))

# The cars with the most dfbeta value for each regressor is:
# Merc 230 Chrysler Imperial          Fiat 128 Chrysler Imperial 
# 9                17                18                17 

# So Chrysler Imperial seems to have more potential for influence
# but its dfbeta values are not much larger than the rest of the data.



# hatvalues: measures the potential for influence for each point
hats <- hatvalues(bestfit)
plot(hats, pch=21, col='black', bg='lightblue', frame=FALSE,
     xlab='cars', ylab='hat values', main='hat values plot')

# All values seem relatively low.



# Inference:

t.test(mpg ~ am, data=mtcars, paired=FALSE, var.equal=FALSE, alternative='less')

