#Load packages
library("AER")

#Load data and view
data <- Affairs
View(Affairs)
summary(Affairs)

#Dummy code affairs
Affairs$ynaffair[Affairs$affairs >  0] <- 1
Affairs$ynaffair[Affairs$affairs == 0] <- 0
Affairs$ynaffair <- factor(Affairs$ynaffair,levels=c(0,1), labels=c("No","Yes"))

#Logistic regression 
fit.full <- glm(ynaffair ~ gender + age + yearsmarried + children 
                + religiousness + education + occupation +rating,
                data=Affairs, family=binomial())
summary(fit.full)
#p-values for the regression coefficients, then we find that gender, 
#presence of children, education, and occupation do not have a significant contribution 
#to our response variable
fit.reduced   <- glm(ynaffair ~ age + yearsmarried + religiousness
                     + rating, data=Affairs, family=binomial())
summary(fit.reduced)

#The two models can be compared using anova. The results show the full model is no better at expalining the model than the reduced model.
anova(fit.reduced, fit.full, test="Chisq")

#log(odds) are hard to interpret, so the coefficents have been transformed by exponentiating the outcome as foll.
exp(coef(fit.reduced))

#Predictions. Say new data is added as follows.
newdata1 <- data.frame(rating=c(1,2,3,4,5),age=mean(Affairs$age),
                       yearsmarried=mean(Affairs$yearsmarried),
                       religiousness=mean(Affairs$religiousness))

#Group identification of affair or not can be predicted as follows.
newdata1$prob <- predict(fit.reduced, newdata=newdata1,
                         type="response")

