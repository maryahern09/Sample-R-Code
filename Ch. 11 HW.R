#install relevant packages
install.packages("multcomp")
library(multcomp)
install.packages(("olsrr"))
library(olsrr)

#download data
load("~/School/Biostats 576A/Data/LEAD.DAT.rdata")
load("~/School/Biostats 576A/Data/VALID.DAT.rdata")

#check download
head(lead)
head(valid)

# 11.13
#create data frame
age  <- c(1:17)
SBP <- c(99, 102, 105, 107, 108, 110, 111, 112, 114, 115, 117, 120, 122, 125, 127, 130, 132)
SBP_DF <- data.frame(age, SBP)

head(SBP_DF)

#fit regression line
SBP_lm <- lm(SBP ~ age, data = SBP_DF)
summary(SBP_lm)


# The p-value for the regression of age and blood pressure is <0.0001 and we can reject the null and say that age is a statistically significant predictor of SBP 

# 11.14
confint(SBP_lm, level = 0.95)

# The 95% confidence interval is 1.79 to 2.05 and the 95% CI for the intercept is 96.5 to 99.1. Neither include 0 confirming the statistical signifcance. 

# 11.15

97.79+1.92*13

# The predicted SBP for a 13 year old boy given the regression is 122.75. 

# 11.16
lincom_13 <- glht(SBP_lm, linfct = c("Intercept + 13*age = 0"))
summary(lincom_13)
confint(lincom_13)


# 11.17

97.79+1.92*17

lincom_17 <- glht(SBP_lm, linfct = c("Intercept + 17*age = 0"))
summary(lincom_17)

# 11.18

age.res = resid(SBP_lm)
library(ggplot2)
age.res.model <- ggplot(SBP_DF, aes(x = age, y = age.res), ylab = "Residuals", xlab = "age") + geom_point()

# No, because as we can see from this graph there is a lack of constant variance which is one of the main assumption of the test.  

# 11.46

lead_lm <- lm(lead$iqf ~ lead$Group + lead$ageyrs + lead$sex)
summary(lead_lm)

#assess goodness of fit
#linearity
model.age <- lm(iqf ~ ageyrs, data = lead)
summary(model.age)
with(lead, plot(ageyrs, iqf, main = "IQF versus Age"))
abline(model.age)

#residual normality
ols_plot_resid_hist(lead_lm)

#constant variance
res_group <- lm(lead$iqf ~ lead$Group)
group.res = resid(res_group)
plot.group.res <- ggplot(lead, aes(x = Group, y = group.res)) + geom_point()
plot.group.res + xlab("Sex") + ylab ("Residuals") 

res_ageyrs <- lm(lead$iqf ~ lead$ageyrs)
age.res = resid(res_ageyrs)
plot.age.res <- ggplot(lead, aes(x = ageyrs, y = age.res)) + geom_point()
plot.age.res + ylab ("Residuals") + xlab("age")

res_sex <- lm(lead$iqf ~ lead$sex)
sex.res = resid(res_sex)
plot.sex.res <- ggplot(lead, aes(x = sex, y = sex.res)) + geom_point()
plot.sex.res + ylab ("Residuals") + xlab("sex")

# When controlled for age and sex, the relationship between IQ and exposure group is not significant. In addition, the R^2 of the model is very low, meaning that only 2.7% of the variance in our dependent variable of IQF is explained by the model. 
#Lastly, when testing the assumptions of the model for the goodness of fit, we see that there is not constant variance amoung the groups and that the residuals for the model are not normally distributed. Given all of this, I do not believe this model 
# is a good fit for the data. 

# 11.72

#visualize data
hist(valid$alco_ffq)
hist(valid$alco_dr)

#spearman
cor.test(valid$alco_ffq, valid$alco_dr, method = c("spearman"), exact = FALSE)

# According to the spearman correlation, there is a statistically significant correlation between the alcohol intake reported on the diet recall and FFQ, and a strong one at the rho= 0.90. 