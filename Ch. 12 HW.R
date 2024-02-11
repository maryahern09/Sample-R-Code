#install relevant packages
install.packages("psych")
library(psych)
install.packages("tidyverse")
library(tidyverse)
install.packages("gglot2")
library(ggplot2)

# 12.6

#create data set
reactivity <- c(20.8, 4.1, 30.0, 24.7, 13.8,7.5, 7.5, 11.9, 4.5, 3.1, 8.0, 4.7, 28.1, 10.3, 10.0, 5.1, 2.2, 9.2, 2.0, 2.5, 6.1, 7.5)
LF <- c("low", "low", "low","low","low", 
        "medium", "medium", "medium","medium","medium","medium","medium","medium","medium","medium","medium","medium",
        "high", "high", "high", "high", "high")
df_so2 <- data.frame(reactivity, LF)

#Anova
lm_so2 <- lm(reactivity ~ LF, data = df_so2)
anova(lm_so2)

# 12.8 

pairwise.t.test(df_so2$reactivity, df_so2$LF, p.adjust.method = "bonferroni")

#When comparing the means of each pairwise comparison using the Bonferroni method, there is a statistically significant difference between low-high (0.025) and low-medium (0.045). 

# 12.42
#If we do not want to assume normality in this data, then we would do a Kruskal Wallace test for non parametric data


# 12.43
#create data set
protein <- c(1.7, 2.0, 2.0, 2.2, 4.0, 4.0, 5.0, 6.7, 7.8, 1.4, 2.4, 2.4, 3.3, 4.4, 4.7, 6.7, 7.6, 9.5, 11.7, 2.9, 3.8, 4.4, 4.7, 5.0, 5.6, 7.4, 9.4, 10.3)
trypsin <- c("low", "low", "low","low","low", "low", "low","low","low", 
        "medium", "medium", "medium","medium","medium","medium","medium","medium","medium","medium",
        "high", "high", "high", "high", "high", "high", "high", "high", "high")
df_pf <- data.frame(protein, trypsin)

#krausal wallace test
kruskal.test(protein ~ trypsin, data = df_pf)

#assess normality by group
ggplot(df_so2, aes(x= protein) + geom_histogram() + facet_grid(~ trypsin)+ stat_bin(bins = 5))

#Anova
lm_pf <- lm(protein ~ trypsin, data = df_pf)
anova(lm_pf)

#According to the ANOVA, there is no statistically significant differences between typsin secretion groups when it came to protein concentration. 