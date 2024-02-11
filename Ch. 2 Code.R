getwd ()
setwd ("C:/Users/marya/OneDrive/Documents/School/Biostats 576A/Data")

#Load Data
cholesterol <- read_xlsx("Cholesterol Revised.xlsx")

#load packages
install.packages("gmodels")
library("gmodels")
install.packages("psych")
library("psych")

#look at data
head(hospital)

#2.1
summary(hospital$Dur_stay)

#2.2
describe(hospital$Dur_stay)

#2.3
hospital$Antibio <- ifelse(hospital$Antibio == 1, "Yes", "No")
boxplot(hospital$Dur_stay ~ hospital$Antibio, main = "Hospital Stay by Antibiotic Use", xlab = "Antibiotic Use", ylab = "Hospital Stay Duration")
#also subset the yes and no antibio to get the descriptive statistics
Antibio_yes <- subset(hospital, Antibio == "Yes")
Antibio_no <- subset(hospital, Antibio == "No")
describe(Antibio_yes$Dur_stay)
describe(Antibio_no$Dur_stay)

#2.13
describe(cholesterol$Difference)

#2.15
stem(cholesterol$Difference, scale = 2)

#2.17
describe(cholesterol$Difference)
boxplot(cholesterol$Difference, main = "Boxplot of Changes in Cholesterol", ylab = "Change in Cholesterol")

#2.31
lead$Group <- ifelse(lead$Group == 1, "Control Group", "Exposed Group")
lead$sex <- ifelse(lead$sex == 1, "Male", "Female")

head(lead$Group)
CrossTable(lead$Group, lead$sex)

CrossTable(lead$Group, lead$sex)
CrossTable(lead$Group, lead$age_cat)
boxplot(lead$ageyrs ~ lead$Group, main = "Exposure by Age", xlab = "Exposure", ylab = "Age")

boxplot(lead$iqv ~ lead$Group, main = "Verbal IQ by Exposure", xlab = "Exposure", ylab = "Verbal IQ")
boxplot(lead$iqp ~ lead$Group, main = "Performance IQ by Exposure", xlab = "Exposure", ylab = "Performance IQ")

Males_subset <- subset(lead, sex == "Male")
Females_subset <- subset(lead, sex == "Female")

Exposed_subset <- subset(lead, Group == "Exposed Group")
Control_subset <- subset(lead, Group == "Control Group")

describe(Exposed_subset$iqv)
describe(Control_subset$iqv)

describe(Exposed_subset$iqp)
describe(Control_subset$iqp)
list= ls()

(lead$ageyrs ~ lead$Group main = "Exposure by Age" xlab = "Exposure" ylab = "Age")


#add more info to 3.32. median, dsitribution, range, outliers
