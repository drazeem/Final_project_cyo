# FINAL PROJECT - CYO [HarvardX Data Science Capstone (HarvardX: PH125.9x)]

# CODE 


# Installing the required packages/libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(Amelia)) install.packages("Amelia", repos = "http://cran.us.r-project.org")
if(!require(psych)) install.packages("psych", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(GoodmanKruskal)) install.packages("GoodmanKruskal", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(party)) install.packages("party", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(ROCR)) install.packages("ROCR", repos = "http://cran.us.r-project.org")
if(!require(pscl)) install.packages("pscl", repos = "http://cran.us.r-project.org")
if(!require(rcompanion)) install.packages("rcompanion", repos = "http://cran.us.r-project.org")

### Calling IBM-HR dataset 
df<- read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv")

# Preperaing the data for analysis

# Examining the dimensions and structure of the data set 

dim(df)
str(df)

# Removing the irrelavant  and those with constant values.
df<- df[-c(1,3,7,8, 9, 10, 11,12,14,15, 16, 17,18, 19, 20, 21, 24, 25,27,28, 32,33)]
str(df)
names(df)

# Renaming the columns to the shorter form.
colnames(df) <- c("age","gen","marital","edu", "income","salHike", "perRate", "stock", "workLifeBal","expComp","expCurrRole","jobSat","attr")
names(df)

# Changing the order of columns
df <- df[,c(1,2,3,4,10,11,5,6,8,9,7,12,13)]

# Examine the structure of the data.
str(df) 

# Converting the integer variables "edu","stock","workLifeBal","perRate", and "jobSat" into factor variable.

table(df$edu)
df$edu <-  as.factor(df$edu)

table(df$stock)
df$stock <-  as.factor(df$stock)

table(df$workLifeBal)
df$workLifeBal <-  as.factor(df$workLifeBal)

table(df$perRate)
df$perRate <-  as.factor(df$perRate)

table(df$jobSat)
df$jobSat <-  as.factor(df$jobSat)

# Replacing  Yes and No in the label (attr) variable by 1 and 0 respectively.

class(df$attr)
df$attr <-  as.character(df$attr)
df$attr <-ifelse(df$attr=="Yes","1","0")
table(df$attr)
df$attr <-  as.factor(df$attr)
str(df)

# Examine for the missing values

missmap(df,main = "Missing Values Vs Observed")

# Univariate Analysis
describe(df) 

# Visual display of the variables

# Age
ggplot(df, aes(x=age)) + ggtitle("Age of Employee") + xlab("age") +
  geom_histogram(bins = 20, aes(y=..density..), color = "red", fill = "red") + geom_density(alpha=.2, fill="#FF6666") +     ylab("Frequency")

# Gender
ggplot(df, aes(x=gen)) + ggtitle("Gender") + xlab("gen") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5, color = "red", fill = "red") + ylab("Percentage") + coord_flip() + theme_minimal()

# Marital Status
ggplot(df, aes(x=marital)) + ggtitle("Marital Status") + xlab("marital") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5, color = "red", fill = "red") + ylab("Percentage") + coord_flip() + theme_minimal()

# Education Level
ggplot(df, aes(x=edu)) + ggtitle("Education Level") + xlab("edu") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5,color = "red", fill = "red") + ylab("Percentage") + coord_flip() + theme_minimal()
 
# Stock Options
ggplot(df, aes(x=stock)) + ggtitle("Stock Option") + xlab("stock") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5, color = "red", fill = "red") + ylab("Percentage") + coord_flip() + theme_minimal()

# Work Life Balance
ggplot(df, aes(x=workLifeBal)) + ggtitle("Work Life Balance") + xlab("workLifeBal") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5, color = "red", fill = "red") + ylab("Percentage") + coord_flip() + theme_minimal()

# Performance Rate
ggplot(df, aes(x=perRate)) + ggtitle("Performance Rate") + xlab("perRate") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5, color = "red", fill = "red") + ylab("Frequency") + coord_flip() + theme_minimal()

# Job Satisfaction

ggplot(df, aes(x=jobSat)) + ggtitle("Job Satisfaction") + xlab("jobSat") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5, color = "red", fill = "red") + ylab("Percentage") + coord_flip() + theme_minimal()

# Experience in Company
ggplot(df, aes(x=expComp)) + ggtitle("Experience in Company") + xlab("expComp") +
  geom_histogram(bins = 20, aes(y=..density..), color = "red", fill = "red") + geom_density(alpha=.2, fill="#FF6666") +     ylab("Frequency")

# Experience in Current Role
ggplot(df, aes(x=expCurrRole)) + ggtitle("Experience in Current Role") + xlab("expCurrRole") +
  geom_histogram(bins = 10, aes(y=..density..), color = "red", fill = "red") + geom_density(alpha=.2, fill="#FF6666") +     ylab("Frequency")

# Monthly Income
ggplot(df, aes(x=income)) + ggtitle("Monthly Income") + xlab("income") +
  geom_histogram(bins = 30, aes(y=..density..), color = "red", fill = "red") + geom_density(alpha=.2, fill="#FF6666") +     ylab("Frequency")

# Salary Hike
ggplot(df, aes(x=salHike)) + ggtitle("Salary Hike") + xlab("salHike") +
  geom_histogram(bins = 15, aes(y=..density..), color = "red", fill = "red") + geom_density(alpha=.2, fill="#FF6666") +     ylab("Frequency")

# Attrition
ggplot(df, aes(x=attr)) + ggtitle("Attrition") + xlab("attr") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5, color = "red", fill = "red") + ylab("Percentage") + coord_flip() + theme_minimal()

# Examining the distribution of the categorical variables in the data for the factor levels representing employees with and without attrition (attr). This may lead to exclude variables that only have few (below 5) samples in any category.
table(df$attr)
xtabs(~ attr + gen, data=df)
xtabs(~ attr + marital, data=df)
xtabs(~ attr + edu, data=df)
xtabs(~ attr + stock, data=df) 
xtabs(~ attr + workLifeBal, data=df)
xtabs(~ attr + perRate, data=df)
xtabs(~ attr + jobSat, data=df)

# Bivariate Analysiss
# Examining the Correlation (continuous scale variables)
df_corr <- cor(df[c(1,5,6,7,8)])
corrplot(df_corr, method="number", diag=FALSE, type="lower",  title = "Correlation among continuous scale variables")

# Examining the association (or independence) among categorical variables.

# Q: Are Gender(gen) and Marital Status (marital) independent?

# Chi-Square Test of Independence
# Ho: They are independent
# H1: They are not independent

# Applying chi-square test
table(df$gen, df$marital)
chisq.test(df$gen, df$marital)

# Q: Are Gender(gen) and Education Level (edu) independent?
# Chi-Square Test of Independence
# Ho: They are independent
# H1: They are not independent

# Applying chi-square test
table(df$gen, df$edu)
chisq.test(df$gen, df$edu)

# Q: Are Gender(gen) and Stock Option (stock) independent?
# Chi-Square Test of Independence
# Ho: They are independent
# H1: They are not independent

# Applying chi-square test
table(df$gen, df$stock)
chisq.test(df$gen, df$stock)

# Q: Are Gender(gen) and Job Role (jobRole) independent?
# Chi-Square Test of Independence
# Ho: They are independent
# H1: They are not independent

# Applying chi-square test
table(df$gen, df$workLifeBal)
chisq.test(df$gen, df$workLifeBal)

# Q: Are Gender(gen) and Overtime (overtime) independent?
# Chi-Square Test of Independence
# Ho: They are independent
# H1: They are not independent

# Applying chi-square test
table(df$gen, df$perRate)
chisq.test(df$gen, df$perRate)

# Q: Are Gender(gen) and Business Travel (travel) independent?
# Chi-Square Test of Independence
# Ho: They are independent
# H1: They are not independent

# Applying chi-square test
table(df$gen, df$jobSat)
chisq.test(df$gen, df$jobSat)

# Q: Are Marital(martial) and Education (edu) independent?
# Chi-Square Test of Independence
# Ho: They are independent
# H1: They are not independent

# Applying chi-square test
table(df$marital, df$edu)
chisq.test(df$marital, df$edu)

### Q: Are Marital(martial) and Stock Option (stock) independent?
# Chi-Square Test of Independence
# Ho: They are independent
# H1: They are not independent

# Applying chi-square test
table(df$marital, df$stock)
chisq.test(df$marital, df$stock)

# Q: Are Marital(martial) and Work Life Balance (workLifeBal) independent?
# Chi-Square Test of Independence
# Ho: They are independent
# H1: They are not independent

# Applying chi-square test
table(df$marital, df$workLifeBal)
chisq.test(df$marital, df$workLifeBal)

# Q: Are Marital(martial) and Performance Rate (perRate) independent?
# Chi-Square Test of Independence
# Ho: They are independent
# H1: They are not independent

# Applying chi-square test
table(df$marital, df$perRate)
chisq.test(df$marital, df$perRate)

# Q: Are Marital(martial) and Job Satisfaction (jobSat) independent?
# Chi-Square Test of Independence
# Ho: They are independent
# H1: They are not independent

# Applying chi-square test
table(df$marital, df$jobSat)
chisq.test(df$marital, df$jobSat)

### Q: Are Education(edu) and Stock Option(stock)independent?
# Chi-Square Test of Independence
# Ho: They are independent
# H1: They are not independent

# Applying chi-square test
table(df$edu, df$stock)

# Q: Are Education(edu) and Work Life Balance(workLifeBal)independent?
# Chi-Square Test of Independence
# Ho: They are independent
# H1: They are not independent

# Applying chi-square test
table(df$edu, df$workLifeBal)

# Q: Are Education(edu) and Performance Rate (perRate)independent?
# Chi-Square Test of Independence
# Ho: They are independent
# H1: They are not independent

# Applying chi-square test
table(df$edu, df$perRate)
chisq.test(df$edu, df$perRate)

# Q: Are Education(edu) and Job Satisfaction (jobSat) independent?
# Chi-Square Test of Independence
# Ho: They are independent
# H1: They are not independent

# Applying chi-square test
table(df$edu, df$jobSat)

# Q: Are Stock Option (stock) and Work Life Balance (workLifeBal) independent?
# Chi-Square Test of Independence
# Ho: They are independent
# H1: They are not independent

# Applying chi-square test
table(df$stock, df$workLifeBal)

# Q: Are Stock Option (stock) and Performance Rate (perRate) independent?
# Chi-Square Test of Independence
# Ho: They are independent
# H1: They are not independent

# Applying chi-square test
table(df$stock, df$perRate)
chisq.test(df$stock, df$perRate)

# Q: Are Stock Option (stock) and Job Satisfaction (jobSat) independent?
# Chi-Square Test of Independence
# Ho: They are independent
# H1: They are not independent

# Applying chi-square test
table(df$stock, df$jobSat)
chisq.test(df$stock, df$jobSat)

# Q: Are Work Life Balance (workLifeBal) and Performance Rate (perRate) independent?
# Chi-Square Test of Independence
# Ho: They are independent
# H1: They are not independent

# Applying chi-square test
table(df$workLifeBal, df$perRate)
chisq.test(df$workLifeBal, df$perRate)

# Q: Are Work Life Balance (workLifeBal) and Job Satisfaction (jobSat) independent?
# Chi-Square Test of Independence
# Ho: They are independent
# H1: They are not independent

# Applying chi-square test
table(df$workLifeBal, df$jobSat)
chisq.test(df$workLifeBal, df$jobSat)

# Q: Are Performance Rate (perRate) and Job Satisfaction (jobSat) independent?
# Chi-Square Test of Independence
# Ho: They are independent
# H1: They are not independent

# Applying chi-square test
table(df$perRate, df$jobSat)
chisq.test(df$perRate, df$jobSat)

# The association can also be studied by the Googman Kruskal values. 
fac_1<- c("gen","marital","edu","stock","workLifeBal","perRate","jobSat","attr")
df1<- subset(df, select = fac_1)
GKmatrix1<- GKtauDataframe(df1)
plot(GKmatrix1, corrColors = "blue")

# Examining the association ( or independence) between Attrition (attr) and other categorical factors.

# Q: Are Gender(gen) and Attrition (attr) independent?
# Chi-Square Test of Independence
# Ho: They are independent
# H1: They are not independent

# Applying chi-square test
chisq.test(df$gen, df$attr)

# Q: Which gender has more tendency towards Attiration?
table(df$gen, df$attr)
prop.table(table(df$gen, df$attr))

# Marital Statuss (marit) and Attrition (attr) 
# Q: Are Marital Status (martial) and Attrition (attr) independent?
# Chi-Square Test of Independence
# Ho: They are independent
# H1: They are not independent

# Applying chi-square test
chisq.test(df$marital, df$attr)

# Q: Whether marital status affects more the decision towards Attiration?
table(df$marital, df$attr)
prop.table(table(df$marital, df$attr))

# Q: Whether gender influences the decision on the bases of the marital status towards Attiration?
table(df$gen, df$marital, df$attr)
prop.table(table(df$gen, df$marital, df$attr))

# Education(edu) and Attrition (attr)
# Q: Are Education(edu) and Attrition (attr) independent?

# Chi-Square Test of Independence
# Ho: They are independent
# H1: They are not independent

# Applying chi-square test
chisq.test(df$edu, df$attr)

# Q: Which qualification level has greater tendency towards attrition?
table(df$edu, df$attr)
prop.table(table(df$edu, df$attr))

# Q: Are Stock Option (stock) and Attrition (attr) independent?
# Chi-Square Test of Independence
# Ho: They are independent
# H1: They are not independent

# Applying chi-square test
chisq.test(df$stock, df$attr)

# Q: Does Attiration depend on the stock options to the employees?
table(df$stock, df$attr)
prop.table(table(df$stock, df$attr))

# Q: Are Work Life Balance (workLifeBal) and Attrition (attr) independent?
# Chi-Square Test of Independence
# Ho: They are independent
# H1: They are not independent

# Applying chi-square test
chisq.test(df$workLifeBal, df$attr)

# Q: Does Attiration has any relevance to the work life balance situation of the employees?
table(df$workLifeBal, df$attr)
prop.table(table(df$workLifeBal, df$attr))

# Q: Are Performance Rate (perRate) and Attrition (attr) independent?
# Chi-Square Test of Independence
# Ho: They are independent
# H1: They are not independent

# Applying chi-square test
chisq.test(df$perRate, df$attr)

# Q: Does performance rate influence the Attiration?
table(df$perRate, df$attr)
prop.table(table(df$perRate, df$attr))

# Q: Are Job Satisfaction (jobSat) and Attrition (attr) independent?
# Chi-Square Test of Independence
# Ho: They are independent
# H1: They are not independent

# Applying chi-square test
chisq.test(df$jobSat, df$attr)

# Q: Does job satisfaction influence the Attiration?
table(df$jobSat, df$attr)
prop.table(table(df$jobSat, df$attr))

# Modeling (Classification)
  
#First We split the data into train and test

set.seed(1)
temp <- sample(2, nrow(df), replace=T, prob = c(0.8,0.2))
train <- df[temp==1,]
test <- df[temp==2,]
str(train)  
str(test)

# Decision Tree
m1 <- rpart(attr ~ .,
            data=train,
            method="class",
            parms=list(split="information"),
            control=rpart.control(usesurrogate=0, 
                                  maxsurrogate=0))

m1
rpart.plot(m1, type = 3)
# Model performance
pred_m1 <- predict(m1, test, type = 'class')
# Obtaining the Confusion matrix 
table(pred_m1,test$attr)
# Examine Accuracy
mean(pred_m1 == test$attr)

# Random Forest 
m2 <- randomForest(attr ~ ., data = train, importance = TRUE)
m2
# Model performance
pred_m2 <- predict(m2, test, type = "class")
# Obtaining the Confusion matrix 
table(pred_m2,test$attr)
# Examining Accuracy
mean(pred_m2 == test$attr)
# Obtaining the key variables
importance(m2) 
varImpPlot(m2)

# Logistic Regressopn
m4 <- glm(attr ~ ., family = binomial(link="logit"), data = train)
summary(m4)
# Model performance
# goodness of fit (pseudo R-Square)
nagelkerke(m4)
# We an also directly get above mentioed p-value by using following function
p_value <- with(m4,pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = F))
p_value
# We can also use following code to get above goodness of fit statistics 
pR2(m4)
# Examine the  Accuracy
fitted.results <- predict(m4,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$attr)
print(paste('Accuracy',1-misClasificError))
# Plotting the ROC curve
# ROC plot and the AUC (area under the curve) are merformance measures for the binary classifier.
# The ROC is a curve generated by plotting the true positive rate (TPR) against the false positive rate (FPR) at various threshold settings while the AUC is the area under the ROC curve. As a rule of thumb, a model with good predictive ability should have an AUC closer to 1 (1 is ideal) than to 0.5.
p <- predict(m4, newdata=test, type="response")
pr <- prediction(p, test$attr)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#######################################################
## References/Bibliograpgy

# Following resources were consulted 

# http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram
# https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
# https://www.kaggle.com/rohitkumar06/let-s-reduce-attrition-logistic-reg-acc-88
# https://www.kaggle.com/pavansubhasht/ibm-hr-analytics-attrition-dataset
# Material provided in the HarvardX: PH125 courses especially in the Machine Learning course (HarvardX: PH125.8x).

###################################################################################

