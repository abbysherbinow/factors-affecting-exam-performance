# Factors Affecting Exam Performance
# Statistical analysis using synthetic student performance data
# Author: Abby Sherbinow

library(ggplot2)
library(dplyr)
library(car)
library(caret)
###Final Project###
data <- read.csv("StudentPerformanceFactors.csv")
StudentPerformanceFactors <- data

is.na(data)
sum(is.na(data))
##no missing values
#dependent (y) variable
exam_scores <- StudentPerformanceFactors$Exam_Score ##B0
#predictor variables
study_hours <- StudentPerformanceFactors$Hours_Studied ##B1
attendance <- StudentPerformanceFactors$Attendance ##B2
sleep <- StudentPerformanceFactors$Sleep_Hours ##B3
#categorical predictor variables
gender <- StudentPerformanceFactors$Gender ##B4
extracurricular <- StudentPerformanceFactors$Extracurricular_Activities ##B5
model <- lm(exam_scores ~ study_hours + attendance + sleep + factor(gender) + factor(extracurricular))
summary(model)
#############################
###EXPLORATORY ANALYSIS###
##GRAPHICAL REPRESENTATIONS
##SCATTERPLOTS- relationships between exam score and quantitative predictors
plot(study_hours, exam_scores,
main = "Scatterplot of Exam Score and Hours Studied",
xlab = "Study Hours",
ylab = "Exam Score",
col = "purple4")
mod1 <- lm(exam_scores ~ study_hours)
abline(mod1, col = "darkblue", lwd = 3)
cor(study_hours, exam_scores)
#0.445455
##there is a moderate positive correlation between study hours and exam scores.
plot(attendance, exam_scores,
main = "Scatterplot of Exam Score and Attendance",
xlab = "Attendance",
ylab = "Exam Score",
col = "purple4")
mod2 <- lm(exam_scores ~ attendance)
abline(mod2, col = "darkblue", lwd = 3)
cor(attendance, exam_scores)
#0.5810719
##there is a moderate positive correlation between study hours and exam scores.
plot(sleep, exam_scores,
main = "Scatterplot of Exam Score and Hours of Sleep",
xlab = "Hours of Sleep",
ylab = "Exam Score",
col = "purple3")
mod3 <- lm(exam_scores ~ sleep)
abline(mod3, col = "darkblue", lwd = 3)
cor(sleep, exam_scores)
#-0.01702163
##there is a very weak negative correlation between sleep and exam scores.
##GRAPHICAL REPRESENTATIONS for categorical variables
##relationship between gender and exam score
##side by side box plot
require(lattice)
bwplot(exam_scores~gender,
col = "purple")
#hard to tell- look very similar
##side by side histogram
histogram(~exam_scores|gender,
xlab = "Exam Score",
col = "purple3")
#hard to tell a difference, but you can see that the exam scores were
#distributed similarly for females and males
##summary statistics for exam scores by gender
tapply(exam_scores,gender,summary)
##females had a slightly higher maximum, mean, and minimum
tapply(exam_scores, gender, sd)
##relationship between extracurricular and exam score
##side by side box plot
require(lattice)
bwplot(exam_scores~extracurricular,
xlab = "Students in an Extracurricular",
ylab = "Exam Score",
col = "purple3")
#students who are involved in extracurriculars seem to have slightly higher
#exam scores
##side by side histogram
histogram(~exam_scores|extracurricular,
xlab = "Exam Score",
col = "purple3")
#hard to tell a difference, but you can see that the exam scores were
#distributed similarly for both students who participate in an extracurricular
#and those who do not
##summary statistics for exam scores based on extra curricular
tapply(exam_scores,extracurricular,summary)
##students who participate in extra curricular had slightly higher minimum,
#mean, and maximum exam scores
tapply(exam_scores, extracurricular, sd)
################################
##FURTHER GRAPHICAL REPRESENTATION FOR Y VARIABLE (frequency)
ggplot(data, aes(x = exam_scores)) +
geom_histogram(bins = 20, fill = "purple3", color = "black", alpha = 0.7) +
theme_minimal() +
labs(title = "Distribution of Exam Scores", x = "Exam Score", y = "Frequency")
##FURTHER GRAPHICAL REPRESENTATION FOR QUANTITATIVE VARIABLES
ggplot(data, aes(x = sleep)) +
geom_histogram(bins = 7, fill = "purple3", color = "black", alpha = 0.7) +
theme_minimal() +
labs(title = "Distribution of Hours of Sleep", x = "Hours of Sleep", y = "Frequency")
ggplot(data, aes(x = attendance)) +
geom_histogram(bins = 20, fill = "purple3", color = "black", alpha = 0.7) +
theme_minimal() +
labs(title = "Distribution of Attendance", x = "Attendance", y = "Frequency")
ggplot(data, aes(x = study_hours)) +
geom_histogram(bins = 15, fill = "purple3", color = "black", alpha = 0.7) +
theme_minimal() +
labs(title = "Distribution of Study Hours", x = "Number of Hours Studied", y = "Frequency")
###############################
##FURTHER GRAPHICAL REPRESENTATIONS FOR CATEGORICAL VARIABLES (frequency)
#extracurricular activity
cs1 <- colSums(StudentPerformanceFactors == "Yes")
cs1[cs1 > 0]
#3938 students participate in an extracurricular activity
#2669 do not
A <- c(3938, 2669)
B <- c("Yes", "No")
barplot(A, names.arg = B, xlab ="Do They Participate in an Extracurricular Activity?",
ylab ="Number of Students", col ="purple",
main ="Students Who Participate in an Extracurricular Activity",
ylim = c(0, max(A) * 1.3))
count <- table(extracurricular)
percentage <- count/sum(count)*100
cbind(count, percentage)
lbls <- c("No","Yes")
pct <- round(percentage, digits = 2)
lbls <- paste(lbls,pct) ##add percents to labels
lbls <- paste(lbls,"%",sep="") ##add % to labels
pie(pct,labels=lbls,col= c("purple3", "blue3"),
main="Pie Chart of Students in Extracurriculars")
#gender
cs2 <- colSums(StudentPerformanceFactors == "Male")
cs2[cs2 > 0]
#3814 students are male
#2793 are female
C <- c(3814, 2793)
D <- c("Male", "Female")
barplot(C, names.arg = D, xlab ="Gender",
ylab ="Number of Students", col ="purple",
main ="Gender of Students",
ylim = c(0, max(A) * 1.3))
count <- table(gender)
percentage <- count/sum(count)*100
cbind(count, percentage)
lbls <- c("Female","Male")
pct <- round(percentage, digits = 2)
lbls <- paste(lbls,pct) ##add percents to labels
lbls <- paste(lbls,"%",sep="") ##add % to labels
pie(pct,labels=lbls,col= c("purple3", "blue3"),
main="Pie Chart of Gender of Students")
###############################
##SUMMARY STATISTICS
summary(exam_scores)
sd(exam_scores)
fivenum(exam_scores)
summary(study_hours)
#mean- 19.98
sd(study_hours)
#sd- 5.990594
#min- 1
#1st Qu.- 16
#median- 20
#3rd Qu.- 24
#max- 44
summary(attendance)
#mean- 79.98
sd(attendance)
#sd- 11.54747
#min- 60
#1st Qu.- 70
#median- 80
#3rd Qu.- 90
#max- 100
summary(sleep)
#mean- 7.029
sd(sleep)
#sd- 1.46812
#min- 4
#1st Qu.- 6
#median- 7
#3rd Qu.- 8
#max- 10
table(extracurricular)
#yes- 3938
#no-2669
table(gender)
#male- 3814
#female- 2793
####################################
##(SPLIT DATASET)###
set.seed(123)
n <- nrow(data)
train_indices <- sample(1:n, size = 0.7 * n)
train <- data[train_indices, ]
nrow(train)
test <- data[-train_indices, ]
nrow(test)
View(train)
full <- lm (Exam_Score ~ Hours_Studied + Attendance + Sleep_Hours + factor(Gender) +
factor(Extracurricular_Activities), data = train)
summary(full)
test_predictions <- predict(full, newdata = test)
actuals <- test$Exam_Score
rmse <- sqrt(mean((test_predictions- actuals)^2))
print(paste("RMSE:", rmse))
##2.5266
##good! means that predictions are typically off by 2.5266
##renaming variables
exam_scores <- train$Exam_Score
study_hours <- train$Hours_Studied
attendance <- train$Attendance
sleep <- train$Sleep_Hours
extracurricular <- train$Extracurricular_Activities
gender <- train$Gender
#####################################
##CORRELATION AND REGRESSION ANALYSIS
##DEVELOP AND INTERPRET FULL AND REDUCED MODELS
##full model (this uses train dataset)
summary(full)
##based on graphical representations, I will remove sleep and gender
reduced <- lm(Exam_Score ~ Hours_Studied + Attendance + factor(Extracurricular_Activities), data=train)
########################################
##HYPOTHESIS TESTING- INDIVIDUAL VARIABLES
#Hours Studied
#p-value: 2e-16 < 0.05 significance
#Ho: B1 = 0 vs Ha: B1 =/ 0
#reject Ho, B1 is significant
#attendance
#p-value: 2e-16 < 0.05
#Ho: B2 = 0 vs Ha: B2 =/ 0
#reject Ho, B2 is significant
#sleep hours
#p-value: 0.182 > 0.05
#fail to reject Ho, B3 is insignificant
#gender
#p-value: 0.724 > 0.05
#Ho: B4 = 0 vs Ha: B4 =/ 0
#fail to reject Ho, B4 is insignificant
#extracurricular
#p-value: 1.74e-14 < 0.05'
#Ho: B5 = 0 vs Ha: B5 =/ 0
#reject Ho, B5 is significant
##F TEST ON FULL MODEL
#Ho: B1=B2=...=B5=0
summary(full)
#vs
#Ha: at least one Bj =/ 0 (j=1,2,3,4,5)
pf(1065, 5, 4618, lower.tail = FALSE)
#p-value: 2.2e-16 < 0.05 significance
#reject Ho- full model is significant
##COMPARING FULL AND REDUCED MODEL
#Ho: B3=B5=0 vs Ha: at least one Bj =/ 0 (j=3,5)
anova(full,reduced)
#p-value: 0.3878
#fail to reject Ho, there is no significant effect from
#B3 and B5
#######################################
##VARIABLE SELECTION
step <- step(full, direction = "both", trace = FALSE)
summary(step)
summary(full)
#study hours, attendance, extracurricular (removed gender and sleep)
#step model is same as reduced
AIC(step) #22185.51
AIC(full) #22187.61
###STEP/REDUCED is best
BIC(step) #22217.7
BIC(full) #22232.68
###STEP/REDUCED is best
#Adjusted R-squared
summary(step)
#0.5352
summary(full)
#0.5351
###STEP/REDUCED is best
###ALL METHODS SHOW STEP/REDUCED IS THE BEST MODEL
######################################
##INTERACTIONS
#using reduced model as full model (still using train data, just only using reduced/step variables)
#study hours * attendance
intmodel1 <- lm(Exam_Score ~ Hours_Studied * Attendance + factor(Extracurricular_Activities), data =
train)
#extra curricular * study hours
intmodel2 <- lm(Exam_Score ~ Attendance + Hours_Studied * factor(Extracurricular_Activities), data =
train)
#extra curricular * attendance
intmodel3 <- lm(Exam_Score ~ Hours_Studied + Attendance * factor(Extracurricular_Activities), data =
train)
anova(reduced, intmodel1)
#p-value: 0.01518
#Ho: there is no significant interaction between study hours and attendance.
#Ha: There is a significant interaction between study hours and attendance.
#0.01518 < 0.05 significance, therefore, reject
#Ho; there is a significant effect from the interaction term.
anova(reduced, intmodel2)
#p-value: 0.03553
#Ho: there is no significant interaction between study hours and extracurriculars.
#Ha: There is a significant interaction between study hours and extracurriculars.
#0.03553 < 0.05 significance, therefore, reject
#Ho; there is a significant effect from the interaction term.
anova(reduced, intmodel3)
#p-value: 0.3482
#Ho: there is no significant interaction between attendance and extracurriculars.
#Ha: There is a significant interaction between attendance and extracurriculars.
#0.3482 > 0.05 significance, therefore, fail to reject
#Ho; there is no significant effect from interaction term.
##INCORPORATE INT TERM 1 OR INT TERM 2
AIC(intmodel1)
#22181.6 BEST
AIC(intmodel2)
#22183.08
BIC(intmodel1)
#22220.24 BEST
BIC(intmodel2)
#22221.72
summary(intmodel1)
#adjusted r-squared: 0.5357 BEST
summary(intmodel2)
#adjusted r-squared: 0.5355
###BEST MODEL- INTMODEL1
new_model <- intmodel1
#####################################
##DIAGNOSTIC TESTS (ASSUMPTIONS) AND TRANSFORMATIONS
par(mfrow = c(2,2))
plot(new_model, col = "purple3", lwd = 3)
plot(new_model, 1, col = "purple3", lwd = 3)
plot(new_model, 2, col = "purple3", lwd = 3)
plot(new_model, 3, col = "purple3", lwd = 3)
##linearity?
#assumption: the relationship between the independent variable and the
#dependent variables are linear.
#model 1- no pattern, spread evenly acrossed y=0.
#assumption- not violated
##multicollinearity?
#assumption: correlation between the predictor variables. violated if they are highly correlated.
cor(train [,c('Hours_Studied', 'Attendance')])
#low correlation (0.0104). no multicollinearity
vif(reduced)
##low- roughly 1. No multicollinearity.
#assumption- not violated.
##homoscedasticity?
#assumption: the variance of the residuals is constant across all levels
#of the independent variable.
#model 1- no pattern, spread evenly across y = 0. model 3- spread equally.
#assumption- not violated
##normality?
#assumption: the residuals are normally distributed
#model 2- does not follow line.
residuals <- residuals(new_model)
shapiro.test(residuals)
#p-value: 2.2e-16 < 0.5 significance
#this means- the data is not normal
#assumption- violated

library(MASS)
boxcox(new_model)
##y transformation- 1/y ?
new_scores <- 1/(train$Exam_Score)
ytrans_mod <- lm(new_scores ~ train$Hours_Studied * train$Attendance +
factor(train$Extracurricular_Activities))
par(mfrow = c(2,2))
plot(ytrans_mod, col = "purple3", lwd = 3)