install.packages("kknn")
library(kknn)

##############  Chapter 10   #####################

#12. Using the data in table 10.5, find the k-nearest neighbor for Record #10, using k = 3.
#Recreate the data from table 10.5
age <- c(22,33,28,51,25,39,54,55,50,66)

marital <- c("Single","Married","Other","Other","Single","Single","Single",
             "Married","Married","Married")
income <- c(46156.98,24188.10,28787.34,23886.72,47281.44,33994.90,28716.50,
            49186.75,46726.50,36120.35)
risk <- c("Bad loss","Bad loss","Bad loss","Bad loss","Bad loss","Good risk",
          "Good risk","Good risk","Good risk","Good risk")

table.10.5 <- data.frame(age,marital,income,risk)
table.10.5$married.1 <- ifelse(table.10.5$marital=="Married",1,0)
table.10.5$single.1 <- ifelse(table.10.5$marital=="Single",1,0)
new <- table.10.5[10,c(1,2,3,5,6)]
View(new)
trueclass <- table.10.5[1:9,4]

#Use the K Nearest Neighbor Function

knn <- knn(table.10.5[1:9,c(1,3,5,6)],
           new[,c(1,3,4,5)],
           cl=trueclass,
           k=3,
           prob=TRUE)
knn
attr(knn,"nn.index")[1]

#Find the record that is the nearest neighbor to Record #1 according to Euclidean Distance
table.10.5[attr(knn,"nn.index")[1],]

###13. Using the ClassifyRisk data set with predictors age, marital status, and income, 
###and target variable risk, find the k-nearest neighbor for Record #1, 
###using k = 2 and Euclidean distance.

##Standardizing all the numeric variables in the dataset

ClassifyRisk <- read.csv("C:/Users/ergup/Desktop/Old_courses/560/ClassifyRisk.txt", header=T)

############# Normalizing the variables ################

ClassifyRisk$income<- (ClassifyRisk$income - min(ClassifyRisk$income))/(max(ClassifyRisk$income)- min(ClassifyRisk$income))
ClassifyRisk$loans<- (ClassifyRisk$loans - min(ClassifyRisk$loans))/(max(ClassifyRisk$loans)-min(ClassifyRisk$loans))
ClassifyRisk$age<- (ClassifyRisk$age - min(ClassifyRisk$age))/(max(ClassifyRisk$age)-min(ClassifyRisk$age))

riskclass = ClassifyRisk[2:246,6]
ClassifyRisk$married <- ifelse(ClassifyRisk$marital_status=="married",1,0)
ClassifyRisk$single <- ifelse(ClassifyRisk$marital_status=="single",1,0)
risk.training <- ClassifyRisk[2:246,]
risk.test <- ClassifyRisk[1,]
risk.knn <- knn(risk.training[,c(3,5,7,8)],
                risk.test[,c(3,5,7,8)],
                cl=riskclass,
                k = 2)
risk.knn
attr(risk.knn,"nn.index")[1]

#Find the record that is the nearest neighbor to Record #1 according to Euclidean Distance
risk.training[attr(risk.knn,"nn.index")[1],]


#14. Using the ClassifyRisk data set with predictors age, marital status, 
##and income, and target variable risk, find the k-nearest neighbor for Record #1, 
##using k = 2 and Minkowski (city-block) distance (Chapter 19).

risk.train <- ClassifyRisk[2:246,]
risk.test <- ClassifyRisk[1,]
risk.kknn <- kknn(risk~.,
                 risk.train[,c(3,5,6, 7, 8)],
                 risk.test[,c(3,5,6, 7, 8)],
                 k = 2,
                 distance = 1)
summary(risk.kknn)
risk.kknn$D
risk.kknn$C[1]


