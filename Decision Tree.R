######## Load required libraries #########
library(rpart)
library("partykit")
library(rpart.plot)
library(pROC)


########### Load churn dataset #########
churn <- read.table('churn.txt', header = T, sep=',', na.strings=c('','NA'))
names(churn) <- c("State","Account.Length","Area.Code","Phone","Intl.Plan.","VMail.Plan","VMail.Message","Day.Mins" ,"Day.Calls" ,
                  "Day.Charge","Eve.Mins" ,"Eve.Calls","Eve.Charge","Night.Mins","Night.Calls" ,"Night.Charge","Intl.Mins" ,"Intl.Calls",
                  "Intl.Charge","CustServ.Calls" ,"Churn.")

#Normalize numerical data
churn$Account.Length_z <- scale(churn$Account.Length)
churn$VMail.Message_z <- scale(churn$VMail.Message)
churn$Day.Mins_z <- scale(churn$Day.Mins)
churn$Day.Calls_z <- scale(churn$Day.Calls)
churn$Day.Charge_z <- scale(churn$Day.Charge)
churn$Eve.Mins_z <- scale(churn$Eve.Mins)
churn$Eve.Calls_z <- scale(churn$Eve.Calls)
churn$Eve.Charge_z <- scale(churn$Eve.Charge)
churn$Night.Mins_z <- scale(churn$Night.Mins)
churn$Night.Calls_z <- scale(churn$Night.Calls)
churn$Night.Charge_z <- scale(churn$Night.Charge)
churn$Intl.Mins_z <- scale(churn$Intl.Mins)
churn$Intl.Calls_z <- scale(churn$Intl.Calls)
churn$Intl.Charge_z <- scale(churn$Intl.Charge)
churn$Intl.Charge_z <- scale(churn$Intl.Charge)
churn$CustServ.Calls_z <- scale(churn$CustServ.Calls)

############ Divide dataset into train and test ##############

smp_size <- floor(0.90 * nrow(churn))
index <- sample(seq_len(nrow(churn)),size=smp_size)
churn.train <- churn[index, ]
churn.test <- churn[-index, ]

#Create CART decision tree with rpart function
churn.CART <- rpart(Churn. ~ Account.Length_z + Intl.Plan. + VMail.Plan + VMail.Message_z
                    + Day.Mins_z + Day.Calls_z + Day.Charge_z + Eve.Mins_z + Eve.Calls_z + 
                      Eve.Charge_z + Night.Mins_z + Night.Calls_z + Night.Charge_z + Intl.Mins_z + Intl.Calls_z + 
                      Intl.Charge_z + CustServ.Calls_z, data = churn.train, method = "class")
print(churn.CART)

rpart.plot(churn.CART,type=3, digits=3, fallen.leaves= TRUE)

########## Predict for test dataset using the trained model  ############
predict.churn  <- predict(churn.CART, churn.test, type = "class" )


######### Create confusion matrix for checking performance metrics ########
confusionMatrix(predict.churn, reference = churn.test$Churn.)


########## Plot ROC curve and calculate AUC ##############
###### Predict using probability #########
predict.churn.prob <- predict(churn.CART, churn.test, type = "prob")

churn_auc <- auc(churn.test$Churn., predict.churn.prob[,2])
churn_auc

plot(roc(churn.test$Churn., predict.churn.prob[,2]))
