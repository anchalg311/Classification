####### Load required libraries #######

library(caret)
library(nnet)
library(NeuralNetTools)


######## Load data ########
churn <- read.table('churn.txt', header = T, sep=',', na.strings=c('','NA'))

churn<- churn[,-c(1,3,4)]


###Normalizing the numerical variables
for(i in 1:ncol(churn))
{
  if(is.numeric(churn[,i])){  ####checks whether the variable is numerical , If yes, then normalize it
    churn[,i]<- ( churn[,i]-min(churn[,i])) / (max(churn[,i])-min(churn[,i]))
  }
}

##Dummy coding categorical variables###

dmyVars <- dummyVars("~.", data = churn)
churn<- data.frame(predict(dmyVars,newdata=churn))


churn$Intl.Plan.Yes <- ifelse(churn$Intl.Plan=="yes", 1,0)
churn$Intl.Plan.No <- ifelse(churn$Intl.Plan=="no", 1,0)
churn$VMail.Plan.Yes <- ifelse(churn$VMail.Plan=="yes", 1,0)
churn$VMail.Plan.No <- ifelse(churn$VMail.Plan=="no",1,0)

churn<-churn[,-c(2,3)]

## 75% of the sample size
smp_size <- floor(0.75 * nrow(churn))
smp_size
## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(churn)), size = smp_size)
train_ind

churn.train <- churn[train_ind, ]
churn.test <- churn[-train_ind, ]

churn.net<-  nnet(Churn.~., data = churn.train, size = 8, linout= FALSE)
plotnet(churn.net, bord_col = "black")


table(round(churn.net$fitted.values, 1)) 
churn.net$wts # Weights
hist(churn.net$wts)


### Sensitivity Analysis #####

garson(churn.net,x_lab=c('A.Length','A.Code','I.Plan','VM.Plan','VM.Msg','D.Mins','D.Calls','D.Charge','E.Mins','E.Calls','E.Charge','N.Mins', 
                         'N.Calls','N.Charge','I.Mins','I.Calls','I.Charge','C.Serv.Calls','Churn'),
       y_lab="Importance for classifying Churn")


####CART model for churn###

churn.CART <- rpart(Churn~ Account.Length + Intl.Plan.No + Intl.Plan.Yes + VMail.Plan.No + VMail.Plan.Yes+ VMail.Message +
                    Day.Mins + Day.Calls + Day.Charge + Eve.Mins + Eve.Calls + Eve.Charge  +  
                    Night.Mins + Night.Calls + Night.Charge + Intl.Mins + Intl.Calls + Intl.Charge +  
                    CustServ.Calls , data = churn.train, method = "class")

churn.CART <- rpart(Churn~ . , data = churn.train, method = "class")
print(churn.CART)

rpart.plot(churn.CART,type=3, digits=3, fallen.leaves= TRUE)

churn.CART.predict<- predict(churn.CART,churn.test,type="class")

costs<-list(loss=matrix(c(0,1,1,0), ncol=2, byrow=TRUE))

churn.CART.cost <- rpart(Churn~ Account.Length + Intl.Plan.No + Intl.Plan.Yes + VMail.Plan.No + VMail.Plan.Yes+ VMail.Message +
                      Day.Mins + Day.Calls + Day.Charge + Eve.Mins + Eve.Calls + Eve.Charge  +  
                      Night.Mins + Night.Calls + Night.Charge + Intl.Mins + Intl.Calls + Intl.Charge +  
                      CustServ.Calls , data = churn.train, method = "class", parms=costs)

churn.CART.cost.predict<- predict(churn.CART.cost,churn.test,type="class")

churn.confMatrix= confusionMatrix(table(churn.CART.cost.predict,churn.test$Churn))
churn.confMatrix


###Gain and Lift Charts
churn.predict<- predict(churn.net, newdata= churn.test)
score<- churn.predict
ClassifyChurn<- churn.test$Churn

#ROCR prediction function
pred = prediction(score, ClassifyChurn)

#the prediction to calculate gain using the ROCR performance function
churn.gain = ROCR::performance(pred, "tpr", "rpp")

#do the same for lift
churn.lift= ROCR::performance(pred, "rpp", "lift")

#plot the two 
plot(x=c(0, 1), y=c(0, 1), type="l", col="red", lwd=2,
     ylab="True Positive Rate", 
     xlab="Rate of Positive Predictions")
lines(x=c(0, 0.5, 1), y=c(0, 1, 1), col="darkgreen", lwd=2)

#get ready to plot the gain

gain.x = unlist(slot(churn.gain, 'x.values'))
gain.y = unlist(slot(churn.gain, 'y.values'))

lines(x=gain.x, y=gain.y, col="orange", lwd=2)

#lines(x=c(0, 0.5, 1), y=c(0, 1, 1), col="darkgreen", lwd=2)

#get ready to plot the lift
lift.x = unlist(slot(churn.lift,'x.values'))
lift.y = unlist(slot(churn.lift,'y.values'))
#lines(y=lift.x, x=lift.y, col="orange", lwd=2)
plot(lift.y, lift.x, type="l", col="red", lwd=2,
     ylab="Rate of positive predictions", 
     xlab="True positive over Rate of Positive Predictions")


############# Assigning misclassification costs #########

costs2<-list(loss=matrix(c(0,1,2,0), ncol=2, byrow=TRUE))

churn.CART.cost2 <- rpart(Churn~ Account.Length + Intl.Plan.No + Intl.Plan.Yes + VMail.Plan.No + VMail.Plan.Yes+ VMail.Message +
                           Day.Mins + Day.Calls + Day.Charge + Eve.Mins + Eve.Calls + Eve.Charge  +  
                           Night.Mins + Night.Calls + Night.Charge + Intl.Mins + Intl.Calls + Intl.Charge +  
                           CustServ.Calls , data = churn.train, method = "class", parms=costs2)

churn.CART.predict.cost2<- predict(churn.CART.cost2,churn.test,type="class")

churn.confMatrix2= confusionMatrix(table(churn.CART.predict.cost2, churn.test$Churn))
churn.confMatrix2
