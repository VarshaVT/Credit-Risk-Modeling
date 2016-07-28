##read the data
mydata <- read.csv("C:\\Users\\Varsha\\OneDrive\\pro\\Credit Risk Analysis\\creditData.csv")

## Explor The Data

head(mydata)
names(mydata)
class(mydata)
str(mydata)
attributes(mydata)
dim(mydata)
rownames(mydata)
colnames(mydata)
summary(mydata)


margin.table(prop.table(table(mydata$Duration.in.Current.address, mydata$Most.valuable.available.asset, mydata$Concurrent.Credits,mydata$No.of.Credits.at.this.Bank,mydata$Occupation,mydata$No.of.dependents,mydata$Telephone, mydata$Foreign.Worker)),1)

margin.table(prop.table(table(mydata$Duration.in.Current.address, mydata$Most.valuable.available.asset, mydata$Concurrent.Credits,mydata$No.of.Credits.at.this.Bank,mydata$Occupation,mydata$No.of.dependents,mydata$Telephone, mydata$Foreign.Worker)),2)

margin.table(prop.table(table(mydata$Duration.in.Current.address, mydata$Most.valuable.available.asset, mydata$Concurrent.Credits,mydata$No.of.Credits.at.this.Bank,mydata$Occupation,mydata$No.of.dependents,mydata$Telephone, mydata$Foreign.Worker)),3)

margin.table(prop.table(table(mydata$Duration.in.Current.address, mydata$Most.valuable.available.asset, mydata$Concurrent.Credits,mydata$No.of.Credits.at.this.Bank,mydata$Occupation,mydata$No.of.dependents,mydata$Telephone, mydata$Foreign.Worker)),4)

margin.table(prop.table(table(mydata$Duration.in.Current.address, mydata$Most.valuable.available.asset, mydata$Concurrent.Credits,mydata$No.of.Credits.at.this.Bank,mydata$Occupation,mydata$No.of.dependents,mydata$Telephone, mydata$Foreign.Worker)),5)

margin.table(prop.table(table(mydata$Duration.in.Current.address, mydata$Most.valuable.available.asset, mydata$Concurrent.Credits,mydata$No.of.Credits.at.this.Bank,mydata$Occupation,mydata$No.of.dependents,mydata$Telephone, mydata$Foreign.Worker)),6)

margin.table(prop.table(table(mydata$Duration.in.Current.address, mydata$Most.valuable.available.asset, mydata$Concurrent.Credits,mydata$No.of.Credits.at.this.Bank,mydata$Occupation,mydata$No.of.dependents,mydata$Telephone, mydata$Foreign.Worker)),7)

margin.table(prop.table(table(mydata$Duration.in.Current.address, mydata$Most.valuable.available.asset, mydata$Concurrent.Credits,mydata$No.of.Credits.at.this.Bank,mydata$Occupation,mydata$No.of.dependents,mydata$Telephone, mydata$Foreign.Worker)),8)

### Cross Tables
library(gmodels)
CrossTable(mydata$Creditability, mydata$Account.Balance, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(mydata$Creditability, mydata$Payment.Status.of.Previous.Credit, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(mydata$Creditability, mydata$Purpose, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)

### Descriptive Statistics
attach(mydata)
summary(mydata)
brksCredit <- seq(0,80, 10)
hist(Duration.of.Credit..month., breaks= brksCredit, xlab="CreditMonth", ylab="Frequency", main = " ")
hist(mydata$Credit.Amount, breaks= brksCredit, xlab="CreditAmount", ylab="Frequency", main = " ")
boxplot(mydata$Duration.of.Credit.Month., bty="n",xlab = "Credit Month")

class(mydata$Creditability)
# convert variables to proper types
mydata$Age..years. <- as.numeric(mydata$Age..years.)
mydata$Purpose <- as.factor(mydata$Purpose)
mydata$Credit.Amount <- as.double(mydata$Credit.Amount)
mydata$Payment.Status.of.Previous.Credit <- as.factor(mydata$Payment.Status.of.Previous.Credit)
mydata$Concurrent.Credits <- as.factor(mydata$Concurrent.Credits)
mydata$Account.Balance <- as.factor(mydata$Account.Balance)
mydata$Sex...Marital.Status <- as.factor(mydata$Sex...Marital.Status)
mydata$Most.valuable.available.asset <- as.factor(mydata$Most.valuable.available.asset)
mydata$Value.Savings.Stocks <- as.factor(mydata$Value.Savings.Stocks)
mydata$Length.of.current.employment <- as.factor(mydata$Length.of.current.employment)
mydata$Instalment.per.cent <- as.factor(mydata$Instalment.per.cent)
mydata$Guarantors <- as.factor(mydata$Guarantors)
mydata$Duration.in.Current.address <- as.factor(mydata$Duration.in.Current.address)
mydata$Instalment.per.cent <- as.factor(mydata$Instalment.per.cent)
mydata$Type.of.apartment <- as.factor(mydata$Type.of.apartment)
mydata$No.of.Credits.at.this.Bank <- as.factor(mydata$No.of.Credits.at.this.Bank)
mydata$Occupation <- as.factor(mydata$Occupation)
mydata$No.of.dependents <- as.factor(mydata$No.of.dependents)
mydata$Telephone <- as.factor(mydata$Telephone)
mydata$Foreign.Worker <- as.factor(mydata$Foreign.Worker)
mydata$Creditability <- as.factor(mydata$Creditability)

class(mydata$Creditability)
levels(mydata$Creditability)
levels(mydata$Creditability)[1] <- "Bad"
levels(mydata$Creditability)[2] <- "Good"

### Classify the data
mydata$Credit.Amount <- as.factor(ifelse(mydata$Credit.Amount <= 2500, '0-2500', ifelse(mydata$Credit.Amount <= 5000, '2600-5000', '5000+')))

# Having a variable in both continuous and binned (discrete form) can result in
#unstable or poorer performing results.

# divide the data into training set and testing set
data <- sort(sample(nrow(mydata), nrow(mydata) * 0.5))
train <- mydata[data,]
test <- mydata[-data,]



#Traditional Credit Scoring Using Logistic Regression in R
mod1 <- glm(Creditability ~ Account.Balance + Payment.Status.of.Previous.Credit + Purpose + Value.Savings.Stocks + Length.of.current.employment + Sex...Marital.Status + Most.valuable.available.asset + Type.of.apartment + Concurrent.Credits + Duration.of.Credit..month.+ Credit.Amount + Age..years., family=binomial, data = train)
summary(mod1)$coef
coef(mod1)

mod2 <- glm(Creditability ~ Account.Balance + Payment.Status.of.Previous.Credit + Purpose + Length.of.current.employment + Sex...Marital.Status, family = binomial, data = train)
summary(mod2)
contrasts(train$Creditability)
fit50 <- fitted.values(mod2)


mod2.prob <- predict(mod2, type = "response", data = test)
mod2.prob[1:10]

## setting threshold
mod2.pred <- rep("Bad", 500)
mod2.pred[mod2.prob >= 0.5] = "Good"

### Confusion Matrix
table(mod2.pred, test$Creditability)
#50+296 correct prediction
#classifier predicts 348 good customers and 152 bad customers
#But in reality 398 good customers and 102 bad customers
# accuracy=(50+296)/500 = 0.69


mean(mod2.pred==test$Creditability)#69% of movements are correctly predicted
mean(mod2.pred==train$Creditability)


library(ROCR)
pred <- prediction(mod2.prob, test$Creditability)
perf <- performance(pred,"tpr","fpr")
plot(perf)

#this code builds on ROCR library by taking the max delt
#between cumulative bad and good rates being plotted by
#ROCR
max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])
#KS is the maximum difference between the cumulative true positive and cumulative false
#positive rate. 

#Calculating top 3 variables affecting Credit Score Function in R
#use the terms in regression
x.pred <- predict(mod2, type = 'terms', data= test)
summary(x.pred)

# functions to pick the top3 reasons
top3 <- function(x, top=3)
{
  res=names(x)[order(x, decreasing = TRUE)][1:top]
  paste(res,collapse=";",sep="")
}

# use top 3 rows
topk = apply(x.pred, 1, top3, top=3)
#add reason list to scored tets sample
test <- cbind(test, topk)

##
## Linear Discriminant Analysis
##
require(MASS)
lda.fit <- lda(train$Creditability ~ train$Value.Savings.Stocks + train$Length.of.current.employment + train$Duration.of.Credit..month.+ train$Credit.Amount + train$Age..years., data = train)
lda.fit
plot(lda.fit)
lda.pred <- predict(lda.fit, test$Creditability)
lda.pred[1:5,]
class(lda.pred)
data.frame(lda.pred)[1:5,]
table(lda.pred$class, test$Creditability)
#348 correct prediction
#classifier predicts 348 good customers and 152 bad customers
#But in reality 436 good customers and 64 bad customers
mean(lda.pred$class== test$Creditability)#gives the correct classification rate 69%
#(32+120)/500
#0.304

##
## Quadratic Discriminant Analysis
##
qda.fit <- qda(train$Creditability ~ train$Value.Savings.Stocks + train$Length.of.current.employment + train$Duration.of.Credit..month.+ train$Credit.Amount + train$Age..years., data = train)
qda.fit
qda.pred <- predict(qda.fit, test$Creditability)
qda.pred[1:5,]
class(qda.pred)
data.frame(qda.pred)[1:5,]
table(qda.pred$class, test$Creditability)
#311 correct prediction
#classifier predicts 348 good customers and 255 bad customers
#But in reality 245 good customers and 64 bad customers
mean(qda.pred$class== test$Creditability)#gives the correct classification rate 62%
#misclassification rate/error rate
#(146+43)/500= 37%

##
## Cutting Age techniques avaliable in r
##Using recursive patitioning
require(rpart)
tree.fit <- rpart(train$Creditability ~.,data = train)
tree.fit
summary(tree.fit)
plot(tree.fit)
text(tree.fit, pretty = 0, cex=0.6)
test$t <- predict(tree.fit, type = 'class', test)

#score the test data
test$score <- predict(tree.fit, type = 'prob', test)
pred <- prediction(test$score[,2], test$Creditability)
pred
per <- performance(pred, "tpr", "fpr")
plot(per)

#build a model using 90% and 10% prior
tree.fit2 <- rpart(train$Creditability ~., data = train, parms = list(prior=c(0.9, 0.1)), cp=0.0002)
tree.fit2
plot(tree.fit2)
text(tree.fit2, pretty = 0, cex=0.6)
test$score2 <- predict(tree.fit2, type = 'prob', test)
pred2 <- prediction(test$score2[,2], test$Creditability)
pred2
per2 <- performance(pred2, "tpr", "fpr")
plot(per2)

#Comparing Complexity and out of Sample Error
#prints complexity and out of sample error
printcp(tree.fit)
#plots complexity vs. error
plotcp(tree.fit)
#prints complexity and out of sample error
printcp(tree.fit2)
#plots complexity vs. error
plotcp(tree.fit2)


##
require(tree)
tree <- tree(Creditability ~ Account.Balance+Duration.of.Credit..month.+Payment.Status.of.Previous.Credit+Purpose+Credit.Amount+Value.Savings.Stocks+Length.of.current.employment+Instalment.per.cent+Sex...Marital.Status+Guarantors+Duration.in.Current.address+Most.valuable.available.asset+Age..years.+Concurrent.Credits+Type.of.apartment+No.of.Credits.at.this.Bank+Occupation+No.of.dependents+Telephone, data=train, method="class")
summary(tree)
plot(tree)
text(tree, pretty = 0, cex=0.6)
test$pred <- predict(tree, test, type = 'class')
table(test$pred, test$Creditability)
#accuracy = (57+301)/500 = 71.6%
#error rate = (47+95)/500 = 28.4%
train.prune <- prune.misclass(tree, best = 8)
test.prune <- predict(train.prune, test, type = 'class')
table(test.prune, test$Creditability)
#accuracy = (77+305)/500 =76.4%
