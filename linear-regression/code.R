#Problem 3

##prepare data
X <- read.csv("~/Desktop/HW/ML/HW1/data_csv/X.txt", header=FALSE)
y <- read.table("~/Desktop/HW/ML/HW1/data_csv/y.txt", quote="\"")
colnames(X)[1] <- "intercept term"
colnames(X)[2] <- "number of cylinders"
colnames(X)[3] <- "displacement"
colnames(X)[4] <- "horsepower"
colnames(X)[5] <- "weight"
colnames(X)[6] <- "acceleration"
colnames(X)[7] <- "model year"
colnames(y)[1] <- "mpg"
X["intercept term"] <- NULL
LRdataset <- cbind(X,y) #combine data into one file

###Part1(a)
LRsubset <- sample(nrow(LRdataset),372) 
trainingsubset <- LRdataset[LRsubset,] #randomly select 372 training data
newcol <- c(1)
newLRtr <- cbind(newcol, trainingsubset)[,-8]
testsubset <- LRdataset[-LRsubset,] 
newLRtest <- cbind(newcol, testsubset) [,-8] 
LXt <- t(newLRtr) #X transpose
LA <- t(newLRtr) %*% data.matrix(newLRtr)
solve(LA) #inverse
LRcoef <- solve(LA) %*% LXt %*% data.matrix(trainingsubset$mpg) #answer to Part1(a)
LRfit <- lm(mpg~., data=trainingsubset) #plus, can get more info use build-in function
summary(LRfit) #get more statistics info for linear regression model

###Part1(b)
MAE <- vector()
R=1000 #repeat 1000 times
for (i in 1:R) {
LRsubset <- sample(nrow(LRdataset),372) 
trainingsubset <- LRdataset[LRsubset,] #randomly select 372 training data
newcol <- c(1)
newLRtr <- cbind(newcol, trainingsubset)[,-8]
testsubset <- LRdataset[-LRsubset,] 
newLRtest <- cbind(newcol, testsubset) [,-8] 
LXt <- t(newLRtr) #X transpose
LA <- t(newLRtr) %*% data.matrix(newLRtr)
solve(LA) #inverse
LRcoef <- solve(LA) %*% LXt %*% data.matrix(trainingsubset$mpg)
sim <- data.matrix(newLRtest) %*% LRcoef
obs <- testsubset$mpg
MAE[i] <- c(mean(abs(sim-obs), na.rm=TRUE))
}
mean(MAE)
sd(MAE)

##test with build-in funciton lm()
#MAE <- vector()
#R=1000 #repeat 1000 times
#for (i in 1:R) {
  #LRsubset <- sample(nrow(LRdataset),372) #randomly select 372 training data
  #trainingsubset <- LRdataset[LRsubset,]
  #LRfit <- lm(mpg~., data=trainingsubset) #apply linear regression model
  #testsubset <- LRdataset[-LRsubset,] #20 test data
  #predict(LRfit, testsubset) #prection model
  #sim <- predict(LRfit, testsubset)
  #obs <- testsubset$mpg
 # MAE[i] <- c(mean(abs(sim-obs), na.rm=TRUE))
#}
#mean(MAE)
#sd(MAE)

###Part2(a)(b)
####p=1
diffp1=matrix(data=NA, nrow=20,ncol=1000)
RMSE1 <- vector()
R=1000
for (i in 1:R){
  PRsubset <- sample(nrow(LRdataset), 372) 
  PRtrainingsubset <- LRdataset[PRsubset,] #randomly select 372 training data
  newcol <- c(1)
  newPRtr <- cbind(newcol, PRtrainingsubset)[,-8]
  PRtestsubset <- LRdataset[-PRsubset,] 
  newPRtest <- cbind(newcol, PRtestsubset) [,-8] #get reorganized 20 test dat ready for implement
  Xt <- t(newPRtr) #X transpose
  A <- t(newPRtr) %*% data.matrix(newPRtr)
  solve(A) #inverse
  coef1 <- solve(A) %*% Xt %*% data.matrix(PRtrainingsubset$mpg) #get coeficients based on equation
  PRy1 <- data.matrix(newPRtest) %*% coef1 #fit model and get the predicted results
  PRobs <- PRtestsubset$mpg #get control ones
  diffp1[,i]<-c(PRobs-PRy1) #colect errors for Part(b)
  RMSE1[i] <- sqrt(mean((PRy1-PRobs)^2))} #RMSW function
RMSEmean1 <- mean(RMSE1)
RMSEsd1 <- sd(RMSE1)
####p=2
diffp2=matrix(data=NA, nrow=20,ncol=1000)
RMSE2 <- vector()
R=1000
for (i in 1:R){
  PRsubset <- sample(nrow(LRdataset), 372) 
  PRtrainingsubset <- LRdataset[PRsubset,] #randomly select 372 training data
  newcol <- c(1)
  newPRtr <- cbind(newcol, PRtrainingsubset)[,-8]
  PRtestsubset <- LRdataset[-PRsubset,] #20 test data
  newPRtest <- cbind(newcol, PRtestsubset) [,-8]
  newPRtr22 <- newPRtr^2
  colnames(newPRtr22) <- c("0","1","2","3","4","5","6") #need to rename the column
  newPRtr2 <- cbind(newPRtr,newPRtr22)[,-8]
  Xt2 <- t(newPRtr2)
  A2 <- Xt2 %*% data.matrix(newPRtr2)
  solve(A2)
  coef2 <- solve(A2) %*% Xt2 %*% data.matrix(PRtrainingsubset$mpg)
  newPRtest22 <- newPRtest^2
  colnames(newPRtest22) <- c("0","1","2","3","4","5","6")
  newPRtest2 <- cbind(newPRtest, newPRtest22)[,-8]
  PRy2 <- data.matrix(newPRtest2) %*% coef2
  PRobs <- PRtestsubset$mpg
  diffp2[,i]<-c(PRobs-PRy2)
  RMSE2[i] <- sqrt(mean((PRy2-PRobs)^2))} #RMSW function
RMSEmean2 <- mean(RMSE2)
RMSEsd2 <- sd(RMSE2)
###p=3
diffp3=matrix(data=NA, nrow=20,ncol=1000)
RMSE3 <- vector()
R=1000
for (i in 1:R){
  PRsubset <- sample(nrow(LRdataset), 372) 
  PRtrainingsubset <- LRdataset[PRsubset,] #randomly select 372 training data
  newcol <- c(1)
  newPRtr <- cbind(newcol, PRtrainingsubset)[,-8]
  PRtestsubset <- LRdataset[-PRsubset,] #20 test data
  newPRtest <- cbind(newcol, PRtestsubset) [,-8]
  newPRtr22 <- newPRtr^2
  colnames(newPRtr22) <- c("0","1","2","3","4","5","6")
  newPRtr2 <- cbind(newPRtr,newPRtr22)[,-8]
  newPRtr33 <- newPRtr^3
  colnames(newPRtr33) <- c("03","13","23","33","43","53","63")
  newPRtr3 <- cbind(newPRtr2,newPRtr33)[,-14]
  Xt3 <- t(newPRtr3)
  A3 <- Xt3 %*% data.matrix(newPRtr3)
  solve(A3)
  coef3 <- solve(A3) %*% Xt3 %*% data.matrix(PRtrainingsubset$mpg)
  newPRtest22 <- newPRtest^2
  colnames(newPRtest22) <- c("0","1","2","3","4","5","6")
  newPRtest2 <- cbind(newPRtest, newPRtest22)[,-8]
  newPRtest33 <- newPRtest^3
  colnames(newPRtest33) <- c("03","13","23","33","43","53","63")
  newPRtest3 <- cbind(newPRtest2, newPRtest33)[,-14]
  PRy3 <- data.matrix(newPRtest3) %*% coef3
  PRobs <- PRtestsubset$mpg
  diffp3[,i]<-c(PRobs-PRy3)
  RMSE3[i] <- sqrt(mean((PRy3-PRobs)^2))} #RMSW function
RMSEmean3 <- mean(RMSE3)
RMSEsd3 <- sd(RMSE3)
###p=4
diffp4=matrix(data=NA, nrow=20,ncol=1000)
R=1000
RMSE4 <- vector(,1000)
for (i in 1:R){
  PRsubset <- sample(nrow(LRdataset), 372) 
  PRtrainingsubset <- LRdataset[PRsubset,] #randomly select 372 training data
  newcol <- c(1)
  newPRtr <- cbind(newcol, PRtrainingsubset)[,-8]
  PRtestsubset <- LRdataset[-PRsubset,] #20 test data
  newPRtest <- cbind(newcol, PRtestsubset) [,-8]
  newPRtr22 <- newPRtr^2
  colnames(newPRtr22) <- c("0","1","2","3","4","5","6")
  newPRtr2 <- cbind(newPRtr,newPRtr22)[,-8]
  newPRtr33 <- newPRtr^3
  colnames(newPRtr33) <- c("03","13","23","33","43","53","63")
  newPRtr3 <- cbind(newPRtr2,newPRtr33)[,-14]
  newPRtr44 <- newPRtr^4
  colnames(newPRtr44) <- c("04","14","24","34","44","54","64")
  newPRtr4 <- cbind(newPRtr3,newPRtr44)[,-20]
  Xt4 <- t(newPRtr4)
  A4 <- Xt4 %*% data.matrix(newPRtr4)
  solve(A4)
  coef4 <- solve(A4) %*% Xt4 %*% data.matrix(PRtrainingsubset$mpg)
  newPRtest22 <- newPRtest^2
  colnames(newPRtest22) <- c("0","1","2","3","4","5","6")
  newPRtest2 <- cbind(newPRtest, newPRtest22)[,-8]
  newPRtest33 <- newPRtest^3
  colnames(newPRtest33) <- c("03","13","23","33","43","53","63")
  newPRtest3 <- cbind(newPRtest2, newPRtest33)[,-14]
  newPRtest44 <- newPRtest^4
  colnames(newPRtest44) <- c("04","14","24","34","44","54","64")
  newPRtest4 <- cbind(newPRtest3, newPRtest44)[,-20]
  PRy4 <- data.matrix(newPRtest4) %*% coef4
  PRobs <- PRtestsubset$mpg
  diffp4[,i]<-c(PRobs-PRy4)
  RMSE4[i] <- sqrt(mean((PRy4-PRobs)^2))} #RMSW function
RMSEmean4 <- mean(RMSE4)
RMSEsd4 <- sd(RMSE4)

RMSEmean_value <- c(RMSEmean1,RMSEmean2,RMSEmean3,RMSEmean4)
RMSEsd_value <- c(RMSEsd1,RMSEsd2,RMSEsd3,RMSEsd4)
RMset <- rbind(RMSEmean_value,RMSEsd_value)
colnames(RMset) <-c("p=1","p=2","p=3","p=4")
RMtable <- as.table(RMset) #table for Part2(a)

###Part2(b) histogram
par(mar=rep(2.5,4), ps=10)
hist(diffp1,breaks=40, col="yellow",xlab="errors(ytest-ypred)",ylab="freq",main="p=1",labels=TRUE)
hist(diffp2,breaks=40, col="blue",xlab="errors(ytest-ypred)",ylab="freq",main="p=2",labels=TRUE)
hist(diffp3,breaks=40, col="red",xlab="errors(ytest-ypred",ylab="freq",main="p=3",labels=TRUE)
hist(diffp4,breaks=40, col="green",xlab="errors(ytest-ypred)",ylab="freq",main="p=4",labels=TRUE)

###Part2(c)
N =20000
###p=1
mu1 <- mean(diffp1)
mup1 <- matrix(data=mu1, nrow=20,ncol=1000) #MLE of mean
sigmap1 <- sqrt(mean((diffp1-mup1)^2)) #MLE of sd
gaussian1 <- (1/(sigmap1*sqrt(2*pi)))*exp(-(diffp1-mu1)^2/(2*sigmap1^2)) #fit gaussian function
loglik1<- sum(log(gaussian1))
###p=2
mu2 <- mean(diffp2)
mup2 <- matrix(data=mu2, nrow=20,ncol=1000)
sigmap2 <- sqrt(mean((diffp2-mup2)^2))
gaussian2 <- (1/(sigmap2*sqrt(2*pi)))*exp(-(diffp2-mu2)^2/(2*sigmap2^2))
loglik2<- sum(log(gaussian2))
###p=3
mu3 <- mean(diffp3)
mup3 <- matrix(data=mu3, nrow=20,ncol=1000)
sigmap3 <- sqrt(mean((diffp3-mup3)^2))
gaussian3 <- (1/(sigmap3*sqrt(2*pi)))*exp(-(diffp3-mu3)^2/(2*sigmap3^2))
loglik3<- sum(log(gaussian3))
###p=4
mu4 <- mean(diffp4)
mup4 <- matrix(data=mu4, nrow=20,ncol=1000)
sigmap4 <- sqrt(mean((diffp4-mup4)^2))
gaussian4 <- (1/(sigmap4*sqrt(2*pi)))*exp(-(diffp4-mu4)^2/(2*sigmap4^2))
loglik4<- sum(log(gaussian4))

