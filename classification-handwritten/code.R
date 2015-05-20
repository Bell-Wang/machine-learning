#import data
Xtest <- read.csv("~/Desktop/HW/ML/HW2/Xtest.txt", header=FALSE)
label_train <- read.table("~/Desktop/HW/ML/HW2/label_train.txt", quote="\"")
Q <- read.csv("~/Desktop/HW/ML/HW2/Q.txt", header=FALSE)
Xtrain <- read.csv("~/Desktop/HW/ML/HW2/Xtrain.txt", header=FALSE)
label_test <- read.table("~/Desktop/HW/ML/HW2/label_test.txt", quote="\"")
Trainds<- cbind(Xtrain,label_train)
Testds <- cbind(Xtest,label_test)
colnames(Trainds)[21] <- "V21"
colnames(Testds)[21] <- "V21"
library("MASS", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")

##define distance function
different <- vector()
euclideandistance <- function(x1, x2) {
  for (x in 1:20){different[x] <-(x1[x]-x2[x])^2}
  distance <- sqrt(sum(different))
  return(distance)
}
##get distance matrix 500*5000
sum <-0 
sqrb <- 0
for (e in 1:500){
  for (o in 1:20){
    di <- (Xtest[e,o]-Xtrain[,o])^2
    sum <- di+sum
    sqr <- sqrt(sum)} ###euclidistant equation
  sqrb <-rbind(sqrb,sqr)}
nsqrb<-sqrb[2:501,]

##neighbork vote
voteresult <- vector()
neighbor <- matrix(data=NA,ncol=5,nrow=500)
inds<-matrix(data=NA,ncol=5,nrow=500)
response<-matrix(data=NA,ncol=5,nrow=500)
bendist<- as.matrix(pdist(Xtest,Xtrain))
vote <- function(k){ 
  for (p in 1:500){
    neighbor[p, 1:k]<-sort(bendist[p,])[1:k] #based on dist, find k smallest values as neighbors
    inds[p,1:k] <- which(bendist[p,] %in% neighbor[p,1:k]) #index location of neighbor in training
    response[p,1:k] <- label_train[c(inds[p,1:k]),1] #return k response
    voteresult[p]<-as.numeric(names(sort(table(response[p,]),decreasing=TRUE)[1]))}
  return(voteresult)
}

##confusion matrix
kcf1<-table(cbind(label_test,vote(1)))
kcf2<-table(cbind(label_test,vote(2)))
kcf3<-table(cbind(label_test,vote(3)))
kcf4<-table(cbind(label_test,vote(4)))
kcf5<-table(cbind(label_test,vote(5)))
acc<- function(cm){
  sum(diag(cm))/500}###define accuracy function
acvector <- c(acc(kcf1),acc(kcf2),acc(kcf3),acc(kcf4),acc(kcf5))
knn <- c(1,2,3,4,5)
plot(acvector,knn)

##image
par(mar=c(1,1,1,1))
Q1 <- as.matrix(Q)
Xtest1 <- as.matrix(Xtest)
image(matrix(Q1 %*% Xtest1[448,],ncol=28,byrow=T))

#Prepare subset of data by Classes
Class0 <- subset(Trainds, V21 ==0)[1:20]
Class1 <- subset(Trainds, V21 ==1)[1:20]
Class2 <- subset(Trainds, V21 ==2)[1:20]
Class3 <- subset(Trainds, V21 ==3)[1:20]
Class4 <- subset(Trainds, V21 ==4)[1:20]
Class5 <- subset(Trainds, V21 ==5)[1:20]
Class6 <- subset(Trainds, V21 ==6)[1:20]
Class7 <- subset(Trainds, V21 ==7)[1:20]
Class8 <- subset(Trainds, V21 ==8)[1:20]
Class9 <- subset(Trainds, V21 ==9)[1:20]

phi <- function(Class){
  nrow(Class)/nrow(Trainds)}
##defined function to get MLE of parameters for each class
muhat <- function(Class){
  as.vector(colSums(Class)/500)
}
tempt <- function(Class,a){
  t(as.matrix(Class[a,]-muhat(Class))) %*% as.matrix(Class[a,]-muhat(Class))
}
sighat <- function(Class){
  sum <- 0
  for (i in 1:500){
    sum <- tempt(Class,i)+sum}
  return(sum/500)
}

##max p in one class
temc <- vector()
cls <- function(Class){
  for (w in 1:500){
    temc[w]<-(t(muhat(Class)) %*% solve(sighat(Class)) %*% t(Xtest[500,])-1/2*t(muhat(Class)) %*% solve(sighat(Class)) %*% muhat(Class) +log(phi(Class)))
  }
  return(temc)
}
##max p among classes
maxcls <- data.frame(cls(Class0),cls(Class1),cls(Class2),cls(Class3),cls(Class4),cls(Class5),cls(Class6),cls(Class7),cls(Class8),cls(Class9))
maxcl <- t(maxcls)
## get the response label
res <- vector()
for (p in 1:500){
  res[p] <- which(maxcl[,p] %in% max(maxcl[,p]))-1 #index location of neighbor in training
}
qda.fit <- qda (V21~V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20, data=Trainds)
qda.class <-predict(qda.fit, Xtest)$class
gclass<-as.vector(qda.class)
##confusion matrix
cf<-table(cbind(label_test,gclass))
acc(cf)
library(mnormt)
pr<-vector()
classnu<-c("Class0","Class1","Class2","Class3","Class4","Class5","CLass6","Class7","Class8","Class9")
predi<-function(numr,Class){dmnorm(Xtest[numr,], mean=muhat(Class), varcov=sighat(Class))}
pre452<-c(predi(452,Class0),predi(452,Class1),predi(452,Class2),predi(452,Class3),predi(452,Class4),predi(452,Class5),
          predi(452,Class6),predi(452,Class7),predi(452,Class8),predi(452,Class9))
data.frame(classnu,pre452)
##image of each Gaussian mean
mtable <-rbind(muhat(Class0),muhat(Class1),muhat(Class2),muhat(Class3),muhat(Class4),muhat(Class5),muhat(Class6),
               muhat(Class7),muhat(Class8),muhat(Class9))
imagef <- function(tble){par(mar=c(1,1,1,1))
                         Q1 <- as.matrix(Q)
                         image(matrix(Q1 %*% as.matrix(tble),ncol=28,byrow=T))}
imagef(mtable[2,])

sigtable <-rbind(sighat(Class0),sighat(Class1),sighat(Class2),sighat(Class3),sighat(Class4),sighat(Class5),sighat(Class6),
                 sighat(Class7),sighat(Class8),sighat(Class9))

#logistic classification
logXtrain <- as.matrix(cbind(Xtrain, 1))
logXtest <- as.matrix(cbind(Xtest,1))

n <-21
k <-10
mm <- 5000
roh <- 0.1/5000
weigh <- matrix(data=0, ncol=k, nrow=n)
prob <- matrix(data=NA, ncol=k, nrow=mm)
p <- matrix (data=0, ncol=k, nrow=mm)
y<-as.matrix(cbind(p,label_train))
error <- matrix(data=NA,ncol=k,nrow=mm)
for (rown in 1:n){
  y[rown,as.numeric(y[rown,11])] <-1}
sump<-matrix(data=0,nrow=mm)
gradiantf <- function(cycle){
  for (iter in 1:cycle){ 
    sump<-matrix(data=0,nrow=mm)
    for (kk in 1:k) {
      sump <- sump + exp(logXtrain %*% weigh[,kk])} 
    for (class in 1:k){
      prob[,class] <-exp((logXtrain) %*% weigh[,class]) / sump}
    error <- y[,class]- prob
    weigh <- weigh + roh*(t(logXtrain) %*% error)}
  return(weigh)
}

n <-21
k <-10
mm <- 5000
roh <- 0.1/5000
weigh <- matrix(data=0, ncol=k, nrow=n)
prob <- matrix(data=NA, ncol=k, nrow=mm)
p <- matrix (data=0, ncol=k, nrow=mm)
y<-as.matrix(cbind(p,label_train))
error <- matrix(data=NA,ncol=k,nrow=mm)
for (rown in 1:n){
  y[rown,as.numeric(y[rown,11])] <-1}
sump<-matrix(data=0,nrow=mm)
gradiantf <- function(cycle){
  for (iter in 1:cycle){ 
    sump<-matrix(data=0,nrow=mm)
    for (kk in 1:k) {
      sump <- sump + exp(logXtrain %*% weigh[,kk])} 
    for (class in 1:k){
      prob[,class] <-exp((logXtrain) %*% weigh[,class]) / sump}
    error <- y[,class]- prob
    weigh <- weigh + roh*(t(logXtrain) %*% error)}
  return(weigh)
}

trainbind<-cbind(logXtrain,label_train)
colnames (trainbind)[22]<-"V22"
mmn <- 500
sumpt<-matrix(data=0,nrow=mmn)
for (kk in 1:k) {
  sumpt <- sumpt + exp(logXtest %*% gradiantf(1000)[,kk])} 
probn <- matrix(data=NA, ncol=k, nrow=mm)
for (class in 1:k){
  probn[,class] <-exp((logXtest) %*% gradiantf(1000))[,class] / sumpt}
library(nnet)
testlogr <-multinom(V22~V1+V2+V3+V4+V5+V6+V7+V8+V9+
                      V10+V11+V12+V13+V14+V15+V16+V17+
                      V18+V19+V20+1, trainbind,maxit=1000)

sofr<-predict(testlogr,logXtest)
sf<-table(cbind(label_test,sofr))
acc(sf)

##image of missclassified samples
imagef <- function(tble){par(mar=c(1,1,1,1))
                         Q1 <- as.matrix(Q)
                         image(matrix(Q1 %*% as.matrix(tble),ncol=28,byrow=T))}
imagef(Xtest1[452,])

