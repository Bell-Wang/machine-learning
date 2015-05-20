#Part1 FUNCTION
p0=c(0.1,0.2,0.3,0.4)
n0=400
k0=4
rX<-function(n0,p0,k0){
  U<-runif(n0)
  X1<-rep(0,n0)
  w1<-which(U<=p0[1])
  X1[w1]<-1
  for (i in 2:k0-1) {
    wi<-which(U>sum(p0[1:i-1]) & (U<sum(p0[1:i])))
    X1[wi]<-i}
  wk<-which(U>sum(p0[1:k0-1]))
  X1[wk]<-k0
  return(X1)}

dist=rX(n0,p0,k0)

library(ggplot2)
qplot(as.factor(dist), geom="histogram",main="n=400",xlab="c")

#Part2
X <- read.csv("~/Desktop/HW/ML/HW3/X.csv", header=FALSE)
y <- read.csv("~/Desktop/HW/ML/HW3/y.csv", header=FALSE)
dsets <- cbind(X,y)
colnames(dsets)[11] <- "label"
obs<-dsets[1:183,]
training<-dsets[184:683,]

###Bayes classifier INITIAL
n=500
tn=183
tt=1000
w<-matrix(NA,n,tt+1)
w[,1]<-1/n
wp<-matrix(NA,n,tt+1)

weakclass<-matrix(0,n,1000)
response<-matrix(0,n,1000)
alphaap<-c()
errap<-c()
testweakclass<-matrix(0,tn,1000)
testresponse<-matrix(0,tn,1000)
aggErrorrate<-c()
testaggErrorrate<-c()

mu<-rbind(colMeans(training[which(training$label==1),2:10]),colMeans(training[which(training$label==-1),2:10]))
co<-cov(training[,2:10])
p1<-length(which(training$label==1))/n
p2<-1-p1

m0<-log(p1/p2)-0.5*t(colSums(mu))%*% solve(co)%*% (-apply(mu,2,diff))
m<-solve(co)%*% (-apply(mu,2,diff))

###response trainingset and testset
r<-as.numeric(m0)+as.matrix(training[,2:10])%*% m
testr<-as.numeric(m0)+as.matrix(obs[,2:10])%*%m

res<-rep(0,n)
testres<-rep(0,tn)

rind<-which(r[,1]<0)
res[rind]<--1
rind2<-which(r[,1]>0)
res[rind2]<-1
temp<-cbind(training,res)

testrind<-which(testr[,1]<0)
testres[testrind]<--1
testrind2<-which(testr[,1]>0)
testres[testrind2]<-1
testtemp<-cbind(obs,testres)

err<-sum(w[which(training$label!=temp$res),1])
alpha<-.5*log((1-err)/err)
alphaap[1]<-alpha
errap[1]<-err
wp[,2]<-w[,1]*exp(-alpha*temp$res*temp$label)
w[,2]<-wp[,2]/sum(wp[,2])

weakclass[,1]<-alpha*res
response[,1]<-sign(rowSums(weakclass,na.rm=TRUE))

testweakclass[,1]<-alpha*testres
testresponse[,1]<-sign(rowSums(testweakclass,na.rm=TRUE))

##ErrorRate of training and test datasets
aggErrorrate[1]=(length(which(temp[,11]!=response[,1]))/n)
testaggErrorrate[1]=(length(which(testtemp[,11]!=testresponse[,1]))/tn)


###Loop
for (t in 2:1000){
  thi<-matrix(0,nrow=n,ncol=11)
  newcdf<-matrix(0,nrow=n,ncol=2)
  cdf<-c()
  index<-c()
  U<-runif(n)  
  for (ii in 1:n){
    cdf[ii]<-sum(w[1:ii,t])}
  
  newcdf[1,1:2]<-c(0,cdf[1])
  for (ii in 2:n){
    newcdf[ii,1:2]<-c(cdf[ii-1],cdf[ii])}
  
  for (ii in 1:n){
    index[ii]<-which(newcdf[,2]>=U[ii]& newcdf[,1]<U[ii])} 
  thi<-training[index,]
  
  mu<-rbind(colMeans(thi[which(thi[,11]==1),2:10]),colMeans(thi[which(thi[,11]==-1),2:10]))
  co<-cov(thi[,2:10])
  p1<-length(which(thi[,11]==1))/n
  p2<-1-p1
  
  m0<-log(p1/p2)-0.5*t(colSums(mu))%*% solve(co)%*% -apply(mu,2,diff)
  m<-solve(co)%*% (-apply(mu,2,diff))
  
  ###response trainingset and testset
  r<-as.numeric(m0)+as.matrix(training[,2:10])%*% m
  testr<-as.numeric(m0)+as.matrix(obs[,2:10])%*%m
  
  res<-rep(0,n)
  testres<-rep(0,tn)
  
  rind<-which(r[,1]<0)
  res[rind]<--1
  rind2<-which(r[,1]>0)
  res[rind2]<-1
  temp<-cbind(training,res)
  
  testrind<-which(testr[,1]<0)
  testres[testrind]<--1
  testrind2<-which(testr[,1]>0)
  testres[testrind2]<-1
  testtemp<-cbind(obs,testres)
  
  err<-sum(w[which(training[,11]!=temp[,12]),t])
  alpha<-.5*log((1-err)/err)
  alphaap[t]<-alpha
  errap[t]<-err
  wp[,t+1]<-w[,t]*exp(-alpha*temp[,11]*temp[,12])
  w[,t+1]<-wp[,t+1]/sum(wp[,t+1])
  
  weakclass[,t]<-alpha*res
  response[,t]<-sign(rowSums(weakclass,na.rm=TRUE))
  
  testweakclass[,t]<-alpha*testres
  testresponse[,t]<-sign(rowSums(testweakclass,na.rm=TRUE))
  
  ##ErrorRate of training and test datasets
  aggErrorrate[t]=(length(which(temp[,11]!=response[,t]))/n)
  testaggErrorrate[t]=(length(which(testtemp[,11]!=testresponse[,t]))/tn)
}

##ANSWER!
#2
par(mar=c(1,1,1,1)+1,mgp=c(0,1,0))
plot(aggErrorrate,type="l",col="blue",ylim=c(0,0.18),ann=FALSE)
lines(testaggErrorrate,type="l",col="red",ann=FALSE)
title(xlab="interation t")
title(ylab="error")
title(main="Part2")
legend("topright",legend=c("training","testing"),cex=1,col=c("blue","red"),lty=1)
#3
testAcc<-(1-testaggErrorrate[1])*100
print(testAcc)
#4
par(mar=c(1,1,1,1)+1,mgp=c(0,1,0))
plot(alphaap,type="l",col="blue",ann=FALSE)
title(xlab="interation t")
title(ylab="alpha")
title(main="Part2. alpha as function of t")
par(mar=c(1,1,1,1)+1,mgp=c(0,1,0))
plot(errap,type="l",col="blue",ann=FALSE)
title(xlab="interation t")
title(ylab="epsi")
title(main="Part2. epsi as function of t")
#5
s<-sample(1:500,3)
par(mar=c(1,1,1,1)+1,mgp=c(0,1,0))
plot(w[s[1],],type="l",col="blue",ann=FALSE)
lines(w[s[2],],type="l",col="red",ann=FALSE)
lines(w[s[3],],type="l",col="green",ann=FALSE)
title(xlab="iteration t")
title(ylab="p")
title(main="Part2. p as function of t")
legend("topright",legend=c(s[1],s[2],s[3]),cex=1,col=c("blue","red","green"),lty=1)

#Part3

n=500
tn=183
tt=1000
lw<-matrix(0,n,tt+1)
lw[,1]<-1/n
lwp<-matrix(0,n,tt+1)

lweakclass<-matrix(0,n,1000)
lresponse<-matrix(0,n,1000)
lalphaap<-c()
lerrap<-c()
ltestweakclass<-matrix(0,tn,1000)
ltestresponse<-matrix(0,tn,1000)
ltestalphaap<-c()
ltesterrap<-c()
laggErrorrate<-c()
ltestaggErrorrate<-c()

###INITIAL

Xlog<-as.matrix(training[,1:10])
ylog<-as.matrix(training[,11])
st<-0.1
wlog<-as.matrix(c(rep(0,10)))
for (ite in 1:n){
  sig<-1/(1+exp((-ylog[ite]*Xlog[ite,])%*% wlog))
  wlog<-wlog+st*(1-sig)*ylog[ite]*Xlog[ite,]}

training1<-as.matrix(training[,1:10])
lp1<-exp((training1[,1:10])%*%wlog)/(1+exp((training1[,1:10])%*%wlog))  
lp2<-1-lp1

like<-log(lp1/lp2)
###response trainingset and testset
testlp1<-exp(as.matrix(obs[,1:10])%*%wlog)/(1+exp(as.matrix(obs[,1:10])%*%wlog))
testlp2<-1-testlp1
testlike<-log(testlp1/testlp2)

lres<-rep(0,n)
ltestres<-rep(0,tn)

lrind<-which(like[,1]<0)
lres[lrind]<--1
lrind2<-which(like[,1]>0)
lres[lrind2]<-1
ltemp<-cbind(training,lres)

ltestrind<-which(testlike[,1]<0)
ltestres[ltestrind]<--1
ltestrind2<-which(testlike[,1]>0)
ltestres[ltestrind2]<-1
ltesttemp<-cbind(obs,ltestres)

lerr<-sum(lw[which(ltemp$label!=ltemp$lres),1])
lalpha<-.5*log((1-lerr)/lerr)
lalphaap[1]<-lalpha
lerrap[1]<-lerr
lwp[,2]<-lw[,1]*exp(-lalpha*ltemp$lres*ltemp$label)
lw[,2]<-lwp[,2]/sum(lwp[,2])

lweakclass[,1]<-lalpha*lres
lresponse[,1]<-sign(rowSums(lweakclass,na.rm=TRUE))

ltestweakclass[,1]<-lalpha*ltestres
ltestresponse[,1]<-sign(rowSums(ltestweakclass,na.rm=TRUE))

##ErrorRate of training and test datasets
laggErrorrate[1]=(length(which(ltemp[,11]!=lresponse[,1]))/n)
ltestaggErrorrate[1]=(length(which(ltesttemp[,11]!=ltestresponse[,1]))/tn)

###start loop
for (t in 2:1000){
  lthi<-matrix(0,nrow=n,ncol=11)
  newcdf<-matrix(0,nrow=n,ncol=2)
  cdf<-c()
  index<-c()
  U<-runif(n)  
  for (ii in 1:n){
    cdf[ii]<-sum(lw[1:ii,t])}
  newcdf[1,1:2]<-c(0,cdf[1])
  for (ii in 2:n){
    newcdf[ii,1:2]<-c(cdf[ii-1],cdf[ii])}
  for (ii in 1:n){
    index[ii]<-which(newcdf[,2]>=U[ii]& newcdf[,1]<U[ii])} 
  lthi<-training[index,]
  
  Xlog<-as.matrix(lthi[,1:10])
  ylog<-as.matrix(lthi[,11])
  st<-0.1
  wlog<-as.matrix(c(rep(0,10)))
  for (ite in 1:n){
    sig<-1/(1+exp(Xlog[ite,]%*% wlog *(-ylog[ite])))
    wlog<-wlog+st*(1-sig)*ylog[ite]*Xlog[ite,]}
  training1<-as.matrix(training)
  

  lp1<-exp(training1[,1:10]%*%wlog)/(1+exp(training1[,1:10]%*%wlog))  
  lp2<-1-lp1 
  like<-log(lp1/lp2)
  
  testlp1<-exp(as.matrix(obs[,1:10])%*%wlog)/(1+exp(as.matrix(obs[,1:10])%*%wlog))
  testlp2<-1-testlp1
  testlike<-log(testlp1/testlp2)
  ###response trainingset and testset
  lres<-rep(0,n)
  ltestres<-rep(0,tn)
  
  lrind<-which(like[,1]<0)
  lres[lrind]<--1
  lrind2<-which(like[,1]>0)
  lres[lrind2]<-1
  ltemp<-cbind(training1,lres)
  
  ltestrind<-which(testlike[,1]<0)
  ltestres[ltestrind]<--1
  ltestrind2<-which(testlike[,1]>0)
  ltestres[ltestrind2]<-1
  ltesttemp<-cbind(obs,ltestres)
  
  lerr<-sum(lw[which(training1[,11]!=ltemp[,12]),t])
  lalpha<-.5*log((1-lerr)/lerr)
  lalphaap[t]<-lalpha
  lerrap[t]<-lerr
  lwp[,t+1]<-lw[,t]*exp(-lalpha*ltemp[,11]*ltemp[,12])
  lw[,t+1]<-lwp[,t+1]/sum(lwp[,t+1])
  
  lweakclass[,t]<-lalpha*lres
  lresponse[,t]<-sign(rowSums(lweakclass,na.rm=TRUE))
  
  ltestweakclass[,t]<-lalpha*ltestres
  ltestresponse[,t]<-sign(rowSums(ltestweakclass,na.rm=TRUE))
  
  ##ErrorRate of training and test datasets
  laggErrorrate[t]=(length(which(ltemp[,11]!=lresponse[,t]))/n)
  ltestaggErrorrate[t]=(length(which(ltesttemp[,11]!=ltestresponse[,t]))/tn)
}

##ANSWER!

#2
par(mar=c(1,1,1,1)+1,mgp=c(0,1,0))
plot(laggErrorrate,type="l",col="blue",ylim=c(0,0.2),ann=FALSE)
lines(ltestaggErrorrate,type="l",col="red",ann=FALSE)
title(xlab="iteration t")
title(ylab="error")
title(main="Part3")
legend("topright",legend=c("training","testing"),cex=1,col=c("blue","red"),lty=1)
#3
testAcc<-(1-ltestaggErrorrate[1])*100
print(testAcc)
(1-laggErrorrate[1])*100
#4
par(mar=c(1,1,1,1)+1,mgp=c(0,1,0))
plot(lalphaap,type="l",col="blue",ann=FALSE)
title(xlab="interation t")
title(ylab="alpha")
title(main="Part3. alpha as function of t")
par(mar=c(1,1,1,1)+1,mgp=c(0,1,0))
plot(lerrap,type="l",col="blue",ann=FALSE)
title(xlab="interation t")
title(ylab="epsi")
title(main="Part3. epsi as function of t")
#5
s<-sample(1:500,3)
par(mar=c(1,1,1,1)+1,mgp=c(0,1,0))
plot(lw[s[1],],type="l",col="blue",ann=FALSE)
lines(lw[s[2],],type="l",col="red",ann=FALSE)
lines(lw[s[3],],type="l",col="green",ann=FALSE)
title(xlab="interation t")
title(ylab="p")
title(main="Part3. p as function of t")
legend("topright",legend=c(s[1],s[2],s[3]),cex=1,col=c("blue","red","green"),lty=1)









