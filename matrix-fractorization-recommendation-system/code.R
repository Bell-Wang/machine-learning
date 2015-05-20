#Prob 1 (K-means)
##sample 500 data
N <- 500
U =runif(N)                                            
rand.samples = matrix(0,N,2)
s1 <- matrix(c(1,0,0,1),nrow=2,byrow=TRUE)
mu1 <- c(0,0)
s2 <- matrix(c(1,0,0,1),nrow=2,byrow=TRUE)
mu2 <- c(3,0)
s3 <- matrix(c(1,0,0,1),nrow=2,byrow=TRUE)
mu3 <- c(0,3)
pi= c(0.2,0.5,0.3)
##Sampling from the mixture
library(MASS)
for(i in 1:N){
  if(U[i]<pi[1]){
    rand.samples[i,] = mvrnorm(1,mu=mu1,Sigma=s1)
  }else if(U[i]<sum(pi[1]+pi[2])){
    rand.samples[i,] = mvrnorm(1,mu=mu2,Sigma=s2)
  }else{
    rand.samples[i,] = mvrnorm(1,mu=mu3,Sigma=s3)
  }
}
##Prep
sam1<-data.frame(rand.samples)
sam=sam1
t=20
L <- matrix(0,nrow=t,ncol=2)
L2 <- matrix(0,nrow=t,ncol=2)
dist <- function (sample,mu){
  res <-(sample[,1]-mu[1])^2+(sample[,2]-mu[2])^2
  return (res)}

##k=2 
K=2
mu1<-c()
mu2<-c()
mu1 <- c(sample(sam[,1],1),sample(sam[,2],1))
mu2 <- c(sample(sam[,1],1),sample(sam[,2],1))
##update c
for (i in 1:t ){
  sam[which(dist(sam,mu1)<dist(sam,mu2)),i+2]<-1
  sam[which(dist(sam,mu1)>dist(sam,mu2)),i+2]<-2
  a<-sum((sam[which(sam[i+2]==1),1]-mu1[1])^2+(sam[which(sam[i+2]==1),2]-mu1[2])^2)  
  b<-sum((sam[which(sam[i+2]==2),1]-mu2[1])^2+(sam[which(sam[i+2]==2),2]-mu2[2])^2)
  L2[i,1] <- a+b
##update centroid
  mu1 <- c(mean(sam[,1][sam[,i+2]==1]),mean(sam[,2][sam[,i+2]==1]))
  mu2 <- c(mean(sam[,1][sam[,i+2]==2]),mean(sam[,2][sam[,i+2]==2]))
  a<-sum((sam[which(sam[i+2]==1),1]-mu1[1])^2+(sam[which(sam[i+2]==1),2]-mu1[2])^2)  
  b<-sum((sam[which(sam[i+2]==2),1]-mu2[1])^2+(sam[which(sam[i+2]==2),2]-mu2[2])^2)
  L2[i,2]= a+b}

##k=3 set up
sam=sam1
K=3
mu1 <- c(sample(sam[,1],1),sample(sam[,2],1))
mu2 <- c(sample(sam[,1],1),sample(sam[,2],1))
mu3 <- c(sample(sam[,1],1),sample(sam[,2],1))
mu_n<-rbind(mu1,mu2,mu3)
L3 <- matrix(0,nrow=t,ncol=2)
dist_n <-matrix(0,nrow=N,ncol=K)
a<-c()
aa<-c()
#update c
for (i in 1:t ){
  for (j in 1:K){dist_n[,j]<-dist(sam,mu_n[j,])}
  for (j in 1:K) {
    sam[which(apply(dist_n,1,min)==dist(sam,mu_n[j,])),i+2] <- j
    a[j]<-sum((sam[which(sam[i+2]==j),1]-mu_n[j,1])^2+(sam[which(sam[i+2]==j),2]-mu_n[j,2])^2)}  
  L3[i,1] <- sum(a)
  #update centroid
  for (j in 1:K) {
    mu_n[j,] <- c(mean(sam[,1][sam[,i+2]==j]),mean(sam[,2][sam[,i+2]==j]))
    aa[j]<-sum((sam[which(sam[i+2]==j),1]-mu_n[j,1])^2+(sam[which(sam[i+2]==j),2]-mu_n[j,2])^2)  
    L3[i,2] <- sum(aa)}}
##k=4 set up
sam=sam1
K=4
mu1 <- c(sample(sam[,1],1),sample(sam[,2],1))
mu2 <- c(sample(sam[,1],1),sample(sam[,2],1))
mu3 <- c(sample(sam[,1],1),sample(sam[,2],1))
mu4 <- c(sample(sam[,1],1),sample(sam[,2],1))
mu_n<-rbind(mu1,mu2,mu3,mu4)
L4 <- matrix(0,nrow=t,ncol=2)
dist_n <-matrix(0,nrow=N,ncol=K)
a<-c()
aa<-c()
#update c
for (i in 1:t ){
  for (j in 1:K){dist_n[,j]<-dist(sam,mu_n[j,])}
  for (j in 1:K) {
    sam[which(apply(dist_n,1,min)==dist(sam,mu_n[j,])),i+2] <- j
    a[j]<-sum((sam[which(sam[i+2]==j),1]-mu_n[j,1])^2+(sam[which(sam[i+2]==j),2]-mu_n[j,2])^2)}  
  L4[i,1] <- sum(a)
  #update centroid
  for (j in 1:K) {
    mu_n[j,] <- c(mean(sam[,1][sam[,i+2]==j]),mean(sam[,2][sam[,i+2]==j]))
    aa[j]<-sum((sam[which(sam[i+2]==j),1]-mu_n[j,1])^2+(sam[which(sam[i+2]==j),2]-mu_n[j,2])^2)  
    L4[i,2] <- sum(aa)}}
##k=5 set up
sam=sam1
K=5
mu1 <- c(sample(sam[,1],1),sample(sam[,2],1))
mu2 <- c(sample(sam[,1],1),sample(sam[,2],1))
mu3 <- c(sample(sam[,1],1),sample(sam[,2],1))
mu4 <- c(sample(sam[,1],1),sample(sam[,2],1))
mu5 <- c(sample(sam[,1],1),sample(sam[,2],1))
mu_n<-rbind(mu1,mu2,mu3,mu4,mu5)
L5 <- matrix(0,nrow=t,ncol=2)
##Run below for each K set up above
dist_n <-matrix(0,nrow=N,ncol=K)
a<-c()
aa<-c()
#update c
for (i in 1:t ){
  for (j in 1:K){dist_n[,j]<-dist(sam,mu_n[j,])}
  for (j in 1:K) {
    sam[which(apply(dist_n,1,min)==dist(sam,mu_n[j,])),i+2] <- j
    a[j]<-sum((sam[which(sam[i+2]==j),1]-mu_n[j,1])^2+(sam[which(sam[i+2]==j),2]-mu_n[j,2])^2)}  
    L5[i,1] <- sum(a)
  #update centroid
  for (j in 1:K) {
    mu_n[j,] <- c(mean(sam[,1][sam[,i+2]==j]),mean(sam[,2][sam[,i+2]==j]))
    aa[j]<-sum((sam[which(sam[i+2]==j),1]-mu_n[j,1])^2+(sam[which(sam[i+2]==j),2]-mu_n[j,2])^2)  
    L5[i,2] <- sum(aa)}}
#Answer! 
library(ggplot2)
library(gglots)
L2_data <- data.frame(x=1:40, val=as.numeric(t(L2)), K=2) 
L3_data <- data.frame(x=1:40, val=as.numeric(t(L3)), K=3) 
L4_data <- data.frame(x=1:40, val=as.numeric(t(L4)), K=4) 
L5_data <- data.frame(x=1:40, val=as.numeric(t(L5)), K=5) 
L_data<-cbind(seq(0.5,20,0.5),as.numeric(t(L2)),as.numeric(t(L3)),as.numeric(t(L4)),as.numeric(t(L5)))
colnames(L_data)<-c("iteration","K2","K3","K4","K5")
L_data<-data.frame(L_data)
p1<-ggplot(data=L_data,aes(x=iteration)) + ggtitle("K2")+geom_line(aes(y=K2),colour="red")+ylab(label="objective function")
p2<-ggplot(data=L_data,aes(x=iteration)) + ggtitle("K3")+geom_line(aes(y=K3),colour="yellow")+ylab(label="objective function")
p3<-ggplot(data=L_data,aes(x=iteration)) + ggtitle("K4")+geom_line(aes(y=K4),colour="blue")+ylab(label="objective function")
p4<-ggplot(data=L_data,aes(x=iteration)) + ggtitle("K5")+geom_line(aes(y=K5),colour="green")+ylab(label="objective function")
library("grid")
library("gridExtra")
pp<-grid.arrange(p1,p2,p3,p4)

s<-sam[,1:2]
k5<-sam$V22
k3<-sam$V22
ppp<-cbind(s,k3,k5)
colnames(ppp)<-c("x","y","k3","k5")
ggplot(ppp,aes(x,y))+geom_point(aes(colour=factor(k3)))
#Prob2 (Matrix factorization)
##training dataset
ratings <- read.csv("~/Desktop/HW/ML/HW4/movies_csv/ratings.txt", header=FALSE)
ratings<-data.frame(ratings)
library(dplyr)
colnames(ratings) <-c("user_id","movie_id","rating")
by_mov<-group_by (ratings,movie_id)
v<-by_mov %>%
  arrange(movie_id)
cn_mov<-summarise(by_mov,count=n())
n_mov<-as.numeric(count(cn_mov))
by_usr <-group_by(ratings,user_id)
u<-by_usr %>%
  arrange(user_id)
cn_usr<-summarise(by_usr,count=n())
n_usr<-as.numeric(count(cn_usr))
m_usr<-max(ratings$user_id)
m_mov<-max(ratings$movie_id)
##test dataset
ratings_test <- read.csv("~/Desktop/HW/ML/HW4/movies_csv/ratings_test.txt", header=FALSE)
ratings_test<-data.frame(ratings_test)
colnames(ratings_test) <-c("user_id","movie_id","rating")
##initial
d<-20
sig_sqrt<-0.25
var<-10
T<-100
ui <- matrix(0,nrow=d,ncol=n_usr)
vj<-matrix(rnorm(as.numeric(d*m_mov),mean=0,sd=sqrt(0.1)),nrow=d)
rmse<-c()
log_like <-c()
##update usr location
for (t in 1:T){
  for (i in 1:n_usr){
    m_id<-ratings %>%
      filter (user_id==i)
    fz<-m_id$rating %*% t(vj[,m_id$movie_id])
    fm<-var*sig_sqrt*diag(d)+vj[,m_id$movie_id]%*% t(vj[,m_id$movie_id])
    ui[,i]<-solve(fm)%*%t(fz)}
##update mov location
  for (j in 1:m_mov){
    m_id<-ratings %>%
      filter (movie_id==j)
    fz<-m_id$rating %*% t(ui[,m_id$user_id])
    fm<-var*sig_sqrt*diag(d)+ui[,m_id$user_id]%*% t(ui[,m_id$user_id])
    vj[,j]<-solve(fm)%*%t(fz)}
  pred<-t(ui) %*% vj
##rmse
  train_result<-matrix(0,nrow=nrow(ratings_test))
  for (w in 1:nrow(ratings_test)){
    train_result[w]<-pred[ratings_test[w,1],ratings_test[w,2]]}
  train_result <-round(train_result)
  rmse[t]<-sqrt(mean((train_result-ratings_test$rating)^2))
##object function
  train_result_t<-matrix(0,nrow=nrow(ratings))
  for (ww in 1:nrow(ratings)){
    train_result_t[ww]<-pred[ratings[ww,1],ratings[ww,2]]}
  train_result_t <-round(train_result_t)
  log_like[t]<-(-(1/(2*sig_sqrt))*sum((train_result_t-ratings$rating)^2))-(10/2)*sum(rowSums(ui^2))-(10/2)*sum(colSums(vj^2))}
p_rmse<-data.frame(rmse)
iteration<-c(seq(1:100))
pl_rmse<-cbind(iteration,p_rmse)
ggplot(data=pl_rmse,aes(x=iteration,y=rmse)) + ggtitle("RMSE as function of iteration")+geom_line()
p_log<-data.frame(log_like)
pl_log<-cbind(iteration,p_log)
ggplot(data=pl_log,aes(x=iteration,y=log_like)) + ggtitle("log joint likelihood as function of iteration")+geom_line()

#2
temp<-sample(ncol(vj),3)
temp_d<-c()
for (z in 1:ncol(vj)){
  temp_d[z]<-sqrt(sum((vj[,z]-vj[,temp[3]])^2))}
temp_sort<-head(sort(temp_d),6)
target_movieid<-c()
for (i in 1:length(temp_sort)){
  target_movieid[i]<-which(temp_d==temp_sort[i])}
movies <- read.delim("~/Desktop/HW/ML/HW4/movies_csv/movies.txt", header=FALSE)
colnames(movies)<-c("movies")
movies[target_movieid,]

#3
##update c
K=30
test_t=1
mu_i<-matrix(rnorm(as.numeric(d*K,mean=0,sd=sqrt(var))),nrow=K)
for (tt in 1:test_t){
  temp_kk<-matrix(0,nrow=K,ncol=ncol(ui))
  c_target<-c()
  for (kk in 1:nrow(mu_i)){
    temp_kk[kk,]<-rowSums((t(ui)-mu_i[kk,])^2)}
  for (temp_k in 1:ncol(temp_kk)){
    c_target[temp_k]<-which(temp_kk[,temp_k]==min(temp_kk[,temp_k]))}
##update centroid
  for (kk in 1:K){
    mu_i[kk,]<-colSums(t(ui[,which(c_target==kk)]))/length(which(c_target==kk))
  }
}

temp_dot<-c()
temp_dot_sort<-c()
five_centroid<-mu_i[sample(ncol(mu_i),5),]##select 5 centroid
f_centroid<-five_centroid
target_movies<-matrix(0,nrow=nrow(f_centroid),ncol=10)
movie_cent<-matrix(0,nrow=nrow(f_centroid),ncol=10)
for (c_i in 1:nrow(f_centroid)){
  temp_dot<-f_centroid[c_i,] %*% vj
  temp_dot_sort<-head(sort(temp_dot,decreasing=TRUE), 10) ##10 movies with larget dot product
  for (s in 1:length(temp_dot_sort)){
    target_movies[c_i,s]<-which(temp_dot==temp_dot_sort[s])}}
for (c_i in 1:nrow(f_centroid)){
  movie_cent[c_i,]<-movies[target_movies[c_i,],]}##get name of movies











