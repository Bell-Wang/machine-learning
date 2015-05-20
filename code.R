library(ggplot2)
library(reshape)
#Prob1
football <- read.csv("~/Desktop/HW/ML/HW5/hw5text/cfb2014scores.csv", header=FALSE)
names(football)<-c("team1","team1_score","team2","team2_score")
legend <- read.table("~/Desktop/HW/ML/HW5/hw5text/legend.txt", quote="\"")
C=759
R=759
rand_walk<-matrix(0,ncol=C,nrow=R)
for (i in 1:nrow(football)){
  if (football[i,2]>football[i,4]){
    rand_walk[football[i,1],football[i,1]]<-rand_walk[football[i,1],football[i,1]]+1+(football[i,2]/(football[i,2]+football[i,4]))
    rand_walk[football[i,3],football[i,1]]<-rand_walk[football[i,3],football[i,1]]+1+(football[i,2]/(football[i,2]+football[i,4]))
  }else {
    rand_walk[football[i,3],football[i,3]]<-rand_walk[football[i,3],football[i,3]]+1+(football[i,4]/(football[i,2]+football[i,4]))
    rand_walk[football[i,1],football[i,3]]<-rand_walk[football[i,1],football[i,3]]+1+(football[i,4]/(football[i,2]+football[i,4]))
  }
}

#normalization
norm_row<-rowSums(rand_walk)
rand_walk_nm<-rand_walk/norm_row

#initialize w
w0<-rep(1/C,C)
#set T=10,100,200,1000
w<-w0
top_score=c()
top_team=c()
T=1000
for (t in 1:T){
  w<-w %*% rand_walk_nm}
  top_score<-sort(w,decreasing=TRUE)[1:20]

for (i in 1:20){
  top_team[i]<-which(w==top_score[i])}
team_name<-legend[top_team,1]
result<-cbind(top_score,as.character(team_name))

eig<-eigen(t(rand_walk_nm))
eig_vector1<-eig$vector[,1]
eig_v<-eig$vectors[eig$values==1][1:759]/sum(eig$vectors[eig$values==1][1:759])

w<-w0
w_v<-c()
T=1000
for (t in 1:T){
  w<-w %*% rand_walk_nm
  w_v[t]<-sum(abs(w-eig_vector1))}
plot_data<-melt(w_v)
qplot(y=value,x=seq(1:T),data=plot_data,geom='line')

#Prob2
faces <- read.csv("~/Desktop/HW/ML/HW5/hw5text/faces.csv", header=FALSE)
r<-1024
c<-1000
k<-25

W<-matrix(runif(r*k),nrow=r,ncol=k)
H<-matrix(runif(k*c),nrow=k,ncol=c)
X<-as.matrix(faces)
obj<-c()
T2=200
for (t in 1:T2){
  H<-H * (t(W) %*% X) / (t(W) %*% W %*% H)
  W<-W * (X %*% t(H) / (W %*% H %*% t(H)))
obj[t]<-sqrt(sum((X-W %*% H)^2))}

plot_data1<-melt(obj)
qplot(y=value,x=seq(1:T2),data=plot_data1,geom='line')

th_image<-sample(ncol(X),3)
org<-X[,th_image]
h<-H[,th_image]
index_max<-c()
for (i in 1:3){
  index_max[i]<-which(h[,i]==apply(h,2,max)[i])}
result2<-W[,index_max]#return 3 column data
org
library(pixmap)

face_plot<-pixmapGrey(data=result2[,3], nrow=32, ncol=32,
       bbox=NULL, bbcent=FALSE, cellres=NULL)

face_plot<-pixmapIndexed(org[,1], nrow=32, col=rainbow)
plot(face_plot)
#Prob3
nyt_data <- read.table("~/Desktop/HW/ML/HW5/hw5text/nyt_data.txt", header=FALSE,na.strings=c("", "NA"))
nytvocab <- read.table("~/Downloads/hw5text/nytvocab.dat", quote="\"")
library(stringr)
r_p2<-3012
c_p2<-8447
k<-25

X_p2<-matrix(0,nrow=r_p2,ncol=c_p2)
#Clean data split by "," and then split by ":"
nyt_data<-data.frame(nyt_data,stringsAsFactors=FALSE)
for (i in 1:nrow(nyt_data)){
  clean_1<-unlist(strsplit(as.character(nyt_data[i,]),","))
  clean_2<-strsplit(as.character(clean_1),":")
#get key and value
  key<-as.numeric(lapply(clean_2,'[',1))
  value<-as.numeric(lapply(clean_2,'[',2))
  key_sort<-data.frame(sort(key,index=TRUE))
#construct matrix
  X_p2[key_sort$x,i] <-key_sort$ix}

W_p2<-matrix(runif(r_p2*k),nrow=r_p2,ncol=k)
H_p2<-matrix(runif(k*c_p2),nrow=k,ncol=c_p2)
obj_p2<-c()
T2=50
for (t in 1:T2){
  H_p2<-H_p2 * ((t(W_p2)/rowSums(t(W_p2))) %*% (X_p2/(W_p2 %*% H_p2+0.001)))
  W_p2<-W_p2 * ((X_p2/(W_p2 %*% H_p2)+0.001) %*% t(t(t(H_p2)/colSums(t(H_p2)))))
  obj_p2[t]<-sum(X_p2*log(1/(W_p2 %*% H_p2))+W_p2%*%H_p2)}

plot_data2<-melt(obj_p2)
qplot(y=value,x=seq(1:T2),data=plot_data2,geom='line')

#normalization
norm_col<-colSums(W_p2)
W_nm<-t(t(W_p2)/norm_col)

samp_topic<-sample(ncol(W_nm),5)
topic<-W_nm[,samp_topic]

topic_sort<-matrix(0,nrow=nrow(topic),ncol=5)
for (i in 1:5){
  topic_sort[,i]<-sort(topic[,1],decreasing=TRUE,index=TRUE)}

index_sort<-matrix(0,nrow=nrow(topic),ncol=5)
for (i in 1:5){
  index_sort[,i]<-sort(topic[,i],decreasing=TRUE,index=TRUE)$ix}

result3<-topic_sort[1:10,]
result3_index<-as.numeric(index_sort[1:10,])
word<-as.matrix(nytvocab[result3_index,1])

result3_fin<-cbind(result3[,1],word[1:10])
for (i in 1:5){
word_name[,i]<-}
result<-cbind(top_score,as.character(team_name))




