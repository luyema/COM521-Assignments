setwd("~/Downloads/uwcom521-assignments-master/week_05")
dataset1<-read.delim("com521_population.tsv")
mean(dataset1$x)
setwd("~/downloads/uwcom521-assignments-master/week_03")
dataset2<-read.csv("week3_dataset-luyue.csv")
mean(dataset2$x)
(1.96*sd(dataset2$x)/sqrt(length(dataset2$x)))*c(-1,1)
mean(dataset2$x)+(1.96*sd(dataset2$x)/sqrt(length(dataset2$x)))
mean(dataset2$x)-(1.96*sd(dataset2$x)/sqrt(length(dataset2$x)))
hist(dataset2$x)
hist(dataset1$x)
summary(dataset2$x)
summary(dataset1$x)
mean(dataset1$y)
mean(dataset2$y)
mean(dataset2$y)+(1.96*sd(dataset2$y)/sqrt(length(dataset2$y)))
mean(dataset2$y)-(1.96*sd(dataset2$y)/sqrt(length(dataset2$y)))
new.vector<-runif(10000,min = 0,max = 9)
mean(new.vector)
hist(new.vector)
mean(sample(new.vector,2))
find.my.mean<-function(i){
  my.sample<-sample(new.vector,2)
  mean(my.sample)
}
new.vector2<-sapply(rep(1,100),find.my.mean)
hist(new.vector2)
find.my.mean.ten<-function(i){
  my.sample<-sample(new.vector,10)
  mean(my.sample)
}
find.my.mean.ten()  
new.vector3<-sapply(rep(1,100),find.my.mean.ten)
hist(new.vector3)
find.my.mean.hundred<-function(i){
  my.sample<-sample(new.vector,100)
  mean(my.sample)
}
find.my.mean.hundred()
new.vector4<-sapply(rep(1,100),find.my.mean.hundred)
hist(new.vector4)
pc5.vector<-rnorm(10000,mean = 42,sd=42)
mean(pc5.vector)
hist(pc5.vector)
find.my.mean.pc5<-function(i){
  my.sample<-sample(pc5.vector,2)
  mean(my.sample)
}
pc5.vector.two<-sapply(rep(1,100),find.my.mean.pc5)
hist(pc5.vector.two)
find.my.mean.pc5.ten<-function(i){
  my.sample<-sample(pc5.vector,10)
  mean(my.sample)
}
pc5.vector.ten<-sapply(rep(1,100),find.my.mean.pc5.ten)
hist(pc5.vector.ten)
find.my.mean.pc5.hundred<-function(i){
  my.sample<-sample(pc5.vector,100)
  mean(my.sample)
}
pc5.vector.hundred<-sapply(rep(1,100),find.my.mean.pc5.hundred)
hist(pc5.vector.hundred)
