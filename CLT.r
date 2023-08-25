
clt<-function(pop,n){
xbar<-c()
for(i in 1:1000){
samp<-sample(pop,n)
xbar<-c(xbar,mean(samp))
}
m<-round(mean(xbar),2)
s<-round(sd(xbar),2)
main=paste("n=",n,", mean=",m,", sd=",s,sep="")
hist(xbar,main=main)
}


par(mfrow=c(4,4))

pop<-rnorm(1000)
m=round(mean(pop),2)
s=round(sd(pop),2)
main=paste("Pop, Normal", ", mean=",m,", sd=",s,sep="")
hist(pop,main=main)
clt(pop,10)
clt(pop,30)
clt(pop,50)

pop<-runif(1000)
m=round(mean(pop),2)
s=round(sd(pop),2)
main=paste("Pop, Uniform", ", mean=",m,", sd=",s,sep="")
hist(pop,main=main)
clt(pop,10)
clt(pop,30)
clt(pop,50)

pop<-rexp(1000)
m=round(mean(pop),2)
s=round(sd(pop),2)
main=paste("Pop, Exponential", ", mean=",m,", sd=",s,sep="")
hist(pop,main=main)

clt(pop,10)
clt(pop,30)
clt(pop,50)

pop<-rbeta(1000,.5,.3)
m=round(mean(pop),2)
s=round(sd(pop),2)
main=paste("Pop, Beta", ", mean=",m,", sd=",s,sep="")
hist(pop,main=main)
clt(pop,10)
clt(pop,30)
clt(pop,50)
