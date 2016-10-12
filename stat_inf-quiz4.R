#Quiz 4

#1
pharm<-data.frame(baseline=c(140,138,150,148,135),
                  wk2= c(132,135,151,146,130) )

t.test(pharm$baseline,pharm$wk2,paired=TRUE) 

#2 - see t confidence level
mu<-1100
n<-9
sd<-30
t<-qt(1-0.975,8) 
mu+c(-1,1)*t*sd/sqrt(n) 

#3 = see p value binomial
pbinom(2,size=4,prob=0.5,lower.tail=FALSE) 

# 4
1/100 #benchmark
lambda<-0.01*1787
ppois(10,lambda) 

#6
#0.95 interval wider than 0.90 interval
#1077 cc to 1123 cc
#1078 cc in 0.90 interval would also be in 0.95 imterval

#7
pnorm(1.645*0.04,mean=0.01,sd=0.04,lower.tail=FALSE) 

#7 use power.t.test
n<-100
mu<-0.01
sd<-0.04

power.t.test(n,delta=mu,sd=sd,type="one.sample",
             alt="one.sided")$power  

#8 use 'ceiling'
ceiling((4*(qnorm(0.95)-qnorm(0.1)))^2 ) 
 
#8 use power.t.test
power<-0.9

power.t.test(power=power,delta=mu,sd=sd,type="one.sample",
             alt="one.sided")$n 

#9 Power goes up as alpha gets larger

# 5 
n1<-n2<-9
x1<- -3 #treated 
x2<-1   #placebo
s1<-1.5 #treated sd
s2<-1.8 #placebo sd

sp<-sqrt(((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2)) 

md<-x1- x2

semd<-sp*sqrt(1/n1+1/n2)  

ts<-md/semd

pval<-2*pt(ts, n1+n2-2)  

pval

