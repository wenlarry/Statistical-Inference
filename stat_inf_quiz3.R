#Quiz 3
#1
x_bar<-1100
s<-30
n<-9
alpha<-0.05
ts<-qt(1-alpha/2,n-1)
round(x_bar+c(-1,1)*ts*s/sqrt(n) ) 

#2
x_bar<--2
n<-9
alpha<-0.05
ts<-qt(1-alpha/2,n-1) 
x_bar*sqrt(n)/ts 

#4
#t18.975 =2.1
#0.60 var for new and 0.68 var for old
sqrt(0.5*0.6+0.5*0.68)
# sqrt ans 0.8
# new hr=3;old hr=5;10 nights for old and new
(3-5)+c(-1,1)*2.1*(0.8* sqrt(0.1+0.1) ) 

#6
n1<-n2<-100
xbar1<-4
xbar2<-6
s1<-0.5
s2<-2
xbar2-xbar1+c(-1,1)*qnorm(0.975)*sqrt(s1^2/n1+s2^2/n2) 

#7
n_x<-9
n_y<-9
x_bar<--3
y_bar<-1
s_x<-1.5
s_y<-1.8
alpha<-0.1
sp_2<-((n_x-1)*s_x^2+(n_y-1)*s_y^2)/(n_x+n_y-2) 
sp<-sqrt(sp_2) 
ts<-qt(1-(alpha/2),n_x+n_y-2) 
(x_bar-y_bar)+c(-1,1)*ts*sp*(sqrt(1/n_x+1/n_y)) 

