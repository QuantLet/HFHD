rm(list=ls(all=T))
library(inline)
library(Rcpp)


src<-'
using namespace std;
using namespace Rcpp;

     vector<double> ts1=as<vector<double> >(ts0);

     double freq=as<double> (f1);
     int l1= ceil((16-9.5)*60*60/freq);
     
     NumericVector b1(l1);
     for (int i=0;i<l1;i++) b1[i]=9.5*60*60+freq*(i+1);
     NumericVector c1(l1);
     vector<double>::iterator up;
     for (int i=0;i<l1;i++)
     {
            up=upper_bound(ts1.begin(),ts1.end(),b1[i]);
            c1[i] =  distance(ts1.begin(),up);
            ts1.erase(ts1.begin(),ts1.begin()+c1[i]);
     }

     return c1;
'

fn1<-cxxfunction(signature(ts0="numeric",f1="numeric"),src,plugin="Rcpp")

#this function calculates the number of limit order book within each time interval
#the length of time interval can be set by the user
#


fs0<-dir(pattern="*.csv")
fs1<-sort(fs0,decreasing=F)
dm<-read.csv(fs1[1],colClasses=c("numeric","integer",rep("NULL",4)),head=F)
dr<-read.csv(fs1[2],colClasses=c(rep(c("numeric","integer"),times=2),rep("NULL",16)),head=F)
data0<-cbind(dm,dr)
# I read the csv file in the folder into R

data0<-data0[data0[,2]!=0&data0[,4]!=0,]
data0<-data0[data0[,2]!=6,]
data0<-cbind(data0,(data0[,3]+data0[,5])/2)
colnames(data0)<-c("time","type","ask","asize","bid","bsize","mid")
data0[,c(3,5,7)]<-sapply(data0[,c(3,5,7)],as.integer)
f1<-1#frequency you want, in second
ts1<-fn1(data0$time,f1)
#ts1 contains the number of limit order book in each interval
#with this result, you can clean the data as you want.




