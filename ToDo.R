#3.1
(2015-2014)/(2014-1997)*100

#3.2
a=2014
b=2015
c=1997

(b-a)/(a-c)*100

rm(list=ls(all=TRUE))

#3.4
a=c(4,5,8,11)
sum(a)

#3.5
x=rnorm(100)
plot(x)

#4
help(sqrt)

#5
source("firstscript.R")

#6.2
P=seq(from=31, to=60)
Q=matrix(data=P,ncol=5)

#6.3
rm(list=ls(all=TRUE))
x1=rnorm(100)
x2=rnorm(100)
x3=rnorm(100)
t=data.frame(a=x1,b=x1+x2,c=x1+x2+x3)
plot(t)
sd(t)

#7
plot(t$a, type="l", ylim=range(t),lwd=3, col=rgb(1,0,0,0.3))
lines(t$b, type="s", lwd=2,col=rgb(0.3,0.4,0.3,0.9))
points(t$c, pch=20, cex=4,col=rgb(0,0,1,0.3))

#8
rm(list=ls(all=TRUE))
d=data.frame(a=c(3,4,5),b=c(12,43,54))
write.table(d,file="tst1.txt",row.names=FALSE)
d2=read.table(file="tst1.txt",header=TRUE)

rm(list=ls(all=TRUE))
d3=data.frame(a=c(1,2,4,8,16,32),g=c(2,4,8,16,32,64),x=c(3,6,12,24,48,96))
write.table(d3,file="tst2.txt",row.names=FALSE)
d4=read.table(file="tst2.txt",header=TRUE)

#9
rm(list=ls(all=TRUE))
a=rnorm(100)
mean(sqrt(a))

#10.2
data1=strptime(c(20160921,20140519,20170915),format="%Y%m%d")
data2=c(0,10,10)
plot(data1,data2)

#11.2
h=seq(from=1,to=100)
s=c()
for(i in 1:4) {s[i]=h[i]*10}
for(j in 91:100) {s[j]=h[j]*10}
for(k in 5:90) {s[k]=h[k]*0.1}

#11.3
h=seq(from=1,to=100)
s=c()
fun1=function(h) for (i in 1:length(h))
  {if(h[i]<5) {s[i]=h[i]*10
  } else if (h[i]>=5 & h[i]<=90) {s[i]=h[i]*0.1
  } else if (h[i]>90 & h[i]<=100) {s[i]=h[i]*10
  }
  return(s)
  }
fun1(h=19)


























