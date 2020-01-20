#¼ÒÀÀ¥Ñ¨Ï¥ÎªÌ¦Û­q¤ñ¨Ò»Pµ¹©w¼Ë¥»¼Æªº«H¿à°Ï¶¡
Prop_CI <- function (p,n,q1,q2) {
  v <- c()
  
  for (i in c(1:1000)) {
    temp <- sample(c(0:1),size = n,replace = T,prob = c(1-p,p))
    temp <- mean(temp)
    v[i] <- temp
  }
  
  v <- sort(v)
  qq1 <- v[1000*q1]
  qq2 <- v[1000*q2]
  print(qq1)
  print(qq2)
}

Prop_CI(0.36,883,0.025,0.975)


#Density: dt(x,df,log = FALSE)


#ƒÞDistribution function: pt(q,df,lower.tail = TRUE)


#ƒÞQuantile function: qt(p,df,lower.tail = TRUE)


#ƒÞRandom generation: rt(n,df)


#6.1 Plotting Probability Distributions
curve(dt(x,df = 30),from = -3,to = 3,lwd = 3,ylab = "y") #lwd=½uªº²Ê²Ó

ind <-c(1,2,3,5,10)
for (i in ind) {curve(dt(x,df = i),-3, 3,add = TRUE)}

curve(dnorm(x),-3,3,col="red",lwd = 2,add = TRUE)


#ƒÞ6.2 Given a Prob. (with df), Find the t-value.
qt(0.01,df= 10,lower.tail= FALSE)
qt(0.01,df= 200,lower.tail= FALSE)
qnorm(0.01,lower.tail= FALSE)


#ƒÞ6.3 Given a t-value (with df), Find the Prob.
pt(2,10,lower.tail= FALSE)
pnorm(2,lower.tail= FALSE)


#ƒÞ6.4 Calculating Confidence Interval (CI)
x <- rnorm(20)
cc <- t.test(x,conf.level=0.95)
CI_Low <- cc$conf.int[1]
CI_High <- cc$conf.int[2]


#ƒÞ6.5 Simulating t-Confidence Interval for a Population Mean (mu)
Sim_CI <- function (n,m,conf.level){ #n:sample size¡F m:trials
  prob <- (1-conf.level)/2
  t.value <- qt(prob,df = n-1,lower.tail = FALSE)

  mean.x <- c()
  se.x <- c()
  CI.half <- c()
  CI.Up <- c()
  CI.Lower <- c()
  Check <- c()
    
  for (i in 1:m) {
    x <- rnorm(n) #mu=0
    mean.x[i] <- mean(x)
    se.x[i] <- sd(x)/sqrt(n)
    CI.half[i] <- t.value*se.x[i]
    CI.Up[i] <- mean.x[i]+CI.half
    CI.Lower[i] <- mean.x[i]-CI.half
    if (CI.Up[i]>0 & CI.Lower[i]<0) Check[i] <- 1 else Check[i] <- 0
  }
  
  plot(c(CI.Up,CI.Lower),type="n",xlim=c(1,m),xlab="Trial",ylab=expression(mu),pch=19) #pch=ÂIªº¼Ë¦¡
  abline(h = 0,col = "blue")
  points(CI.Up,col = "green",pch = 20)
  points(mean.x,col = "green",pch = 10)
  points(CI.Lower,col = "green",pch = 20)
  
  for(i in 1:m) {
    if (Check[i]==1) lines(c(i,i),c(CI.Lower[i],CI.Up[i]),col = "green",pch = 19)
    else lines(c(i,i),c(CI.Lower[i],CI.Up[i]),col = "red",pch = 19)
  }
  
  title(expression(paste("Simulating t-CONFIDENCE INTERVALS for ", mu)))
  legend(0,-.85,legend = c(expression(paste(mu," Captured")),expression(paste(mu," Not Captured"))),fill = c(3,2))
  No.Caputured <- m-sum(Check)
  RESULT <- list(Trial=m,Sample.Size=n,Confidence=conf.level,No.Caputured=No.Caputured)
  return(RESULT)
}


#³æ¤@¥ÀÅé¤ñ¨Òªº°²³]ÀË©w»Pp­È­pºâ
z_0.01<-qnorm(0.01)

load("UCBAdmissions.Rdata")
head(A)

x <- xtabs(Freq ~ Admit,data = A)
Suc <- x[1]
Rej <- x[2]
p_hat <- Suc/ sum(x)
z_score <- (p_hat-0.4)/sqrt(0.4*0.6/sum(x))
if (abs(z_score) > abs(z_0.01)) CHECK <- "reject NULL" else CHECK <- "can NOT reject NULL"
p.value <- pnorm(z_score)


#¤ñ¸û¨â­Ó¥ÀÅé¤ñ¨Òªº°²³]ÀË©w»Pp­È­pºâ
x2 <- xtabs(Freq ~ Gender+Admit,data = A)

Total_Male <- sum(x2[1,])
Total_Female <- sum(x2[2,])
Male_Rate <- x2[1,1]/Total_Male
Female_Rate <- x2[2,1]/Total_Female
Total_Rate <- (x2[1,1]+x2[2,1])/sum(x2)
se2 <- sqrt(Total_Rate*(1-Total_Rate)*(1/Total_Male+1/Total_Female))
z2_score <- (Male_Rate-Female_Rate)/se2
z2_0.005 <- qnorm(0.995)

if (abs(z2_score) > abs(z2_0.01)) CHECK2 <- "reject NULL" else CHECK2 <- "can NOT reject NULL"
p.value2 <- 2(1-pnorm(abs(z2_score)))


#µez*
z = qnorm(0.8)
z = round(z,2)

curve(dnorm(x, mean=0, sd=1),from=-4,to=4,add=FALSE,lwd=2,main="z* for 60%CI",ylab="Probability density",xlab="z")
lines(c(z,z),c(-1,dnorm(z)),col = "red",pch = 19)
lines(c(-z,-z),c(-1,dnorm(-z)),col = "red",pch = 19)
axis(side = 1,at = c(-z,z))


#bar chart
c1 <- c(0.32)
c2 <- c(0.18)
data <- data.frame(row.names=c("Have heart disease"),"Police"=c1,"Others"=c2)
data <- as.matrix(data)

barplot(data,col=c("red","blue"),main="Comparisons of heart disease",xlab="Occupation",ylab="Proportion",beside=T)


#¼ÒÀÀ¥Ñ¨Ï¥ÎªÌ¦Û­q¨â­Ó¼Ë¥»¤ñ¨Ò»Pµ¹©w¼Ë¥»¼Æªº«H¿à°Ï¶¡¡A¨ÃÃ¸»s¨â¼Ë¥»ªº¤ñ¨Ò®tªºª½¤è¹Ï
Prop_CI_Diff=function(p1,n1,p2,n2,q1,q2){
  a=c()
  
  for(i in 1:1000) {a[i]=mean(sample(c(0,1),n1,replace=T,c(1-p1,p1)))-mean(sample(c(0,1),n2,replace=T,c(1-p2,p2)))}
  
  hist(a,breaks=20,main="Histogram of md1-md2",xlab="md1-md2")
  
  a <- sort(a)
  LCL <- a[ceiling(q1*1000)]
  UCL <- a[ceiling(q2*1000)]
  
  list(CI=matrix(c(paste(100*q1,"%"),paste(100*q2,"%"),LCL,UCL),nrow=2,byrow=T),mean=mean(a),se=sd(a))
}
Prop_CI_Diff(0.351,262,0.093,632,0.025,0.975)


#¥Ñ¨Ï¥ÎªÌ¦Û­q¨â­Ó¼Ë¥»¤ñ¨Ò¡A¤ñ¸û¦b¬Y­Ó«H¤ß¤ô·Ç¤U¡A¨â­Ó¼Ë¥»¤ñ¨Ò¬O§_¦³®t²§
Prop_CI_Diff2=function(p1,n1,p2,n2,conf){
  a=c()
  b=c()
  
  for(i in 1:1000){
    a[i]=mean(sample(c(0,1),n1,replace=T,c(1-p1,p1)))
    b[i]=mean(sample(c(0,1),n2,replace=T,c(1-p2,p2)))
  }
  
  a=sort(a)
  b=sort(b)
  
  a_LCL=a[ceiling((100-conf)/2*10)]
  a_UCL=a[ceiling((100+conf)/2*10)]
  
  b_LCL=b[ceiling((100-conf)/2*10)]
  b_UCL=b[ceiling((100+conf)/2*10)]
  
  text=""
  if(any(a_LCL<b_LCL & a_UCL<b_LCL,b_LCL<a_LCL & b_UCL<a_LCL)) {text="¦³ÅãµÛ®t²§"} else {text="¨S¦³ÅãµÛ®t²§"}
  
  list(Check=text)
}

Prop_CI_Diff2(0.118,160,0.213,244,95)


#Comparsion of the two confidence interval
plot(c(0,3),type='n',xlim=c(0,10),xlab='Mean days',ylab='',axes=FALSE,main='Comparsion of the two confidence interval')

lines(c(3.8395523,5.1604477),c(2,2))
text(2.5,2,labels='zinc')
lines(c(3.8395523,3.8395523),c(1.95,2.05))
lines(c(4.5,4.5),c(1.95,2.05))
lines(c(5.1604477,5.1604477),c(1.95,2.05))

lines(c(7.3216217,8.8783783),c(1,1))
text(6,1,labels='placebo')
lines(c(7.3216217,7.3216217),c(0.95,1.05))
lines(c(8.1,8.1),c(0.95,1.05))
lines(c(8.8783783,8.8783783),c(0.95,1.05))

axis(1)


#The confidence interval
plot(c(0,1),type='n',xlim=c(-2,10),xlab='Mean difference in the weight of baby',ylab='',axes=FALSE,main='The confidence interval')
lines(c(3.1794802,6.8205198),c(0.5,0.5))
arrows(x0=0,y0=0.8,x1=0,y1=0.1)
text(0,1,label='0 mean difference')
lines(c(3.1794802,3.1794802),c(0.45,0.55))
lines(c(5,5),c(0.45,0.55))
lines(c(6.8205198,6.8205198),c(0.45,0.55))

axis(1,seq(-2,10,by=1))