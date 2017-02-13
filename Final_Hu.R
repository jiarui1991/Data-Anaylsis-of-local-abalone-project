
mydata<-read.table("c:/abalone.data",
   sep=",",header=F,col.names=c("SEX","LENGTH",
   "DIAMETER","HEIGHT","WHOLEWEIGHT","SHUCKEDWEIGHT","VISCERAWEIGHT",
   "SHELLWEIGHT","RINGS"))

typeof(mydata$LENGTH[mydata$LENGTH!="?"])
typeof(mydata$SEX[mydata$SEX!="?"])

mydata$SEX<-as.character(mydata$SEX)

save(mydata,file="c:/mydata.RData")

load("c:/mydata.RData")

###Analyze for category data
sex<-mydata$SEX

table(sex)

table(sex)/length(sex)

barplot(table(sex),
  col="cyan", ylim=c(0,1600),
  xlab="Sex", ylab="Frequency")

sexdata<-table(sex)
slice.labels<-names(sexdata)
slice.percents<-round(sexdata/sum(sexdata)*100)
slice.labels<-paste(slice.labels,slice.percents)
slice.labels<-paste(slice.labels,"%",sep="")
pie(sexdata,labels=slice.labels,
 col=hcl(c(0,60,120)))

###Analyze for numeric data
rings<-mydata$RINGS
mean(rings)
median(rings)
table(rings)
which(table(rings)==max(table(rings)))
range(rings)
diff(range(rings))
var(rings)
sd(rings)
summary(rings)

barplot(table(rings),
  col="cyan",ylim=c(0,700),
  xlab="Age", ylab="Frequency")

boxplot(rings,horizontal=TRUE,xaxt="n")
axis(side=1,at=fivenum(rings),labels=TRUE,las=2)

####Exam Distribution
height<-mydata$HEIGHT
mean(height)
table(height)
sd(height)

barplot(table(height),
  col="cyan",ylim=c(0,300),
  xlab="Height", ylab="Frequency")

####Draw sample

rings<-mydata$RINGS

samplemean<-mean(rings)

samplesd<-sd(rings)

set.seed(123)

par(mfrow = c(1,2))

sample<-rnorm(30,mean=samplemean,sd=samplesd)

hist(sample,prob=TRUE,
 xlim=c(0,20),ylim=c(0,0.2), main = paste("Sample Size =", 30))

sample<-rnorm(1000,mean=samplemean,sd=samplesd)

hist(sample,prob=TRUE,
 xlim=c(0,20),ylim=c(0,0.2), main = paste("Sample Size =", 1000))


####Applicability of Central Limti Theorem 

samples<-4000

xbar<-numeric(samples)

par(mfrow = c(2,2))


for (ssize in c(10, 20, 30, 40)) {
	for (i in 1:samples) {
	  xbar[i] <- mean(rnorm(ssize, 
	                  mean = samplemean, sd = samplesd))
    }

  hist(xbar, prob = TRUE, 
    xlim=c(5,15), ylim = c(0, 0.8),
    main = paste("Sample Size =", ssize))
   
  cat("Sample Size = ", ssize, " Mean = ", mean(xbar),
        " SD = ", sd(xbar), "\n")
}

####Sampling methods
library(sampling)

###Simpl Random Sample

s<-srswor(200,nrow(mydata))
rows<-(1:nrow(mydata))[s!=0]
rows<-rep(rows,s[s!=0])
rows
sample.1<-mydata[rows,]
head(sample.1,10)

###Systematic Sample

N<-nrow(mydata)
n<-100

k<-ceiling(N/n)
k

r<-sample(k,1)
r

s<-seq(r,by=k,length=n)
sample.2<-mydata[s,]
head(sample.2,10)
 
###Stratrified Sampling

sex<-mydata$SEX
table(sex)

st.1<-strata(mydata,stratanames=c("SEX"),
 size=rep(7,3),method="srswor",
 description=TRUE)
  
st.1

sample.3<-getdata(mydata,st.1)

sample.3

### Cluster Sampling
table(sex)
cl<-cluster(mydata,c("SEX"),
  size=2,method="srswor")

sample.4<-getdata(mydata,cl)
table(sample.4$SEX)

####Confidence intervals.

###CI for sample.1$LENGTH

conf<-c(80,90)
alpha<-1-conf/100
alpha

pop.mean<-mean(mydata$LENGTH)
pop.sd<-sd(mydata$LENGTH)

sample.size<-nrow(sample.1)
sd.sample.means<-pop.sd/sqrt(sample.size)
sd.sample.means

xbar<-mean(sample.1$LENGTH[!is.na(sample.1$LENGTH)])
xbar

for(i in alpha){
  str<-sprintf("%2d%% Conf Level (alpha=%.2f), CI=%.3f-%.3f",
   100*(1-i),i,
   xbar-qnorm(1-i/2)*sd.sample.means,
   xbar+qnorm(1-i/2)*sd.sample.means)
 cat(str,"\n")
}

pop.mean

###CI for sample.2$LENGTH

sample.size<-nrow(sample.2)
sd.sample.means<-pop.sd/sqrt(sample.size)
sd.sample.means

xbar<-mean(sample.2$LENGTH[!is.na(sample.2$LENGTH)])
xbar

for(i in alpha){
  str<-sprintf("%2d%% Conf Level (alpha=%.2f), CI=%.3f-%.3f",
   100*(1-i),i,
   xbar-qnorm(1-i/2)*sd.sample.means,
   xbar+qnorm(1-i/2)*sd.sample.means)
 cat(str,"\n")
}

pop.mean

###CI for sample.3$LENGTH

sample.size<-nrow(sample.3)
sd.sample.means<-pop.sd/sqrt(sample.size)
sd.sample.means

xbar<-mean(sample.3$LENGTH[!is.na(sample.3$LENGTH)])
xbar

for(i in alpha){
  str<-sprintf("%2d%% Conf Level (alpha=%.2f), CI=%.3f-%.3f",
   100*(1-i),i,
   xbar-qnorm(1-i/2)*sd.sample.means,
   xbar+qnorm(1-i/2)*sd.sample.means)
 cat(str,"\n")
}

pop.mean 

###CI for sample.4$LENGTH

sample.size<-nrow(sample.4)
sd.sample.means<-pop.sd/sqrt(sample.size)
sd.sample.means

xbar<-mean(sample.4$LENGTH[!is.na(sample.4$LENGTH)])
xbar

for(i in alpha){
  str<-sprintf("%2d%% Conf Level (alpha=%.2f), CI=%.3f-%.3f",
   100*(1-i),i,
   xbar-qnorm(1-i/2)*sd.sample.means,
   xbar+qnorm(1-i/2)*sd.sample.means)
 cat(str,"\n")
}

pop.mean



