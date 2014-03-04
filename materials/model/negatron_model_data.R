####Data for model comparison

#Use negatrondata2a (3 person contexts, between subjects) and negatrondata2b (4 person contexts, between subjects)

#Libraries:
library(bootstrap)

#Functions:
# for bootstrapping 95% confidence intervals
theta <- function(x,xdata) {mean(xdata[x])}
ci.low <- function(x) {
	quantile(bootstrap(1:length(x),1000,theta,x)$thetastar,.025)}
ci.high <- function(x) {
  quantile(bootstrap(1:length(x),1000,theta,x)$thetastar,.975)}

#######################Load in Study 2a data###################
data2a <- read.csv("data/negatronStudy2a.longdata.csv")
data2a $context.standard <- data2a $context.condition/3
data2a $version <- "Study 2a"

data2a $subid <- as.factor(data2a $subid)
##Remove people with less than 80% accuracy
data2a.propcorrect <- aggregate(correct~subid, data=data2a , mean)
data2a.reject <- data2a.propcorrect[data2a.propcorrect$correct < .8,]
for (i in data2a.reject$subid) {
  data2a<-data2a[data2a$subid != i,]
}
#Remove incorrect
data2a.c <- data2a[data2a$correct == 1,]
#LogRT
data2a.c$log.rt<-log(data2a.c$rt)
#trim outliers
data2a.lrt <- data2a.c$log.rt
data2a.ct <- data2a.c[data2a.lrt < mean(data2a.lrt) + 3*sd(data2a.lrt) & data2a.lrt > mean(data2a.lrt) - 3*sd(data2a.lrt),]

#Aggregate data
data2a.mss <- aggregate(rt~context.condition+context.standard+response+text.condition+subid+version, data=data2a.ct, mean)
data2a.ms <- aggregate(rt~context.condition+context.standard+response+text.condition+version, data=data2a.mss, mean)
data2a.ms$cih <- aggregate(rt~context.condition+context.standard+response+text.condition+version, data=data2a.mss, ci.high)$rt
data2a.ms$cil <- aggregate(rt~context.condition+context.standard+response+text.condition+version, data=data2a.mss, ci.low)$rt

data2a.ms$context.condition <- factor(data2a.ms$context.condition, levels = c(0,1,2,3), labels=c("0/3","1/3","2/3","3/3"))

#################Load in Study 2b data###############
data2b <- read.csv("data/negatronStudy2b.longdata.csv")
data2b$context.standard <- data2b$context.condition/4
data2b$version <- "Study 2b"

data2b$subid <- as.factor(data2b$subid)

##Remove people with less than 80% accuracy
data2b.propcorrect <- aggregate(correct~subid, data=data2b, mean)
data2b.reject <- data2b.propcorrect[data2b.propcorrect$correct < .8,]
for (i in data2b.reject$subid) {
	data2b<-data2b[data2b$subid != i,]
}
#Remove incorrect
data2b.c <- data2b[data2b$correct == 1,]
#LogRT
data2b.c$log.rt<-log(data2b.c$rt)
#trim outliers
data2b.lrt <- data2b.c$log.rt
data2b.ct <- data2b.c[data2b.lrt < mean(data2b.lrt) + 3*sd(data2b.lrt) & data2b.lrt > mean(data2b.lrt) - 3*sd(data2b.lrt),]

#Aggregate data
data2b.mss <- aggregate(rt~context.condition+context.standard+response+text.condition+subid+version, data=data2b.ct, mean)
data2b.ms <- aggregate(rt~context.condition+context.standard+response+text.condition+version, data=data2b.mss, mean)
data2b.ms$cih <- aggregate(rt~context.condition+context.standard+response+text.condition+version, data=data2b.mss, ci.high)$rt
data2b.ms$cil <- aggregate(rt~context.condition+context.standard+response+text.condition+version, data=data2b.mss, ci.low)$rt

data2b.ms$context.condition <- factor(data2b.ms$context.condition, levels = c(0,1,2,3,4), labels=c("0/4","1/4","2/4","3/4","4/4"))

##########rbind two data sets
data <- rbind(data2a.ms, data2b.ms)

names(data) <- c("context.label","context.type","truth.value","sentence.type","version","rt","cih","cil")

data$truth.value <- factor(data$truth.value, levels=c(TRUE, FALSE), labels=c("True","False"))
data$sentence.type <- factor(data$sentence.type, levels=c("plural","lexical"), labels=c("Positive","Negative"))



#Just data2a
data.2a <- data2a.ms
names(data.2a) <- c("context.label","context.type","truth.value","sentence.type","version","rt","cih","cil")

data.2a$truth.value <- factor(data.2a$truth.value, levels=c(TRUE, FALSE), labels=c("True","False"))
data.2a$sentence.type <- factor(data.2a$sentence.type, levels=c("plural","lexical"), labels=c("Positive","Negative"))

#Just data2b
data.2b <- data2b.ms
names(data.2b) <- c("context.label","context.type","truth.value","sentence.type","version","rt","cih","cil")

data.2b$truth.value <- factor(data.2b$truth.value, levels=c(TRUE, FALSE), labels=c("True","False"))
data.2b$sentence.type <- factor(data.2b$sentence.type, levels=c("plural","lexical"), labels=c("Positive","Negative"))




