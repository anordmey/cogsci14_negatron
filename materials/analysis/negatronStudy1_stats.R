rm(list=ls())

######Libraries######
library(lattice)
library(rjson)
library(plotrix)
library(lme4)
library(ggplot2)

######Functions######
ci95 <- function(x) {qt(.975,df=length(x)-1)*sd(x)/sqrt(length(x))}


######Data processing######
long.data <- read.csv("data/negatronStudy1.longdata.csv")

long.data$subid <- as.factor(long.data$subid)

#Remove people with less than 80% accuracy
propcorrect <- aggregate(correct~subid, data=long.data, mean)
reject <- propcorrect[propcorrect$correct < .8,]

for (i in reject$subid) {
	long.data<-long.data[long.data$subid != i,]
}

#Remove incorrect
long.data.c <- long.data[long.data$correct == 1,]

#LogRT
histogram(long.data.c$rt)
long.data.c$log.rt<-log(long.data.c$rt)
histogram(long.data.c$log.rt)

#Trim outliers
lrt <- long.data.c$log.rt
long.data.ct <- long.data.c[lrt < mean(lrt) + 3*sd(lrt) & lrt > mean(lrt) - 3*sd(lrt),]

histogram(long.data.ct$rt)
histogram(long.data.ct$log.rt)



######Plot data######
long.data.ct$text.condition <- factor(long.data.ct$text.condition,levels=c("plural","lexical"), labels=c("Positive","Negative"))
long.data.ct$response <- factor(long.data.ct$response,levels=c("TRUE","FALSE"))
long.data.ct$context.condition <- factor(long.data.ct$context.condition, levels = c("0","1"), labels = c("No Context", "Context"))

#add some style elements for ggplot2
plot.style <- theme_bw() + theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(), legend.position="right", axis.line = element_line(colour="black",size=.5), axis.ticks = element_line(size=.5), axis.title.x = element_text(vjust=-.5), axis.title.y = element_text(angle=90,vjust=0.25))

#Get subject means
ms <- aggregate(rt ~ text.condition + response + context.condition + subid,data=long.data.ct,mean)

#Get means & 95% confidence intervals
mss <- aggregate(rt ~ text.condition + context.condition + response,data=ms,mean)
errs <- aggregate(rt ~ text.condition + context.condition + response,data=ms,ci95)
limits <- aes(ymax = mss$rt + errs$rt, ymin = mss$rt - errs$rt)

#make line plot
quartz()
ggplot(mss, aes(colour=text.condition, y=rt, x=context.condition)) +
	 geom_point() + geom_line(aes(group=text.condition), size=.5) +
	 facet_wrap(~ response, nrow=1) +
	 geom_errorbar(limits, width=.25, size=.25) +
	 scale_y_continuous(name = "RT (ms)", breaks=seq(1000, 2000, 100)) + 
	 xlab("Context Condition") + 
	 scale_colour_grey("Sentence Type") + 
	 plot.style

######Analysis######
long.data.ct$response <- factor(long.data.ct$response,levels=c("FALSE","TRUE"))

model1 <- lmer(rt ~ text.condition*context.condition*response
       + (text.condition*response |subid)
       + (text.condition*response |item),
       data=long.data.ct)
       
