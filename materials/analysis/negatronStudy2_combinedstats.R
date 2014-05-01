rm(list=ls())

######Libraries######
library(lattice)
library(rjson)
library(plotrix)
library(lme4)
library(ggplot2)

######Functions######
ci95 <- function(x) {qt(.975,df=length(x)-1)*sd(x)/sqrt(length(x))}


##################STUDY 2A (3-person contexts)##################

######Data Processing (2a)######

long.data.2a <-  read.csv("data/negatronStudy2a.longdata.csv")
long.data.2a$subid <- as.factor(long.data.2a$subid)

#Remove people with less than 80% accuracy
propcorrect <- aggregate(correct~subid, data=long.data.2a, mean)
reject <- propcorrect[propcorrect$correct < .8,]

for (i in reject$subid) {
	long.data.2a<-long.data.2a[long.data.2a$subid != i,]
}

#Remove incorrect
long.data.2a.c <- long.data.2a[long.data.2a$correct == 1,]

#LogRT
histogram(long.data.2a.c$rt)
long.data.2a.c$log.rt<-log(long.data.2a.c$rt)
histogram(long.data.2a.c$log.rt)

#trim outliers
lrt <- long.data.2a.c$log.rt
long.data.2a.ct <- long.data.2a.c[lrt < mean(lrt) + 3*sd(lrt) & lrt > mean(lrt) - 3*sd(lrt),]

#Create continuous context variable (for combined analyses)
long.data.2a.ct$context.standard <- long.data.2a.ct$context.condition/3


######Plot Data (2a)######
long.data.2a.ct$text.condition <- factor(long.data.2a.ct$text.condition,levels=c("plural","lexical"), labels=c("Positive","Negative"))
long.data.2a.ct$response <- factor(long.data.2a.ct$response,levels=c("TRUE","FALSE"))
long.data.2a.ct$context.condition <- factor(long.data.2a.ct$context.condition, levels = c("0","1","2","3"), labels = c("0/3", "1/3","2/3","3/3"))

#add some style elements for ggplot2
plot.style <- theme_bw() + theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(), legend.position="right", axis.line = element_line(colour="black",size=.5), axis.ticks = element_line(size=.5), axis.title.x = element_text(vjust=-.5), axis.title.y = element_text(angle=90,vjust=0.25))

#Get subject means
ms.2a <- aggregate(rt ~ text.condition + response + context.condition + subid,data=long.data.2a.ct,mean)

#Get means & 95% confidence intervals
mss.2a <- aggregate(rt ~ text.condition + context.condition + response,data=ms.2a,mean)
errs.2a <- aggregate(rt ~ text.condition + context.condition + response,data=ms.2a,ci95)
limits.2a <- aes(ymax = mss.2a$rt + errs.2a$rt, ymin = mss.2a$rt - errs.2a$rt)

#make line plot
#quartz()
ggplot(mss.2a, aes(colour=text.condition, y=rt, x=context.condition)) +
  geom_point() + geom_line(aes(group=text.condition), size=.5) +
  facet_wrap(~ response, nrow=1) +
  geom_errorbar(limits.2a, width=.25, size=.25) +
  scale_y_continuous(name = "RT (ms)", breaks=seq(1000, 2000, 100)) + 
  xlab("Context Condition") + 
  scale_colour_grey("Sentence Type") + 
  plot.style


##################STUDY 2B (4-person contexts)##################

######Data Processing (2b)######

long.data.2b <-  read.csv("data/negatronStudy2b.longdata.csv")
long.data.2b$subid <- as.factor(long.data.2b$subid)

#Remove people with less than 80% accuracy
propcorrect <- aggregate(correct~subid, data=long.data.2b, mean)
reject <- propcorrect[propcorrect$correct < .8,]

for (i in reject$subid) {
	long.data.2b<-long.data.2b[long.data.2b$subid != i,]
}

#Remove incorrect
long.data.2b.c <- long.data.2b[long.data.2b$correct == 1,]

#LogRT
histogram(long.data.2b.c$rt)
long.data.2b.c$log.rt<-log(long.data.2b.c$rt)
histogram(long.data.2b.c$log.rt)

#trim outliers
lrt <- long.data.2b.c$log.rt
long.data.2b.ct <- long.data.2b.c[lrt < mean(lrt) + 3*sd(lrt) & lrt > mean(lrt) - 3*sd(lrt),]

#Create continuous context variable (for combined analyses)
long.data.2b.ct$context.standard <- long.data.2b.ct$context.condition/4

######Plot Data (2b)######
long.data.2b.ct$text.condition <- factor(long.data.2b.ct$text.condition,levels=c("plural","lexical"), labels=c("Positive","Negative"))
long.data.2b.ct$response <- factor(long.data.2b.ct$response,levels=c("TRUE","FALSE"))
long.data.2b.ct$context.condition <- factor(long.data.2b.ct$context.condition, levels = c("0","1","2","3","4"), labels = c("0/4", "1/4","2/4","3/4","4/4"))

#add some style elements for ggplot2
plot.style <- theme_bw() + theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(), legend.position="right", axis.line = element_line(colour="black",size=.5), axis.ticks = element_line(size=.5), axis.title.x = element_text(vjust=-.5), axis.title.y = element_text(angle=90,vjust=0.25))

#Get subject means
ms.2b <- aggregate(rt ~ text.condition + response + context.condition + subid,data=long.data.2b.ct,mean)

#Get means & 95% confidence intervals
mss.2b <- aggregate(rt ~ text.condition + context.condition + response,data=ms.2b,mean)
errs.2b <- aggregate(rt ~ text.condition + context.condition + response,data=ms.2b,ci95)
limits.2b <- aes(ymax = mss.2b$rt + errs.2b$rt, ymin = mss.2b$rt - errs.2b$rt)

#make line plot
#quartz()
ggplot(mss.2b, aes(colour=text.condition, y=rt, x=context.condition)) +
  geom_point() + geom_line(aes(group=text.condition), size=.5) +
  facet_wrap(~ response, nrow=1) +
  geom_errorbar(limits.2b, width=.25, size=.25) +
  scale_y_continuous(name = "RT (ms)", breaks=seq(1000, 2000, 100)) + 
  xlab("Context Condition") + 
  scale_colour_grey("Sentence Type") + 
  plot.style



##################STUDY 2 combined##################

######Combine data from 2a and 2b######
long.data.2a.ct$version <- "Study 2a"
long.data.2b.ct$version <- "Study 2b"
long.data.ct <- rbind(long.data.2a.ct,long.data.2b.ct)


######Plot Data (combined 2a and 2b)######
long.data.ct$version <- factor(long.data.ct$version, levels=c("Study 2a","Study 2b"))

ms <- aggregate(rt ~ text.condition + response + context.standard + subid + version,data=long.data.ct,mean)

errs <- aggregate(rt ~ text.condition + response + context.standard + version,data=ms,ci95)
mss <- aggregate(rt ~ text.condition + response + context.standard + version,data=ms,mean)
mss$errs <- errs$rt

ggplot(mss, aes(y=rt, x=context.standard)) + 
	geom_pointrange(aes(x=context.standard,y=rt, color=text.condition, ymin=rt-errs,ymax=rt+errs), position=position_dodge(.05)) + 
	facet_wrap(~ response) +
	stat_smooth(aes(fill=text.condition, color=text.condition),span=1) + 
	#coord_cartesian(ylim=c(-.6,.8)) +
	xlab("Context Proportion") + ylab("Reaction Time") +
	theme_bw() +
	scale_shape("Version", labels=c("Study 2a","Study 2b")) +
  scale_color_manual("Sentence Condition", values=c("#A43946","#123159")) +
  scale_fill_manual("Sentence Condition", values=c("#EB97A1","#A1BBDC")) +
	plot.style

######Analyses (combined data)######
long.data.ct$response <- factor(long.data.ct$response,levels=c("FALSE","TRUE"))

#linear effect of context
lin <- lmer(rt ~ context.standard*text.condition*response
       + (text.condition*response |subid)
       + (text.condition*response |item),
       data=long.data.ct, REML=F)
       
#quadratic effect of context
quad <- lmer(rt ~ context.standard*text.condition*response + I(context.standard^2)
       + (text.condition*response |subid)
       + (text.condition*response |item),
       data=long.data.ct, REML=F)
       
#compare models
anova(quad, lin)

