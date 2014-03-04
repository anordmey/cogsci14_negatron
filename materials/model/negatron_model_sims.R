rm(list=ls())

#Libraries
library(bootstrap)
library(ggplot2)
library(plyr)
library(gridExtra)

#Functions
# for bootstrapping 95% confidence intervals
theta <- function(x,xdata) {mean(xdata[x])}
ci.low <- function(x) {
  mean(x) - quantile(bootstrap(1:length(x),1000,theta,x)$thetastar,.025)}
ci.high <- function(x) {
  quantile(bootstrap(1:length(x),1000,theta,x)$thetastar,.975) - mean(x)}



source("model/negatron_model.R")
source("model/negatron_model_data.R") 
#data.2a is Study 2a (3 person contexts)
#data.2b is Study 2b (4 person contexts)

##################### PARAMETERS ##########################
n.pos <- 1 # number of positive features shared by everyone
n.neg <- 10 # number of negative features shared by everyone
#note that in this simulation I am assuming that there is only one feature that varies across characters.  This is not strictly true in my experiment; e.g. different people have different hair, or different shirts, but there is only one binary feature that varies across characters (e.g. has or does not have apples).  The other all remain context, e.g. "Has a shirt", "Is a boy", "Is not a girl", "Does not have bananas"

#Creates the vocabulary - all possible words that could describe these characters                  
vocab <- c(1:(1+n.pos+n.neg),-(1+n.pos+n.neg):-1)

# create the full set of positive contexts, i.e. the target character has the feature of interest
# 0/3 context: 3 person context with zero target features
p.context.0in3 <- matrix(c(1, rep(1,n.pos), rep(0,n.neg),
                      0, rep(1,n.pos), rep(0,n.neg),
                      0, rep(1,n.pos), rep(0,n.neg),
                      0, rep(1,n.pos), rep(0,n.neg)), 
                  byrow=TRUE,
                  nrow=4, ncol=1 + n.pos + n.neg)

#1/3 context
p.context.1in3 <- p.context.0in3
p.context.1in3[2,1] <- 1

#2/3 context
p.context.2in3 <- p.context.1in3
p.context.2in3[3,1] <- 1

#3/3 context
p.context.3in3 <- p.context.2in3
p.context.3in3[4,1] <- 1

#0/4 context: 4 person context with zero target features                 
p.context.0in4 <- matrix(c(1, rep(1,n.pos), rep(0,n.neg),
                      0, rep(1,n.pos), rep(0,n.neg),
                      0, rep(1,n.pos), rep(0,n.neg),
                      0, rep(1,n.pos), rep(0,n.neg),
                      0, rep(1,n.pos), rep(0,n.neg)), 
                  byrow=TRUE,
                  nrow=5, ncol=1 + n.pos + n.neg)

#1/4 context
p.context.1in4 <- p.context.0in4
p.context.1in4[2,1] <- 1

#2/4 context
p.context.2in4 <- p.context.1in4
p.context.2in4[3,1] <- 1

#3/4 context
p.context.3in4 <- p.context.2in4
p.context.3in4[4,1] <- 1

#4/4 context
p.context.4in4 <- p.context.3in4
p.context.4in4[5,1] <- 1

#binds all positive contexts into a single list.
p.contexts <- list(p.context.0in3,p.context.1in3,p.context.2in3,p.context.3in3, p.context.0in4,p.context.1in4,p.context.2in4,p.context.3in4, p.context.4in4)


# now create negative contexts, i.e. the target character does not have the feature of interest
n.contexts <- p.contexts
n.contexts[[1]][1,1] <- 0
n.contexts[[2]][1,1] <- 0
n.contexts[[3]][1,1] <- 0
n.contexts[[4]][1,1] <- 0
n.contexts[[5]][1,1] <- 0
n.contexts[[6]][1,1] <- 0
n.contexts[[7]][1,1] <- 0
n.contexts[[8]][1,1] <- 0
n.contexts[[9]][1,1] <- 0

n <- length(n.contexts)

####################compare model to data (Study 2a and Study 2b)#####################

#############Individual simulation###############
##PARAMETERS:
cost = .4
lambda = .2
weight = .4

probs <- data.frame(image.type=c(rep("positive",n*2), rep("negative",n*2)), context.label=rep(c("0/3","1/3","2/3","3/3","0/4","1/4","2/4","3/4","4/4"),4),      				context.type=rep(c(0/3,1/3,2/3,3/3,0/4,1/4,2/4,3/4,4/4),4), 
 	word=rep(c(rep("apples",n),rep("no apples",n)),2), 
	sentence.type=rep(c(rep("Positive",n),rep("Negative",n)),2),
  truth.value=c(rep("True",n),rep("False",n),rep("False",n),rep("True",n)),
    p.word=NA, p.person = NA)

#set up variables for loop
contexts <- list(p.contexts,n.contexts) #makes a list of all possible contexts, both something and nothing items.
words <- c(1,-1) #two possible words: e.g "apples" and "no apples"
count <- 1

for (context.valence in 1:2) {
 	for (w in 1:2) {
    	for (context.num in 1:n) {
    	
    			probs$p.word[count] <- p.word.given.context(words[w],
                       	contexts[[context.valence]][[context.num]],
                        cost.per.word=cost)
                probs$p.person[count] <- p.person.in.context(words[w], contexts[[context.valence]][[context.num]], lambda=lambda)
                        
               	count <- count + 1

    	}
	}
}
probs$sentence.type <- factor(probs$sentence.type, levels=c("Positive","Negative"))
probs$truth.value <- factor(probs$truth.value, levels=c("True","False"))

# calculate surprisal of sentence given context
probs$word.surprisal <- -log(probs$p.word)
# calculate surprisal of seeing person given context
probs$person.surprisal <- -log(probs$p.person)
#add together to get total surprisal
probs$total <- probs$word.surprisal + weight*probs$person.surprisal
	
#merge data with model predictions
#Replace "data" with "data.2a" or "data.2b" to see model predictions for Study 2a only or Study 2b only
mdata <- merge(probs, data)
mdata$condition <- paste(mdata$truth.value, mdata$sentence.type)
        
#Correlations: 
cor.test(mdata$rt, mdata$total, na.action=na.omit)$estimate  

truepos <- mdata[mdata$condition=="True Positive",]
cor.test(truepos$rt, truepos$total, na.action=na.omit)$estimate

trueneg <- mdata[mdata$condition=="True Negative",]
cor.test(trueneg$rt, trueneg$total, na.action=na.omit)$estimate  


##PLOT:
plot.style <- theme_bw() + theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(), legend.position="right", axis.line = element_line(colour="black",size=.5), axis.ticks = element_line(size=.5), axis.title.x = element_text(vjust=-.5), axis.title.y = element_text(angle=90,vjust=0.25))

qplot(context.type, total, colour=sentence.type, geom="point", data=mdata) + ggtitle(paste("cost=",cost," lambda=",lambda," weight=",weight)) + ylab("Surprisal") + xlab("Context Type") + plot.style

ggplot(mdata, aes(color=condition, y=rt, x=total)) + 
		geom_pointrange(aes(ymin=cil, ymax=cih), size=.1) +  	
		geom_point(aes(color=condition)) + 
		#geom_smooth(aes(group=condition), method="lm", color="grey", se=F) + 
		geom_smooth(method="lm", color="grey", se=F) + 
		geom_text(aes(label=mdata$context.label), hjust=-.2, vjust=.5, size=3) +
		annotate("text", x=2, y=1650, label="True Positive", color="#C77CFF", size=3.5) + 
		annotate("text", x=2.4, y=1850, label="True Negative", color="#00BFC4", size=3.5) + 
		#ylim(1000,2000) + xlim(0,5) +
		ylab("RT") +  xlab("Model Predictions (Surprisal)") + 
		#ggtitle(paste("cost=",cost," lambda=",lambda," weight=",weight)) +
		plot.style    





###############Look at how model fit changes across different parameters###############
##PARAMETERS:
probs <- data.frame(image.type=c(rep("positive",n*2), rep("negative",n*2)), context.label=rep(c("0/3","1/3","2/3","3/3","0/4","1/4","2/4","3/4","4/4"),4),        			context.type=rep(c(0/3,1/3,2/3,3/3,0/4,1/4,2/4,3/4,4/4),4), 
                    word=rep(c(rep("apples",n),rep("no apples",n)),2), 
                    sentence.type=rep(c(rep("Positive",n),rep("Negative",n)),2),
                    truth.value=c(rep("True",n),rep("False",n),rep("False",n),rep("True",n)),
                    p.word=NA, p.person = NA)

#set up variables for loop
contexts <- list(p.contexts,n.contexts) #makes a list of all possible contexts, both something and nothing items.
words <- c(1,-1) #two possible words: e.g "apples" and "no apples"


#####VARY LAMBDA AND COST#####
costs = c(.2,.4,.6,.8,1,1.2,1.4,1.6,1.8,2,2.2,2.4,2.6,2.8,3)
lambdas = c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1,1.1,1.2,1.3,1.4,1.5)
weights = .4

corrs <- data.frame(cost = c(rep(costs[1],length(lambdas)), rep(costs[2],length(lambdas)), rep(costs[3],length(lambdas)), rep(costs[4],length(lambdas)), rep(costs[5],length(lambdas)), rep(costs[6],length(lambdas)),rep(costs[7],length(lambdas)),rep(costs[8],length(lambdas)),rep(costs[9],length(lambdas)),rep(costs[10],length(lambdas)),rep(costs[11],length(lambdas)),rep(costs[12],length(lambdas)),rep(costs[13],length(lambdas)),rep(costs[14],length(lambdas)),rep(costs[15],length(lambdas))), lambda = rep(lambdas, length(costs)), cor.all=NA, cor.all_wordonly=NA, cor.all_persononly=NA, cor.truepos=NA, cor.truepos_wordonly=NA, cor.truepos_persononly=NA, cor.trueneg=NA, cor.trueneg_wordonly=NA, cor.trueneg_persononly=NA)

for (lambda in lambdas) {
	for (cost in costs) {
		count <- 1
	for (context.valence in 1:2) {
 		for (w in 1:2) {
   			for (context.num in 1:n) {
    			probs$p.word[count] <- p.word.given.context(words[w],
                	     contexts[[context.valence]][[context.num]],
                    	 cost.per.word=cost)
            	probs$p.person[count] <- p.person.in.context(words[w], contexts[[context.valence]][[context.num]], lambda=lambda)
                        
            	count <- count + 1

			   	}
			}
		}
		for (weight in weights) {	
		probs$sentence.type <- factor(probs$sentence.type, levels=c("Positive","Negative"))
		probs$truth.value <- factor(probs$truth.value, levels=c("True","False"))

		# calculate surprisal of sentence given context
		probs$word.surprisal <- -log(probs$p.word)
		# calculate surprisal of seeing person given context
		probs$person.surprisal <- -log(probs$p.person)
		#add together to get total surprisal
		probs$total <- probs$word.surprisal + weight*probs$person.surprisal
	
		#merge data with model predictions
		#Replace "data" with "data.2a" or "data.2b" to fit model to Study 2a only or Study 2b only
		mdata <- merge(probs, data)
		mdata$condition <- paste(mdata$truth.value, mdata$sentence.type)
        
		#Correlations: 
		corrs[corrs$cost==cost & corrs$lambda==lambda,]$cor.all <- cor.test(mdata$rt, mdata$total, na.action=na.omit)$estimate   

		truepos <- mdata[mdata$condition=="True Positive",]
		corrs[corrs$cost==cost & corrs$lambda==lambda,]$cor.truepos <- cor.test(truepos$rt, truepos$total, na.action=na.omit)$estimate

		trueneg <- mdata[mdata$condition=="True Negative",]
		corrs[corrs$cost==cost & corrs$lambda==lambda,]$cor.trueneg <- cor.test(trueneg$rt, trueneg$total, na.action=na.omit)$estimate 	
		}	
	}
}

corrs$lambda <- as.factor(corrs$lambda)
corrs$cost <- as.factor(corrs$cost)

#Make correlation heatmap
ggplot(data = corrs, aes(x=cost, y=lambda)) + geom_tile(aes(fill=corrs$cor.all))

ggplot(data = corrs, aes(x=cost, y=lambda)) + geom_tile(aes(fill=corrs$cor.truepos)) 

ggplot(data = corrs, aes(x=cost, y=lambda)) + geom_tile(aes(fill=corrs$cor.trueneg))



#####VARY COST AND WEIGHT#####
costs = c(.2,.4,.6,.8,1,1.2,1.4,1.6,1.8,2,2.2,2.4,2.6,2.8,3)
lambdas = .2
weights = c(.2,.4,.6,.8,1,1.2,1.4,1.6,1.8,2,2.2,2.4,2.6,2.8,3)

corrs <- data.frame(cost = c(rep(costs[1],length(weights)), rep(costs[2],length(weights)), rep(costs[3],length(weights)), rep(costs[4],length(weights)), rep(costs[5],length(weights)), rep(costs[6],length(weights)),rep(costs[7],length(weights)),rep(costs[8],length(weights)),rep(costs[9],length(weights)),rep(costs[10],length(weights)),rep(costs[11],length(weights)),rep(costs[12],length(weights)),rep(costs[13],length(weights)),rep(costs[14],length(weights)),rep(costs[15],length(weights))), weight = rep(weights, length(costs)), cor.all=NA, cor.all_wordonly=NA, cor.all_persononly=NA, cor.truepos=NA, cor.truepos_wordonly=NA, cor.truepos_persononly=NA, cor.trueneg=NA, cor.trueneg_wordonly=NA, cor.trueneg_persononly=NA)


for (lambda in lambdas) {
	for (cost in costs) {
		count <- 1
	for (context.valence in 1:2) {
 		for (w in 1:2) {
   			for (context.num in 1:n) {
    			probs$p.word[count] <- p.word.given.context(words[w],
                	     contexts[[context.valence]][[context.num]],
                    	 cost.per.word=cost)
            	probs$p.person[count] <- p.person.in.context(words[w], contexts[[context.valence]][[context.num]], lambda=lambda)
                        
            	count <- count + 1

			   	}
			}
		}
		for (weight in weights) {	
		probs$sentence.type <- factor(probs$sentence.type, levels=c("Positive","Negative"))
		probs$truth.value <- factor(probs$truth.value, levels=c("True","False"))

		# calculate surprisal of sentence given context
		probs$word.surprisal <- -log(probs$p.word)
		# calculate surprisal of seeing person given context
		probs$person.surprisal <- -log(probs$p.person)
		#add together to get total surprisal
		probs$total <- probs$word.surprisal + weight*probs$person.surprisal
	
		#merge data with model predictions
		#Replace "data" with "data.2a" or "data.2b" to fit model to Study 2a only or Study 2b only
		mdata <- merge(probs, data)
		mdata$condition <- paste(mdata$truth.value, mdata$sentence.type)
        
		#Correlations: 
		corrs[corrs$cost==cost & corrs$weight==weight,]$cor.all <- cor.test(mdata$rt, mdata$total, na.action=na.omit)$estimate   

		truepos <- mdata[mdata$condition=="True Positive",]
		corrs[corrs$cost==cost & corrs$weight==weight,]$cor.truepos <- cor.test(truepos$rt, truepos$total, na.action=na.omit)$estimate
		
		trueneg <- mdata[mdata$condition=="True Negative",]
		corrs[corrs$cost==cost & corrs$weight==weight,]$cor.trueneg <- cor.test(trueneg$rt, trueneg$total, na.action=na.omit)$estimate 	

		}	
	}
}

corrs$weight <- as.factor(corrs$weight)
corrs$cost <- as.factor(corrs$cost)

#Make correlation heatmap
ggplot(data = corrs, aes(x=cost, y=weight)) + geom_tile(aes(fill=corrs$cor.all))

ggplot(data = corrs, aes(x=cost, y=weight)) + geom_tile(aes(fill=corrs$cor.truepos)) 

ggplot(data = corrs, aes(x=cost, y=weight)) + geom_tile(aes(fill=corrs$cor.trueneg))



#####VARY LAMBDA AND WEIGHT#####
cost = .4
lambdas = c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1.0,1.1,1.2,1.3,1.4,1.5)
weights = c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1.0,1.1,1.2,1.3,1.4,1.5)

corrs <- data.frame(weight = c(rep(weights[1],length(lambdas)), rep(weights[2],length(lambdas)), rep(weights[3],length(lambdas)), rep(weights[4],length(lambdas)), rep(weights[5],length(lambdas)), rep(weights[6],length(lambdas)),rep(weights[7],length(lambdas)),rep(weights[8],length(lambdas)),rep(weights[9],length(lambdas)),rep(weights[10],length(lambdas)),rep(weights[11],length(lambdas)),rep(weights[12],length(lambdas)),rep(weights[13],length(lambdas)),rep(weights[14],length(lambdas)),rep(weights[15],length(lambdas))), lambda = rep(lambdas, length(weights)), cor.all=NA, cor.all_wordonly=NA, cor.all_persononly=NA, cor.truepos=NA, cor.truepos_wordonly=NA, cor.truepos_persononly=NA, cor.trueneg=NA, cor.trueneg_wordonly=NA, cor.trueneg_persononly=NA)


for (lambda in lambdas) {
	count <- 1
	for (context.valence in 1:2) {
 		for (w in 1:2) {
   			for (context.num in 1:n) {
    		probs$p.word[count] <- p.word.given.context(words[w],
                     contexts[[context.valence]][[context.num]],
                     cost.per.word=cost)
            probs$p.person[count] <- p.person.in.context(words[w], contexts[[context.valence]][[context.num]], lambda=lambda)
                        
            count <- count + 1

		   	}
		}
	}
	for (weight in weights) {	
		probs$sentence.type <- factor(probs$sentence.type, levels=c("Positive","Negative"))
		probs$truth.value <- factor(probs$truth.value, levels=c("True","False"))

		# calculate surprisal of sentence given context
		probs$word.surprisal <- -log(probs$p.word)
		# calculate surprisal of seeing person given context
		probs$person.surprisal <- -log(probs$p.person)
		#add together to get total surprisal
		probs$total <- probs$word.surprisal + weight*probs$person.surprisal
	
		#merge data with model predictions
		#Replace "data" with "data.2a" or "data.2b" to fit model to Study 2a only or Study 2b only
		mdata <- merge(probs, data)
		mdata$condition <- paste(mdata$truth.value, mdata$sentence.type)
        
		#Correlations: 
		corrs[corrs$weight==weight & corrs$lambda==lambda,]$cor.all <- cor.test(mdata$rt, mdata$total, na.action=na.omit)$estimate  

		truepos <- mdata[mdata$condition=="True Positive",]
		corrs[corrs$weight==weight & corrs$lambda==lambda,]$cor.truepos <- cor.test(truepos$rt, truepos$total, na.action=na.omit)$estimate

		trueneg <- mdata[mdata$condition=="True Negative",]
		corrs[corrs$weight==weight & corrs$lambda==lambda,]$cor.trueneg <- cor.test(trueneg$rt, trueneg$total, na.action=na.omit)$estimate 		
	}
}

corrs$weight <- as.factor(corrs$weight)
corrs$lambda <- as.factor(corrs$lambda)
#Make correlation heatmap
ggplot(data = corrs, aes(x=weight, y=lambda)) + geom_tile(aes(fill=corrs$cor.all))

ggplot(data = corrs, aes(x=weight, y=lambda)) + geom_tile(aes(fill=corrs$cor.truepos)) 

ggplot(data = corrs, aes(x=weight, y=lambda)) + geom_tile(aes(fill=corrs$cor.trueneg)) 