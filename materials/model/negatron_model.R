## NEGATRON MODEL
## model for predicting probability of an utterance in context

######### FUNCTIONS ############

## get the cost of a word, given a "cost per word" parameter
get.word.cost <- function(word, cost.per.word) {
  
  if (word < 0) { # if a word is negative (literally < 0)
    cost <- cost.per.word * 2
  } else { # otherwise it's just one word
    cost <- cost.per.word
  }
  
  return(cost)
}

## get the extension of a word in context
get.word.extension <- function (word, context) {
  if (word > 0) { # positive word
    extension <- context[,word] #This gives us the value of the target feature for each character in the context (i.e. do they have or not have the target feature).  For positive sentences, the extension is simply the characters who do have the target feature.  
  } else if (word < 0) { # negated word
    extension <- 1 - context[,abs(word)] #For negative sentences, the extension is the number of characters who DO NOT have the target feature.

  }
  return(extension)
}

## check if words are true of a particular referent
word.true <- function(word, context, referent) {
  extent <- context[referent,abs(word)] #referent gives us the column which specifies the feature of interest (should be column 1), which either is or is not present for each character.  abs(word) gives us the row for the character of interest
  if (word < 0 & extent == 0) { # if negative and feature absent 
    truth.value <- TRUE
  } else if (word > 0 & extent == 1) { # if positive and feature 
    truth.value <- TRUE
  } else {
    truth.value <- FALSE
  }
}


## score individual word in context.  score gives us the utility of the word in the context.
score.word.given.context <- function(word, context, 
                                     referent=1,
                                     cost.per.word,
                                     debug=FALSE) {
  
  # uniform distribution over examples
  word.extension <- sum(get.word.extension(word, context))
  word.info <- log(1 / word.extension)
  word.cost <- get.word.cost(word, cost.per.word)
  
  # score according to utility, IF word applies. utility is informativity of word - cost of word
  if (word.true(word,context,referent)) {
    s <- word.info - word.cost
  } else {
    s <- NA # if the word isn't true
  }
  
  if (debug) {
    print(paste("extension:",word.extension))
    print(paste("informativeness:",word.info))
    print(paste("word cost:",word.cost))
    print(paste("total:",s))    
  }
  
  return(s)
}

## main scoring function: aggregates across vocabulary
p.word.given.context <- function(word, context, 
                                 debug=FALSE,
                                 referent=1, 
                                 cost.per.word = cost.per.word, 
                                 alpha=1) {
  
  scores <- array(NA,dim=length(vocab)) #makes an empty array that is the same length of the vocabulary.  The vocabulary consists of positive and negative versions of all possible features (defined manually.  Technically there are infinite features which could be positive or negative.)
  
  for (w in 1:length(vocab)) {
      scores[w] <- score.word.given.context(vocab[w], context, 
                                            referent=referent,												
                                           	cost.per.word = cost.per.word)
  }
  
  if (debug) {
    print(scores)
  }
  
  summed.scores <- sum(exp(alpha * scores), na.rm=TRUE) #scores summed across all possible words as they apply to the target character.
  this.word.score <- exp(alpha * score.word.given.context(word, 									context, 
                                    referent=referent, 
                                    cost.per.word = cost.per.word)) #the score for the word of interest (1 or -1)
  
  p <-  this.word.score / summed.scores #divide score for word of interest by summed scores, to normalize
  
  if (debug) {
    print(paste("this word:",this.word.score))
    print(paste("summed:",summed.scores))
    print(paste("total:",p))
  }
  
  return(p)
}


##how many items in the context have the target feature
get.feature.extension <- function (word, context) {
    feature.extension <- context[,abs(word)] #This gives us the value of the feature for each character (i.e. do they have or not have the target feature).      
  return(feature.extension)
}

#get the probability of seeing the target person in context
p.person.in.context <- function (word, context, lambda=lambda) {
	
	feature.extension <- get.feature.extension(word,context)
	context.list <- feature.extension[2:length(feature.extension)] #this just gives us the context, removes target character.
  	feature.value <- feature.extension[1]
  	
	total.people <- length(context.list)
	total.feature <- sum(context.list)
	
	if (feature.value == 1)	{
		p.person <- (total.feature + lambda) / (total.people + 2*lambda)
		return(p.person)
	}
	else {
		p.person <- 1 - ((total.feature + lambda) / (total.people + 2*lambda))
		return(p.person)
	}	
}
