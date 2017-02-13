#' @return \code{NULL}
#'
#' @method dotrules default
#' @noRd
#' @name dotrules default
#' @title dotrules default
#' @export
dotrules.default <- function(data, x,y, sampsize=.1,sample=NULL, ...){
  #####################################  format input #####################################
  # dataframe
  if(data.table::is.data.table(data)){data <- data.frame(data)}
  # subset columns
  data <- data[,c(x,y)]
  # paste variables to create rules
  ## src: source, cat for x
  ## trgt: target, x + y (response var y is the target)
  data$RuleSrc  <- apply(data[,x],1,paste,collapse="")
  data$RuleTrgt  <- apply(data[,c('RuleSrc',y)],1,paste,collapse="")

  ## test/train sample
  if(is.null(sample)){
    data <- .sampledata(data,sampsize)
  }else{
    data$sample <- sample
  }
  test <- data[data$sample=="test",]
  train <- data[data$sample=="train",]

  ##################################### RULES from TRAINING #####################################
  # rules proba from training sample
  ## source rules
  rules <- as.data.frame(table(train$RuleSrc))
  names(rules) <- c("RuleSrc","SrcFreq")

  ## target rules
  rules2 <- as.data.frame(table(train$RuleTrgt))
  names(rules2) <- c("RuleTrgt","TrgtFreq")
  rules2$RuleSrc <- substr(rules2$RuleTrgt,start = 1,stop = length(x))
  rules <- merge(rules,rules2, by="RuleSrc")

  ## rule src freq based on src and possible outcome
  ## calc rule entropy by rule set based on possible outcome freq
  e <- stats::aggregate(rules$TrgtFreq,by=list(rules$RuleSrc),FUN=entropy::entropy)
  names(e) <- c("RuleSrc","TrgtFreqEntropy")

  ## calc rule proba
  # proba of the rule / total
  rules$TrgtSumProba <- rules$TrgtFreq/sum(rules$TrgtFreq)
  # proba of the rule by source
  rules$TrgtSrcProba <- rules$TrgtFreq/rules$SrcFreq
  rules <- merge(rules,e,"RuleSrc")

  ## sel rule with maj trgt freq
  rules2 <- stats::aggregate(rules$TrgtFreq,by=list(rules$RuleSrc),FUN=max)
  names(rules2) <- c("RuleSrc","TrgtFreq")

  # merge result
  rules <- merge(rules2,rules,"RuleSrc")
  rules <- rules[rules$TrgtFreq.x==rules$TrgtFreq.y,]
  rules <- rules[,-2]
  names(rules)[4] <- "TrgtFreq"

  # for max rules with equal frequency, select one outcome randomly
  # (order by random and remove duplicates)
  rando <- stats::rnorm(nrow(rules))
  rules <- rules[order(rando),]
  rules <- rules[!duplicated(rules$RuleSrc),]

  # get label for classif
  rules$Label <- substr(rules$RuleTrgt,length(x)+1,length(x)+1)

  ##################################### CLASSIF #####################################
  ## use the rules for classif train
  train <- merge(x=train,y=rules[,c("RuleSrc","Label")],"RuleSrc",all.x=TRUE)

  ## use the rules for classif test
  # test dataset
  test <- merge(x=test,y=rules[,c("RuleSrc","Label")],"RuleSrc",all.x=TRUE)

  # get closest rule for missing rules
  n <- length(x)
  rules$RuleSrc <- as.character(rules$RuleSrc)
  test$RuleSrc <- as.character(test$RuleSrc)
  while(sum(is.na(test$Label))>0){
    # new rule
    newrules <- rules
    newrules$RuleSrc <- substr(newrules$RuleSrc,1,n-1)

    # max rule
    newrules2 <- stats::aggregate(newrules$TrgtFreq,by=list(newrules$RuleSrc),FUN=max)
    names(newrules2) <- c("RuleSrc","TrgtFreq")

    # merge result
    newrules <- merge(newrules2,newrules,"RuleSrc")
    newrules <- newrules[newrules$TrgtFreq.x==newrules$TrgtFreq.y,]
    newrules <- newrules[,-2]
    names(newrules)[4] <- "TrgtFreq"

    # for max rules with equal frequency, select one outcome randomly
    # (order by random and remove duplicates)
    rando <- stats::rnorm(nrow(newrules))
    newrules <- newrules[order(rando),]
    newrules <- newrules[!duplicated(newrules$RuleSrc),]

    # get label for classif
    newrules$Label2 <- substr(newrules$RuleTrgt,length(x)+1,length(x)+1)

    # merge
    sel <- is.na(test$Label)
    test[sel,"RuleSrc"] <- substr(test[sel,"RuleSrc"],1,n-1)
    test2 <- merge(x=test[sel,],y=newrules[,c("RuleSrc","Label2")],"RuleSrc",all.x=TRUE)
    test[sel,"Label"] <- test2$Label2
    # add matching new rules to dico
    newrules <- newrules[newrules$RuleSrc %in% test[,"RuleSrc"],]
    newrules$Label <- newrules$Label2
    newrules <- newrules[,-ncol(newrules)]
    rules <- rbind(rules,newrules)
    n <- n - 1
  }

  ##################################### RESULTS #####################################
  testtrain <- rbind(test,train)
  testtrain <- testtrain[,c(x,y,"sample","RuleSrc","RuleTrgt","Label")]

  # CF matrices ## NAs produced by integer overflow ##
  CFMtrain <- caret::confusionMatrix(train$Label,train[,y])
  CFMtest <- caret::confusionMatrix(test$Label,test[,y])
  CFMall <- caret::confusionMatrix(testtrain$Label,testtrain[,y])

  # order rules by global proba
  rules <- rules[order(rules$TrgtSumProba,decreasing = TRUE),]

  ## results
  rules <- rules[,c("RuleSrc","RuleTrgt","SrcFreq","TrgtFreq",
                    "TrgtSrcProba","TrgtSumProba","TrgtFreqEntropy","Label")]
  names(rules) <- c("RuleSrc","RuleTrgt","TrainSrcRuleFreq","TrainTrgtRuleFreq",
                    "TrainTrgtRuleProba","TrainTotalRuleProba","TrainTrgtRuleEntropy","Label")
  testtrain <- testtrain
  testtrain$Result <- testtrain$Label==testtrain[,y]
  accuracy <- stats::aggregate(testtrain$Result,by=list(testtrain$RuleSrc),FUN=sum)
  accuracy2 <- stats::aggregate(testtrain$Result,by=list(testtrain$RuleSrc),FUN=length)
  accuracy$x <- accuracy$x/accuracy2$x
  accuracy$RuleFreq <- accuracy2$x
  names(accuracy)[1:2] <- c("RuleSrc","RuleAccuracy")

  rules <- merge(x=rules,y=accuracy,"RuleSrc",all.x=TRUE)

  results <- list(data=data.table::data.table(testtrain),rules=data.table::data.table(rules),
                  accuracy=CFMall,accuracy_train=CFMtrain,accuracy_test=CFMtest)
  class(results) <- c("dotrules", class(results))
  return(results)
}

#' Sample a dataframe
#'
#' Based on a percentage, sample input dataframe
#' @param x Dataframe
#' @param sampsize Percentage, between 0 and 1
#' @keywords internal
#' @noRd
.sampledata <- function(x,sampsize){
  #Number of rows
  NR <- nrow(x)
  #Sample size (s)
  s <- ceiling(NR * sampsize)
  x$sample <- "test"
  x$sample[sample(NR, s)] <- "train"
  return(x)
}
