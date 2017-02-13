#' Plot results
#'
#' Plot method for a dotrules object.
#' Plot the probability, the entropy, and the accuracy, of the extracted rules.
#' With the base graphics and plotly.
#'
#' @param x dotrules object
#' @param ... other arguments
#' @export
plot.dotrules <- function(x, ...){
  graphics::par(ask = TRUE)
  graphics::plot(y=x$rules$TrainTrgtRuleProba,x=x$rules$TrainTrgtRuleEntropy,
       pch=ifelse(x$rules$RuleAccuracy>0.5,20,3),
       col=ifelse(x$rules$RuleAccuracy>0.5,"green","red"),
       xlab = 'Train Target Rule Entropy', ylab = "Train Target Rule Probability")
  graphics::legend("topright",c("> 0.5","<= 0.5"),pch = 1,
                   col=c("green","red"),title = "Rule Accuracy")

  p <- plotly::plot_ly(x$rules,x=~TrainTrgtRuleEntropy,y=~TrainTrgtRuleProba,
          color = ~RuleAccuracy,type = 'scatter',mode='markers')
  readline(prompt="Press [enter] to continue")
  print(p)

  p <- plotly::plot_ly(x$rules,x=~TrainTrgtRuleEntropy,y=~TrainTrgtRuleProba,z=~RuleAccuracy,
          color = ~RuleAccuracy,type='scatter3d',mode='markers')
  readline(prompt="Press [enter] to continue")
  print(p)
}
