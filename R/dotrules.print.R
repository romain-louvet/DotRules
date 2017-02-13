#' Print results
#'
#' Print method for a dotrules object.
#' Print confusion matrices of the overall accuracy,
#' the training sample accuracy, and the testing sample
#' accuracy.
#'
#' @param x dotrules object
#' @param ... other arguments
#' @export
print.dotrules <- function(x, ...){
  print("Overall Accuracy")
  readline(prompt="Press [enter] to continue")
  print(x$accuracy)

  print("Training Data Set Accuracy")
  readline(prompt="Press [enter] to continue")
  print(x$accuracy_train)

  print("Testing Data Set Accuracy")
  readline(prompt="Press [enter] to continue")
  print(x$accuracy_test)
}
