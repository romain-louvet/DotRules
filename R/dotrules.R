#' Create a dictionary of trusted rules
#'
#' The model fit a dictonary of rules based on the frequency
#' by outcomes in the training sample in order to predict
#' classes. The predictors are concatenated to create rules
#' based on the training sample. Only the most frequent outocome is selected
#' for each rule. The rules are then joined to the testing sample to
#' predict the results. If there is no match, the closest rules is
#' created by removing the last predictor, until the prediction is complete.
#' The entropy of the rules is calculated based on the frequencies of
#' every similar rules.
#'
#' @param data data frame
#' @param x vector of variable names of predictors
#' @param y name of the response variable
#' @param sampsize percentage, between 0 and 1, of samples to draw from data to differentiate between testing and training samples
#' @param sample vector of train/test. If provided sampsize is ignored
#' @param ... others arguments
#' @examples
#' data('lulc_data')
#'
#' x <- c('maxlike_1985','DFU_85','maxlike_1991','DFU_91','maxlike_2000','DFU_00',
#'       'F00_3by3_W','F00_3by3_R','F00_3by3_B','F00_3by3_A','F00_3by3_U')
#' y <- 'maxlike_2015'
#'
#' set.seed(999)
#' lulc_model <- dotrules(lulc_data,x,y, sampsize =.1)
#' @return The function will return a dotrules object which is a list of input data, rules, and accuracy metrics
#' @export
dotrules <- function(data, x,y, sampsize=.1,sample=NULL, ...) UseMethod("dotrules")
