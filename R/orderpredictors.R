#' Order predictors based on entropy
#'
#' Sort columns by increasing entropy values
#' based on the factors of the response variable.
#'
#' The mean entropy value is calculated for each variables
#' based on every outcome in the response variable.
#' Then this value is used for sorting the predictors.
#'
#' @param data Dataframe
#' @param x vector of variable names of predictors
#' @param y response variable
#' @examples
#' data('lulc_data')
#'
#' x <- c('maxlike_1985','DFU_85','maxlike_1991','DFU_91','maxlike_2000','DFU_00',
#'       'F00_3by3_W','F00_3by3_R','F00_3by3_B','F00_3by3_A','F00_3by3_U')
#' y <- 'maxlike_2015'
#'
#' orderedx <- orderpredictors(lulc_data, x, y)
#'
#' lulc_model_orderedx <- dotrules(lulc_data,orderedx$x,y, sampsize =.1)
#' @return A vector of predictor names ordered by increasing entropy and corresponding entropy values
#' @export
orderpredictors <- function(data,x,y){
  if(data.table::is.data.table(data)){data <- data.frame(data)}
  data <- data[,c(x,y)]
  cats <- unique(data[,y])
  cols <- x
  e <- c()
  for(c in cols){
    e <- c(e,mean(sapply(cats,FUN=function(x) entropy::entropy(data[data[,y]==x,c]))))
  }
  cols <- cols[order(e)]
  results <- list(x=cols,entropy=e[order(e)])
  return(results)
}
