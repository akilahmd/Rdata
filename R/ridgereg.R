#' Ridge Regression
#'
#' This function calculates regression coeeficients and fitted values.
#'
#'@param formula, writen as a formula-object, see example.
#'@param data, a data.frame object in this particular case data(iris)
#'@param Lambda, numeric
#'@examples
#'ridreg(Sepal.Length ~ Sepal.Width, iris, 2)
#'@return a ridgereg class-object containing regression outputs
#'@author Aqeel Ahmed, Marhawi Tewolde
#'@details This function calculates Ridge regression
#'@import ggplot2
#'@export
ridgereg<- function(formula, data, lambda)
{
  stopifnot(formula == as.formula(formula), is.data.frame(data),is.numeric(lambda))
  x<-model.matrix(formula,data)
  y<-all.vars(formula)[1]
  y<-as.matrix(data[y])
  i<-ncol(x)
  rid_beta<-as.vector(solve((t(x)%*%x)+diag( x= lambda, nrow = i, ncol = i )*diag(i))%*%t(x)%*%y)
  rid_fitted<-as.vector(x%*%rid_beta)
  ridobject <- ridgereg_class$new(formula = as.character(formula),
                            rid_beta=rid_beta ,
                            rid_fitted=rid_fitted)
  return(ridobject)
  
}
