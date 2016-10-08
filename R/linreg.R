#' Store output from a Regression model in a linreg object
#'
#' This function calculates fitted values, residuals, degrees of freedom, t-ratio and p-values
#'
#'@param formula, writen as a formula-object, see example.
#'@param data, a data.frame object in this particular case data(iris)
#'@examples
#'linreg(Sepal.Length ~ Sepal.Width, iris)
#'@return a linreg class-object containing regression outputs
#'@author Aqeel Ahmed, Marhawi Tewolde
#'@details This function calculates ordrinary least square models (OLS)
#'@import ggplot2
#'@export

linreg<-function(formula,data)
 { 
  x<-model.matrix(formula,data)
  y<-all.vars(formula)[1]
  y<-as.matrix(data[y])
  RC<-as.vector(solve((t(x)%*%x))%*%t(x)%*%y)
  FV<-as.vector(x%*%RC)
  RD<-y-FV
  DF<- nrow(x)-ncol(x)
  RV<- c((t(RD)%*%RD)/DF)
  VRC<-diag(as.vector(RV)*solve(t(x)%*%x))
  tvalues<-RC/sqrt(VRC)
  tvalues<-round(x=tvalues,digits = 2)
  
 list<-list()
  
  
  formulas <- lin_class$new(formula = as.character(formula),
  beta=RC ,
  fitted=FV, 
  resd =c(RD),
  df=DF,
  resvariance=RV,
  varcoef = c(diag(VRC)),
  tvalues=tvalues)
 
     D1<- data.frame(x = FV, y = as.numeric(RD))
     D2<- data.frame(x = FV, y = as.numeric(sqrt(abs(RD))))
     
     plot <- plot_class$new(D1 = D1,D2 = D2)
     
     list$plot <- plot
     
     list$formulas <- formulas
     
     
     return(list)                       
}

