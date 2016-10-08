
lin_class <- setRefClass(
  "lin_class",
  fields = list(
    formula = "character",
    beta = "numeric",
    fitted = "numeric",
    resd = "numeric",
    df = "integer",
    resvariance = "numeric",
    varcoef = "numeric",
    tvalues = "numeric"),
  methods = list(
    print = function() {
      cat('Call:\n','linreg(formula = ',formula[2],'~',formula[3],", data = iris",")\n\n",sep = '',"coefficients:\n\n")
      colnames = names(beta)
      beta
      
    },
    
    resid = function() {
      return(resd)
    },
    pred = function() {
      return(fitted)
    },
    coef = function() {
      return(resvariance) 
    },
    summary = function() {
      cat('Call:\n','linreg(formula = ',formula[2],'~',formula[3],')\n\n',sep = '')
      mat <- matrix(c(beta,varcoef, tvalues),nrow = length(beta), ncol=3,byrow=FALSE)
      colnames(mat) <- c('Estimate', 'Std. Error', 't value')
      rownames(mat) <- names(beta)
     print.table(as.table(mat))
      cat('\n Residual standard error:', resvariance, 'on', df, 'degress of freedom', sep = ' ')
    })
)
