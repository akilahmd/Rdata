
ridgereg_class <- setRefClass (
  "ridgereg_class",
  fields = list(
    formula = "character",
    rid_beta = "numeric",
    rid_fitted = "numeric",
    lamda = "numeric"
    ),
  methods = list(
    
    ridge_coef = function() {
      return(rid_beta) 
    },
    ridge_predict = function() {
      return(rid_fitted)
      }
    )
)