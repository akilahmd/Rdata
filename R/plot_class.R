
plot_class <- setRefClass(
  "plot_class",
  fields = list(
    D1 = "data.frame",
    D2 = "data.frame"
  ),
  methods = list(
    plot = function() {
      library(gridExtra)
      graf1<-ggplot(data=D1,aes(x = x, y = y))+
        ggtitle("residuals Vs Fitted ")+
        xlab("fitted_values")+
        ylab("residuals")+
        geom_point()+geom_smooth(aes(col = "red"), method = "lm")
      
      graf2<-ggplot(data=D2, aes(x = x, y = y))+
        ggtitle("scale location ")+
        xlab("fitted_values")+
        ylab("standard_residuals")+
        geom_point()+geom_smooth(aes(col = "red"), method = "lm")
      
      grid.arrange(graf1,graf2)
    })
)