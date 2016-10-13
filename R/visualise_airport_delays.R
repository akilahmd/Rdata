library(nycflights13)
library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
data(flights)
data(airports)


visualize_airport_delays<-function(){
  
  ddelay <- select(flights, origin,dep_delay)
  colnames(ddelay) <- c("new_airports","dminutes")
  adelay <- select(flights, dest, arr_delay)
  colnames(adelay) <- c("new_airports","dminutes")
  bind_delays<-bind_rows(ddelay,adelay)
  grouped_delays<-arrange(bind_delays,new_airports)
  
  #Calculating mean delays,joining data sets, and filtering
  grouped_mean<-grouped_delays %>%
    group_by(new_airports) %>%
    summarise(mean_delay = mean(dminutes, na.rm= TRUE))%>%
    left_join(airports,by=c("new_airports"="faa"))  %>% 
    filter(!is.na(mean_delay)) %>%  filter(!is.na(lon)) %>% filter(!is.na(lat))
  
  #Ploting 
  us<-map_data("usa")
  
  delayplot<-ggplot(us,aes(x=long, y=lat,group = group)) +geom_polygon() +
    geom_point(data=grouped_mean,aes(x=lon,y=lat,group = mean_delay ,color=mean_delay)) + 
    scale_colour_gradient(high = "blue",low="yellow")+
    labs(title= " Map of average delay ", y = "Latitude", x ="Longitude") 
  
  return(delayplot)
  
}

#Latitude values increase or decrease along the vertical axis, the Y axis.
#Longitude changes value along the horizontal access, the X axis.
#X = Longitude, Y = Latitude
#http://www.gisdoctor.com/site/2015/09/01/long-x-lat-free-illustration-included/
