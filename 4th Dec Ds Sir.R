attach(cars)
summary(lm(dist~speed))

library(tidyverse)
lm(dist~speed)
ggplot(cars,aes(x=speed,y=dist))+
  geom_point()+
  stat_function(fun = function(x) 2.47+0.913*x+0.099*x^2)
  
attach(chickwts)
view(chickwts)
a=summary(aov(weight~feed))
sb_square=231129
sw_square=195556

s_sq=sb_square+sw_square;s_sq
e_sq=sb_square/s_sq;e_sq

#ni=aggregate(chickwts,by=list(feed),FUN = length)$feed
feed_code=rep(1:6,c(10,12,14,12,11,12))

wts=data.frame(weight,feed_code)
wts

summary(aov(wts$weight~as.factor(wts$feed_code)))
