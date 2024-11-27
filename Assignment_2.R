#Assesment
library(MASS)
attach(Boston)
library(ggplot2)
library(gridExtra)
y=medv
x=lstat
my_data= data.frame(lstat= Boston$lstat,medv= Boston$medv);my_data
lin_reg= lm(medv~lstat);lin_reg
coef= as.vector(coefficients(lin_reg));coef
a_hat= coef[1]; b_hat= coef[2]
quad_reg= lm(medv~(lstat+I(lstat^2)));quad_reg
cubic_reg= lm(medv~(lstat+I(lstat^2)+I(lstat^3)));cubic_reg
Plot= ggplot(my_data,aes(x= lstat,y= medv))+
  geom_point()+ 
  geom_abline(aes(intercept= a_hat,slope= b_hat))+
  geom_smooth(method = 'lm',formula = y~x+I(x^2),se=F)

Plot

#residual plot
res= resid(lin_reg);res
pred_value= predict(lin_reg)
lin_reg2= data.frame(my_data, y= pred_value, e= res)
res_plot= ggplot(lin_reg2,aes(x= lstat,y= e))+
  geom_point();res_plot

#Histogram of the residuals
Hist= ggplot(my_data,aes(y=res))+
  geom_histogram()+ coord_flip();Hist

grid.arrange(Plot,res_plot,Hist,ncol= 2, nrow= 2)
