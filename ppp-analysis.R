


##The principle of purchasing power parity (PPP) states that over long periods of time exchange rate changes will tend to offset the differences in inflation rate between two countries. 

#In an efficient international economy, exchange rates would give each currency the same purchasing power in its own economy. Even if it does not hold exactly, the PPP model provides a benchmark to suggest the levels that exchange rates should achieve. 

#Examine a data example for the application of the simple linear regression model, specifically evaluating the purchasing power theory 
#Apply linear regression to evaluate whether the purchasing power theory holds
#Acknowledgment: This example was made available by Dr. Jeffrey Simonoff from the New York University.

# Purchasing power parity - is it true?
# Use read.table R command: pay attention to the file type to use the correct read file!#
# Data files that are comma-delimited (csv) files are easy to read into R using the read.csv() command#
# sep="\t? specifies how the data values are separated; & header=T is for specification of the column names

ppp<- read.table("PPP-2.dat",sep="\t",header=T, row.names = NULL)
## Check to make sure you read the data in R correctly 
ppp[1:2,]

#How many countries are there?
dim(ppp)
##  Brazil is an outlier and it was not included in the data set initially; I am adding it back as follows

addp = data.frame("Brazil",-76,-73,0)
names(addp)=names(ppp)
ppp= data.frame(rbind(ppp,addp))
attach(ppp)
#The attach() function makes the variables in the ppp data frame available in the R search path, allowing you to refer to the columns of ppp directly by their names without using the $ operator.
# Append the new row to the existing data frame using dplyr
#ppp <- bind_rows(ppp, addp)
str(ppp)
## Re-label the ‘Developed’ column to differentiate between Developed and Developing countries
Developed[Developed==1]="Developed"
Developed[Developed==0]="Developing"
str(ppp)
# exploratory data analysis in R
#Evaluate the linear relationship : Perform scatterplot of the two variables
plot(Inflation.difference,
     Exchange.rate.change,main ="Scatter plot of Exchange rate change vs inflation difference",
xlab= "Inflation difference",ylab="Excange rate change")
####using ggplot2
library(ggplot2)
ggplot(ppp,aes(x=Inflation.difference,y=Exchange.rate.change))+geom_point()+
  labs(title = "Scatter plot of Exchange rate change vs inflation difference",x="",y="")+
  theme_minimal()
# Evaluate differences between developed and developing coutries.
boxplot(ppp$Exchange.rate.change~as.factor(ppp$Developed), main="Boxplot of Exchange rate change by Developed vs 
Developing Countries",xlab="Country Class",ylab="Exchange rate change")
ggplot(ppp, aes(x = as.factor(Developed), y = Exchange.rate.change)) +
  geom_boxplot() +
  labs(title = "Boxplot of Exchange Rate Change by Developed vs Developing Countries",
       x = "Country Class",
       y = "Exchange Rate Change") +
  theme_minimal()

#Fitting linear regression in R
ppa<-lm(Exchange.rate.change~Inflation.difference,data = ppp)
summary(ppa)
#Does the theory hold?
#The principle of purchasing power parity (PPP) states:
#Average annual change in the exchange rate =Difference in average annual inflation rates  + random error 


  
  
  #The economic theory says that 𝛽_0=0, 〖  𝛽〗_1=1. 
#The estimates for these coefficients are: 𝛽 ̂_0=−1.519, 𝛽 ̂_1=0.961 

#Testing the theory:
  #𝛽_0=0: Based on the t-test of statistical significance we find that 𝛽_0 is statistically different from zero.

#𝛽_1=1: We need to perform a t-test with this as the null hypothesis:
  #T-value = ("β"  ̂_"1"  "−1" )/("se" ("β"  ̂_"1" )) = "0.9618−1" /"0.0178 "  "=" -2.1448
#p-value = 2(1-P(T39<|- 2.1448|)) = 0.038

library(car)

# Partial F-test for slope coefficient equaling 1
linearHypothesis(ppa,c(0,1),rhs=1)

# Alternatively, you can compute the t-value and p-value as follows:
tvalue = (0.9618-1)/0.01781
tvalue

pvalue = 2*(1-pt(abs(tvalue),39))
pvalue

#Function for fitted line plot
regplot.confbands.fun <- function(x,y,confidencelevel=.95,CImean=T,PI=T,CIregline=F,legend=F){
  #### Modified from a function written by Sandra McBride, Duke University
  #### For a simple linear regression line, this function
  #### will plot the line, CI for mean response, prediction intervals, 
  #### and (optionally) a simulataneous CI for the regression line.
  xx <- x[order(x)]
  yy <- y[order(x)]
  lm1 <- lm(yy~xx)    
  plot(xx,yy,ylim=c(min(yy),(max(yy)+.2*max(yy))),main="Fitted Line Plot",ylab="Exchange rate change",
       xlab="Inflation difference")
  abline(lm1$coefficients)
  #### calculation of components of intervals ####
  n <- length(yy)
  sx2 <- (var(xx))
  shat <- summary(lm1)$sigma
  s2hat <- shat^2
  SEmuhat <- shat*sqrt(1/n+ ((xx-mean(xx))^2)/((n-1)*sx2))
  SEpred <- sqrt(s2hat+SEmuhat^2)
  t.quantile <- qt(confidencelevel,lm1$df.residual)
  ####
  if (CImean==T){
    mean.up <- lm1$fitted+t.quantile*SEmuhat
    mean.down <- lm1$fitted-t.quantile*SEmuhat
    lines(xx,mean.up,lty=2)
    lines(xx,mean.down,lty=2)
  }
  if (PI==T){
    PI.up <- lm1$fitted+t.quantile*SEpred
    PI.down <- lm1$fitted-t.quantile*SEpred
    lines(xx,PI.up,lty=3)
    lines(xx,PI.down,lty=3)
  }
  if (CIregline==T){
    HW <- sqrt(2*qf(confidencelevel,n-lm1$df.residual,lm1$df.residual))*SEmuhat 
    CIreg.up <- lm1$fitted+HW
    CIreg.down <- lm1$fitted-HW
    lines(xx,CIreg.up,lty=4)
    lines(xx,CIreg.down,lty=4)
  }   
  if (legend==T){
    choices <- c(CImean,PI,CIregline)
    line.type <- c(2,3,4)
    names.line <- c("CI for mean resp.","Prediction Int.","CI for reg. line")
    legend(max(xx)-.2*max(xx),max(yy)+.2*max(yy),legend=names.line[choices],lty=line.type[choices])
  }
}
regplot.confbands.fun(ppp$Inflation.difference,ppp$Exchange.rate.change) 
#The fitted line plot shows several lines:
#The continuous line is the fitted regression line.
#The wider interrupted line band  is the prediction confidence band.
#The narrower interrupted line band  is the confidence band.
#The circles correspond to outliers.

#confidence and prediction interwal for new observation.
newppp = data.frame(Inflation.difference = c(-0.68))

predict(ppa,newppp,interval=c("confidence"))
predict(ppa,newppp,interval=c("prediction"))
#Interpretation of the two intervals:
#The 95% confidence limits of the average exchange rate change for all countries inflation difference equal to -0.68 are (-2.757,-1.590);

#The 95% confidence limits for the exchange rate change for one country with inflation difference equal to -0.68 are (-5.554,1.207). 
# 4 in 1 residual plot. 
par(mfrow=c(2,2))
plot(ppp$Inflation.difference, residuals(ppa),xlab="Inflation Difference",ylab="Residuals",main="Versus Predictor")
abline(h=0,lty=2)
plot(fitted(ppa),residuals(ppa),xlab="Fitted values",ylab="Residuals",main="Versus Fits")
abline(h=0,lty=2)
qqnorm(residuals(ppa))
abline(0,1,lty=1,col="red")
hist(residuals(ppa),main="Histogram of residuals",xlab="Residuals")
#Leverage Points: The isolated point in residual plots is Brazil. Why is Brazil a leverage point?
#Brazil had a period of hyperinflation from 1980 to 1994, a time period during which prices went up by a factor of roughly 1 trillion.
#Why do we care about leverage points?
#It can have a strong effect on the fitted regression, drawing the line away from the bulk of the points. It also can affect measures of fit like R-squared and t–statistics.
##### Repeat Analysis: Omit Brazil #########

newppp = ppp[ppp$Country!="Brazil",]
newppp
par(mfrow=c(1,1))
plot(newppp$Inflation.difference,newppp$Exchange.rate.change, main="Scatterplot of Exchange rate change 
     vs Inflation difference", xlab="Inflation difference",ylab="Exchange rate change")
pppn = lm(newppp$Exchange.rate.change ~ newppp$Inflation.difference)
summary(pppn)

linearHypothesis(pppn,c(0,1),rhs=1)
### Test whether the slope is equal to 1 (PPP theory)
tvalue = (0.9915-1)/ 0.02626
tvalue
pvalue = 2*(1-pt(abs(tvalue),38))
pvalue
# We are seeing violations of PPP with respect to intercept only.
# 4 in 1 residual plot. 
par(mfrow=c(2,2))
plot(newppp$Inflation.difference, residuals(pppn),xlab="Inflation Difference",ylab="Residuals",main="Versus Predictor")
abline(h=0,lty=2)
plot(fitted(pppn),residuals(pppn),xlab="Fitted values",ylab="Residuals",main="Versus Fits")
abline(h=0,lty=2)
qqnorm(residuals(pppn))
abline(0,1,lty=1,col="red")
hist(residuals(pppn),main="Histogram of residuals",xlab="Residuals")           

#Assumptions:
#Linearity: No pattern in the residuals with respect to the predicting variable.
#Constant Variance: The variance is higher for higher fitted values. Does not hold.
#Uncorrelated Errors: No grouping of the residuals
#Normality: Except for the presence of an outlier, it is reasonably symmetric.

#Outliers (observations for which the residual value is away from the range):
#The isolated point in the residual plots is Indonesia. Would omitting Indonesia change anything? The strength of the relationship would increase, but so the rejection of PPP.

## Split data

#attach(newppp)
developed = newppp[newppp$Developed=="Developed",] #20 developed countries
developed
developing = newppp[newppp$Developed=="Developing",] #20 developing countries

## Developed Country

attach(developed)
pppb = lm(developed$Exchange.rate.change ~ developed$Inflation.difference, data=developed)
summary(pppb)

linearHypothesis(pppb,c(0,1),rhs=1)

par(mfrow=c(2,2))
plot(developed$Inflation.difference, residuals(pppb),xlab="Inflation Difference",ylab="Residuals",main="Versus Predictor")
abline(h=0,lty=2)
plot(fitted(pppb),residuals(pppb),xlab="Fitted values",ylab="Residuals",main="Versus Fits")
abline(h=0,lty=2)
qqnorm(residuals(pppb))
abline(0,1,lty=1,col="red")
hist(residuals(pppb),main="Histogram of residuals",xlab="Residuals")
```

## Developing Country
```{r}
#attach(developing)
plot(developing$Inflation.difference,developing$Exchange.rate.change,
     main="Scatterplot of Exchange rate change 
     vs Inflation difference",
     xlab="Inflation difference",ylab="Exchange rate change")
pppc = lm(Exchange.rate.change ~ Inflation.difference, data=developing)
summary(pppc)
anova(pppc)
linearHypothesis(pppc,c(0,1),rhs=1)

par(mfrow=c(2,2))
plot(developing$Inflation.difference, residuals(pppc),xlab="Inflation Difference",ylab="Residuals",main="Versus Predictor")
abline(h=0,lty=2)
plot(fitted(pppc),residuals(pppc),xlab="Fitted values",ylab="Residuals",main="Versus Fits")
abline(h=0,lty=2)
qqnorm(residuals(pppc))
abline(0,1,lty=1,col="red")
hist(residuals(pppc),main="Histogram of residuals",xlab="Residuals")
```

## Omit Indonesia
```{r}
newdeveloping = developing[developing$Country!="Indonesia",]
#attach(newdeveloping)
plot(newdeveloping$Inflation.difference,newdeveloping$Exchange.rate.change,
     main="Scatterplot of Exchange rate change 
     vs Inflation difference",
     xlab="Inflation difference",ylab="Exchange rate change")
pppd = lm(Exchange.rate.change ~ Inflation.difference, data= newdeveloping)
summary(pppd)
anova(pppd)
linearHypothesis(pppd,c(0,1),rhs=1)

par(mfrow=c(2,2))
plot(newdeveloping$Inflation.difference, residuals(pppd),xlab="Inflation Difference",ylab="Residuals",main="Versus Predictor")
abline(h=0,lty=2)
plot(fitted(pppd),residuals(pppd),xlab="Fitted values",ylab="Residuals",main="Versus Fits")
abline(h=0,lty=2)
qqnorm(residuals(pppd))
abline(0,1,lty=1,col="red")
hist(residuals(pppd),main="Histogram of residuals",xlab="Residuals")
#Findings:
#Support is decidedly mixed
#Developed countries: 
  #Changes in inflation difference do seem to be balanced by exchange rate changes
#One outlier: Greece
#Developing countries: 
  #The case for PPP is considerably weaker;
#Brazil and Indonesia
#PPP is not robust to unusual economic or political conditions 
