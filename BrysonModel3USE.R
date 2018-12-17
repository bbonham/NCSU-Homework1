library(haven)
library(forecast)
library(fma)
library(tseries)
library(expsmooth)
library(lmtest)
library(zoo)

install.packages('MLmetrics')
install.packages('TSPred')

setwd('C:/Users/bryso/OneDrive - North Carolina State University/HWTeam (Fall 1)/TS HW1')

#####
#Time Series of our Training Dataset
#Training data set stored as harr
#Validation data set stored as harrval

TS <- ts(harr$temp, frequency =24)

# Time Series Decomposition, initalizes for ESM #
model <- stl(TS, s.window = 7)
plot(model)

day = strftime(harr$datetime, '%d')
hour = strftime(harr$datetime, '%H')
harr = cbind(day, hour, harr)


# Building a Single Expontential Smoothing Model - harr #
# Model 1 #
SES.TS <- ses(TS, initial = "optimal", seasonal = 'additive', h=23)
summary(SES.TS)
plot(SES.TS)

# Building a Linear Exponential Smoothing Model - harr #
# Model 2 #
LES.TS <- holt(TS, initial = "optimal", h=23)
summary(LES.TS)
plot(LES.TS)

# Building a Linear Damped Exponential Smoothing Model - harr #
# Model 3 #
LDES.TS <- holt(TS, initial = "optimal", h = 23, damped = TRUE)
summary(LDES.TS)
plot(LDES.TS)

# Building a Holt-Winters ESM - harr #
# Model 4 #
HWES.TS <- hw(TS, seasonal = "additive", h=23)
summary(HWES.TS)
plot(HWES.TS)

# Building a Holt-Winters ESM - harr #
# Model 4 #
HWESM.TS <- hw(TS, seasonal = "multiplicative", h=23, damped =TRUE)
summary(HWESM.TS)
plot(HWESM.TS)



#Grabs predicted values from model
#Change the 'word before .ts' to the model you wish to run 
#EXAMPLE - HWESM.TS to HWES.TS
#################################
#################################
#################################
Transfer = HWES.TS

################################
#Plotting Section don't touch!##

summary(Transfer)
Transfer.Subset = Transfer$mean
Predicted = c(Transfer.Subset)
Predicted
Timestmp = c(Transfer$x)
Predicted

harrval2 = harrval
harrval2$temppredicted = Predicted

Predicted
plot (Predicted)
Delta = Actual - Predicted
TimePoint = harrval2$datetime2
TableofStuff = data.frame(Actual, Predicted, Delta, TimePoint)
rownames(TableofStuff, "")
write.csv(TableofStuff, file='Valid.csv')
getwd()

write.csv(Transfer, file='Transfer.csv')

#Plots of Predicted and Actual
#Don't touch, just change in above code

Upper = Transfer$upper
CIUp = Upper[,2]
CILow = Transfer$lower[,2]
CI=data.frame(as.numeric(CIUp), as.numeric(CILow))
plot(CI[,2])
points(CI[,1])




plot(harrval2$datetime2, harrval2$temp2, main = "Forecasted Temperature of Aug 27, 2017 with Confidence Interval", xlab = "Time", ylab = "Temperature (F)")
points(harrval2$datetime2, harrval2$temppredicted)
lines(harrval2$datetime2, harrval2$temppredicted, col='blue', lwd=2.5)
lines(harrval2$datetime2, harrval2$temp2, col='red', lwd=2.5)
legend('topleft', y=NULL, legend=c('Predicted','Actual'), lty=1, col=c('blue','red'), bty='n')
#points(harrval2$datetime2, CI[,2])
#points(harrval2$datetime2, CI[,1])
polygon(c(harrval2$datetime2, rev(harrval2$datetime2)), lty = 0, c((CI[,2]), rev(CI[,1])), col=rgb(0,0,.5,alpha=0.1))
?polygon

