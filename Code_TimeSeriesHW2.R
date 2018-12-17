
library(haven)
library(forecast)
library(fma)
library(tseries)
library(expsmooth)
library(lmtest)
library(zoo)
library(TSPred)
library(MLmetrics)

#############################################
#                                           #
#               Cleaning Data               #
#                                           #
#############################################

#     Reading and creating separate data frames for 2 cities      
rawdata <- read.csv(file='C:/Users/bryso/OneDrive - North Carolina State University/HWTeam (Fall 1)/TS HW2/AZ_SALES.csv', header=TRUE, sep=",")
rawdata[,1] <- as.Date(rawdata[,1], format= '%m/%d/%Y') #convert to date

#     data contains entries beyond 9/4 so subsetting to correct date range
tus<- data.frame(rawdata[rawdata[,1] <= as.Date('09/04/2017', format= '%m/%d/%Y'),c(1,3)])
phx <- data.frame(rawdata[rawdata[,1] <= as.Date('09/04/2017', format= '%m/%d/%Y'),1:2])

#     Getting training and validation
a <- length(tus[,1])-16   #getting all values except last 16
b <- length(tus[,1])      
tus.tr <- tus[1:a,]
phx.tr <- phx[1:a,]
tus.val <- tus[(a + 1):b,]
phx.val <- phx[(a+1):b,]

#     Visualizing weekly sales for each location    
tusts <- ts(tus.tr[,2], start=c(2012,38), frequency=365.25/7)
plot(tusts, xlab ='Date', ylab='Weekly Sales', main = 'Tucson Sales from 9/18/12 to 9/4/17')
phxts <- ts(phx.tr[,2],start=c(2012,38), frequency=365.25/7)
plot(phxts, xlab ='Date', ylab='Weekly Sales', main = 'Phoenix Sales from 9/18/12 to 9/4/17')

#############################################
#                                           #
#           Building ESM Models             #
#                                           #
#############################################

#     Tuscon data is not seasonal and it has a trend --> use Holt ESM
holt.tu <- holt(tusts, initial = 'optimal', damped=FALSE, h=16)
summary(holt.tu)
plot(holt.tu, main = "Tuscon Sales - Holt Linear ESM Forecast", xlab = "Date", ylab = "Sales")

#     Phoenix data is not seasonal and does not have a trend --> use Simple ESM
model <- stl(phxts, s.window=7)
plot(model)
ses.phx <- ses(phxts, initial = "optimal", h = 16)
summary(ses.phx)
plot(ses.phx, main = "Phoenix Sales - Simple ESM Forecast", xlab = "Date", ylab = "Sales")


#############################################
#                                           #
#           Checking Stationarity           #
#                                           #
#############################################

#     Tuscon data is not seasonal and has trend --> ADF Trend test 
#     Null: series has a unit root
#     Alternative: series has stationarity
ADF.Pvalues.tus <- rep(NA, 3)
for(i in 0:2){
  ADF.Pvalues.tus[i+1] <- adf.test(tusts, alternative = "stationary", k = i)$p.value
}
#     Reject null --> stationary around trend up to lag 2 
#     Deterministic trend - Recommend adding a trendline


#     Phoenix data is not seasonal and does not have trend --> Single mean ADF
ADF.Pvalues.phx <- rep(NA, 3)
for(i in 0:2){
  ADF.Pvalues.phx[i+1] <- adf.test(tusts, alternative = "stationary", k = i)$p.value
}
#     Reject null --> stationary around mu
#     Model is good - no further recommendations

?kpss.test
kpss.test(tus[,2], null='Trend')

kpss.test(phx[,2], null='Level')

#############################################
#                                           #
#                 Validation                #
#                                           #
#############################################

#     Accuracy measures
MAPE(holt.tu$mean, tus.val$SALES_TU)
MAPE(ses.phx$mean, phx.val$SALES_PH)

#     Plotting predicted and actual sales
pred.tu <- as.numeric(holt.tu$mean)
pred.phx <- as.numeric(ses.phx$mean)

plot(tus.val$SALES_TU, col='lightblue', lwd=3.5, main='Tuscon Sales - 16 Week Forecast', 
     ylab='Sales (Thousands of Dollars)', 
     xlab='Week',
     ylim=c(70,120))
lines(tus.val$SALES_TU, col='lightblue', lwd=3.5)
points(pred.tu, col='darkblue', lwd=3.5)
lines(pred.tu, col='darkblue', lwd=3.5)
legend('topleft', y=NULL, legend=c('Actual','Predicted'), 
       lty=1, lwd=3.5, col=c('lightblue','darkblue'), bty='n')

plot(phx.val$SALES_PH, col='lightblue', lwd=3.5, main='Phoenix Sales - 16 Week Forecast', 
     ylab='Sales (Thousands of Dollars)', 
     xlab='Week',
     ylim=c(90,140))
lines(phx.val$SALES_PH, col='lightblue', lwd=3.5)
points(pred.phx, col='darkblue', lwd=3.5)
lines(pred.phx, col='darkblue', lwd=3.5)
legend('topleft', y=NULL, legend=c('Actual','Predicted'), 
       lty=1, lwd=3.5, col=c('lightblue','darkblue'), bty='n')


###############################################################################################

