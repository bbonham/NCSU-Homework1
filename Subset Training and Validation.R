library(forecast)

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
tusts <- ts(tus.tr[,2],frequency=1)
plot(tusts, xlab ='Weeks', ylab='Weekly Sales', main = 'Tucson Sales from 9/18/12 to 9/4/17')
phxts <- ts(phx.tr[,2],frequency=1)
plot(phxts, xlab ='Weeks', ylab='Weekly Sales', main = 'Phoenix Sales from 9/18/12 to 9/4/17')


