################################
####### LOADING DATA ###########
################################

install.packages("readr")
library(readr)

rawdata <- readxl::read_excel("C:\\Users\\bryso\\OneDrive - North Carolina State University\\HWTeam (Fall 1)\\TS HW1\\Harrisburg_VALID.xlsx")

# Extracting only relevant variables TEMP and YR_MODAHRMN #
# Variable YR_MODAHRMN format converted to datetime #
# Variable TEMP converted to numeric #
datetime2 <- as.POSIXct(strptime(rawdata$'YR--MODAHRMN', '%Y%m%d%H%M'))
temp2 <- as.numeric(rawdata$TEMP)

# Creating new variables date and time which extract only the date and minute from datetime #
date2 <- as.Date(strptime(rawdata$'YR--MODAHRMN', '%Y%m%d'))
minute2 <- strftime(datetime2, '%M')

# New formatted variables datetime and temp stored in new data frame: harrval along with new #
# variables date and minute #
harrval <- data.frame(datetime2, temp2, date2, minute2)

# Checking for 24 hourly readings per day #
table(harrval$date) 
# Shows there are more than 24 hourly readings per day #
# I noticed a pattern that readings are usually taken at the 56th minute of every hour #
# Occasionally an additional reading is taken seconds either before or after the 56th minute #

# Subset for times where minutes == 56 #
harrval <- harrval[harrval$minute==56, ]
table(harrval$date) 
# Now each day has only 24 hourly readings at the 56th minute of every hour #

# Removing variables date and min #
harrval <- harrval[, c(1:2)]
