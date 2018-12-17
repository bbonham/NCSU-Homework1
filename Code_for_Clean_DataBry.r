################################
####### LOADING DATA ###########
################################

install.packages("readr")
library(readr)

rawdata <- readxl::read_excel("C:\\Users\\bryso\\OneDrive - North Carolina State University\\HWTeam (Fall 1)\\TS HW1\\Harrisburg_TRAIN.xlsx")

# Extracting only relevant variables TEMP and YR_MODAHRMN #
# Variable YR_MODAHRMN format converted to datetime #
# Variable TEMP converted to numeric #
datetime <- as.POSIXct(strptime(rawdata$'YR--MODAHRMN', '%Y%m%d%H%M'))
temp <- as.numeric(rawdata$TEMP)

# Creating new variables date and time which extract only the date and minute from datetime #
date <- as.Date(strptime(rawdata$'YR--MODAHRMN', '%Y%m%d'))
minute <- strftime(datetime, '%M')

################################
####### CLEANING DATA ##########
################################


# New formatted variables datetime and temp stored in new data frame: harr along with new #
# variables date and minute #
harr <- data.frame(datetime, temp, date, minute)

# Checking for 24 hourly readings per day #
table(harr$date) 
# Shows there are more than 24 hourly readings per day #
# I noticed a pattern that readings are usually taken at the 56th minute of every hour #
# Occasionally an additional reading is taken seconds either before or after the 56th minute #

# Subset for times where minutes == 56 #
harr <- harr[harr$minute==56, ]
table(harr$date) 
# Now each day has only 24 hourly readings at the 56th minute of every hour #

# Removing variables date and min #
harr <- harr[, c(1:2)]