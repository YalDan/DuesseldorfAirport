## load libraries

library(data.table)
library(lubridate)
library(ggplot2)
library(forecast)
library(openxlsx)
library(imputeTS)
library(readxl)

#load excel file
wb <- loadWorkbook("Monheim.xlsx")

#write sheets into a list of data frames
name <- names(wb)
P1 <- c("t1","t2","t3","t8","t9","t10")
P2 <- c("t1","t2","t4","t5","t6","t7","t9","t10")
P3 <- c("t1","t2","t4","t11","t12","t13")
P <- list("P1" = P1, "P2"=P2, "P3"=P3)

df <- lapply(name, function(x) readWorkbook(wb, sheet = x, startRow = 1, colNames = TRUE, 
                  rowNames = FALSE, detectDates = TRUE, skipEmptyRows = TRUE, skipEmptyCols = TRUE, 
                  rows = NULL, cols = NULL, check.names = FALSE, namedRegion = NULL, 
                  na.strings = "NA", fillMergedCells = FALSE))

#assign names to columns
names(df) <- name
str(df)

#merge DFs into one list
DT <- rbindlist(df)

#convert columns
DT[, datetime := as.POSIXct( DT$datetime*24*3600 + as.POSIXct("1899-12-30 00:00", 
                                                tz="Etc/GMT-2"), tz="Etc/GMT-1")]

#add dare column
DT[, date := as.Date(DT$datetime, tz="Etc/GMT-1", format= "%d-%m-%Y")]
DT[, value := gsub("[^0-9]", "", DT$value)]
DT[, value := as.numeric(DT$value)]
DT[, value := na.locf(DT$value, option = "locf", na.remaining = "rev")]
str(DT)


# check how many segments contain full data
count_ID <- DT[, .N, date]
full <- count_ID[N == max(N), .(date)]
nrow(full) # number of extracted IDs
period <- 144

# add weekdays
DT[, week := weekdays(datetime)]
unique(DT[, week])

# extract relevant vectors
n_weekdays <- unique(DT[, week])
n_date <- unique(DT[, date])

## create an overview for the sections
date <- data.table(DT[ID %in% "t1", datetime])
sP <- list(date)
for (i in 1:length(P))
{
  sP[[i+1]] <- lapply(P[[i]], function(x) data.table(DT[ID %in% x, value]))
}

sP1 <- data.table(as.data.table(sP[[1]]), as.data.table(sP[[2]]))
names(sP1) <- c("datetime", P1)
sP1[ ,sum := rowSums(.SD), .SDcols = 2:7]
sP1[, date := as.Date(sP1$datetime, tz="Etc/GMT-1", format= "%d-%m-%Y")]
sP1[, week := weekdays(sP1$datetime)]
sP2 <- data.table(as.data.table(sP[[1]]), as.data.table(sP[[3]]))
names(sP2) <- c("datetime", P2)
sP2[ ,sum := rowSums(.SD), .SDcols = 2:9]
sP2[, date := as.Date(sP2$datetime, tz="Etc/GMT-1", format= "%d-%m-%Y")]
sP2[, week := weekdays(sP2$datetime)]
sP3 <- data.table(as.data.table(sP[[1]]), as.data.table(sP[[4]]))
names(sP3) <- c("datetime", P3)
sP3[ ,sum := rowSums(.SD), .SDcols = 2:7]
sP3[, date := as.Date(sP3$datetime, tz="Etc/GMT-1", format= "%d-%m-%Y")]
sP3[, week := weekdays(sP3$datetime)]

## get a table to compare minimum values

minsr <- data.table("P1" = sP1$sum, "P2" = sP2$sum, 
                   "P3" = sP3$sum)
minsr[, min:= apply(minsr[,1:3], 1, which.min)]
minsr[, time:=sP3$datetime]
print(minsr)


### Additional methods for estimation
# ## STL + EXP
# stlEXPPred <- function(Y, period = 48){
#   ts_Y <- ts(Y, start = 0, freq = period) # Transform to ts
#   dekom <- stl(ts_Y, s.window = "periodic", robust = T) # STL decomposition
#   expo <- forecast(dekom, h = period, method = "ets", etsmodel = "ZZN") # Predict decomposed time series with Exponential Smoothing
#   return(as.vector(expo$mean))
# }
# 
# ## HW and snaive:
# ## Holt-Winters ES
# HWPred <- function(Y, period = 48){
#   ts_Y <- ts(Y, start = 0, freq = period) # Transform to ts
#   HW <- HoltWinters(ts_Y, beta = F, seasonal = "additive") # Holt-Winters ES , alpha = 0.15, gamma = 0.95
#   HW_pred <- forecast(HW, period) # Predict
#   return(as.vector(HW_pred$mean))
# }
# 
# ## Seasonal naive forecast
# snaivePred <- function(Y, period = 48){
#   ts_Y <- ts(Y, start = 0, freq = period)
#   naive_pred <- snaive(ts_Y, period)
#   return(as.vector(naive_pred$mean))
# }

## STL + ARIMA
stlARIMAPred <- function(Y, period = 144){ #dummyperiod
  ts_Y <- ts(Y, start = 0, freq = period)
  dekom <- stl(ts_Y, s.window = "periodic", robust = TRUE)
  arima <- forecast(dekom, h = period, method = "arima")
  return(as.vector(arima$mean))
}
## Create a function that stores the fitted models
stlARIMAFits <- function(Y, period = 144){ #dummyperiod
  ts_Y <- ts(Y, start = 0, freq = period)
  dekom <- stl(ts_Y, s.window = "periodic", robust = TRUE)
  arima <- forecast(dekom, h = period, method = "arima")
  fits <- arima
  return(fits)
}


#create function that predicts forecast for a week
predictWeek <- function(data, set_of_date, FUN, train_win = 0){
  
  for_mon <- FUN(data[(week == n_weekdays[4] & date %in% set_of_date), value])
  for_tue <- FUN(data[(week == n_weekdays[5] & date %in% set_of_date), value])
  for_wed <- FUN(data[(week == n_weekdays[6] & date %in% set_of_date), value])
  for_thu <- FUN(data[(week == n_weekdays[7] & date %in% set_of_date), value])
  for_fri <- FUN(data[(week == n_weekdays[1] & date %in% set_of_date), value])
  for_sat <- FUN(data[(week == n_weekdays[2] & date %in% set_of_date), value])
  for_sun <- FUN(data[(week == n_weekdays[3] & date %in% set_of_date), value])
  
  return(c(for_mon, for_tue, for_wed, for_thu, for_fri, for_sat, for_sun))
}


## create forecast for road segments and write them into data table

Arimas <- lapply(name, function (x) predictWeek(DT[ID %in% x], n_date[2:24], stlARIMAPred) ) 
Arimas <- as.data.table(Arimas)
names(Arimas) <- name

# create list with different paths

ArimaP <- list()
for (i in 1:length(P)){
  ArimaP[[i]] <- lapply(P[[i]], function(x) data.table(Arimas[,get(x)]))
}

sAP1 <- as.data.table(ArimaP[[1]])
names(sAP1) <- P1
sAP1[ ,sum := rowSums(.SD), .SDcols = 1:6]
sAP2 <- as.data.table(ArimaP[[2]])
names(sAP2) <- P2
sAP2[ ,sum := rowSums(.SD), .SDcols = 1:8]
sAP3 <- as.data.table(ArimaP[[3]])
names(sAP3) <- P3
sAP3[ ,sum := rowSums(.SD), .SDcols = 1:6]


## Show path with the minimum travel time
mins <- data.table("P1" = sAP1$sum, "P2" = sAP2$sum, 
          "P3" = sAP3$sum)
mins[, min:= apply(mins[,1:3], 1, which.min)]
mins[min %in% "3", .N]
mins[, time:=t]

View(mins)

## Add timestamps for illustration
t <- seq(as.POSIXct("2017-11-06 00:00:00", tz="Etc/GMT-2"), as.POSIXct("2017-11-12 23:50:00", tz="Etc/GMT-2"), by="10 mins")
sAP1[,datetime:=t]
sAP1[,date:=as.Date(t)]
sAP2[,datetime:=t]
sAP2[,date:=as.Date(t)]
sAP3[,datetime:=t]
sAP3[,date:=as.Date(t)]


## Create a list that contains the fitted models
Fits <- lapply(name, function (x) predictWeek(DT[ID %in% x], n_date[2:24], stlARIMAFits) ) 


### Data analysis ###

## add hours
sP3[, hour := hour(sP3$datetime)]
n_hours <- unique(sP3[date %in% n_date[2], hour])


## Cross Validation

# load data
data <- read_excel("MonheimOld.xlsx", col_types = c("text", "numeric", "text"))

# create dataframe with used variables
mn <- data.frame("data.time" = data$datetime, data$durtr, data$dist)

# parse timestamps into POSIXct
tt <- parse_date_time(mn$data.time, "mdy_HMS")
mn$data.time <- tt
mn$date <- as.Date(mn$data.time)

# parse to data.table
mn <- as.data.table(mn)
View(mn)

# check how many values for each day
num_date <- mn[(data.time != is.na(data.time) & data.dist %in% "42.8 km"), .N, .(date)]
table(num_date[, N])

mn[, hour := hour(mn$data.time)]
mn[, week := weekdays(data.time)]
mn_weekdays <- unique(mn[, week])
mn_date <- unique(mn[, date])
mn_hours <- unique(mn[, hour])
summary(mn[hour == "8" & week == mn_weekdays[1], data.durtr])

# replace NAs
mn$data.durtr <- na.locf(mn$data.durtr, option = "locf", na.remaining = "rev")

# aggregate values
DT_144 <- mn[data.dist %in% "42.8 km"]
cprdata <- function(DATE){
DTst <- list()
for (i in 1:7)
{
  x <-  DT_144[(data.time != is.na(data.time) & date %in% mn_date[DATE+i-1]), .N]
  y <- x/144
  temp <- DT_144[date %in% mn_date[DATE+i-1], data.durtr, by = (seq(nrow(DT_144[date %in% mn_date[DATE+i-1]])) - 1) %/% y]
  temp <- temp[seq(1, nrow(temp), by = y)]
  temp[, seq := NULL]
  DTst[i] <- temp
}
return(as.data.table(unlist(DTst)))
}

## Create a function for comparing plots

compplot <- function(DATA, LVL){
cmpre <- data.table(value = c(sAP3$sum, DATA$V1+LVL),
                    date = c(rep(t,2)),
                    type = c(rep("ARIMA", length(sAP3$sum)), rep("GOOGLE", length(DATA$V1))))

ggplot(data = cmpre, aes(date, value, group = type, colour = type)) +
  geom_line(size = 0.8) +
  theme(panel.border = element_blank(), panel.background = element_blank(),
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        title = element_text(size = 14),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Date", y = "Travel time",
       title = "Comparison of ARIMA and Google")
}

# Plot different weeks
t <- seq(as.POSIXct("2017-09-11 00:00:00", tz="Etc/GMT-1"), as.POSIXct("2017-09-17 23:50:00", tz="Etc/GMT-1"), by="10 mins")
compplot(cprdata(26),5)

t <- seq(as.POSIXct("2017-09-18 00:00:00", tz="Etc/GMT-1"), as.POSIXct("2017-09-24 23:50:00", tz="Etc/GMT-1"), by="10 mins")
compplot(cprdata(33),5)

t <- seq(as.POSIXct("2017-09-25 00:00:00", tz="Etc/GMT-1"), as.POSIXct("2017-10-01 23:50:00", tz="Etc/GMT-1"), by="10 mins")
compplot(cprdata(40),5)



## Mean Absolute Percentage Error
mape <- function(real, pred){
  return(100 * mean(abs((real - pred)/real)))
}

## Find the best level shift with MAPEs

# Compare whole week's MAPE for different level shifts
checkmape <- function(DATE){
mapec <- list()
for (j in 1:6){
mapec[j] <- mape(cprdata(DATE)$V1+j, sAP3$sum)
}
return(mapec)
}

# Compare single days' MAPE for different level shifts
checkwmape <- function(DATE){
  mapec <- list()
  for (j in 1:5){
   mapec[[j]] <- sapply(0:6, function(i) mape((cprdata(DATE)$V1+j)[((i*period)+1):((i+1)*period)], sAP3$sum[((i*period)+1):((i+1)*period)]))
  }
  return(mapec)
}

# View comparisons
ldate <- c("9/11 - 9/17" = 26,"9/18 - 9/24" = 33,"9/26 - 10/01" = 40)

aggmape <- sapply(ldate, function(x) checkmape(x))
aggwmape <- lapply(ldate, function(x) checkwmape(x))

print(aggmape)
print(aggwmape)

## Compare time series of the different paths

cmpre <- data.table(value = c(sP1$sum, sP2$sum, sP3$sum),
                    date = c(rep(sP3[, datetime],3)),
                    type = c(rep("P1", length(sP1$sum)), rep("P2", length(sP2$sum)), rep("P3", length(sP3$sum))))

ggplot(data = cmpre, aes(date, value, group = type, colour = type)) +
  geom_line(size = 0.8) +
  theme(panel.border = element_blank(), panel.background = element_blank(),
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        title = element_text(size = 14),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(x = "Date", y = "Travel time",
       title = "Comparison of different ways predicted by Google")

## Compare traffic for weekday with same day of prior week
stats <- data.table(sP1$datetime, sP1$t9)
stats[, hour:= hour(sP1$datetime)]
stats[, date:= as.Date(sP1$datetime)]

calcdiff <- function(fordate){
  x <- stats[(hour %in% n_hours[8:21] & date %in% n_date[fordate]), V2]
  y <- stats[(hour %in% n_hours[8:21] & date %in% n_date[fordate-7]), V2]
  return((x/y-1)*100)
}

# View comparison for all weekdays
sts <- data.table(stats[(hour %in% n_hours[8:21] & date %in% n_date[11]), hour], calcdiff(11), calcdiff(12), calcdiff(13), calcdiff(14), calcdiff(15))
names(sts) <- c("hour","10/16","10/17","10/18","10/19","10/20")
View(sts)

# View traffic only on certain trajectory
tst <- DT[(week %in% "Freitag" & ID %in% "t13")]
summary(tst)
View(tst)


# View Monday
sPP <- sP1[date %in% n_date[32], sum]
sAPP <- sAP1[,sum][1:length(sPP)]
sDTE <- sAP1[,datetime][1:length(sPP)]

# View Wednesday
sPP <- sP3[date %in% n_date[34], sum]
sAPP <- sAP3[,sum][289:(288 + length(sPP))]
sDTE <- sAP1[,datetime][289:(288 + length(sPP))]

tst <- data.table(value = c(sAPP, sPP), 
                  date = c(rep(sDTE, 2)),
                  type = c(rep("ARIMA", length(sAPP)), rep("GOOGLE", length(sPP))))

ggplot(data=tst, aes(date, value, group = type, colour = type)) +
  geom_line(size = 0.8) +
  theme(panel.border = element_blank(), panel.background = element_blank(),
        panel.grid.minor = element_line(colour = "grey90"),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.major.x = element_line(colour = "grey90"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        strip.text = element_text(size = 12, face = "bold")) +
  labs(x = "Date", y = "Travel time")

