library(pacman)

p_load(tidyverse,lubridate,stringr,scales,cowplot,dygraphs,xts,imputeTS,forecast,zoo,fma,expsmooth,fpp2,doParallel)
#######################

# Prepare Parallel Process
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

#######################

#import the data
household_power_consumption <- read_delim("C:\\Users\\Javier Villasmil\\Desktop\\Ubiqum\\Repositories\\Time Series Analysis - Household Power Consumption\\1. Dataset\\household_power_consumption.txt", ";", 
                                          escape_double = FALSE, 
                                          trim_ws = TRUE)

originalhpc <- read_delim("C:\\Users\\Javier Villasmil\\Desktop\\Ubiqum\\Repositories\\Time Series Analysis - Household Power Consumption\\1. Dataset\\household_power_consumption.txt", ";", 
                          escape_double = FALSE, 
                          trim_ws = TRUE)

#join the data and time column as.POSIXct class. change the time zone so all time saving values are taken into account
household_power_consumption$DateTime <- as.POSIXct(paste(household_power_consumption$Date, household_power_consumption$Time), format="%d/%m/%Y %H:%M:%S", tz = "GMT")
#switch the order of columns
household_power_consumption <- household_power_consumption[,c(10,1:9)]
#change the $date column class to "date"
household_power_consumption$Date <- as.Date(household_power_consumption$Date, "%d/%m/%Y")

#inspect the data frame.
head(household_power_consumption)
summary(household_power_consumption)

#remove 2006 values
index2006 <- which(year(household_power_consumption$DateTime)== 2006) 
household_power_consumption <- household_power_consumption[-index2006,]
glimpse(household_power_consumption)

#count registers per each year
df1 <- count(household_power_consumption,year(DateTime))

#count NA's per each year
df2 <- household_power_consumption %>%
  filter(is.na(household_power_consumption$Global_active_power)) %>%
  count(year(DateTime))

df1 
df2
sum(df1$n)
sum(df2$n)

#add one hour for the Daylight Saving Time for each year
onehour <- hours(1)

###################################

#2007#
x2k7index <- which(household_power_consumption$DateTime >= as.POSIXct("2007-03-25 02:00:00",tz = "GMT") & household_power_consumption$DateTime <= as.POSIXct("2007-10-28 02:00:00",tz = "GMT"))
household_power_consumption$DateTime[x2k7index] <- household_power_consumption$DateTime[x2k7index] + onehour
#averaging the values#
x2k7newvalues <- household_power_consumption %>%
  filter(DateTime >=  as.POSIXct("2007-10-28 02:00:00",tz = "GMT") & DateTime <= as.POSIXct("2007-10-28 03:00:00",tz = "GMT")) %>%
  group_by(DateTime) %>%
  summarise_all(funs(mean))
#remove overlapping#
x2k7overlapindex <- which(household_power_consumption$DateTime >= as.POSIXct("2007-10-28 02:00:00",tz = "GMT") & household_power_consumption$DateTime <= as.POSIXct("2007-10-28 03:00:00",tz = "GMT"))
x2k7overlapindex
#add the avaraged values to the dataset#
household_power_consumption <- household_power_consumption[-x2k7overlapindex,]
household_power_consumption <- bind_rows(household_power_consumption, x2k7newvalues)


ds1 <- household_power_consumption %>%
  filter(year(DateTime)==2007)
View(ds1)
###################################


#2008#
x2k8index <- which(household_power_consumption$DateTime >= as.POSIXct("2008-03-30 02:00:00",tz = "GMT") & household_power_consumption$DateTime <= as.POSIXct("2008-10-26 02:00:00",tz = "GMT"))
household_power_consumption$DateTime[x2k8index] <- household_power_consumption$DateTime[x2k8index] + onehour
#averaging the values#
x2k8newvalues <- household_power_consumption %>%
  filter(DateTime >=  as.POSIXct("2008-10-26 02:00:00",tz = "GMT") & DateTime <= as.POSIXct("2008-10-26 03:00:00",tz = "GMT")) %>%
  group_by(DateTime) %>%
  summarise_all(funs(mean))
#remove overlapping#
x2k8overlapindex <- which(household_power_consumption$DateTime >= as.POSIXct("2008-10-26 02:00:00",tz = "GMT") & household_power_consumption$DateTime <= as.POSIXct("2008-10-26 03:00:00",tz = "GMT"))
x2k8overlapindex
#add the avaraged values to the dataset#
household_power_consumption <- household_power_consumption[-x2k8overlapindex,]
household_power_consumption <- bind_rows(household_power_consumption, x2k8newvalues)

ds2 <- household_power_consumption %>%
  filter(year(DateTime)==2008)
View(ds2)
###################################


#2009
x2k9index <- which(household_power_consumption$DateTime >= as.POSIXct("2009-03-29 02:00:00",tz = "GMT") & household_power_consumption$DateTime <= as.POSIXct("2009-10-25 02:00:00",tz = "GMT"))
household_power_consumption$DateTime[x2k9index] <- household_power_consumption$DateTime[x2k9index] + onehour
#averaging the values#
x2k9newvalues <- household_power_consumption %>%
  filter(DateTime >=  as.POSIXct("2009-10-25 02:00:00",tz = "GMT") & DateTime <= as.POSIXct("2009-10-25 03:00:00",tz = "GMT")) %>%
  group_by(DateTime) %>%
  summarise_all(funs(mean))
#remove overlapping#
x2k9overlapindex <- which(household_power_consumption$DateTime >= as.POSIXct("2009-10-25 02:00:00",tz = "GMT") & household_power_consumption$DateTime <= as.POSIXct("2009-10-25 03:00:00",tz = "GMT"))
x2k9overlapindex
#add the avaraged values to the dataset#
household_power_consumption <- household_power_consumption[-x2k9overlapindex,]
household_power_consumption <- bind_rows(household_power_consumption, x2k9newvalues)

ds3 <- household_power_consumption %>%
  filter(year(DateTime)==2009)
View(ds3)
###################################


#2010
x2k10index <- which(household_power_consumption$DateTime >= as.POSIXct("2010-03-28 02:00:00",tz = "GMT") & household_power_consumption$DateTime <= as.POSIXct("2010-10-31 02:00:00",tz = "GMT"))
household_power_consumption$DateTime[x2k10index] <- household_power_consumption$DateTime[x2k10index] + onehour
#averaging the values#
x2k10newvalues <- household_power_consumption %>%
  filter(DateTime >=  as.POSIXct("2010-10-31 02:00:00",tz = "GMT") & DateTime <= as.POSIXct("2010-10-31 03:00:00",tz = "GMT")) %>%
  group_by(DateTime) %>%
  summarise_all(funs(mean))
#remove overlapping#
x2k10overlapindex <- which(household_power_consumption$DateTime >= as.POSIXct("2010-10-31 02:00:00",tz = "GMT") & household_power_consumption$DateTime <= as.POSIXct("2010-10-31 03:00:00",tz = "GMT"))
x2k10overlapindex
#add the avaraged values to the dataset#
household_power_consumption <- household_power_consumption[-x2k10overlapindex,]
household_power_consumption <- bind_rows(household_power_consumption, x2k10newvalues)


ds4 <- household_power_consumption %>%
  filter(year(DateTime)==2010)
View(ds4)
###################################

#Force time-zone
household_power_consumption$DateTime <- force_tz(household_power_consumption$DateTime, "Europe/Paris")
statsNA(household_power_consumption$Global_active_power)

naindex <- which(is.na(household_power_consumption$Global_active_power)==TRUE)
str(naindex)

############################################################################################
Breaks <- c(0, which(diff(naindex) != 1), length(naindex)) 
listNA <- sapply(seq(length(Breaks) - 1), function(i) naindex[(Breaks[i] + 1):Breaks[i+1]]) 

i = 1
j = 1
vector1 <- c()
vector2 <- c()
vector3 <- c()

for (i in 1:length(listNA)){
  if (length(listNA[[i]]) <= 60){
    h <- length(listNA[[i]])
    for (j in 1:h){
      vector1 <- c(vector1,listNA[[i]][j])
    }
  } else if (length(listNA[[i]]) > 60 & length(listNA[[i]]) <= 1440){
    h <- length(listNA[[i]])
    for (j in 1:h){
      vector2 <- c(vector2,listNA[[i]][j])
      }
    } else {
      h <- length(listNA[[i]])
      for (j in 1:h){
        vector3 <- c(vector3,listNA[[i]][j])
        }
      }
    }

# FROM 1 to 60 Na's
vector1
# FROM 60 to 1440 Na's
vector2
# OVER 1440 Na's
vector3

##################################################################
#IMPUTE NA'S FOR GROUP VECTOR 1 - USING LAST OBSRVATION CARRIED FORWARD
for (i in 4:10){
  household_power_consumption[vector1,i] <- na.locf(unname(unlist(household_power_consumption[,i])))[vector1]
}
summary(household_power_consumption)

##################################################################
#IMPUTE NA'S FOR GROUP VECTOR 2 - AVERAGE OF THE WEEK BEFORE AND AFTER
for (i in 4:10){
  household_power_consumption[vector2,i] <- (household_power_consumption[vector2+10080,i] + 
                                            household_power_consumption[vector2+20160,i] +
                                            household_power_consumption[vector2-10080,i] +
                                            household_power_consumption[vector2-20160,i])/4
}
summary(household_power_consumption)
#IMPUTE NA'S FOR GROUP VECTOR 3  - AVERAGE OF THE WEEK BEFORE AND AFTER
for (i in 4:10){
    household_power_consumption[vector3,i] <- min(household_power_consumption[vector3+10080,i],
                                                  household_power_consumption[vector3-10080,i])
}


summary(household_power_consumption)

#calculate the energy consumed by other equipments
household_power_consumption$Other_Equipment <- apply(household_power_consumption[,c('Global_active_power', 'Sub_metering_1',"Sub_metering_2",'Sub_metering_3')], 1, function(x) { (x[1] * 1000 / 60) - x[2] - x[3] - x[4] } )
household_power_consumption$EnergyConsumed <- apply(household_power_consumption[,c('Global_active_power')], 1, function(x) { (x[1] * 1000) / 60})

#DATA CLEANED#
#DATA CLEANED#


#############
#FORECASTING
#############

#grouping by 15min
groupMINUTES <- household_power_consumption %>%
  mutate(dategroup = floor_date(DateTime,"15 minutes")) %>%
  group_by(dategroup) %>%
  summarise_at(funs(sum),.vars = c("Sub_metering_1","Sub_metering_2","Sub_metering_3","Other_Equipment","EnergyConsumed"))
groupMINUTES <- groupMINUTES[,c(1,5:9)]

#Grouping by DAY - ENERGY
groupDAYS <- household_power_consumption %>%
  group_by(date(Date)) %>%
  summarise_at(funs(sum),.vars = c("Sub_metering_1","Sub_metering_2","Sub_metering_3","Other_Equipment","EnergyConsumed"))
groupDAYS <- mutate_at(groupDAYS,funs(./1000), .vars = c("Sub_metering_1","Sub_metering_2","Sub_metering_3","Other_Equipment","EnergyConsumed"))
groupDAYS <- groupDAYS[,c("Sub_metering_1","Sub_metering_2","Sub_metering_3","Other_Equipment","EnergyConsumed")]
  
#Grouping by WEEK - ENERGY
groupWEEK <- household_power_consumption %>%
  group_by(year(DateTime),month(DateTime),week(DateTime)) %>%
  summarise_at(funs(sum),.vars = c("Sub_metering_1","Sub_metering_2","Sub_metering_3","Other_Equipment","EnergyConsumed"))
groupWEEK <- mutate_at(groupWEEK,funs(./1000), .vars = c("Sub_metering_1","Sub_metering_2","Sub_metering_3","Other_Equipment","EnergyConsumed"))
groupWEEK <- groupWEEK[,c("Sub_metering_1","Sub_metering_2","Sub_metering_3","Other_Equipment","EnergyConsumed")]

#grouping by MONTH - ENERGY
groupMONTH <- household_power_consumption %>%
  group_by(year(DateTime),month(DateTime)) %>%
  summarise_at(funs(sum),.vars = c("Sub_metering_1","Sub_metering_2","Sub_metering_3","Other_Equipment","EnergyConsumed"))
###arrange(year(DateTime),month(DateTime))# <- use this if you want to sort by year and month, it's already sorted thought#
groupMONTH <- mutate_at(groupMONTH,funs(./1000), .vars = c("Sub_metering_1","Sub_metering_2","Sub_metering_3","Other_Equipment","EnergyConsumed"))
groupMONTH <- groupMONTH[,c("Sub_metering_1","Sub_metering_2","Sub_metering_3","Other_Equipment","EnergyConsumed")]

#timeSeries
dailyTS <- ts(groupDAYS,frequency = 365.25, start = c(2007,001))
weeklyTS <- ts(groupWEEK,frequency = 52, start = c(2007,1))
monthlyTS <- ts(groupMONTH, frequency = 12, start = c(2007,1))

#TrainingSets
dailyTRAIN <- window(dailyTS,c(2007,001),c(2009,365))
weeklyTRAIN <- window(weeklyTS,2007,2009.981) #weekly subsetting (?)
monthlyTRAIN <- window(monthlyTS,2007,c(2009,12))

#testSets
monthtlyTEST <- window(monthlyTS,c(2010,1))
autoplot(monthlyTS)
autoplot(monthlyTS, facets = 1)

#Decomposing Monthly Energy Consumed
monthlyDC <- decompose(monthlyTS[,"EnergyConsumed"])
plot(monthlyDC)

#methods forecasting - MONTHLY
#TSLM
monthlyTSLM <- tslm(monthlyTRAIN[,"EnergyConsumed"]~season+trend)
forecastTSLM <- forecast(monthlyTSLM, h = 11)
#ARIMA
monthlyARIMA <- auto.arima(monthlyTRAIN[,"EnergyConsumed"], seasonal = TRUE)
forecastARIMA <- forecast(monthlyARIMA,h = 11)
#HOLT-WINTERS
monthlyHOLT <- HoltWinters(monthlyTRAIN[,"EnergyConsumed"])
forecastHOLT <- forecast(monthlyHOLT,h = 11)

#RESIDUAL ANALYSIS#
autoplot(monthlyTS[,"EnergyConsumed"]) +
  xlab("Year") + ylab("kWh") +
  ggtitle("Monthly Energy Consumption")

resTSLM <-residuals(monthlyTSLM)
autoplot(resTSLM) + xlab("Day") + ylab("") +
  ggtitle("Residuals from TSLM")
gghistogram(resTSLM) + ggtitle("Histogram of residuals TSLM")
ggAcf(resTSLM) + ggtitle("ACF of residuals TSLM")


checkresiduals(monthlyTSLM)
checkresiduals(monthlyARIMA)
checkresiduals(monthlyHOLT)

#ACCURACY ANALYSIS#
autoplot(monthlyTS[,"EnergyConsumed"]) + 
  autolayer(forecastTSLM, series="Linear Model", PI=TRUE)

autoplot(monthlyTS[,"EnergyConsumed"]) + 
  autolayer(forecastARIMA, series="ARIMA", PI=TRUE)

autoplot(monthlyTS[,"EnergyConsumed"]) + 
  autolayer(forecastHOLT, series="Holt-Winters", PI=TRUE)

#Models plotted together
autoplot(monthlyTS[,"EnergyConsumed"]) + 
  autolayer(forecastTSLM, series="Linear Model", PI=FALSE) +
  autolayer(forecastARIMA, series="ARIMA", PI=FALSE) +
  autolayer(forecastHOLT, series="Holt-Winters", PI=FALSE) +
  ggtitle("Forecasts for Energy Consumption") +
  xlab("Year") + ylab("kWh") +
  guides(colour=guide_legend(title="Forecast Model"))
  

maccuracyTSLM <- accuracy(forecastTSLM,monthtlyTEST[,"EnergyConsumed"])
maccuracyARIMA <- accuracy(forecastARIMA,monthtlyTEST[,"EnergyConsumed"])
maccuracyHOLT <- accuracy(forecastHOLT,monthtlyTEST[,"EnergyConsumed"])

dtest <- data.frame(maccuracyTSLM[2,],maccuracyARIMA[2,],maccuracyHOLT[2,]) 
colnames(dtest) <- c("TSLM","ARIMA","HOLT-WINTERS")
dtest<-t(dtest)
dtest <- dtest[,c("RMSE","MAE","MAPE","MASE")]

dtrain <- data.frame(maccuracyTSLM[1,],maccuracyARIMA[1,],maccuracyHOLT[1,]) 
colnames(dtrain) <- c("TSLM","ARIMA","HOLT-WINTERS")
dtrain<-t(dtrain)
dtrain <- dtrain[,c("RMSE","MAE","MAPE","MASE")]

dtrain
dtest

#####################################################
par(mfrow=c(3,1))
plot(forecastTSLM)
lines(monthtlyTEST[,"EnergyConsumed"])

plot(forecastARIMA)
lines(monthtlyTEST[,"EnergyConsumed"])

plot(forecastHOLT)
lines(monthtlyTEST[,"EnergyConsumed"])

#####################################################

#FORECASTING SELECTION - TSLM#

finalModel <- tslm(monthlyTS[,"EnergyConsumed"]~season+trend)

finalForecast <- forecast(finalModel, h = 13)

checkresiduals(finalForecast)

autoplot(monthlyTS[,"EnergyConsumed"]) + 
  autolayer(finalForecast, series="Linear Model", PI=FALSE) +
  ggtitle("Forecasts for Energy Consumption") +
  xlab("Year") + ylab("kWh") +
  guides(colour=guide_legend(title="Forecast Model"))

accuracy(finalModel)

#saving models
save(finalModel, file = "TSLM_house.rda")
save(monthlyARIMA, file = "ARIMA_house.rda")
save(monthlyHOLT, file = "HOLT_house.rda")


#####################################################
#CREATING A DATA SET FOR EXPORTATION
xts <- as.xts(monthlyTS)
write.zoo(xts,file = "ts2.csv")

write.csv(cbind("time" = time(finalForecast$mean), "val" = finalForecast$mean), file = "ts3.csv") # Adds dates

write.csv(cbind("time" = time(monthlyTS), "val" = monthlyTS), file = "ts1.csv") # Adds dates
#####################################################

#DESCRIPTIVE ANALYSIS
#####################
dosmilsiete <- household_power_consumption %>%
  filter(year(DateTime)==2007) %>%
  group_by(year(DateTime),day(DateTime),month(DateTime)) %>%
  summarise_all(funs(sum))

dosmilsiete$REAL <- with(dosmilsiete, ymd(sprintf('%04d%02d%02d', `year(DateTime)`, `month(DateTime)`, `day(DateTime)`)))
dosmilsiete$DIA <- format(dosmilsiete$REAL,"%A")
dosmilsiete$MES <- format(dosmilsiete$REAL,"%B") 

#creating factor and ordering the levels
dosmilsiete$MES <- factor(dosmilsiete$MES, levels = c("enero","febrero","marzo","abril","mayo","junio","julio","agosto","septiembre","octubre","noviembre","diciembre"))
dosmilsiete$DIA <- factor(dosmilsiete$DIA, levels = c("lunes","martes","miércoles","jueves","viernes","sábado","domingo"))

str(dosmilsiete$MES)
str(dosmilsiete$DIA)


don <- xts(x = dosmilsiete[,c("Sub_metering_1","Sub_metering_2","Sub_metering_3")], order.by = dosmilsiete$REAL)
dygraph(don) %>%
  dyOptions(axisLineWidth = 1.5, fillGraph = FALSE, drawGrid = FALSE,stackedGraph = FALSE,includeZero = TRUE) %>%
  dyAxis("y", label = "kWh",valueRange = c(0, 50000)) %>%
  dySeries("Sub_metering_1", label = "Kitchen") %>%
  dySeries("Sub_metering_2", label = "Laundry Room") %>%
  dySeries("Sub_metering_3", label = "Heating Devices") %>%
  dyRangeSelector()

##############################################################################

#HEATMAP
col1 = "#d8e1cf" 
col2 = "#438484"

p1 <- ggplot(dosmilsiete, aes(dosmilsiete$MES, dosmilsiete$DIA)) + geom_tile(aes(fill = dosmilsiete$Sub_metering_1),colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2)+
  theme(text = element_text(size=10) ,axis.text.x = element_text(size = 7),axis.text.y = element_text(size = 7))

p2 <- ggplot(dosmilsiete, aes(dosmilsiete$MES, dosmilsiete$DIA)) + geom_tile(aes(fill = dosmilsiete$Sub_metering_2),colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2)+
  theme(text = element_text(size=10) ,axis.text.x = element_text(size = 7),axis.text.y = element_text(size = 7))

p3 <- ggplot(dosmilsiete, aes(dosmilsiete$MES, dosmilsiete$DIA)) + geom_tile(aes(fill = dosmilsiete$Sub_metering_3),colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = col1, high = col2)+
  theme(text = element_text(size=10) ,axis.text.x = element_text(size = 7),axis.text.y = element_text(size = 7))


plot_grid(p1,p2,p3)

#END OF CODE 
#######################################################################################################################