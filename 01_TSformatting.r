# Date - supports dates without time
#-----------------------------------

library(TSstudio)
library(xts) # extensible time series


today <- Sys.Date() # return the actual data as object of class Date
date() # returns actual date only as character!
class(date())

# calculating with dates
yesterday <- today - 1
yesterday 
tomorrow <- today + 1
difftime(today, yesterday)

# create a Date object from a character string
day <- "2015-04-20"
class(day)
day <- as.Date(day)
class(day)

# define format of the character string
as.Date("20.04.2015", format="%d.%m.%Y")
as.Date("2012/05/31", format="%Y/%m/%d")
as.Date("120 2015", format="%j %Y") # 120. day of the year

?strptime # explains the used abbreviations for 'format'

# create a sequence of days and extract some information
days <- seq(as.Date("2015-01-01"), as.Date("2015-12-31"), by="day")
weekdays(days)
months(days)
quarters(days)

# POSIX - supports date, time, time zones and daylight saving time
#-----------------------------------------------------------------

now <- Sys.time()

class(now)
# POSIXct - represents time as seconds since Januar 1, 1970 GMT
# POSIXlt - is a list of 9 components and tzone attribut
# convert to POSIXlt
now.lt <- as.POSIXlt(now)
str(now.lt)
now.lt$year + 1900


# calculation with times
now + 5 # add 5 seconds
now + 86400 # add 1 day = 60 sec * 60 min * 24 h

# by default, the time zone of the system is used
Sys.timezone()
OlsonNames() # return names of available time zones known to Olson database

# create POSIX from charcter string
as.POSIXct("2015-04-20 13:55:00")
as.POSIXct("2015-04-20 13:55:00", tz="UTC") # add definition of time zone
as.POSIXct("2015-04-20 13:55:00") - as.POSIXct("2015-04-20 13:55:00", tz="GMT")
as.POSIXct("20.04.2015 13:55:00", format="%d.%m.%Y %H:%M:%S")

# extract information from POSIX
days <- c(as.POSIXct("1985-06-12 10:30:00"), as.POSIXct("2014-02-01 14:10:00"), Sys.time())
months(days)
weekdays(days)
quarters(days)
format(days, "%Y") # year 
format(days, "%m") # month number
format(days, "%B") # month name
format(days, "%b") # month name (abbreviated)
format(days, "%d") # day
format(days, "%j") # day of the year
format(days, "%A") # weekday
format(days, "%a") # weekday (abbreviated)
format(days, "%u") # day of the week
format(days, "%H") # hour
format(days, "%M") # minute
format(days, "%S") # second
format(days, "%Z") # time zone

# conversion between Date and POSIXct
posix <- Sys.time()
dates <- Sys.Date()
posix == dates
as.Date(posix) # converts POSIX to Date
as.Date(posix) == dates
as.POSIXct(dates) # converts Date to POSIX - but take care about the time!

# when working with POSIX, it is better to make conversions always through charaters:
dates <- Sys.Date()
char <- format(dates, "%Y-%m-%d")
as.POSIXct(paste(dates, "00:00:00"))


########################## creating xts objects #########################

##### xts data consits of a index (hold time dimension) and matrix (data)

# creating dates as a Date class object from 2016-01-01
dates=seq(as.Date("2016-01-01"), length=5, by="days")
dates
# creating 5 random numbers
data=rnorm(5)

# creating object called 'stock' using data and datas as the index
stock=xts(x=data, order.by=dates)

# add a new attribute bDay
bDay=as.POSIXct("1999-05-08")

stock2=xts(x=data, order.by=dates, born=bDay)
stock2

#### extracting index and matrix part seperately

stock2_core=coredata(stock2)  # extracting the core data
class(stock2_core)  # matrix class
stock2_index=index(stock2) # extracting the dates
class(stock2_index)

# time based indices

dates=as.Date("2016-01-01")+ 0:4
# create time series
time_A=xts(x=1:5, order.by=dates)
time_B=xts(x=1:5, order.by=as.POSIXct(dates))

index_time_A=index(time_A)
index_time_B=index(time_B)  # have different time formats

time_A[index(time_B)] # can extract the rows
time_B[index(time_A)] # cannot extract the rows


############### importing, exporting and converting time series ####################

data(sunspots)   # use inbuilt sunspots dataset
class(sunspots)  # time series dataset
head(sunspots)

sunspots_xts=as.xts(sunspots)
class(sunspots_xts)
head(sunspots_xts)


data(austres)
class(austres)
head(austres)

au=as.xts(austres)  # convert the timeseries obejct into xts
am=as.matrix(au)    # has dates in the data
am2=as.matrix(austres)   # has no dates in the data if didn't convert to ts first

########################## converting to ts ###############################

ndvi= read.csv(file.choose())   # read the ndvi.csv
ndvi=ndvi[,-1]
ndvi <- ts(ndvi, start=c(1982, 1), frequency=12)
ndvi=as.xts(ndvi) 

# ts data
# extract temporal information
ts_info(ndvi)
ts_plot(ndvi)
start(ndvi)
end(ndvi)
frequency(ndvi)
time(ndvi)
cycle(ndvi)
deltat(ndvi)


# useful plotting
plot(ndvi)
monthplot(ndvi)


########################### time based queries ###############################
# international time standard > YYYY-MMPDDTHH:MM:SS format

########## subsetting by time
# one and two sided interval query

head(ndvi["1990-01",1])   # we choose first in the month Jan1990
head(ndvi["1990-01/1990-03",1]) # now we represent a range of time
                                # from Jan1990 till Mar1990 (shortcut way)

############## subsetting by rows
ndvi[index(ndvi) >"1995-01-01"]

################################ modifying time series ############################

dates=seq(as.Date("2016-01-01"), length=5, by="days")

data[dates]=NA  # changing all the data that fall in this range as NA
data["1980-01-01/"]=0  # all data from 01Jan1980 onwards will be zero


############################## periods from time series ############################
# first and last function in xts
first(ndvi[ ,1], "4 months")  # can change to any number of months
last(ndvi[ ,1], "1 year")   # can change to any number of years
last(ndvi[ ,1], "-6 months") # everything except last 6 months

# can drill down to hours, minutes or seconds even
# can nest the functions
# we want first 5 months of the last 2 years in the dataset
first(last(ndvi[ ,1], "2 years"), "5 months")

lastyear=last(ndvi,"1 year") # we want the last week of dataset
last(lastyear,"2 months") # we want last 2 months of last year
last(lastyear, "-2 months") # we want all but last 2 month of last year

first(last(first(ndvi,"2 years"), "1 year"), "3 months")

################################### math operations in xts #######################
# create 2 xts objects

dates=seq(as.Date("2016-01-01"), length=5, by="days") # 5 dates from 01Jan2016
data=rnorm(5) # 5 random number as data
stock1=xts(x=data, order.by=dates)

dates2=seq(as.Date("2016-01-03"), length=5, by="days") # 5 dates from 03Jan2016
data2=rnorm(5) # 5 random number as data
stock2=xts(x=data2, order.by=dates2)

#### addition of data
# adding stock1 and stock2 will only give the results of addition of values in data
# that INTERSECT based on time >> important point
stock1 + stock2


################################ apply by time ################################
####### endpoint fxn and "apply" by period fnx

endpoints(ndvi, on="years") # find the last value for all the years
endpoints(ndvi, on="months", k=3) # find last values for every 3 months

# calculate the mean by years
ep=endpoints(ndvi, "years")
period.apply(ndvi, INDEX=ep, FUN=mean) # similar to apply family in base R


############################### split data into chunks ###################################

split(ndvi, f="months")

# example using edhec dataset
ndvi_qtrs=split(ndvi[,1], f="quarters")  # split the data into quarters
ndvi_qtrs[[3]]         # select the 3rd quarter


############################################### indexing ######################3
indexClass(stock1)   # see the class of dates in index
indexTZ(stock1)      # see the timezone of dates in index

# we can reformat the date format by:
indexFormat(stock1)="%b %d %Y"   # % d is the 3 letter for month

# we can reformat the time zone by:
tzone(stock2)="Asia/HongKong"

############################################# periods ###########################
# data frequency

periodicity(ndvi)

periodicity(to.yearly(ndvi))


# counting distinct timeframes
nquarters(ndvi) 

# time expressed with POSIXlt components
index(ndvi)
