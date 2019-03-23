library(TSstudio)
library(fpp2)

## basic plots
ts_plot(beer.ts)
ts_ma(beer.ts)
ts_heatmap(beer.ts)
ts_seasonal(beer.ts)
ts_acf(beer.ts)
ts_surface(beer.ts)
monthplot(beer.ts)
boxplot(beer.ts ~ cycle(beer.ts))


### plotting moving averages
par(mfrow = c(2,2))
plot(beer.ts, col="gray", main = "1 Year Moving Average Smoothing")
lines(ma(beer.ts, order = 12), col = "red", lwd=3)
plot(beer.ts, col="gray", main = "3 Year Moving Average Smoothing")
lines(ma(beer.ts, order = 36), col = "blue", lwd=3)
plot(beer.ts, col="gray", main = "5 Year Moving Average Smoothing")
lines(ma(beer.ts, order = 60), col = "green", lwd=3)
plot(beer.ts, col="gray", main = "10 Year Moving Average Smoothing")
lines(ma(beer.ts, order = 84), col = "yellow4", lwd=3)

### plotting transformation
plot.ts(beer.ts/monthdays(beer.ts))
plot.ts(log(beer.ts))                # log transform
plot.ts(diff(beer.ts))              # difference between previous value


### lag plots
ts_lags(beer.ts)


##########################3  plot with ggplot 2 #############################3
library(astsa)
library(ggplot2)
par(mfrow = c(1,1))
par(mar=c(2,2,0,.5)+.5, mgp=c(1.6,.6,0))                   # trim the margins       
plot(globtemp, ylab='Temperature Deviations', type='n')    # set up the plot
grid(lty=1, col=gray(.9))                                  # add a grid
lines(globtemp, type='o', col=4)

### time series plot

tsplot(soi, main='Southern Oscillation Index', col=4)

### multiple time series
gtemp.df    = data.frame(Time=c(time(globtemp)), gtemp=c(globtemp), gtempl=c(globtempl))
ggplot(data = gtemp.df)             +
              ylab('Temperature Deviations')                                 +
              geom_line(aes(x = Time, y=gtemp, col = 'Land/Ocean'),  size=1, alpha=.5)  +
              geom_line(aes(x = Time, y=gtempl, col = 'Land Only'), size=1, alpha=.5)    +
              theme(legend.position=c(.1,.85))	


par(mfrow=c(3,1))
tsplot(cmort, ylab='Mortality', col=4, main='Pollution Kills', cex.main=1.5)
tsplot(tempr, ylab="Temperature", col=6)
tsplot(part, ylab="Particulates", col=3)

autoplot(cbind(Mortality=cmort, Temperature=tempr, Particulates=part), 
         xlab='Time', facets=FALSE)


culer = c("#44AA99", "#332288", "#88CCEE", "#CC6677", "#117733", "#339999", "#DD3377", "#AA4499")
par(mfrow=c(4,2), cex.lab=1.1)
for (i in 1:8){
 tsplot(eqexp[,i+8], col=culer[i], ylab=colnames(eqexp)[i+8], margins=.05)
}


#### ribbon plot
cblue = rgb(144,195,212, max=255)
cred  = rgb(195,144,212, max=255)
df    = data.frame(Time=c(time(soi)), SOI=c(soi), d=ifelse(c(soi)<0,0,1))
ggplot( data=df, aes(x=Time, y=SOI) )+                              
 geom_ribbon(aes(ymax=d*SOI, ymin=0,  fill = "cold")) +            
 geom_ribbon(aes(ymax=0,  ymin=(1-d)*SOI, fill = "hot"))+          
 scale_fill_manual(name='SST', values=c("cold"=cblue,"hot"=cred))+ 
 theme(legend.position=c(.05,.1)) 



df = data.frame(Time=c(time(soi)), SOI=c(soi))
ggplot( data=df, aes(x=Time, y=SOI) )  +          
        geom_line(col=rgb(0, 0,.9, alpha=.4)) +    
        stat_smooth(span=1/12, col='red', se=FALSE) + 
        stat_smooth(col='green', se = FALSE)    

par(mfrow=c(1,1))
plot(sp500w, ylab="Weekly Returns", main='S&P 500')


## discrete-valued series plotted

par(mar=c(2, 2, 0, 0)+.5, mgp=c(1.6,.6,0))
plot(EQcount, type='n')
grid(lty=1, col=gray(.9))
points(EQcount, pch=21, col=4, cex=1.1, bg = 6)   # looks better without this
lines(EQcount, type='s', col=4)

par(mar=c(2, 2, 0, 0)+.5, mgp=c(1.6,.6,0))
plot(EQcount, type='n')
grid(lty=1, col=gray(.9))
lines(EQcount, type='h', col=rgb(0,0,.9, alpha=.5), lwd=2)


#############################

library(ggTimeSeries)

# Before that, setting a minimal theme -

minimalTheme = theme_set(theme_bw(12))
minimalTheme = theme_update(
   axis.ticks = element_blank(),
   legend.position = 'none',
   strip.background = element_blank(),
   panel.border = element_blank(),
   panel.background = element_blank(),
   panel.grid = element_blank()
)


# A calendar heatmap is a great way to visualise daily data. 
# Its structure makes it easy to detect weekly, monthly, or seasonal patterns.

# creating some data
library(ggTimeSeries)
library(data.table)

dtData = data.table(
      DateCol = seq(
         as.Date("1/01/2014", "%d/%m/%Y"),
         as.Date("31/12/2015", "%d/%m/%Y"),
         "days"
      ),
      ValueCol = runif(730)
   )
dtData[, ValueCol := ValueCol + (strftime(DateCol,"%u") %in% c(6,7) * runif(1) * 0.75), .I]
dtData[, ValueCol := ValueCol + (abs(as.numeric(strftime(DateCol,"%m")) - 6.5)) * runif(1) * 0.75, .I]

# base plot
p1 = ggplot_calendar_heatmap(
   dtData,
   'DateCol',
   'ValueCol'
)

# adding some formatting
p1 +
   xlab(NULL) +
   ylab(NULL) +
   scale_fill_continuous(low = 'green', high = 'red') +
   facet_wrap(~Year, ncol = 1)


# creating some categorical data
dtData[, CategCol := letters[1 + round(ValueCol * 7)]]

# base plot
p2 = ggplot_calendar_heatmap(
   dtData,
   'DateCol',
   'CategCol'
)

# adding some formatting
p2 +
   xlab(NULL) +
   ylab(NULL) +
   facet_wrap(~Year, ncol = 1)
