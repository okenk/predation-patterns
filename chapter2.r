area <- read.csv('area.csv')
stock <- read.csv('stock.csv')
assessment <- read.csv('assessment.csv')
timeseries <- read.csv('timeseries.csv')
tsmetrics <- read.csv('tsmetrics.csv')
require(plyr)

# new england only contains US data, not Canadian
new.england.names <- c('Gulf of Maine', 'Gulf of Maine / Cape Hatteras', 'Gulf of Maine / Georges Bank', 
                       'Gulf of Maine / Georges Bank-Southern New England', 'Georges Bank', 'Georges Bank-Southern New England', 
                       'Gulf of Maine / Northern Georges Bank')
bering.sea.names <- c('Bering Sea', 'Bering Sea and Aleutian Islands', 'Central Bering Sea', 'Eastern Bering Sea',
                      'Eastern Bering Sea and Aleutian Islands', 'Eastern Bering Sea / Aleutian Islands / Gulf of Alaska',
                      'Eastern Bering Sea and Gulf of Alaska')
baltic.sea.names <- c('Western Baltic', 'Baltic Areas 22-32', 'Eastern Baltic', 'Baltic Sea')
north.sea.names <- c('IIIa and North Sea', 'IIIa, VI and North Sea', 'IIIa, VIId and North Sea', 'North Sea', 'Northern North Sea',
                     'Central North Sea', 'Southern North Sea')
area.codes <- as.character(area$AREAID[which(area$AREANAME %in% c(new.england.names, bering.sea.names, baltic.sea.names, north.sea.names) &
                                               area$COUNTRY != 'Canada')])
stock.codes <- as.character(stock$STOCKID[which(stock$AREAID %in% area.codes)])
stock.names <- stock$COMMONNAME[which(stock$AREAID %in% area.codes)]
assess.codes <- as.character(assessment$ASSESSID[which(assessment$STOCKID %in% stock.codes)])
recruit.ids <- as.character(tsmetrics$TSUNIQUE[which(tsmetrics$TSCATEGORY == 
                                                    'RECRUITS (NOTE: RECRUITS ARE OFFSET IN TIME SERIES BY THE AGE OF RECRUITMENT)')])
recruit.ts.orig <- timeseries[which(timeseries$ASSESSID %in% assess.codes & timeseries$TSID %in% recruit.ids),]

join1 <- join(recruit.ts.orig, assessment[,c('STOCKID', 'ASSESSID')], by='ASSESSID') 
join2 <- join(join1, stock[,c('STOCKID', 'COMMONNAME', 'AREAID')], by='STOCKID')
join3 <- join(join2, area[,c('AREAID', 'AREANAME')], by='AREAID')
names(join3)[which(names(join3)=='TSID')] <- 'TSUNIQUE'
join4 <- join(join3, tsmetrics[,c('TSUNIQUE', 'TSLONG', 'TSUNITSLONG')], by='TSUNIQUE')
head(join4)

recruit.ts <- join4[,c('AREANAME', 'COMMONNAME', 'TSYEAR', 'TSVALUE', 'TSLONG', 'TSUNITSLONG')]
head(recruit.ts)

head(timeseries)
length(stock.names)
stock.names

head(timeseries)

head(stock)