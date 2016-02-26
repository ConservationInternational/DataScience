library(raster)

AllData <- brick('EnSO Shiny App/Allbrick.grd')

soi <- read.csv('soi.csv', stringAsFactors=F)

select_dates <- as.character(soi$date[soi$value < -10])

dates_parsed <- paste0('X', substring(select_dates, 1, 4), '.', substring(select_dates, 6, 7))

ENSObrick <- subset(AllData, dates_parsed)

ENSOmean <- mean(ENSObrick)

AllMean <- mean(AllData)
