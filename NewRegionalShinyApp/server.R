library(reshape2)
library(zoo)
library(lubridate)
library(plyr)
library(ggplot2)
library(raster)
library(rgdal)

setwd('D:/Documents and Settings/mcooper/Documents/DataScience/New Regional Shiny App/')

#Soi data from http://www.bom.gov.au/climate/current/soihtm1.shtml


#####
#El Nino Time Plot
ENSOtime <- read.csv('ONI.csv')
ENSOtime$date <- ymd(ENSOtime$date)
ENSOtime$color <- as.factor(c(rep('black',772),rep('red',20)))
ENSOtime$size <- c(rep(1,772),rep(2,20))

Ninoplot <- function(yearmonth){
  cleandate <- as.POSIXct(yearmonth, origin="1970-01-01 UTC")
  ggplot(ENSOtime) + geom_line(aes(date, value), color=ENSOtime$color, size=ENSOtime$size, group=1) + 
  xlab("Date") + ylab("Oceanic Niño Index") + theme_bw() +
  geom_rect(aes(NULL, NULL, 
                  xmin=cleandate-weeks(2), xmax=cleandate+weeks(2), 
                  ymin=min(value, na.rm=T), ymax=max(value, na.rm=T), 
                  fill='red'), alpha=.5) + guides(fill=F)
}

#####
#Set Areal Boundaries

Amzxlim <- c(-75,-39)
Amzylim <- c(-23,8)

Afrxlim <- c(-18,51)
Afrylim <- c(-35,38)

Mekxlim <- c(95,109)
Mekylim <- c(9,28)
  
#####
#Map of El Nino Affecting Precip in Amazon, Mekong, Africa

precipStDev <- raster('GlobalONIStDev.asc')
worldborders <- readOGR('.', 'TM_WORLD_BORDERS-0.3')


#####
#GIF of Precip since 2014

# precip <- brick('UDelPrecipData.grd')
# writeRaster(precip[[601:1380]], 'ENSOAppPrecip.grd')
precip <- brick('ENSOAppPrecip.grd')
ndvi <- brick('ndvi.grd')

#function for building brick subset

cleansub <- function(UnixTime){
  POSIX <- as.Date(as.POSIXct(UnixTime, origin="1970-01-01 UTC"))
  paste0('X', year(POSIX), '.', ifelse(month(POSIX)>9,month(POSIX),paste0('0',month(POSIX))))
}

#####
#GIF of NDVI up to 2013

shinyServer(function(input, output) {
  
  clickpoint <- reactiveValues(x=-631152000)
  
  observeEvent(input$yearmonth$x, {
    clickpoint$x <- input$yearmonth$x
  })
  
  output$ENSOtime <- renderPlot({
    if(!exists("clickpoint")){
      return(Ninoplot(-631152000))}
    else{
      Ninoplot(clickpoint$x)}
  })
  
  
  observeEvent(input$YR, {
    clickpoint$x <- input$YR
  })
  
  output$AmazonStD <- renderPlot({
    plot(precipStDev, xlim=Amzxlim, ylim=Amzylim, axes=F)
    plot(worldborders, add=T)
  })
  
  output$AmazonPrecip <- renderPlot({
    if(is.null(clickpoint)){
      plot(precip[[1]], xlim=Amzxlim, ylim=Amzylim, axes=F)
      plot(worldborders, add=T)}
    else{
      plot(precip[[cleansub(clickpoint$x)]], xlim=Amzxlim, ylim=Amzylim, axes=F)
      plot(worldborders, add=T)}
  })
  
  output$AmazonNdvi <- renderPlot({
    if(is.null(clickpoint)){
      plot(ndvi[[1]], xlim=Amzxlim, ylim=Amzylim, axes=F)
      plot(worldborders, add=T)}
    else{
      plot(ndvi[[cleansub(clickpoint$x)]], xlim=Amzxlim, ylim=Amzylim, axes=F)
      plot(worldborders, add=T)}
  })
  
  output$AfricaStD <- renderPlot({
    plot(precipStDev, xlim=Afrxlim, ylim=Afrylim, axes=F)
    plot(worldborders, add=T)
  })
  
  output$AfricaPrecip <- renderPlot({
    if(is.null(clickpoint)){
      plot(precip[[1]], xlim=Afrxlim, ylim=Afrylim, axes=F)
      plot(worldborders, add=T)}
    else{
      plot(precip[[cleansub(clickpoint$x)]], xlim=Afrxlim, ylim=Afrylim, axes=F)
      plot(worldborders, add=T)}
  })
  
  output$AfricaNdvi <- renderPlot({
    if(is.null(clickpoint)){
      plot(ndvi[[1]], xlim=Afrxlim, ylim=Afrylim, axes=F)
      plot(worldborders, add=T)}
    else{
      plot(ndvi[[cleansub(clickpoint$x)]], xlim=Afrxlim, ylim=Afrylim, axes=F)
      plot(worldborders, add=T)}
  })
  
  output$MekongStD <- renderPlot({
    plot(precipStDev, xlim=Mekxlim, ylim=Mekylim, axes=F)
    plot(worldborders, add=T)
  })
  
  output$MekongPrecip <- renderPlot({
    if(is.null(clickpoint)){
      plot(precip[[1]], xlim=Mekxlim, ylim=Mekylim, axes=F)
      plot(worldborders, add=T)}
    else{
      plot(precip[[cleansub(clickpoint$x)]], xlim=Mekxlim, ylim=Mekylim, axes=F)
      plot(worldborders, add=T)}
  })
  
  output$MekongNdvi <- renderPlot({
    if(is.null(clickpoint)){
      plot(ndvi[[1]], xlim=Mekxlim, ylim=Mekylim, axes=F)
      plot(worldborders, add=T)}
    else{
      plot(ndvi[[cleansub(clickpoint$x)]], xlim=Mekxlim, ylim=Mekylim, axes=F)
      plot(worldborders, add=T)}
  })
  
})