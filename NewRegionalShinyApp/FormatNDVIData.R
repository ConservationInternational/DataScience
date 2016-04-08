#Ndvi data brought in using azvoleff's ndvi3g github repo

#This script formats it to be used in the New Regional Shiny App

#3g str date - 81jul15a
#3g end date - 12dec15b

library(itertools)
library(foreach)
library(doParallel)

registerDoParallel(3)

setwd('D:/Documents and Settings/mcooper/Documents/DataScience/')

ndvi <- stack('ndvi.tif')


yr <- paste0('X', seq(1981, 2012))

mn <- paste0('.',c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12'))

all <- NULL
for (y in yr){
  for (m in mn){
    for (a in c('a', '')){
      all <- c(all, paste0(y, m, a))
    }
  }
}

all <- all[13:768]

names(ndvi) <- all

extent(ndvi) <- c(xmn=-180, xmx=180, ymn=-90, ymx=90)

ndvi <- ndvi[[which(!grepl('a', names(ndvi)))]]

samp <- raster(nrow=180*2, ncol=360*2, xmn=-180, xmx=180, ymn=-90, ymx=90)

ndvis <- foreach(ras=iter(names(ndvi)), .packages="raster", 
                 .combine=c) %dopar% {
                   r <- resample(ndvi[[ras]], samp)
                   writeRaster(r, paste0('tmp/', ras, '.tif'))
                 }

ndvib <- brick(stack(ndvis))
