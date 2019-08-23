
#script to read multipe Capital maps from asc file and create animated gif 

rm(list=ls())

library(raster)
library(rasterVis)
library(gridExtra)
library(animation)
library(sf)
library(viridisLite)

fig_yrs <- c(2001, 2005, 2010, 2015) #year animation should be made for

path <- "C:/Users/k1076631/Google Drive/Shared/Crafty Telecoupling/Data/CRAFTYInput/Data/agricultureCapital/"
basefn <- "agricultureCapital"  #base filename to read
label <- "Agriculture Capital"  #use as title of plot

ras_pal <- viridis(100)  #map palette

mps <- list()   #list of plots

trim_map <- T  #shoudl the raster be trimmed? (may take some time)

#loop on fig_yrs to create list of map figures
for(i in seq_along(fig_yrs)){

  #read raster file
  mymap <- raster(paste0(path,basefn,fig_yrs[i],".asc"))
  
  if(trim_map) { 
    print(paste0("trim ",fig_yrs[i]))
    mymap <- trim(mymap) 
  }
  
  #create the plot
  p <- levelplot(mymap,
    col.regions=ras_pal, 
    at=seq(from=0,to=1,by=0.01), 
    contour=F, 
    margin=F,
    main = (paste0(label, " - ", fig_yrs[i]))
  )

  #add to list
  mps[[i]] <- p

}

#create gif  
saveGIF(
  for(i in seq_along(mps)){
      print(mps[[i]])
    },
  movie.name = paste0(path,basefn,"_anim",fig_yrs[1],"-",fig_yrs[length(fig_yrs)],".gif"),
    interval=2,
    loop=T
)
