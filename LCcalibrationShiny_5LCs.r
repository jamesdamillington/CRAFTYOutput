#script to create interactive Shiny maps for comparison of modelled vs observed land cover 
#reads file created by CRAFTYsummary.r


rm(list=ls())

#script only creates Shiny for a single year (set here, year must be in the input data file)
yr<-2005

library(tidyverse)
library(sf)
library(RColorBrewer)  #for plotting
library(shiny)
library(leaflet)


#input/putput variables
cDat <- readr::read_csv("Data/CRAFTYmunisLC.csv")

#note following shp was created using simplyfying_shapefiles.r
BRmunis <- st_read(paste(input_path, "Data/Vector/BRmunis_sim10_simple2.shp", sep = ""))

cDat_map <- left_join(BRmunis, filter(cDat, Year == yr), by = c("CD_GEOCMUn" ="muniID")) 
cDat_map <- cDat_map %>% mutate(muniID = CD_GEOCMUn)


#if paltype == "numeric", mypal should be a RColorBrewer palette, e.g. "BuPu" or "Greens"
#if paltype == "factor", mypal should be a character vector of names colours
plotPolys <- function(mypal = "Greens", paltype = "numeric", shp = BR2000, data = cDat_map$diffcPropLC1, muni = cDat_map$muniID, label = "null")
{

  #print("pal")
  if(paltype == "numeric")
  {
    pal <- colorNumeric(
      palette = mypal,
      domain = data)
  }
  
  if(paltype == "factor")
  {
    pal <- colorFactor(
      palette = mypal,
      domain = data)
  }
  
  pop <- paste0("Value: ", data, "<br/>","muniID: ", muni)
 
  leaflet(shp) %>% addTiles() %>%
    addPolygons(color = ~pal(data), weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.75,
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = TRUE),
                popup = pop
    )  %>%
    
    addLegend(pal = pal, values = ~data, title = label)
}




#shiny output
#####
ui<-fluidPage(
            mainPanel(
              selectInput("inpt", "Select Map:", c("diffcPropLC1" = "LC1",
                "diffcPropLC2" = "LC2",
                "diffcPropLC3" = "LC3",
                "diffcPropLC4"="LC4",
                "diffcPropLC5"="LC5",
                "ObsMode" = "oM",
                "ModMode" = "mM",
                "ModeError" = "eM",
                "CellsCorrectProp" = "ccp"))
  ),
  leafletOutput("plot",height = "800px")
)

server<- function(input, output){
  
  
  
  output$plot<-renderLeaflet({


    if(input$inpt=="LC1"){
      plotPolys(mypal = "RdYlGn", paltype = "numeric", shp = cDat_map, data = cDat_map$diffcProp1, 
        muni = cDat_map$muniID, label = "Nature Prop Diffc")
    }

    else if(input$inpt=="LC2"){
      plotPolys(mypal ="RdYlGn", paltype = "numeric", shp = cDat_map, data = cDat_map$diffcProp2, 
        muni = cDat_map$muniID, label = "Other Agri Prop Diffc")
    }

    else if(input$inpt=="LC3"){
      plotPolys(mypal = "RdYlGn", paltype = "numeric", shp = cDat_map, data = cDat_map$diffcProp3, 
        muni = cDat_map$muniID, label = "Agriculture Prop Diffc")
    }
    
    else if(input$inpt=="LC4"){
      plotPolys(mypal = "RdYlGn", paltype = "numeric", shp = cDat_map, data = cDat_map$diffcProp4, 
        muni = cDat_map$muniID, label = "Other Prop Diffc")
    }
    
    else if(input$inpt=="LC5"){
      plotPolys(mypal = "RdYlGn", paltype = "numeric", shp = cDat_map, data = cDat_map$diffcProp5, 
                muni = cDat_map$muniID, label = "Pasture Prop Diffc")
    }
    
    else if(input$inpt=="oM"){
      map_pal <- c("darkgreen", "darkcyan", "green", "grey", "khaki")
      plotPolys(mypal = map_pal, paltype = "factor", shp = cDat_map, data = cDat_map$ObsMode, 
        muni = cDat_map$muniID, label = "Observed Mode")
    }
    
    else if(input$inpt=="mM"){
      map_pal <- c("darkgreen", "darkcyan", "green", "grey", "khaki")
      plotPolys(mypal = map_pal, paltype = "factor", shp = cDat_map, data = cDat_map$ModMode, 
        muni = cDat_map$muniID, label = "Model model")
    }
    
    else if(input$inpt=="eM"){
      map_pal <- c("darkgreen","red")
      plotPolys(mypal = map_pal, paltype = "factor", shp = cDat_map, data = cDat_map$diffcMode, 
        muni = cDat_map$muniID, label = "Mode Difference?")
    }
    
    else if(input$inpt=="ccp"){
      plotPolys(mypal = "Reds", paltype = "numeric", shp = cDat_map, data = cDat_map$cellDiffcProp, 
        muni = cDat_map$muniID, label = "Cell Diffc (Prop)")
    }

  })
}
shinyApp(ui=ui, server=server)
