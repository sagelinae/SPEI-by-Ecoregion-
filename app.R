##################################
#SPEI by Ecoregion - The shiny version
#SE Winter 2019
#I don't remember how to do this lol
##################################
library(shiny)
library(shinyjs)
library(leaflet)
library(rgdal)
library(raster)

#Read in SPEI raster file
SPEI.raw <- brick("data\\spei01.nc")

#Read in shapefiles for Ecoregions
ecorgn <- readOGR("data\\NA_CEC_Eco_Level3.shp")
eco.fix <- spTransform(ecorgn, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 "))

#This is fucked; what if I cut it by what users want instead of doing it all here? Seems like it'd be more efficient than this.
#SPEIValues <- extract(SPEI.raw, eco.fix, fun = mean, na.rm = T, sp = T)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  
  titlePanel("SPEI Values by Ecoregion - 1901:2015"),
  
  sidebarLayout(
    
    sidebarPanel(
      uiOutput("year")
    ),
    
    mainPanel(
      leafletOutput("map", width = "97%", height = "650px")
    )
  
  )
    
)

server <- function(input, output){
  output$year <- renderUI({
    selectInput(
      inputId =  "year", 
      label = "Select a year:", 
      choices = 1901:2015
    )
  }) 
  
  qpal <- colorNumeric("RdYlBu", SPEIValues$X2015)
  
  output$map <- renderLeaflet({
    leaflet(SPEIValues) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% addPolygons(stroke = T, fillColor = ~qpal(year), opacity = 1, 
                                                                   fillOpacity = 0.9, color = "black", smoothFactor = 0.5, weight = 1)
  })
  
  proxy <- leafletProxy('map')
  observeEvent(input$year, {
    proxy %>% clearShapes()
    proxy %>% addPolygons(stroke = T, fillColor = ~qpal(X2015), opacity = 1, 
                          fillOpacity = 0.9, color = "black", smoothFactor = 0.5, weight = 1) %>% 
              addLegend(values = ~X2015, pal = qpal, title = "SPEI")
  })
  
}

shinyApp(ui = ui, server = server)