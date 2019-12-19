############################################
#Extract values for SPEI based on EPA's ecoregions
#SPEI Data: https://spei.csic.es/database.html - Time-scale 1 month
#EPA Ecoregion: https://www.epa.gov/eco-research/ecoregions-north-america - Level III 
#SLE Winter 2019
############################################
library(raster)
library(ncdf4)
library(rgdal)
library(sf)

#Visuals
library(leaflet)
library(ggmap)
library(maps)
#library(rasterVis)
#library(maptools)

##################################
#Set Up
##################################

load("X:/BSS_TVR_waterfowl/scripts/mallards_pre_season/start.RData")
#plot(ecorgns, main = "Ecoregions")

#Specific regions we're interested in from mall_region.R
codes <- c('8.5.4','8.1.1','5.3.1','5.3.3','8.1.7','8.1.3','8.1.8','8.1.9','8.3.1','5.2.3', #AF
           '5.4.1','5.4.3','3.3.2',                                                         #BF
           '6.2.8','6.2.11','11.1.1','11.1.2','10.1.3','10.1.5',                            #CV
           '5.2.2','5.2.1','8.1.4','8.1.5','8.2.1','8.1.6','8.2.2','8.1.2','8.1.10',        #GL
           '9.2.1','9.3.1','9.2.2',                                                         #PPR
           '7.1.7','7.1.9','10.1.1','10.1.2','10.1.8','6.2.9','6.2.3','6.2.2'               #PNW
           )

#Our regions
AF <- c('8.5.4','8.1.1','5.3.1','5.3.3','8.1.7','8.1.3','8.1.8','8.1.9','8.3.1','5.2.3') # Atlantic flyway
BF <- c('5.4.1','5.4.3','3.3.2') # Boreal forest
CV <- c('6.2.8','6.2.11','11.1.1','11.1.2','10.1.3','10.1.5') # Central Valley
GL <- c('5.2.2','5.2.1','8.1.4','8.1.5','8.2.1','8.1.6','8.2.2','8.1.2','8.1.10') # Great Lakes
PP <- c('9.2.1','9.3.1','9.2.2') # PPR
RM <- c('7.1.7','7.1.9','10.1.1','10.1.2','10.1.8','6.2.9','6.2.3','6.2.2') # Rivers and mountains in Pacific Northwest

#Subset out the ecoregions map based on the regions we're interested in above
oureco <- ecorgns[ecorgns@data$NA_L3CODE %in% codes,]
crs(oureco)

#Change the projection to match the projection in the raster of SPEI
EcoFixProj <- spTransform(oureco, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 "))
crs(EcoFixProj)
plot(EcoFixProj, main = "Study Ecoregions w/ Fixed Projection")

##################################
#Extracting SPEI Values
##################################

setwd("X:\\Public\\Data_proofing_scripts\\Sage\\Sex Ratio")

#Read in the SPEI data
ncfname <- "spei01.nc"
SPEI.raw <- brick(ncfname)
crs(SPEI.raw)

#Cut SPEI to the months we're interested in (April-September)
month.patterns <- c("\\.04\\.", "\\.05\\.", "\\.06\\.", "\\.07\\.", "\\.08\\.", "\\.09\\.")
SPEI.month <- raster::subset(SPEI.raw, grep(pattern = paste0(month.patterns, collapse = "|"), names(SPEI.raw), value = T))

#Cut to the years we're interested in (1960 - 2015)
yr.patterns <- c("1960":"2015")
SPEI.yr <- raster::subset(SPEI.month, grep(pattern = paste0(yr.patterns, collapse = "|"), names(SPEI.month), value = T))
rm(SPEI.month)

#Average those months for a yearly estimate
#https://stevemosher.wordpress.com/2010/08/28/brick-testing-stackapply/
yeardex <- rep(1:56, each = 6) #Creates a vector to take the timeline of months and years. 

#this takes a while :/
SPEI.avg <- stackApply(SPEI.yr, indices = yeardex, fun = mean, na.rm=TRUE)
names(SPEI.avg) <- c(1960:2015)
#plot(SPEI.year)

###
#Test with one year - 1960
###

#Just testing it with one month in 1960, can help make it easier to understand what's going on lol
YR1960 <- SPEI.avg[[1]] 
plot(YR1960, main = "SPEI - 1960")

#overlaying our ecoregions so we get the correct values
YR1960.crop <- crop(YR1960, EcoFixProj)
YR1960.mask <- mask(YR1960.crop, EcoFixProj)
plot(YR1960.mask)
plot(EcoFixProj, add = TRUE, lwd = 2)

###
#Extract SPEI values for all years by ecoregions, take the average among all the pixels for each level 3 polygon
###

SPEIValues <- extract(SPEI.avg, EcoFixProj, fun = mean, na.rm = T, sp = T)

#*** Weights???
#SPEIValuesWeights <- extract(SPEI.avg, EcoFixProj, fun = mean, na.rm = T, sp = T, )

###
#Average SPEI for 1 value per region
###

meanloc  <-  function(region){
                df <- data.frame(matrix(NA, nrow = 1, ncol = 2)) 
                colnames(df) <- c("YEAR", "SPEI")
                for(i in grep(names(SPEIValues), pattern = "X")){
                  df <- rbind(df, c(names(SPEIValues[i]), mean(SPEIValues@data[which(SPEIValues@data$NA_L3CODE %in% region), i], na.rm = T) ))
                }
                df <- df[-1,]
                return(df)
              }

AF.SPEI <- meanloc(region = AF)
BF.SPEI <- meanloc(region = BF)
CV.SPEI <- meanloc(region = CV)
GL.SPEI <- meanloc(region = GL)
PP.SPEI <- meanloc(region = PP)
RM.SPEI <- meanloc(region = BF)


##################################
#Visualization
##################################

###
#Interactive map is kinda just for fun :)
###
#https://hautahi.com/rmaps
#Replace years with the year you're interested in. 
#Yooo this could easily be a shiny map where you toggle what year you wanna see 
#Could be fun to add like some points of birds to show where they are? If that data exists?
qpal <- colorNumeric("RdYlBu", SPEIValues$X2015)

leaflet(SPEIValues) %>% 
  addPolygons(stroke = T, fillColor = ~qpal(X2015), opacity = 1, 
              fillOpacity = 0.9, color = "black", smoothFactor = 0.5, weight = 1) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addLegend(values = ~X2015, pal = qpal, title = "SPEI")

###
#Static Map
#GGmap here? For static visualization? Nvm ggmap is literally witchcraft 
###

#Basemap
NoAm <- c(-140.976563, 24.046464, -58.886719, 64.396938)
BaseMap <- get_map(location = NoAm, source = "osm",  crop = F)

sitemap <- ggmap(BaseMap) 

nicemap <- sitemap +
           labs(x = "Longitude", y = "Latitude") +
           geom_polygon(data = EcoFixProj)

###
#Cool gif through the years here? kinda like mG's? 
###

