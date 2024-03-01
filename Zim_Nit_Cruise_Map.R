####################################################################################################
############################## Sequence Data Processing Pipeline  ##################################
# Created by SPS on 02/08/2020, following pipelines by: 
# Following tips from Steph Anderson about how to apply nitrate to maps for poster at OSM 2020

####################################################################################################

##### MAIN GOAL: Plot nitrogen values across map of USA ########################################################################################################################################################################
## Set working directory for Zimmerman Data:
setwd("WorkingDir/AE1812_Maps")

# standard packages:
#install.packages(c("devtools","stringr"))# only need to do once
#install.packages("ggplot2")

# some standard map packages.
#install.packages(c("maps", "mapdata")) # only need to do once


library(RColorBrewer)
library(dplyr)
library(devtools)
library(maps)
library(mapdata)
library(measurements)

####################### Read in Nitrate #############################################################
library(raster)
library(rasterVis)
library(rgdal)
library(lattice)
library(extrafont)



################## Read in Temperature #############################################################

# load raster file downloaded from Bio-Oracle
temperature<-raster("Present.Surface.Temperature.Mean.asc")

# crop the file
ne.atlantic.ext <- extent(-100, -45, 15, 50) # minLon, maxLon, minLat, maxLat
str(temperature)
max(temperature)
temp_map<-crop(temperature, ne.atlantic.ext)

# plot
# can easily change colors etc.
spplot(temp_map)
my.colors.temp = colorRampPalette(c("blue","red"))

######## Trying to add map to layer: #################################################################
AE1812<-read.csv("AE1812_Map_data_filt.csv") # note had to replace degree with a space in excel:
# Convert from decimal minutes to decimal degrees:
AE1812$Lat <- measurements::conv_unit(AE1812$Lat, from = 'deg_dec_min', to = 'dec_deg')
AE1812$Lon <- measurements::conv_unit(AE1812$Lon, from = 'deg_dec_min', to = 'dec_deg')
# convert longitude to proper are of world:
str(AE1812)
AE1812$Lat<-as.numeric(AE1812$Lat)
AE1812$Lon<-as.numeric(AE1812$Lon)
AE1812$Lon<-AE1812$Lon*(-1)

#Make sure group is read in as a character:
AE1812$group<-as.character(AE1812$group)

# Make sure region is in the right order:
par(mar=c(2,2,2,2))
plot(temp_map,col=my.colors.temp(200), axes=TRUE, box=FALSE, colNA="black")
points(x=AE1812$Lon, y=AE1812$Lat, pch=16)

#############################################
# Now add to ssplot:
# create function to make continents a different color (by classifying NAs as a value):
fun <- function(x) {x[is.na(x)] <- 100; return(x) }
fun2 <- function(x) {x[x<100] <- NA; return(x) }

# change your nitrogen raster into a raster of the continents
continents<- calc(temp_map, fun)
continents<- calc(continents, fun2)

# creating color palettes
cutpts <- seq(4, 30, by=0.5) # min, max, by
colourCount = length(unique(cutpts))
my.palette <- colorRampPalette(rev(brewer.pal(n = 11, name = "RdYlBu")), bias=0.55) # pick any color brewer palette your heart desires. Bias will alter where the 0 point is

# use the palette:
levelplot(raster, col.regions=my.palette(colourCount),at=cutpts)

# Or use this color palette:
# my.colors = colorRampPalette(c("#0738D0","#DEB519", "#D61919"))

# layer the rasters on top of one another.
p0 <- levelplot(temp_map, contour=FALSE, col.regions=my.palette(colourCount),at=cutpts, main=list(cex=2.5), xlab=list(label="Longitude",cex=2.5), ylab=list(label="Latitude",cex=2.5), scales=list(cex=2.5), labels=list(cex=2.5),colorkey=list(labels=list(cex=2.5)))
p1 <- levelplot(continents, col.regions="black")
p3<-p0 + as.layer(p1, under = TRUE) 
# p0 will control the color scale so make adjustments to that

library(sp)
# you need to convert your points to a spatial points data frame
# if xy = some data frame with your points (columns x&y)
# Sargasso Sea first:
AE1812_SS<-AE1812[3:22,8:9]
SS =AE1812_SS 
SS$x=AE1812_SS$Lon
SS$y=AE1812_SS$Lat
SS<-SS[,3:4]
AE_SS_points<-SpatialPointsDataFrame(SS, data.frame(ID=1:length(SS$x)))# the order of lat, lon makes a difference..
# Gulf Stream next:
AE1812_GS<-AE1812[c(23:24, 26:35),8:9]
GS =AE1812_GS 
GS$x=AE1812_GS$Lon
GS$y=AE1812_GS$Lat
GS<-GS[,3:4]
AE_GS_points<-SpatialPointsDataFrame(GS, data.frame(ID=1:length(GS$x)))# the order of lat, lon makes a difference..
# Coastal Waters next:
AE1812_CW<-AE1812[37:43,8:9]
CW =AE1812_CW 
CW$x=AE1812_CW$Lon
CW$y=AE1812_CW$Lat
CW<-CW[,3:4]
AE_CW_points<-SpatialPointsDataFrame(CW, data.frame(ID=1:length(CW$x)))# the order of lat, lon makes a difference..

library(latticeExtra)


# add a layer to your plot 
pdf("Zim_AE1812_TempMap.pdf")
p3 + layer(sp.points(AE_SS_points, cex=1.5, col="black", pch=16))  + layer(sp.points(AE_GS_points, cex=1.5, col="black", pch=17), under=FALSE) + layer(sp.points(AE_CW_points, cex=1.5, col="black", pch=15), under=FALSE) 
dev.off()

# Add legend
ggplot(data=AE1812, aes(x=Lat, Lon)) +
  geom_point(aes(shape=Region), size=5) +
  scale_shape_manual(labels=c("CW"="Coastal Waters","GS"="Gulf Stream","SS"="Sargasso Sea"),values=c(15,17,16))+
  theme_bw() +
  theme(legend.title = element_text(size = 30),
    legend.text = element_text(size = 30))


##################################################################################################################
#### Plotting nitrate

# load raster file downloaded from Bio-Oracle
nitrogen<-raster("Present.Surface.Nitrate.Mean.asc")

# crop the file
ne.atlantic.ext <- extent(-100, -45, 15, 50) # minLon, maxLon, minLat, maxLat
str(nitrogen)
max(nitrogen)
nit<-log10(nitrogen)
nit_map<-crop(nit, ne.atlantic.ext)

# plot
# can easily change colors etc.
spplot(nit_map)
my.colors = colorRampPalette(c("#0738D0","#DEB519", "#D61919"))


######## Trying to add map to layer: #################################################################
AE1812<-read.csv("AE1812_Map_data_filt.csv") # note had to replace degree with a space in excel:
# Convert from decimal minutes to decimal degrees:
AE1812$Lat <- measurements::conv_unit(AE1812$Lat, from = 'deg_dec_min', to = 'dec_deg')
AE1812$Lon <- measurements::conv_unit(AE1812$Lon, from = 'deg_dec_min', to = 'dec_deg')
# convert longitude to proper are of world:
str(AE1812)
AE1812$Lat<-as.numeric(AE1812$Lat)
AE1812$Lon<-as.numeric(AE1812$Lon)
AE1812$Lon<-AE1812$Lon*(-1)

#Make sure group is read in as a character:
AE1812$group<-as.character(AE1812$group)

# Make sure region is in the right order:
par(mar=c(2,2,2,2))
plot(nit_map,col=my.colors(200), axes=TRUE, box=FALSE, colNA="black")
points(x=AE1812$Lon, y=AE1812$Lat, pch=16)

#############################################
# Now add to ssplot:
# create function to make continents a different color (by classifying NAs as a value):
fun <- function(x) {x[is.na(x)] <- 100; return(x) }
fun2 <- function(x) {x[x<100] <- NA; return(x) }

# change your nitrogen raster into a raster of the continents
continents<- calc(nit_map, fun)
continents<- calc(continents, fun2)

# creating color palettes
cutpts <- seq(-6, 2, by=0.5) # min, max, by
colourCount = length(unique(cutpts))
my.palette <- colorRampPalette(rev(brewer.pal(n = 11, name = "PuOr")), bias=0.55) # pick any color brewer palette your heart desires. Bias will alter where the 0 point is

# use the palette:
levelplot(raster, col.regions=my.palette(colourCount),at=cutpts)

# Or use this color palette:
# my.colors = colorRampPalette(c("#0738D0","#DEB519", "#D61919"))

# layer the rasters on top of one another.
p0 <- levelplot(nit_map, contour=FALSE, col.regions=my.palette(colourCount),at=cutpts, main=list(cex=2.5), xlab=list(label="Longitude",cex=2.5), ylab=list(label="Latitude",cex=2.5), scales=list(cex=2.5), labels=list(cex=2.5),colorkey=list(labels=list(cex=2.5)))
p1 <- levelplot(continents, col.regions="black")
p3<-p0 + as.layer(p1, under = TRUE) 
p3


##################################################################################################################
#### Plotting chlorophyll

# load raster file downloaded from Bio-Oracle
chlorophyll<-raster("Present.Surface.Chlorophyll.Mean.asc")

# crop the file
ne.atlantic.ext <- extent(-100, -45, 15, 50) # minLon, maxLon, minLat, maxLat
str(chlorophyll)
max(chlorophyll)
Chl<-log10(chlorophyll)
Chl_map<-crop(Chl, ne.atlantic.ext)

# plot
# can easily change colors etc.
spplot(Chl_map)
my.colors = colorRampPalette(c("#3895D3","#072F5F","#2e856e"))


######## Trying to add map to layer: #################################################################
AE1812<-read.csv("AE1812_Map_data_filt.csv") # note had to replace degree with a space in excel:
# Convert from decimal minutes to decimal degrees:
AE1812$Lat <- measurements::conv_unit(AE1812$Lat, from = 'deg_dec_min', to = 'dec_deg')
AE1812$Lon <- measurements::conv_unit(AE1812$Lon, from = 'deg_dec_min', to = 'dec_deg')
# convert longitude to proper are of world:
str(AE1812)
AE1812$Lat<-as.numeric(AE1812$Lat)
AE1812$Lon<-as.numeric(AE1812$Lon)
AE1812$Lon<-AE1812$Lon*(-1)

#Make sure group is read in as a character:
AE1812$group<-as.character(AE1812$group)

# Make sure region is in the right order:
par(mar=c(2,2,2,2))

points(x=AE1812$Lon, y=AE1812$Lat, pch=16)

#############################################
# Now add to ssplot:
# create function to make continents a different color (by classifying NAs as a value):
fun <- function(x) {x[is.na(x)] <- 100; return(x) }
fun2 <- function(x) {x[x<100] <- NA; return(x) }

# change your nitrogen raster into a raster of the continents
continents<- calc(Chl_map, fun)
continents<- calc(continents, fun2)

# creating color palettes
cutpts <- seq(-2, 0.5, by=0.1) # min, max, by
colourCount = length(unique(cutpts))
my.palette <- colorRampPalette(brewer.pal(n = 9, name = "BuGn"), bias=1.2) # pick any color brewer palette your heart desires. Bias will alter where the 0 point is

# use the palette:
rasterVis::levelplot(Chl_map, col.regions=my.palette(colourCount),at=cutpts)
plot(Chl_map,col=my.colors(200), box=TRUE,axes=TRUE, colNA="black")

# Or use this color palette:
## my.colors = colorRampPalette(c("#0738D0","#DEB519", "#D61919"))

# layer the rasters on top of one another.
p0 <- levelplot(Chl_map, contour=FALSE, col.regions=my.colors,at=cutpts, main=list(cex=2.5), xlab=list(label="Longitude",cex=2.5), ylab=list(label="Latitude",cex=2.5), scales=list(cex=2.5), labels=list(cex=2.5),colorkey=list(labels=list(cex=2.5)))
p1 <- levelplot(continents, col.regions="black")
p3<-p0 + as.layer(p1, under = TRUE) 
p3















######################################################################################################################
# Older code
#############################################
######## Trying with more countries included:
w2hr<-map_data("world2Hires")
dim(w2hr)
head(w2hr)
ggplot() + geom_polygon(data = w2hr, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)

## subset countries around sampling:
east_coast <- subset(w2hr, region %in% c("USA", "Canada", "Mexico", "Bermuda", "Cuba"))
east_coast$long2<-((east_coast$long)-360)
AE1812_MAP2<-ggplot() + 
  geom_polygon(data = east_coast, aes(x=long2, y = lat, group = group)) +
  coord_fixed(xlim = c(-100, -45),  ylim = c(15,50), ratio = 1.3)
# change cast to factor:
AE1812$Lon
AE1812$cast<-as.factor(AE1812$cast)

AE1812_Map_v2<-ggplot() + 
  geom_polygon(data = east_coast, aes(x=long2, y = lat, group=group)) +
  geom_polygon(data = nit_map, aes(x=lon, y = lat, group=group)) +
  coord_fixed(xlim = c(-100, -45),  ylim = c(15,50), ratio = 1.3) +
  geom_point(data = AE1812, aes(x = Lon, y = Lat), size = 4)

library(ggspatial)
nit_df <- as.data.frame(nit_map, xy = TRUE) 

AE1812_Map_v2<-ggplot() + 
  geom_raster(data=nit_df)
  geom_polygon(data = east_coast, aes(x=long2, y = lat, group=group)) +
  coord_fixed(xlim = c(-100, -45),  ylim = c(15,50), ratio = 1.3) +
  geom_point(data = AE1812, aes(x = Lon, y = Lat), size = 4)

  
###########################################################################################################
### Try to use with map:
# data processing
library(ggplot2)
# spatial
library(raster)
library(rasterVis)
library(rgdal)
library(tidyverse)


##### NOw for cast data:
library(measurements)
area.map(m, regions = ".", sqmi=TRUE)

AE1812<-read_csv("AE1812_Map_data_filt.csv") # note had to replace degree with a space in excel:
# Convert from decimal minutes to decimal degrees:
AE1812$Lat <- measurements::conv_unit(AE1812$Lat, from = 'deg_dec_min', to = 'dec_deg')
AE1812$Lon <- measurements::conv_unit(AE1812$Lon, from = 'deg_dec_min', to = 'dec_deg')
# convert longitude to proper are of world:
str(AE1812)
AE1812$Lat<-as.numeric(AE1812$Lat)
AE1812$Lon<-as.numeric(AE1812$Lon)
AE1812$Lon<-AE1812$Lon*(-1)

#Make sure group is read in as a character:
AE1812$group<-as.character(AE1812$group)

# Make sure region is in the right order:
AE1812$Region = factor(AE1812$Region, levels=c('CW', 'GS', 'SS'))
usa <- map_data("usa")

########

ggplot(test) +  
  geom_tile(aes(fill=factor(value),alpha=0.8)) + 
  geom_polygon(data = usa, aes(x=long, y = lat, group = group))+
  coord_equal()



###### AE1812 Map #####################################################################################
# help(map_data)
library(measurements)
library(ggrepel)
library("tidyverse")
area.map(m, regions = ".", sqmi=TRUE)
setwd("WorkingDir/AE1812_Maps")

AE1812<-read_csv("AE1812_Map_data_filt.csv") # note had to replace degree with a space in excel:
AE1812<-AE1812 %>% filter(group!=4 & Cruise!="AR 2017")

# Convert from decimal minutes to decimal degrees:
# AE1812$Lat <- measurements::conv_unit(AE1812$Lat, from = 'deg_dec_min', to = 'dec_deg')
# AE1812$Lon <- measurements::conv_unit(AE1812$Lon, from = 'deg_dec_min', to = 'dec_deg')
# convert longitude to proper are of world:
str(AE1812)
AE1812$Lat<-as.numeric(AE1812$Lat)
AE1812$Lon<-as.numeric(AE1812$Lon)
AE1812$Lon<-AE1812$Lon*(-1)

#Make sure group is read in as a character:
AE1812$group<-as.character(AE1812$group)

# Make sure region is in the right order:
AE1812$Region = factor(AE1812$Region, levels=c('CW', 'GS', 'SS'))

# Try adding depth contours:
xlim <- c(23, 43)
ylim <- c(-81, -59)
# install.packages("marmap")
library(marmap)
depth <- 
  getNOAA.bathy(lat1 = xlim[1], lat2 = xlim[2], lon1 = ylim[1], lon2 = ylim[2], resolution = 1) %>% 
  fortify()
# take a look at depth data:
glimpse(depth)
# take a look at depth map:
depth %>% 
  filter(z <= 0) %>% 
  ggplot() +
  geom_raster(aes(x, y, fill = z))
# depth below sea level:
depth_f <- depth %>% filter(z <= 0)

# now map:
usa <- map_data("usa")

######## Trying with more countries included:
w2hr<-map_data("world2Hires")
dim(w2hr)
head(w2hr)
ggplot() + geom_polygon(data = w2hr, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)

## subset countries around sampling:
east_coast <- subset(w2hr, region %in% c("USA", "Canada", "Mexico", "Bermuda"))
east_coast$long2<-(360-east_coast$long)*-1
AE1812_MAP2<-ggplot() + geom_polygon(data = east_coast, aes(x=long2, y = lat, group = group)) + 
  coord_fixed(xlim=c(-80,-60),ylim = c(29, 42.5), ratio = 1.3)

# now add depth contours:
AE1812_MAP<- ggplot() + geom_polygon(data = east_coast, aes(x=long2, y = lat, group = group)) + 
  geom_raster(data=depth_f, aes(x, y, fill = z)) +
  coord_fixed(xlim=c(-80,-60),ylim = c(29, 42.5), ratio = 1.3) +
  guides(fill=guide_legend(title="Depth (m)",reverse=T))

## Final plot!!!
AE1812_Map_Final<- AE1812_MAP +
  geom_point(data = AE1812, aes(x = Lon, y = Lat), size = 4, color="black") +
  geom_point(data = AE1812, aes(x = Lon, y = Lat, color=Region), size = 3) + 
  geom_text_repel(data = AE1812, aes(x = Lon, y = Lat, label=station), color="white", size=5) +
  labs(x="Longitude",y="Latitude") +
  scale_shape_manual(values=c(16,17)) +
  scale_color_manual(values=c("#800000FF","#767676FF","#FFA319FF"), breaks=c("CW", "GS", "SS"),
                     labels=c("Coastal Waters", "Gulf Stream", "Sargasso Sea")) +
  theme(axis.text=element_text(size=25, face="bold"),
        axis.title=element_text(size=25, face="bold"),
        legend.title=element_text(size=20),
        legend.text=element_text(size=18),
        text = element_text(size=20, family='Arial')) +
  guides(shape = guide_legend(title="Cruise",override.aes = list(shape = c(16,17)),
          color=guide_legend(override.aes = list(size=6))))

# Save map
ggsave("AE1812_Map.png", plot = AE1812_Map_Final, device = "png", path = NULL, scale = 1, width = 25, height = 15, units = "cm", dpi = 300)

