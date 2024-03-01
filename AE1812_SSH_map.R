#From http://geog.uoregon.edu/bartlein/courses/geog490/week04-netCDF.html#reshaping-from-raster-to-rectangular

library(chron)
library(RColorBrewer)
library(lattice)
library(ncdf4)
library(fields)
library(reshape2)
library(plotly)
# Additional libraries to use:
library(tidyverse)
# BiocManager::install("ncdump")
library(ncdump)
library(sf)
# BiocManager::install("spData")
library(spData)
# BiocManager::install("insol")
library(insol)
# Test # 2 library
library(raster)
library(knitr)
#BiocManager::install("kableExtra")
library(kableExtra)
library(plyr)
library(stringr)
# BiocManager::install("arrayhelpers")
library(arrayhelpers)
library(maps)
library(mapdata)
library(ggmap)
library(measurements)

#set working directory
setwd("WorkingDir/AE1812_Maps/")

####### ONLY USE CODE IN THIS SECTION IF YOU NEED TO READ IN A NEW VARIABLE!!! ################################################################################

files = dir("WorkingDir/SSH_CMEMS_May2018/",pattern = ".nc")

adt_total<-data.frame()                     # make an empty dataframe
for (f in files){
  adt = raster(f, level=180, varname= "adt")
  adt.df = raster::as.data.frame(adt, xy = TRUE)
  file_date<-strsplit(f, "_")[[1]][6]
  adt.df$date=rep(file_date, nrow(adt.df))
  adt_total<-rbind(adt_total, adt.df)
}

# # Transform dates in dataframe:
adt_total<-transform(adt_total, date=as.Date(as.character(date), "%Y%m%d"))

# # Save dataframe of all SSH:
# write.csv(adt_total, "SSH_0502_0515_2018.csv")

temp_total<-data.frame()                     # make an empty dataframe
for (f in files){
  temp = raster(f, level=180, varname= "temperature")
  temp.df = raster::as.data.frame(temp, xy = TRUE)
  file_date<-strsplit(f, "_")[[1]][6]
  temp.df$date=rep(file_date, nrow(temp.df))
  temp_total<-rbind(temp_total, temp.df)
}

# # Transform dates in dataframe:
adt_total<-transform(adt_total, date=as.Date(as.character(date), "%Y%m%d"))

######## STEP 1: Read in SSH data
# Read in SSH CSV
ae1812_adt<-read_csv("WorkingDir/AE1812_Maps/SSH_CMEMS_May2018/SSH_0502_0515_2018.csv")

head(ae1812_adt$Absolute.dynamic.topography)
######## STEP 2: Read in Cruise Station data
# Read in file with station data from cruise
AE1812<-read_csv("WorkingDir/AE1812_Maps/AE1812_Map_data_filt.csv") 
# Get rid of incubation points in dataset.
AE1812<-AE1812 %>% filter(group!=4)
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
AE1812$Region = factor(AE1812$Region, levels=c('SS', 'GS', 'CW'))

######## STEP 3: Visualize Data

# subtract lats so they can be plotted on worldmap:
AE1812$Lon2<-360-(AE1812$Lon*-1)
AE1812$date<-as.Date(as.character(AE1812$date), "%m/%d/%y")

# Load map:
w2hr<-map_data("world2Hires")
## subset countries around sampling:
east_coast <- subset(w2hr, region %in% c("USA", "Canada", "Mexico", "Bermuda", "Cuba"))
usa<-map_data("usa")

### 05.02.2018 ###
ae1812_0502<-ggplot()+
  geom_raster(data = ae1812_adt %>% filter(date=="2018-05-02"),  aes(x = x, y = y, fill = Absolute.dynamic.topography), interpolate = TRUE)+
  geom_polygon(data = east_coast, aes(x=long, y = lat, group = group), fill="black") + 
  coord_fixed(xlim = c(275, 300),  ylim = c(23, 42), ratio = 1.3) +
  geom_sf(data = spData::world, fill = "grey80", col = "black")+
  # geom_path(data = data, aes(x = lon, y = lat, col = id), size = .75)+
  coord_sf(xlim = c(275,300), ylim = c(25,42))+
  scale_fill_gradientn(name = "ADT\n(m)", colours = oce::oceColorsJet(120))+
  geom_point(data = AE1812, aes(x = Lon2, y = Lat, shape=Region), size = 3, color="darkgrey") +
  scale_shape_manual(values=c(15,16,17),  breaks=c("CW", "GS", "SS"),
                     labels=c("Coastal Waters", "Gulf Stream", "Sargasso Sea")) +
  labs(title = "May 2, 2018", x = "", y = "")+
  theme_bw()+
  guides(shape = guide_legend(order = 1)) +
  theme(axis.text = element_text(size = 14, colour = 1))

ggsave("AE1812_ADT_02052018.png", plot = ae1812_0502, device = "png", path = NULL, scale = 1, width = 25, height = 15, units = "cm", dpi = 300)

### 05.03.2018 ###
ae1812_0503<-ggplot()+
  geom_raster(data = adt_total %>% filter(date=="2018-05-03"),  aes(x = x, y = y, fill = Absolute.dynamic.topography), interpolate = TRUE)+
  geom_polygon(data = east_coast, aes(x=long, y = lat, group = group), fill="#7D7D7D") + 
  coord_fixed(xlim = c(275, 300),  ylim = c(23, 42), ratio = 1.3) +
  geom_sf(data = spData::world, fill = "grey80", col = "black")+
  # geom_path(data = data, aes(x = lon, y = lat, col = id), size = .75)+
  coord_sf(xlim = c(275,300), ylim = c(25,42))+
  scale_fill_gradientn(name = "ADT\n(m)", colours = oce::oceColorsJet(120))+
  geom_point(data = AE1812, aes(x = Lon2, y = Lat, shape=Region), size = 3, color="darkgrey") +
  geom_point(data = AE1812 %>% filter(date=="5/3/2018"), aes(x = Lon2, y = Lat, shape=Region), size = 4, color="black") +
  scale_shape_manual(values=c(15,16,17),  breaks=c("CW", "GS", "SS"),
                     labels=c("Coastal Waters", "Gulf Stream", "Sargasso Sea")) +
  labs(title = "May 3, 2018", x = "", y = "")+
  theme_bw()+
  guides(shape = guide_legend(order = 1)) +
  theme(axis.text = element_text(size = 14, colour = 1))

ggsave("AE1812_ADT_03052018.png", plot = ae1812_0503, device = "png", path = NULL, scale = 1, width = 25, height = 15, units = "cm", dpi = 300)

### 05.04.2018 ###
ae1812_0504<-ggplot()+
  geom_raster(data = adt_total %>% filter(date=="2018-05-04"),  aes(x = x, y = y, fill = Absolute.dynamic.topography), interpolate = TRUE)+
  geom_polygon(data = east_coast, aes(x=long, y = lat, group = group), fill="#7D7D7D") + 
  coord_fixed(xlim = c(275, 300),  ylim = c(23, 42), ratio = 1.3) +
  geom_sf(data = spData::world, fill = "grey80", col = "black")+
  # geom_path(data = data, aes(x = lon, y = lat, col = id), size = .75)+
  coord_sf(xlim = c(275,300), ylim = c(25,42))+
  scale_fill_gradientn(name = "ADT\n(m)", colours = oce::oceColorsJet(120))+
  geom_point(data = AE1812, aes(x = Lon2, y = Lat, shape=Region), size = 3, color="darkgrey") +
  geom_point(data = AE1812 %>% filter(date=="5/4/2018"), aes(x = Lon2, y = Lat, shape=Region), size = 4, color="black") +
  scale_shape_manual(values=c(15,16,17),  breaks=c("CW", "GS", "SS"),
                     labels=c("Coastal Waters", "Gulf Stream", "Sargasso Sea")) +
  labs(title = "May 4, 2018", x = "", y = "")+
  theme_bw()+
  guides(shape = guide_legend(order = 1)) +
  theme(axis.text = element_text(size = 14, colour = 1))

ggsave("AE1812_ADT_04052018.png", plot = ae1812_0504, device = "png", path = NULL, scale = 1, width = 25, height = 15, units = "cm", dpi = 300)

### 05.05.2018 ###
ae1812_0505<-ggplot()+
  geom_raster(data = adt_total %>% filter(date=="2018-05-05"),  aes(x = x, y = y, fill = Absolute.dynamic.topography), interpolate = TRUE)+
  geom_polygon(data = east_coast, aes(x=long, y = lat, group = group), fill="#7D7D7D") + 
  coord_fixed(xlim = c(275, 300),  ylim = c(23, 42), ratio = 1.3) +
  geom_sf(data = spData::world, fill = "grey80", col = "black")+
  # geom_path(data = data, aes(x = lon, y = lat, col = id), size = .75)+
  coord_sf(xlim = c(275,300), ylim = c(25,42))+
  scale_fill_gradientn(name = "ADT\n(m)", colours = oce::oceColorsJet(120))+
  geom_point(data = AE1812, aes(x = Lon2, y = Lat, shape=Region), size = 3, color="darkgrey") +
  geom_point(data = AE1812 %>% filter(date=="5/5/2018"), aes(x = Lon2, y = Lat, shape=Region), size = 4, color="black") +
  scale_shape_manual(values=c(15,16,17),  breaks=c("CW", "GS", "SS"),
                     labels=c("Coastal Waters", "Gulf Stream", "Sargasso Sea")) +
  labs(title = "May 5, 2018", x = "", y = "")+
  theme_bw()+
  guides(shape = guide_legend(order = 1)) +
  theme(axis.text = element_text(size = 14, colour = 1))

ggsave("AE1812_ADT_05052018.png", plot = ae1812_0505, device = "png", path = NULL, scale = 1, width = 25, height = 15, units = "cm", dpi = 300)

### 05.06.2018 ###
ae1812_0506<-ggplot()+
  geom_raster(data = adt_total %>% filter(date=="2018-05-06"),  aes(x = x, y = y, fill = Absolute.dynamic.topography), interpolate = TRUE)+
  geom_polygon(data = east_coast, aes(x=long, y = lat, group = group), fill="#7D7D7D") + 
  coord_fixed(xlim = c(275, 300),  ylim = c(23, 42), ratio = 1.3) +
  geom_sf(data = spData::world, fill = "grey80", col = "black")+
  # geom_path(data = data, aes(x = lon, y = lat, col = id), size = .75)+
  coord_sf(xlim = c(275,300), ylim = c(25,42))+
  scale_fill_gradientn(name = "ADT\n(m)", colours = oce::oceColorsJet(120))+
  geom_point(data = AE1812, aes(x = Lon2, y = Lat, shape=Region), size = 3, color="darkgrey") +
  geom_point(data = AE1812 %>% filter(date=="5/6/2018"), aes(x = Lon2, y = Lat, shape=Region), size = 4, color="black") +
  scale_shape_manual(values=c(15,16,17),  breaks=c("CW", "GS", "SS"),
                     labels=c("Coastal Waters", "Gulf Stream", "Sargasso Sea")) +
  labs(title = "May 6, 2018", x = "", y = "")+
  theme_bw()+
  guides(shape = guide_legend(order = 1)) +
  theme(axis.text = element_text(size = 14, colour = 1))

ggsave("AE1812_ADT_06052018.png", plot = ae1812_0506, device = "png", path = NULL, scale = 1, width = 25, height = 15, units = "cm", dpi = 300)

### 05.07.2018 ###
ae1812_0507<-ggplot()+
  geom_raster(data = ae1812_adt %>% filter(date=="2018-05-07"),  aes(x = x, y = y, fill = Absolute.dynamic.topography), interpolate = TRUE)+
  geom_polygon(data = east_coast, aes(x=long, y = lat, group = group), fill="#7D7D7D") + 
  coord_fixed(xlim = c(275, 300),  ylim = c(23, 42), ratio = 1.3) +
  geom_sf(data = spData::world, fill = "grey80", col = "black")+
  # geom_path(data = data, aes(x = lon, y = lat, col = id), size = .75)+
  coord_sf(xlim = c(275,300), ylim = c(25,42))+
  scale_fill_gradientn(name = "ADT\n(m)", colours = oce::oceColorsJet(120))+
  geom_point(data = AE1812, aes(x = Lon2, y = Lat, shape=Region), size = 3, color="darkgrey") +
  geom_point(data = AE1812 %>% filter(date=="5/7/2018"), aes(x = Lon2, y = Lat, shape=Region), size = 4, color="black") +
  scale_shape_manual(values=c(15,16,17),  breaks=c("CW", "GS", "SS"),
                     labels=c("Coastal Waters", "Gulf Stream", "Sargasso Sea")) +
  labs(title = "May 7, 2018", x = "", y = "")+
  theme_bw()+
  guides(shape = guide_legend(order = 1)) +
  theme(axis.text = element_text(size = 14, colour = 1))

ggsave("AE1812_ADT_07052018.png", plot = ae1812_0507, device = "png", path = NULL, scale = 1, width = 25, height = 15, units = "cm", dpi = 300)

### 05.08.2018 ###
ae1812_0508<-ggplot()+
  geom_raster(data = adt_total %>% filter(date=="2018-05-08"),  aes(x = x, y = y, fill = Absolute.dynamic.topography), interpolate = TRUE)+
  geom_polygon(data = east_coast, aes(x=long, y = lat, group = group), fill="#7D7D7D") + 
  coord_fixed(xlim = c(275, 300),  ylim = c(23, 42), ratio = 1.3) +
  geom_sf(data = spData::world, fill = "grey80", col = "black")+
  # geom_path(data = data, aes(x = lon, y = lat, col = id), size = .75)+
  coord_sf(xlim = c(275,300), ylim = c(25,42))+
  scale_fill_gradientn(name = "ADT\n(m)", colours = oce::oceColorsJet(120))+
  geom_point(data = AE1812, aes(x = Lon2, y = Lat, shape=Region), size = 3, color="darkgrey") +
  geom_point(data = AE1812 %>% filter(date=="5/8/2018"), aes(x = Lon2, y = Lat, shape=Region), size = 4, color="black") +
  scale_shape_manual(values=c(15,16,17),  breaks=c("CW", "GS", "SS"),
                     labels=c("Coastal Waters", "Gulf Stream", "Sargasso Sea")) +
  labs(title = "May 8, 2018", x = "", y = "")+
  theme_bw()+
  guides(shape = guide_legend(order = 1)) +
  theme(axis.text = element_text(size = 14, colour = 1))

ggsave("AE1812_ADT_08052018.png", plot = ae1812_0508, device = "png", path = NULL, scale = 1, width = 25, height = 15, units = "cm", dpi = 300)

### 05.09.2018 ###
ae1812_0509<-ggplot()+
  geom_raster(data = adt_total %>% filter(date=="2018-05-09"),  aes(x = x, y = y, fill = Absolute.dynamic.topography), interpolate = TRUE)+
  geom_polygon(data = east_coast, aes(x=long, y = lat, group = group), fill="#7D7D7D") + 
  coord_fixed(xlim = c(275, 300),  ylim = c(23, 42), ratio = 1.3) +
  geom_sf(data = spData::world, fill = "grey80", col = "black")+
  # geom_path(data = data, aes(x = lon, y = lat, col = id), size = .75)+
  coord_sf(xlim = c(275,300), ylim = c(25,42))+
  scale_fill_gradientn(name = "ADT\n(m)", colours = oce::oceColorsJet(120))+
  geom_point(data = AE1812, aes(x = Lon2, y = Lat, shape=Region), size = 3, color="darkgrey") +
  geom_point(data = AE1812 %>% filter(date=="5/9/2018"), aes(x = Lon2, y = Lat, shape=Region), size = 4, color="black") +
  scale_shape_manual(values=c(15,16,17),  breaks=c("CW", "GS", "SS"),
                     labels=c("Coastal Waters", "Gulf Stream", "Sargasso Sea")) +
  labs(title = "May 9, 2018", x = "", y = "")+
  theme_bw()+
  guides(shape = guide_legend(order = 1)) +
  theme(axis.text = element_text(size = 14, colour = 1))

ggsave("AE1812_ADT_09052018.png", plot = ae1812_0509, device = "png", path = NULL, scale = 1, width = 25, height = 15, units = "cm", dpi = 300)

### 05.10.2018 ###
ae1812_0510<-ggplot()+
  geom_raster(data = adt_total %>% filter(date=="2018-05-10"),  aes(x = x, y = y, fill = Absolute.dynamic.topography), interpolate = TRUE)+
  geom_polygon(data = east_coast, aes(x=long, y = lat, group = group), fill="#7D7D7D") + 
  coord_fixed(xlim = c(275, 300),  ylim = c(23, 42), ratio = 1.3) +
  geom_sf(data = spData::world, fill = "grey80", col = "black")+
  # geom_path(data = data, aes(x = lon, y = lat, col = id), size = .75)+
  coord_sf(xlim = c(275,300), ylim = c(25,42))+
  scale_fill_gradientn(name = "ADT\n(m)", colours = oce::oceColorsJet(120))+
  geom_point(data = AE1812, aes(x = Lon2, y = Lat, shape=Region), size = 3, color="darkgrey") +
  geom_point(data = AE1812 %>% filter(date=="5/10/2018"), aes(x = Lon2, y = Lat, shape=Region), size = 4, color="black") +
  scale_shape_manual(values=c(15,16,17),  breaks=c("CW", "GS", "SS"),
                     labels=c("Coastal Waters", "Gulf Stream", "Sargasso Sea")) +
  labs(title = "May 10, 2018", x = "", y = "")+
  theme_bw()+
  guides(shape = guide_legend(order = 1)) +
  theme(axis.text = element_text(size = 14, colour = 1))

ggsave("AE1812_ADT_10052018.png", plot = ae1812_0510, device = "png", path = NULL, scale = 1, width = 25, height = 15, units = "cm", dpi = 300)

### 05.11.2018 ###
ae1812_0511<-ggplot()+
  geom_raster(data = adt_total %>% filter(date=="2018-05-11"),  aes(x = x, y = y, fill = Absolute.dynamic.topography), interpolate = TRUE)+
  geom_polygon(data = east_coast, aes(x=long, y = lat, group = group), fill="#7D7D7D") + 
  coord_fixed(xlim = c(275, 300),  ylim = c(23, 42), ratio = 1.3) +
  geom_sf(data = spData::world, fill = "grey80", col = "black")+
  # geom_path(data = data, aes(x = lon, y = lat, col = id), size = .75)+
  coord_sf(xlim = c(275,300), ylim = c(25,42))+
  scale_fill_gradientn(name = "ADT\n(m)", colours = oce::oceColorsJet(120))+
  geom_point(data = AE1812, aes(x = Lon2, y = Lat, shape=Region), size = 3, color="darkgrey") +
  geom_point(data = AE1812 %>% filter(date=="5/11/2018"), aes(x = Lon2, y = Lat, shape=Region), size = 4, color="black") +
  scale_shape_manual(values=c(15,16,17),  breaks=c("CW", "GS", "SS"),
                     labels=c("Coastal Waters", "Gulf Stream", "Sargasso Sea")) +
  labs(title = "May 11, 2018", x = "", y = "")+
  theme_bw()+
  guides(shape = guide_legend(order = 1)) +
  theme(axis.text = element_text(size = 14, colour = 1))

ggsave("AE1812_ADT_11052018.png", plot = ae1812_0511, device = "png", path = NULL, scale = 1, width = 25, height = 15, units = "cm", dpi = 300)

### 05.12.2018 ###
ae1812_0512<-ggplot()+
  geom_raster(data = adt_total %>% filter(date=="2018-05-12"),  aes(x = x, y = y, fill = Absolute.dynamic.topography), interpolate = TRUE)+
  geom_polygon(data = east_coast, aes(x=long, y = lat, group = group), fill="#7D7D7D") + 
  coord_fixed(xlim = c(275, 300),  ylim = c(23, 42), ratio = 1.3) +
  geom_sf(data = spData::world, fill = "grey80", col = "black")+
  # geom_path(data = data, aes(x = lon, y = lat, col = id), size = .75)+
  coord_sf(xlim = c(275,300), ylim = c(25,42))+
  scale_fill_gradientn(name = "ADT\n(m)", colours = oce::oceColorsJet(120))+
  geom_point(data = AE1812, aes(x = Lon2, y = Lat, shape=Region), size = 3, color="darkgrey") +
  geom_point(data = AE1812 %>% filter(date=="5/12/2018"), aes(x = Lon2, y = Lat, shape=Region), size = 4, color="black") +
  scale_shape_manual(values=c(15,16,17),  breaks=c("CW", "GS", "SS"),
                     labels=c("Coastal Waters", "Gulf Stream", "Sargasso Sea")) +
  labs(title = "May 12, 2018", x = "", y = "")+
  theme_bw()+
  guides(shape = guide_legend(order = 1)) +
  theme(axis.text = element_text(size = 14, colour = 1))

ggsave("AE1812_ADT_12052018.png", plot = ae1812_0512, device = "png", path = NULL, scale = 1, width = 25, height = 15, units = "cm", dpi = 300)

### 05.13.2018 ###
ae1812_0513<-ggplot()+
  geom_raster(data = adt_total %>% filter(date=="2018-05-13"),  aes(x = x, y = y, fill = Absolute.dynamic.topography), interpolate = TRUE)+
  geom_polygon(data = east_coast, aes(x=long, y = lat, group = group), fill="#7D7D7D") + 
  coord_fixed(xlim = c(275, 300),  ylim = c(23, 42), ratio = 1.3) +
  geom_sf(data = spData::world, fill = "grey80", col = "black")+
  # geom_path(data = data, aes(x = lon, y = lat, col = id), size = .75)+
  coord_sf(xlim = c(275,300), ylim = c(25,42))+
  scale_fill_gradientn(name = "ADT\n(m)", colours = oce::oceColorsJet(120))+
  geom_point(data = AE1812, aes(x = Lon2, y = Lat, shape=Region), size = 3, color="darkgrey") +
  geom_point(data = AE1812 %>% filter(date=="5/13/2018"), aes(x = Lon2, y = Lat, shape=Region), size = 4, color="black") +
  scale_shape_manual(values=c(15,16,17),  breaks=c("CW", "GS", "SS"),
                     labels=c("Coastal Waters", "Gulf Stream", "Sargasso Sea")) +
  labs(title = "May 13, 2018", x = "", y = "")+
  theme_bw()+
  guides(shape = guide_legend(order = 1)) +
  theme(axis.text = element_text(size = 14, colour = 1))

ggsave("AE1812_ADT_13052018.png", plot = ae1812_0513, device = "png", path = NULL, scale = 1, width = 25, height = 15, units = "cm", dpi = 300)

### 05.14.2018 ###
ae1812_0514<-ggplot()+
  geom_raster(data = adt_total %>% filter(date=="2018-05-14"),  aes(x = x, y = y, fill = Absolute.dynamic.topography), interpolate = TRUE)+
  geom_polygon(data = east_coast, aes(x=long, y = lat, group = group), fill="#7D7D7D") + 
  coord_fixed(xlim = c(275, 300),  ylim = c(23, 42), ratio = 1.3) +
  geom_sf(data = spData::world, fill = "grey80", col = "black")+
  # geom_path(data = data, aes(x = lon, y = lat, col = id), size = .75)+
  coord_sf(xlim = c(275,300), ylim = c(25,42))+
  scale_fill_gradientn(name = "ADT\n(m)", colours = oce::oceColorsJet(120))+
  geom_point(data = AE1812, aes(x = Lon2, y = Lat, shape=Region), size = 3, color="darkgrey") +
  geom_point(data = AE1812 %>% filter(date=="5/14/2018"), aes(x = Lon2, y = Lat, shape=Region), size = 4, color="black") +
  scale_shape_manual(values=c(15,16,17),  breaks=c("CW", "GS", "SS"),
                     labels=c("Coastal Waters", "Gulf Stream", "Sargasso Sea")) +
  labs(title = "May 14, 2018", x = "", y = "")+
  theme_bw()+
  guides(shape = guide_legend(order = 1)) +
  theme(axis.text = element_text(size = 14, colour = 1))

ggsave("AE1812_ADT_14052018.png", plot = ae1812_0514, device = "png", path = NULL, scale = 1, width = 25, height = 15, units = "cm", dpi = 300)

### 05.15.2018 ###
ae1812_0515<-ggplot()+
  geom_raster(data = adt_total %>% filter(date=="2018-05-15"),  aes(x = x, y = y, fill = Absolute.dynamic.topography), interpolate = TRUE)+
  geom_polygon(data = east_coast, aes(x=long, y = lat, group = group), fill="#7D7D7D") + 
  coord_fixed(xlim = c(275, 300),  ylim = c(23, 42), ratio = 1.3) +
  geom_sf(data = spData::world, fill = "grey80", col = "black")+
  # geom_path(data = data, aes(x = lon, y = lat, col = id), size = .75)+
  coord_sf(xlim = c(275,300), ylim = c(25,42))+
  scale_fill_gradientn(name = "ADT\n(m)", colours = oce::oceColorsJet(120))+
  geom_point(data = AE1812, aes(x = Lon2, y = Lat, shape=Region), size = 3, color="darkgrey") +
  geom_point(data = AE1812 %>% filter(date=="5/15/2018"), aes(x = Lon2, y = Lat, shape=Region), size = 4, color="black") +
  scale_shape_manual(values=c(15,16,17),  breaks=c("CW", "GS", "SS"),
                     labels=c("Coastal Waters", "Gulf Stream", "Sargasso Sea")) +
  labs(title = "May 15, 2018", x = "", y = "")+
  theme_bw()+
  guides(shape = guide_legend(order = 1)) +
  theme(axis.text = element_text(size = 14, colour = 1))

ggsave("AE1812_ADT_15052018.png", plot = ae1812_0515, device = "png", path = NULL, scale = 1, width = 25, height = 15, units = "cm", dpi = 300)


