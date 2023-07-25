#testing the bedrock geology maps for the Columbia River wetlands manuscrip revivsions

library(c("cowplot", "googleway", "ggplot2", "ggrepel", 
          "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata", "rgeos","ggmap","maps","mapproj",
          "mapdata","maptools","raster","rgdal","dismo"))

library(cowplot)
library(ggrepel)
library(ggspatial)
library(rgeos)
library(ggmap)
library(rgdal)

library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2)


#sites<-read.csv("C:/Users/crrem/Dropbox/documents/UWaterloo/columbia river delta/GIS/GIS coordinates for all CRD sites.csv")
sites<-read.csv("C:/Users/Casey/Downloads/GIS coordinates for all CRD sites (1).csv")




base = get_map(location=c(-117,50,-115.5,51.33), zoom=10, source="stamen", maptype="terrain")
#convert sites from UTM to decimal degrees
utmraw<-cbind(sites$Easting,sites$Northing)
sputm <- SpatialPoints(utmraw, proj4string=CRS("+proj=utm +zone=11 +datum=WGS84")) 
spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))
dec.deg<-as.data.frame(spgeo)

sites2<-cbind(sites, dec.deg)

sites2_sf<-st_as_sf(sites2, coords = c("coords.x1", "coords.x2"))
sites2_sf<-st_set_crs(sites2_sf, "+proj=utm +zone=11 +datum=WGS84")

sites3<-st_as_sf(sites2, coords = c("Easting", "Northing"))
sites3_sf<-st_set_crs(sites3, "+proj=utm +zone=11 +datum=WGS84")

#st_crs(sites2_sf)<-4326



map1 = ggmap(base)+xlab("Longitude") + ylab("Latitude") + 
  geom_point(data=sites2, x=sites2$coords.x1, y=sites2$coords.x2)

map1+
  theme()


BCgeo<-st_read("C:/Users/Casey/Downloads/bedrockgeology2018/BC_bedrock_ll83.shp")

BCgeo_crop<-st_crop(BCgeo, c(xmin=-117, xmax=-115.5, ymin=50, ymax=51.33))

BCgeo_crop84<-st_transform(BCgeo, "+proj=utm +zone=11 +datum=WGS84")

#polygon test
poly<-readOGR("C:/Users/Casey/Downloads/poly.kml")
polysf<-st_as_sf(poly)
ply<-st_transform(polysf, "+proj=utm +zone=11 +datum=WGS84")
ggplot()+geom_sf(data=poly)


poly_test<-st_crop(BCgeo_crop84, ply)
tst2<-st_intersection(ply,BCgeo_crop84)
#downloading the watershed boundary for the columbia river
#cwb<-st_read("C:/Users/Casey/Downloads/Columbia_Basin_Watershed_Boundary/Columbia_Basin_Watershed_Boundary.shp")
#cwb_crop<-st_crop(cwb, c(xmin=-117, xmax=-115.5, ymin=50, ymax=51.33))
#cwb_utm<-st_transform(cwb_crop, "+proj=utm +zone=11 +datum=WGS84")
#rktyp<-BCgeo_crop84[,13]
#cwb_tst<-st_intersection(cwb_utm,rktyp)



##to get good labels
#install.packages("devtools")
library(devtools)
devtools::install_github("yutannihilation/ggsflabel")
library(ggsflabel)

##changing to 3 types, replacing all spring & wells with "groundwater"-------
sites2$Site.type[sites2$Site.type=="Spring Pond"] <- 'Groundwater'
sites2$Site.type[sites2$Site.type=="Spring"] <- 'Groundwater'
sites2$Site.type[sites2$Site.type=="Well"] <- 'Groundwater'

   
sites3_sf$Site.type[sites3_sf$Site.type=="Spring Pond"] <- 'Groundwater'
sites3_sf$Site.type[sites3_sf$Site.type=="Spring"] <- 'Groundwater'
sites3_sf$Site.type[sites3_sf$Site.type=="Well"] <- 'Groundwater'
   

###THIS IS IT THIS WORKS ##now adding labels above, then insets--------------
rocktype<-
  ggplot()+ geom_sf(data=tst2, aes(fill=rock_type),lwd = 0,
                    colour = "white", alpha=0.5)+
  xlab("Longitude") + ylab("Latitude") + 
  geom_sf(data=sites3_sf, size=2, aes(shape=Site.type))+
  scale_shape_manual(name="", values=c(3, 13, 2, 17,  16))+
  geom_sf_text_repel(data=sites3_sf, aes(label=Site.number), size=3.5, max.overlaps=30, fontface="bold")+
  theme_minimal() +
  scale_fill_discrete(name="Geology",labels = label_wrap(25))+
  theme(legend.key.width = unit(0.5,"cm"),legend.key.size = unit(0.5, "cm"),
        legend.text = element_text(size=8),legend.position = c(1.15, 0.5))+
  guides(fill = guide_legend(byrow = TRUE))
  


##insets
#library(tidyverse)
##first insert (lower)----------------------
inset1<-dplyr::filter(sites2, Site.number %in% c('41', '88'))
#making a dataframe with just the inset points for labelling purposes
inset1full<-dplyr::filter(sites2, Easting >= min(inset1[,3]) & Easting <= max(inset1[,3]) & Northing >= min(inset1[,4]) & Northing <= max(inset1[,4])) 
inset1fullx<-st_as_sf(inset1full, coords = c("Easting", "Northing"))
inset1full_sf<-st_set_crs(inset1fullx, "+proj=utm +zone=11 +datum=WGS84")

##second inset (higher)----------------
inset2<-dplyr::filter(sites2, Site.number %in% c('64', '30'))
#making a dataframe with just the inset points for labelling purposes
inset2full<-dplyr::filter(sites2, Easting >= min(inset2[,3]) & Easting <= max(inset2[,3]) & Northing >= min(inset2[,4]) & Northing <= max(inset2[,4])) 
inset2fullx<-st_as_sf(inset2full, coords = c("Easting", "Northing"))
inset2full_sf<-st_set_crs(inset2fullx, "+proj=utm +zone=11 +datum=WGS84")
    
  

##attempt two (to be used)--------------------
geo_crop<-st_crop(x=BCgeo_crop84,
                  xmin=(min(inset1[,3])-1000), 
                  xmax=max((inset1[,3])+1000),
                  ymin=min((inset1[,4])-1000), 
                  ymax=max((inset1[,4])+1000))

geo_crop2<-st_crop(x=BCgeo_crop84,
                  xmin=(min(inset2[,3])-1000), 
                  xmax=max((inset2[,3])+1000),
                  ymin=min((inset2[,4])-1000), 
                  ymax=max((inset2[,4])+1000))


rock_inset<- 
  ggplot()+ geom_sf(data=geo_crop, aes(fill=rock_type),lwd = 0,
                               colour = "white", alpha=0.5)+
  geom_sf(data=inset1full_sf, size=3)+
  geom_sf_text_repel(data=inset1full_sf, aes(label=Site.number), size=3.5, max.overlaps=30, fontface="bold")+
  theme_minimal() +
  scale_fill_discrete(name="Geology",labels = label_wrap(25))+
  theme(legend.key.width = unit(0.5,"cm"),legend.key.size = unit(0.5, "cm"),
        legend.text = element_text(size=8),legend.position = c(1.15, 0.6))+
  guides(fill = guide_legend(byrow = TRUE))+
  coord_sf(
    xlim = c(min(inset1[,3])-1000, max(inset1[,3])+1000),
    ylim = c(min(inset1[,4])-1000, max(inset1[,4])+1000),
    expand = FALSE)

rock_inset2<- 
  ggplot()+ geom_sf(data=geo_crop2, aes(fill=rock_type),lwd = 0,
                    colour = "white", alpha=0.5)+
  geom_sf(data=inset2full_sf, size=3)+
  geom_sf_text_repel(data=inset2full_sf, aes(label=Site.number), size=3.5, max.overlaps=30,fontface="bold")+
  theme_minimal() +
  scale_fill_discrete(name="Geology",labels = label_wrap(25))+
  theme(legend.key.width = unit(0.5,"cm"),legend.key.size = unit(0.5, "cm"),
        legend.text = element_text(size=8),legend.position = c(1.15, 0.6))+
  guides(fill = guide_legend(byrow = TRUE))+
  coord_sf(
    xlim = c(min(inset2[,3])-1000, max(inset2[,3])+1000),
    ylim = c(min(inset2[,4])-1000, max(inset2[,4])+1000),
    expand = FALSE)


rock_inset <- 
  rock_inset +
  geom_rect( aes(
    xmin=(min(inset1[,3])-1000), 
    xmax=max((inset1[,3])+1000),
    ymin=min((inset1[,4])-1000), 
    ymax=max((inset1[,4])+1000)),
    fill = NA,
    colour = "red", ##figure out colour
    lwd=2)


rock_inset2 <- 
  rock_inset2 +
  geom_rect( aes(
    xmin=(min(inset2[,3])-1000), 
    xmax=max((inset2[,3])+1000),
    ymin=min((inset2[,4])-1000), 
    ymax=max((inset2[,4])+1000)),
    fill = NA,
    colour = "magenta1", ##figure out colour
    lwd=2)

rocktype_ <- 
  rocktype +
  geom_rect( aes(
    xmin=min(inset1[,3]), 
    xmax=max(inset1[,3]),
    ymin = min(inset1[,4]),
    ymax=max(inset1[,4])),
    fill = NA,
    colour = "red",
    lwd=2) +
  geom_rect( aes(
    xmin=(min(inset2[,3])-1000), 
    xmax=max((inset2[,3])+1000),
    ymin=min((inset2[,4])-1000), 
    ymax=max((inset2[,4])+1000)),
    fill = NA,
    colour = "magenta1",#figure out colour
    lwd=2)

##inset with main map------------
Geology_map<-
  ggdraw(rocktype_) +
  draw_plot(
    {
      rock_inset+
        theme(legend.position = "none",
              axis.text.x = element_blank(),
              axis.title.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.y = element_blank())
        },
    # The distance along a (0,1) x-axis to draw the left edge of the plot
    x = 0.22, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.08,
    width = 0.3, 
    height = 0.28)+
  draw_plot(
    {
      rock_inset2+
        theme(legend.position = "none",
              axis.text.x = element_blank(),
              axis.title.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.y = element_blank())
    },
    # The distance along a (0,1) x-axis to draw the left edge of the plot
    x = 0.5, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.7,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.3, 
    height = 0.3)
 

##this works and looks good
ggsave("Geology_map.pdf")
ggsave("Geology_map.png")




###sitemap------------


# courtesy R Lovelace
ggmap_rast <- function(map){
  map_bbox <- attr(map, 'bb') 
  .extent <- extent(as.numeric(map_bbox[c(2,4,1,3)]))
  my_map <- raster(.extent, nrow= nrow(map), ncol = ncol(map))
  rgb_cols <- setNames(as.data.frame(t(col2rgb(map))), c('red','green','blue'))
  red <- my_map
  values(red) <- rgb_cols[['red']]
  green <- my_map
  values(green) <- rgb_cols[['green']]
  blue <- my_map
  values(blue) <- rgb_cols[['blue']]
  stack(red,green,blue)
}


base.rast <-ggmap_rast(map = base) # convert google map to raster object
base.poly <- mask(base.rast, poly) ## has to be OGR not sf object

base.poly.df <- data.frame(rasterToPoints(base.poly))
base.sp<- SpatialPoints(base.poly.df , proj4string=CRS("+proj=longlat")) 

utm.base<-spTransform(base.sp, CRS("+proj=utm +zone=11 +datum=WGS84"))
utm.base.df<-as.data.frame(utm.base)

utm.base.sf<-st_as_sf(utm.base.df, coords = c("x", "y"))

utm.base.sf.proj<-st_set_crs(utm.base.sf, "+proj=utm +zone=11 +datum=WGS84")



##this works but its distorted 
ggplot(utm.base.df) + 
  xlab("Longitude") + ylab("Latitude") + 
  geom_point(aes(x=x, y=y, col=rgb(layer.1/255, layer.2/255, layer.3/255)))+
  scale_color_identity()




##this is working i think testing zone 
##need to add insets

site_map<-
ggplot()+
geom_point(data=base.poly.df, aes(x=x, y=y, col=rgb(layer.1/255, layer.2/255, layer.3/255)))+
scale_color_identity()+
xlab("Longitude") + ylab("Latitude")+
geom_point(data=sites2, size=2.5, aes(shape=Site.type, x=coords.x1, y=coords.x2))+
scale_shape_manual(name="", values=c(3, 13, 2, 17,  16))+
geom_text_repel(data=sites2, aes(label=Site.number,x=coords.x1, y=coords.x2), size=3.5, max.overlaps=30, fontface="bold")+
theme_minimal() +
theme(legend.key.width = unit(0.5,"cm"),legend.key.size = unit(0.5, "cm"),
        legend.text = element_text(size=8),legend.position = c(1.15, 0.5))+
   guides(fill = guide_legend(byrow = TRUE))

#using ggmap
site_map<-
  map1+
  xlab("Longitude") + ylab("Latitude")+
  geom_point(data=sites2, size=2.5, aes(shape=Site.type, x=coords.x1, y=coords.x2))+
  scale_shape_manual(name="", values=c(3, 13, 2, 17,  16))+
  geom_text_repel(data=sites2, aes(label=Site.number,x=coords.x1, y=coords.x2), size=3.5, max.overlaps=20, fontface="bold")+
  theme_minimal() +
  theme(legend.key.width = unit(0.5,"cm"),legend.key.size = unit(0.5, "cm"),
        legend.text = element_text(size=7),legend.position = c(1.1, 0.5))+
  guides(fill = guide_legend(byrow = TRUE))

##first insert (lower) for base map----------------------
inset1<-dplyr::filter(sites2, Site.number %in% c('41', '88'))
#making a dataframe with just the inset points for labelling purposes
inset1full_base<-dplyr::filter(sites2, coords.x1 >= min(inset1[,5]) & coords.x1 <= max(inset1[,5]) & coords.x2 >= min(inset1[,6]) & coords.x2 <= max(inset1[,6])) 


baseinset1 = get_map(location=c(min(inset1[,5])-0.01,min(inset1[,6])-0.01,max(inset1[,5])+0.01,max(inset1[,6])+0.01), zoom=13, source="stamen", maptype="terrain")
base_map_ins1=ggmap(baseinset1)


##second inset for base map
inset2_base<-dplyr::filter(sites2, Site.number %in% c('64', '30'))
#making a dataframe with just the inset points for labelling purposes
inset2full_base<-dplyr::filter(sites2, coords.x1 >= min(inset2_base[,5]) & coords.x1 <= max(inset2_base[,6]) & coords.x2 >= min(inset2_base[,5]) & coords.x2 <= max(inset2_base[,6])) 


##turning into a raster and dataframe but probably not using this-------------
base_crop<-crop(x=base.rast,
                extent(c(xmin=(min(inset1_base[,5])), xmax=max((inset1_base[,5])),
                  ymin=min((inset1_base[,6])), 
                  ymax=max((inset1_base[,6])))))

base_cropdf <- data.frame(rasterToPoints(base_crop))

geo_crop2<-st_crop(x=BCgeo_crop84,
                   xmin=(min(inset2[,3])-1000), 
                   xmax=max((inset2[,3])+1000),
                   ymin=min((inset2[,4])-1000), 
                   ymax=max((inset2[,4])+1000))

##inset 1---------------
base_inset<- 
  base_map_ins1+
  xlab("Longitude") + ylab("Latitude")+
  geom_point(data=sites2, size=2.5, aes(shape=Site.type, x=coords.x1, y=coords.x2))+
  scale_shape_manual(name="", values=c(3, 13, 2, 17,  16))+
  geom_text_repel(data=sites2, aes(label=Site.number,x=coords.x1, y=coords.x2), size=3.5, max.overlaps=20, fontface="bold")+
  theme_minimal() +
  theme(legend.key.width = unit(0.5,"cm"),legend.key.size = unit(0.5, "cm"),
        legend.text = element_text(size=8),legend.position = c(1.1, 0.5))+
  guides(fill = guide_legend(byrow = TRUE))


#####THIS IS WHERE YOU ARE RIGHT NOW

baseinset2 = get_map(location=c(min(inset2[,5])-0.01,min(inset2[,6])-0.01,max(inset2[,5])+0.01,max(inset2[,6])+0.01), zoom=13, source="stamen", maptype="terrain")
base_map_ins2=ggmap(baseinset2)

base_inset2<- 
  base_map_ins2+
  xlab("Longitude") + ylab("Latitude")+
  geom_point(data=sites2, size=2.5, aes(shape=Site.type, x=coords.x1, y=coords.x2))+
  scale_shape_manual(name="", values=c(3, 13, 2, 17,  16))+
  geom_text_repel(data=sites2, aes(label=Site.number,x=coords.x1, y=coords.x2), size=3.5, max.overlaps=20, fontface="bold")+
  theme_minimal() +
  theme(legend.key.width = unit(0.5,"cm"),legend.key.size = unit(0.5, "cm"),
        legend.text = element_text(size=8),legend.position = c(1.1, 0.5))+
  guides(fill = guide_legend(byrow = TRUE))


base_inset <- 
  base_inset +
  geom_rect( aes(
    xmin=(min(inset1[,5])-0.01), 
    xmax=max((inset1[,5])+0.01),
    ymin=min((inset1[,6])-0.01), 
    ymax=max((inset1[,6])+0.01)),
    fill = NA,
    colour = "red", ##figure out colour
    lwd=2)


base_inset2 <- 
  base_inset2 +
  geom_rect( aes(
    xmin=(min(inset2[,5])-0.01), 
    xmax=max((inset2[,5])+0.01),
    ymin=min((inset2[,6])-0.01), 
    ymax=max((inset2[,6])+0.01)),
    fill = NA,
    colour = "magenta1", ##figure out colour
    lwd=2)

site_map <- 
  site_map +
  geom_rect( aes(
    xmin=(min(inset1[,5])-0.01), 
    xmax=max((inset1[,5])+0.01),
    ymin=min((inset1[,6])-0.01), 
    ymax=max((inset1[,6])+0.01)),
    fill = NA,
    colour = "red",
    lwd=2) +
  geom_rect( aes(
    xmin=(min(inset2[,5])-0.01), 
    xmax=max((inset2[,5])+0.01),
    ymin=min((inset2[,6])-0.01), 
    ymax=max((inset2[,6])+0.01)),
    fill = NA,
    colour = "magenta1",#figure out colour
    lwd=2)
 

##site map inset with main map------------
site_map_final<-
  ggdraw(site_map) +
  draw_plot(
    {
      base_inset+
        theme(legend.position = "none",
              axis.text.x = element_blank(),
              axis.title.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.y = element_blank())
    },
    # The distance along a (0,1) x-axis to draw the left edge of the plot
    x = 0.22, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.065,
    width = 0.3, 
    height = 0.28)+
  draw_plot(
    {
      base_inset2+
        theme(legend.position = "none",
              axis.text.x = element_blank(),
              axis.title.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.y = element_blank())
    },
    # The distance along a (0,1) x-axis to draw the left edge of the plot
    x = 0.6, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.65,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.3, 
    height = 0.3)


##this works and looks good
ggsave("site_map.pdf")
ggsave("site_map.png")



