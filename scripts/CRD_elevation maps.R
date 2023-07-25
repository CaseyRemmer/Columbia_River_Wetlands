
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

BCdemK<-raster("C:/Users/Casey/Documents/cdem_dem_082K_tif/cdem_dem_082K.tif")
BCdemJ<-raster("C:/Users/Casey/Documents/cdem_dem_082J_tif/cdem_dem_082J.tif")
BCdemN<-raster("C:/Users/Casey/Documents/cdem_dem_082N_tif/cdem_dem_082N.tif")
BCdemM<-raster("C:/Users/Casey/Documents/cdem_dem_082M_tif/cdem_dem_082M.tif")

# max extent
ext<- extent(c(-120.0001, -113.9999,49.9999, 52.0001))


# Create an empty raster with the desired extent and resolution
r_out <- raster(ext, resolution = 0.0002083333)

# Combine the rasters into the output raster
r_out[] <- c(BCdemN[], BCdemM[],BCdemK[], BCdemJ[])

# Save the output raster to a file
writeRaster(r_out, "output.tif", format = "GTiff", overwrite = TRUE)


sites3<-st_as_sf(sites, coords = c("Easting", "Northing"))
sites3_sf<-st_set_crs(sites3, "+proj=utm +zone=11 +datum=WGS84")

BC_watershed<-st_read("C:/Users/Casey/Downloads/Columbia_Basin_Watershed_Boundary/Columbia_Basin_Watershed_Boundary.shp")
BC_watershed<-st_transform(BC_watershed, "+proj=utm +zone=11 +datum=WGS84")


#----------
O4 <- raster("C:/Users/Casey/Downloads/IsotopeMaps/IsotopeMaps/O04c.asc") #april
O5 <- raster("C:/Users/Casey/Downloads/IsotopeMaps/IsotopeMaps/O05c.asc") #may
O6 <- raster("C:/Users/Casey/Downloads/IsotopeMaps/IsotopeMaps/O06c.asc") #june
O7 <- raster("C:/Users/Casey/Downloads/IsotopeMaps/IsotopeMaps/O07c.asc") #july
O8 <- raster("C:/Users/Casey/Downloads/IsotopeMaps/IsotopeMaps/O08c.asc") #august
O9 <- raster("C:/Users/Casey/Downloads/IsotopeMaps/IsotopeMaps/O09c.asc") #Sept


##what if we didnt project it and just regressed them
##spring

BCdemK_prof <- projectRaster(BCdemK,
                            crs = crs(O4))
Kcrop4<-crop(O4,BCdemK)

Kcrop4_df<-rasterToPoints(Kcrop4, fun=NULL, spatial=TRUE)
dempoints<-extract(BCdemK_prof, Kcrop4_df)
head(Kcrop4_df$O04c)

Kcrop5<-crop(O5,BCdemK)

Kcrop5_df<-rasterToPoints(Kcrop5, fun=NULL, spatial=TRUE)
head(Kcrop5_df$O05c)
Kcrop5_df$O05c[1]
Kcrop4_df$O04c[1]

spring_dt4<-cbind(Kcrop4_df$O04c,dempoints)
spring_dt5<-cbind(Kcrop5_df$O05c,dempoints)
spring_dt<-rbind(spring_dt4,spring_dt5)
colnames(spring_dt)<-c("O", "elevation")

ggplot(as.data.frame(spring_dt))+geom_point(aes(y=O, x=elevation))
lm(O~elevation, as.data.frame(spring_dt))



##okay so now J N and M?
##loop??

BCdemK<-raster("C:/Users/Casey/Documents/cdem_dem_082K_tif/cdem_dem_082K.tif")
BCdemJ<-raster("C:/Users/Casey/Documents/cdem_dem_082J_tif/cdem_dem_082J.tif")
BCdemN<-raster("C:/Users/Casey/Documents/cdem_dem_082N_tif/cdem_dem_082N.tif")
BCdemM<-raster("C:/Users/Casey/Documents/cdem_dem_082M_tif/cdem_dem_082M.tif")

dems<-c(BCdemK,BCdemJ,BCdemN,BCdemM)
spO<-c(O4, O5)

spring_dt<-data.frame()


for (i in 1:4){
  for (n  in 1:2){
demsproj <- projectRaster(dems[[i]],
                             crs = crs(O4))
Kcrop<-crop(spO[[n]],demsproj)

Kcrop_df<-rasterToPoints(Kcrop, fun=NULL, spatial=TRUE)
dempoints<-extract(demsproj, Kcrop_df)
dempoints<-as.data.frame(dempoints)
colnames(dempoints)<-c("elev")
Kcrop_df<-as.data.frame(Kcrop_df)
colnames(Kcrop_df)<-c("O18")
result<-cbind(Kcrop_df, dempoints)
# Add the result to the data frame
spring_dt <- rbind(spring_dt, data.frame(result))
  }
  
}

write.csv(spring_dt, file="spring_elevation_isotope_data.csv")
spring_dt<-read.csv(file.choose())



colnames(spring_dt)[colnames(spring_dt) == "O18"] <- "O"

spr.lm<-lm(elev~O, spring_dt)
ggplot(spring_dt, aes(elev, O18)) 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red")
  
new_sp<-as.data.frame(data_SW_Sp$O)
colnames(new_sp) <- "O"

sp.elev.predict<-predict(spr.lm, new_sp)
write.csv(sp.elev.predict, file="predicted elevations spring.csv")

##calculating the elevation of the recharge zone of the springs



##summer

dems<-c(BCdemK,BCdemJ,BCdemN,BCdemM)
spO<-c(O6, O7)

summer_dt<-data.frame()


for (i in 1:4){
  for (n  in 1:2){
    demsproj <- projectRaster(dems[[i]],
                              crs = crs(O4))
    Kcrop<-crop(spO[[n]],demsproj)
    
    Kcrop_df<-rasterToPoints(Kcrop, fun=NULL, spatial=TRUE)
    
    dempoints<-extract(demsproj, Kcrop_df)
    dempoints<-as.data.frame(dempoints)
    colnames(dempoints)<-c("elev")
    Kcrop_df<-as.data.frame(Kcrop_df)
    colnames(Kcrop_df)<-c("O18")
    result<-cbind(Kcrop_df, dempoints)
    # Add the result to the data frame
    summer_dt <- rbind(summer_dt, data.frame(result))
  }
  
}

write.csv(summer_dt, file="summer_elevation_isotope_data.csv")

summer_dt<-read.csv(file.choose())


colnames(summer_dt)[colnames(summer_dt) == "O18"] <- "O"
summ.lm<-lm(elev~O, summer_dt)
ggplot(summer_dt, aes(elev, O))+
geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red")
new_sum<-as.data.frame(data_SW_S$O)
colnames(new_sum) <- "O"

sum.elev.predict<-predict(summ.lm, new_sum)
write.csv(sum.elev.predict, file="predicted elevations summer.csv")

##fall
dems<-c(BCdemK,BCdemJ,BCdemN,BCdemM)
spO<-c(O8, O9)
fall_dt<-data.frame()


for (i in 1:4){
  for (n  in 1:2){
    demsproj <- projectRaster(dems[[i]],
                              crs = crs(O4))
    Kcrop<-crop(spO[[n]],demsproj)
    
    Kcrop_df<-rasterToPoints(Kcrop, fun=NULL, spatial=TRUE)
    
    dempoints<-extract(demsproj, Kcrop_df)
    dempoints<-as.data.frame(dempoints)
    colnames(dempoints)<-c("elev")
    Kcrop_df<-as.data.frame(Kcrop_df)
    colnames(Kcrop_df)<-c("O18")
    result<-cbind(Kcrop_df, dempoints)
    # Add the result to the data frame
    fall_dt <- rbind(fall_dt, data.frame(result))
  }
  
}

write.csv(fall_dt, file="fall_elevation_isotope_data.csv")

fall_dt<-read.csv(file.choose())


colnames(fall_dt)[colnames(fall_dt) == "O18"] <- "O"
fall.lm<-lm(elev~O, fall_dt)
ggplot(fall_dt, aes(elev, O))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red")
new_fall<-as.data.frame(data_SW_F$O)
colnames(new_fall) <- "O"

fall.elev.predict<-predict(fall.lm, new_fall)
write.csv(fall.elev.predict, file="predicted elevations fall.csv")



###mean annual precip
Oma <- raster("C:/Users/Casey/Downloads/IsotopeMaps/IsotopeMaps/Oma.asc") 

##spring

BCdemK_prof <- projectRaster(BCdemK,
                             crs = crs(Oma))
Kcrop4<-crop(Oma,BCdemK)

Kcrop4_df<-rasterToPoints(Kcrop4, fun=NULL, spatial=TRUE)
dempoints<-extract(BCdemK_prof, Kcrop4_df)

Kcrop5<-crop(O5,BCdemK)

Kcrop5_df<-rasterToPoints(Kcrop5, fun=NULL, spatial=TRUE)
head(Kcrop5_df$O05c)
Kcrop5_df$O05c[1]
Kcrop4_df$O04c[1]

spring_dt4<-cbind(Kcrop4_df$O04c,dempoints)
spring_dt5<-cbind(Kcrop5_df$O05c,dempoints)
spring_dt<-rbind(spring_dt4,spring_dt5)
colnames(spring_dt)<-c("O", "elevation")

ggplot(as.data.frame(spring_dt))+geom_point(aes(y=O, x=elevation))
lm(O~elevation, as.data.frame(spring_dt))



##okay so now J N and M?
##loop??

BCdemK<-raster("C:/Users/Casey/Documents/cdem_dem_082K_tif/cdem_dem_082K.tif")
BCdemJ<-raster("C:/Users/Casey/Documents/cdem_dem_082J_tif/cdem_dem_082J.tif")
BCdemN<-raster("C:/Users/Casey/Documents/cdem_dem_082N_tif/cdem_dem_082N.tif")
BCdemM<-raster("C:/Users/Casey/Documents/cdem_dem_082M_tif/cdem_dem_082M.tif")

dems<-c(BCdemK,BCdemJ,BCdemN,BCdemM)


ma_dt<-data.frame()


for (i in 1:4){
    demsproj <- projectRaster(dems[[i]],
                              crs = crs(Oma))
    Kcrop<-crop(Oma,demsproj)
    
    Kcrop_df<-rasterToPoints(Kcrop, fun=NULL, spatial=TRUE)
    dempoints<-extract(demsproj, Kcrop_df)
    dempoints<-as.data.frame(dempoints)
    colnames(dempoints)<-c("elev")
    Kcrop_df<-as.data.frame(Kcrop_df)
    colnames(Kcrop_df)<-c("O")
    result<-cbind(Kcrop_df, dempoints)
    # Add the result to the data frame
    ma_dt <- rbind(spring_dt, data.frame(result))
  }
  

write.csv(ma_dt, file="ma_elevation_isotope_data.csv")
#ma_dt<-read.csv(file.choose())



su<-lm(elev~O, ma_dt)
ggplot(ma_dt, aes(elev, O))+
geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red")

new_sp<-as.data.frame(data_SW_Sp$O)
colnames(new_sp) <- "O"

sp.elev.predict<-predict(ma.lm, new_sp)
write.csv(sp.elev.predict, file="predicted elevations spring.csv")

##summer
new_sum<-as.data.frame(data_SW_S$O)
colnames(new_sum) <- "O"

sum.elev.predict<-predict(ma.lm, new_sum)
write.csv(sum.elev.predict, file="predicted elevations summer.csv")

##fall
new_fall<-as.data.frame(data_SW_F$O)
colnames(new_fall) <- "O"

fall.elev.predict<-predict(ma.lm, new_fall)
write.csv(fall.elev.predict, file="predicted elevations fall.csv")


