#A first attempt at making maps in R
#need to extract precip isotope values and this will hopefully be for efficent!
#based on tutorial found at https://r-spatial.org/r/2018/10/25/ggplot2-sf.html and https://geocompr.robinlovelace.net/adv-map.html

install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata", "rgeos","ggmap","maps","mapproj",
                   "mapdata","maptools","raster","rgdal","dismo"))

library(c("cowplot", "googleway", "ggplot2", "ggrepel", 
                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata", "rgeos","ggmap","maps","mapproj",
                   "mapdata","maptools","raster","rgdal","dismo"))


library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2)
require(ggplot2)
require(ggmap)
require(maps)
require(mapproj)
require(mapdata)
require(rgeos)
require(maptools)
require(sp)
require(raster)
require(rgdal)
require(dismo)




sites<-read.csv("GIS/GIS coordinates for all CRD sites.csv")
sites<-read.csv(file.choose())


##base map pull and plotting location-------------------------

base = get_map(location=c(-117,50,-115.5,51.33), zoom=10, maptype="terrain", source="stamen")
#convert sites from UTM to decimal degrees
utmraw<-cbind(sites$Easting,sites$Northing)
sputm <- SpatialPoints(utmraw, proj4string=CRS("+proj=utm +zone=11 +datum=WGS84")) 
spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))
dec.deg<-as.data.frame(spgeo)

#add new lat/long to the sites data frame

sites2<-cbind(sites, dec.deg)

map1 = ggmap(base)+xlab("Longitude") + ylab("Latitude") + 
        geom_point(data=sites2, x=sites2$coords.x1, y=sites2$coords.x2)
     


#import SPATIAL precip data---------------------------
#strtaing with H
library(raster)
r4 <- raster("C:/Users/Casey/Dropbox/columbia river delta/GIS/IsotopeMaps/H04c.asc") #april
r5 <- raster("C:/Users/Casey/Dropbox/columbia river delta/GIS/IsotopeMaps/H05c.asc") #may
r6 <- raster("C:/Users/Casey/Dropbox/columbia river delta/GIS/IsotopeMaps/H06c.asc") #june
r7 <- raster("C:/Users/Casey/Dropbox/columbia river delta/GIS/IsotopeMaps/H07c.asc") #july
r8 <- raster("C:/Users/Casey/Dropbox/columbia river delta/GIS/IsotopeMaps/H08c.asc") #august
r9 <- raster("C:/Users/Casey/Dropbox/columbia river delta/GIS/IsotopeMaps/H09c.asc") #Sept
bigr<-cbind(r4,r5,r6,r7,r8,r9)

plot(r4)
#crop the world raster down to the plot extent for each month
e<-extent(-117,-115.5,50,51.33)

rc<-crop(r4, e)

##plot raster on map--------------------------
t<-
  ggmap(base)+xlab("Longitude") + ylab("Latitude") + 
  geom_point(data=sites2, x=sites2$coords.x1, y=sites2$coords.x2)+
  inset_raster(as.raster(rc),-117,-115.5,50,51.33) #plot raster over map

##extract wetland points

wsite <- subset(sites2, Site.type=="Wetland",
                select=Site.number:coords.x2)
wsite_coords<-cbind(wsite$coords.x1,wsite$coords.x2)

extract(rc,wsite_coords)

##build table of extracted values

H<-data.frame(matrix(ncol=6, nrow=nrow(wsite)))
H[,1]=wsite$Site.number
H[,2]=extract(rc,wsite_coords)
names(H)[1]<-"site number"
names(H)[2]<-"H4"

#### OKAY WE ARE READY TO GET LOOPY
#starting with Hydrogen

bigrc<-bigr

for (i in 1:6) {
  bigrc[[i]]<-crop(bigr[[i]], e)
}

for (i in 1:6) {
  H[,i+1]=extract(bigrc[[i]],wsite_coords)
  names(H)[i+1]<- paste("H", i+3)
}

head(H)
write.csv(H, "H2 data for ice free season.csv")
## repeating for the Hydrogen confidence intervals ##HELLO DO WE EVEN NEED THIS??##

Hci4 <- raster("C:/Users/crrem/Dropbox/documents/UWaterloo/columbia river delta/GIS/IsotopeMaps/H04CI.asc") #april
Hci5 <- raster("C:/Users/crrem/Dropbox/documents/UWaterloo/columbia river delta/GIS/IsotopeMaps/H05CI.asc") #may
Hci6 <- raster("C:/Users/crrem/Dropbox/documents/UWaterloo/columbia river delta/GIS/IsotopeMaps/H06CI.asc") #june
Hci7 <- raster("C:/Users/crrem/Dropbox/documents/UWaterloo/columbia river delta/GIS/IsotopeMaps/H07CI.asc") #july
Hci8 <- raster("C:/Users/crrem/Dropbox/documents/UWaterloo/columbia river delta/GIS/IsotopeMaps/H08CI.asc") #august
Hci9 <- raster("C:/Users/crrem/Dropbox/documents/UWaterloo/columbia river delta/GIS/IsotopeMaps/H09CI.asc") #Sept
bigHci<-cbind(Hci4,Hci5,Hci6,Hci7,Hci8,Hci9)

##crop to map extent----------
bigHcic<-bigHci

for (i in 1:6) {
  bigHcic[[i]]<-crop(bigHci[[i]], e)
}

#check on a map
ggmap(base)+xlab("Longitude") + ylab("Latitude") + 
  inset_raster(as.raster(bigHcic[[5]]),-117,-115.5,50,51.33)+ #plot raster over map
  geom_point(data=sites2, x=sites2$coords.x1, y=sites2$coords.x2)

##extract values of H CI at sampling points and place in a table

Hci<-data.frame(matrix(ncol=6, nrow=nrow(wsite)))
Hci[,1]=wsite$Site.number
names(Hci)[1]<-"site number"

for (i in 1:6) {
  Hci[,i+1]=extract(bigHcic[[i]],wsite_coords)
  names(Hci)[i+1]<- paste("Hci", i+3)
}

##nxt up O!------------

O4 <- raster("C:/Users/crrem/Dropbox/documents/UWaterloo/columbia river delta/GIS/IsotopeMaps/O04c.asc") #april
O5 <- raster("C:/Users/crrem/Dropbox/documents/UWaterloo/columbia river delta/GIS/IsotopeMaps/O05c.asc") #may
O6 <- raster("C:/Users/crrem/Dropbox/documents/UWaterloo/columbia river delta/GIS/IsotopeMaps/O06c.asc") #june
O7 <- raster("C:/Users/crrem/Dropbox/documents/UWaterloo/columbia river delta/GIS/IsotopeMaps/O07c.asc") #july
O8 <- raster("C:/Users/crrem/Dropbox/documents/UWaterloo/columbia river delta/GIS/IsotopeMaps/O08c.asc") #august
O9 <- raster("C:/Users/crrem/Dropbox/documents/UWaterloo/columbia river delta/GIS/IsotopeMaps/O09c.asc") #Sept
#missing projection for some reason so adding that in for each raster##
crs(O4)<-"+proj=longlat +datum=WGS84"
crs(O5)<-"+proj=longlat +datum=WGS84"
crs(O6)<-"+proj=longlat +datum=WGS84"
crs(O7)<-"+proj=longlat +datum=WGS84"
crs(O8)<-"+proj=longlat +datum=WGS84"
crs(O9)<-"+proj=longlat +datum=WGS84"

bigO<-cbind(O4,O5,O6,O7,O8,O9)

##crop to map extent
bigOc<-bigO

for (i in 1:6) {
  bigOc[[i]]<-crop(bigO[[i]], e)
}

#check on a map
ggmap(base)+xlab("Longitude") + ylab("Latitude") + 
  inset_raster(as.raster(bigOc[[1]]),-117,-115.5,50,51.33)+ #plot raster over map
  geom_point(data=sites2, x=sites2$coords.x1, y=sites2$coords.x2)

##extract values of o at sampling points and place in a table

O<-data.frame(matrix(ncol=6, nrow=nrow(wsite)))
O[,1]=wsite$Site.number
names(O)[1]<-"site number"

for (i in 1:6) {
  O[,i+1]=extract(bigOc[[i]],wsite_coords)
  names(O)[i+1]<- paste("O", i+3)
}

head(O)
write.csv(O, "18O data for ice free season.csv")
###import the conductivity data from NEON###

cond_NEON <- read.csv("Conductivity/NEON cond data for mixing model_csv.csv") 


## Bring in data for the mixing models
##mixing modelllss----------
##SPRING MODEL-----------------------------------------------------

Spring_CRD<-read.csv("C:/Users/Casey/Dropbox/columbia river delta/inputs- raw isotopes/Spring_isotopes with conductivity.csv")

#check on map

ggplot(Spring_CRD, aes(x=O, y=Cond., col=Type))+
  geom_point(size=2)+ theme_classic()

### get data ready for the moxing model

library(simmr)

data_W_Sp <- subset(Spring_CRD, Type == "Wetland", select = c("O","H","Cond.", "Type", "Site"))
data_R_Sp <- subset(Spring_CRD, Type == "River", select = c("O","H","Cond.", "Type","Site"))
data_S_Sp <- subset(Spring_CRD, Type == "Spring", select = c("O","H","Cond.", "Type","Site"))
data_Wl_Sp <- subset(Spring_CRD, Type == "Well", select = c("O","H","Cond.", "Type","Site"))
data_SW_Sp <- rbind(data_S_Sp, data_Wl_Sp)


mix_Sp= matrix(c(data_W_Sp$O, data_W_Sp$H, data_W_Sp$Cond.), ncol=3, nrow=nrow(data_W_Sp))
colnames(mix_Sp)= c("d18O","d2H", "conductivity")

## create spring mean + sd for precipitation ## dont need to re-run after outlier removal

cond_NEON$date<-as.Date(cond_NEON$date, format = "%Y-%m-%d") ##converting dates from factor to dates 
cond_NEON_Sp<-subset(cond_NEON, date > "2019-04-01" & date < "2019-05-31")

s_names = c("River", "Groundwater", "Precipitation") ##only need to run once, same for all seasons
s_means_Sp = matrix(c(mean(data_R_Sp$O), mean(data_SW_Sp$O), mean(c(O$`O 4`, O$`O 5`)),  mean(data_R_Sp$H), mean(data_SW_Sp$H), mean(c(H$`H 4`, H$`H 5`)), mean(data_R_Sp$Cond.), mean(data_SW_Sp$Cond.), mean(cond_NEON_Sp$precipConductivity)), ncol=3, nrow=3)
s_sds_Sp = matrix(c(sd(data_R_Sp$O), sd(data_SW_Sp$O),sd(c(O$`O 4`, O$`O 5`)) , sd(data_R_Sp$H), sd(data_SW_Sp$H), sd(c(H$`H 4`, H$`H 5`)) , sd(data_R_Sp$Cond.), sd(data_SW_Sp$Cond.), sd(cond_NEON_Sp$precipConductivity)), ncol=3, nrow=3)
grp_Sp = as.integer(c(1:nrow(data_W_Sp)))

##run model

simmr_Spring = simmr_load(mixtures=mix_Sp,
                          source_names=s_names,
                          source_means=s_means_Sp,
                          source_sds=s_sds_Sp,
                          group = grp_Sp)



plot(simmr_Spring,tracers=c(1,2), group = 1:28) ##site 30 is an outlier (mix #7, W #10)
plot(simmr_Spring,group=1:28,tracers=c(1,3)) #site 35 is an outlier (mix #10, W #13): site 84 (mix#25, W#32)
plot(simmr_Spring,group=1:28,tracers=c(2,3))

##need to re run the simmr_in with the outliers removed

###removeing outlier

## now re-run with less wetlands

data_W_Sp <- data_W_Sp[ !(data_W_Sp$Site %in% c(30,35,84)), ]
print (data_W_Sp)

plot(simmr_Spring,tracers=c(1,2), group = 1:25) ##site 30 is an outlier (mix #7, W #10)
plot(simmr_Spring,group=1:25,tracers=c(1,3)) #site 35 is an outlier (mix #10, W #13): site 84 (mix#25, W#32)
plot(simmr_Spring,group=1:25,tracers=c(2,3))

###time to run these mixing model for each wetland!

simmr_out_spring = simmr_mcmc(simmr_Spring)
summary(simmr_out_spring,type='diagnostics')
posterior_predictive(simmr_out_spring)
prior_viz(simmr_out_spring)
spring_results<-summary(simmr_out_spring, group=1:25)
stats_sp<-summary(simmr_out_spring,type='statistics', group=1:25)
quants_sp<-summary(simmr_out_spring,type='quantiles', group=c(1:25))
plot(simmr_out_spring,type='density')
plot(simmr_out_spring,type='boxplot')
plot(simmr_out_spring,type='matrix')

###USE THIS TO MAKE BOXPLOTS OF PROPORTION
b<-boxplot(stats_sp$quantiles$group_1[3,])
boxplot(spring_results$quantiles$group_1["Groundwater",])

pdf("spring 2019 prop boxplots_CRD.pdf",
    width = 9.2, height = 9.2)

# set number of diagrams on one page ##1 wide and 3 heigh (spring/summer/fall)
par(mfrow=c(3,1),mar=c(2,4,2,0.5), oma=c(1.5,1,1,1))
boxplot((spring_results$quantiles$group_1["Groundwater",]),
        (spring_results$quantiles$group_2["Groundwater",]),
        (spring_results$quantiles$group_3["Groundwater",]),
        (spring_results$quantiles$group_4["Groundwater",]),
        (spring_results$quantiles$group_5["Groundwater",]),
        (spring_results$quantiles$group_6["Groundwater",]),
        (spring_results$quantiles$group_7["Groundwater",]),
        (spring_results$quantiles$group_8["Groundwater",]),
        (spring_results$quantiles$group_9["Groundwater",]),
        (spring_results$quantiles$group_10["Groundwater",]),
        (spring_results$quantiles$group_11["Groundwater",]),
        (spring_results$quantiles$group_12["Groundwater",]),
        (spring_results$quantiles$group_13["Groundwater",]),
        (spring_results$quantiles$group_14["Groundwater",]),
        (spring_results$quantiles$group_15["Groundwater",]),
        (spring_results$quantiles$group_16["Groundwater",]),
        (spring_results$quantiles$group_17["Groundwater",]),
        (spring_results$quantiles$group_18["Groundwater",]),
        (spring_results$quantiles$group_19["Groundwater",]),
        (spring_results$quantiles$group_20["Groundwater",]),
        (spring_results$quantiles$group_21["Groundwater",]),
        (spring_results$quantiles$group_22["Groundwater",]),
        (spring_results$quantiles$group_23["Groundwater",]),
        (spring_results$quantiles$group_24["Groundwater",]),
        (spring_results$quantiles$group_25["Groundwater",]),
        names = data_W_Sp$Site, ylab= "Proportion Groundwater", cex.lab=1.25, cex.axis=1.25, las=2)
title(main = "Spring", adj =0, line=1)


boxplot((spring_results$quantiles$group_1["River",]),
        (spring_results$quantiles$group_2["River",]),
        (spring_results$quantiles$group_3["River",]),
        (spring_results$quantiles$group_4["River",]),
        (spring_results$quantiles$group_5["River",]),
        (spring_results$quantiles$group_6["River",]),
        (spring_results$quantiles$group_7["River",]),
        (spring_results$quantiles$group_8["River",]),
        (spring_results$quantiles$group_9["River",]),
        (spring_results$quantiles$group_10["River",]),
        (spring_results$quantiles$group_11["River",]),
        (spring_results$quantiles$group_12["River",]),
        (spring_results$quantiles$group_13["River",]),
        (spring_results$quantiles$group_14["River",]),
        (spring_results$quantiles$group_15["River",]),
        (spring_results$quantiles$group_16["River",]),
        (spring_results$quantiles$group_17["River",]),
        (spring_results$quantiles$group_18["River",]),
        (spring_results$quantiles$group_19["River",]),
        (spring_results$quantiles$group_20["River",]),
        (spring_results$quantiles$group_21["River",]),
        (spring_results$quantiles$group_22["River",]),
        (spring_results$quantiles$group_23["River",]),
        (spring_results$quantiles$group_24["River",]),
        (spring_results$quantiles$group_25["River",]),
        names = data_W_Sp$Site, ylab= "Proportion River", cex.lab=1.25, cex.axis=1.25, las=2)



boxplot((spring_results$quantiles$group_1["Precipitation",]),
        (spring_results$quantiles$group_2["Precipitation",]),
        (spring_results$quantiles$group_3["Precipitation",]),
        (spring_results$quantiles$group_4["Precipitation",]),
        (spring_results$quantiles$group_5["Precipitation",]),
        (spring_results$quantiles$group_6["Precipitation",]),
        (spring_results$quantiles$group_7["Precipitation",]),
        (spring_results$quantiles$group_8["Precipitation",]),
        (spring_results$quantiles$group_9["Precipitation",]),
        (spring_results$quantiles$group_10["Precipitation",]),
        (spring_results$quantiles$group_11["Precipitation",]),
        (spring_results$quantiles$group_12["Precipitation",]),
        (spring_results$quantiles$group_13["Precipitation",]),
        (spring_results$quantiles$group_14["Precipitation",]),
        (spring_results$quantiles$group_15["Precipitation",]),
        (spring_results$quantiles$group_16["Precipitation",]),
        (spring_results$quantiles$group_17["Precipitation",]),
        (spring_results$quantiles$group_18["Precipitation",]),
        (spring_results$quantiles$group_19["Precipitation",]),
        (spring_results$quantiles$group_20["Precipitation",]),
        (spring_results$quantiles$group_21["Precipitation",]),
        (spring_results$quantiles$group_22["Precipitation",]),
        (spring_results$quantiles$group_23["Precipitation",]),
        (spring_results$quantiles$group_24["Precipitation",]),
        (spring_results$quantiles$group_25["Precipitation",]),
        names = data_W_Sp$Site, ylab= "Proportion Precipitation", cex.lab=1.25, cex.axis=1.25, las=2)

dev.off()


outputs_table_sp=matrix(data=NA, ncol=6, nrow=25)
for (i in 1:25) {
  outputs_table_sp[i,1]= stats_sp$statistics[[i]][[2]]
  
}

for (i in 1:25) {
  outputs_table_sp[i,2]= stats_sp$statistics[[i]][[2,2]]
  
}

for (i in 1:25) {
  outputs_table_sp[i,3]= stats_sp$statistics[[i]][[3]]
  
}

for (i in 1:25) {
  outputs_table_sp[i,4]= stats_sp$statistics[[i]][[3,2]]
  
}

for (i in 1:25) {
  outputs_table_sp[i,5]= stats_sp$statistics[[i]][[4]]
  
}

for (i in 1:25) {
  outputs_table_sp[i,6]= stats_sp$statistics[[i]][[4,2]]
  
}

colnames(outputs_table_sp)= c("mriver","riversd","mgw","gwsd", "mprecip", "precipsd")
outputs_table_sp= cbind(data_W_Sp$Site, outputs_table_sp)
colnames(outputs_table_sp)= c("site", "mriver","riversd","mgw","gwsd", "mprecip", "precipsd")
write.csv(outputs_table_sp, file = "spring CRD wetland water proportions.csv")

##Summer## model--------------------------------------------------------------------------------------


Summer_CRD<-read.csv("C:/Users/Casey/Dropbox/columbia river delta/inputs- raw isotopes/June CRD isotopes_for R with conductivity.csv")


ggplot(Summer_CRD, aes(x=O, y=cond., col=Type))+
  geom_point(size=2)+ theme_classic()

data_W_S <- subset(Summer_CRD, Type == "Wetland", select = c("O","H","cond.", "Type", "site"))
data_R_S <- subset(Summer_CRD, Type == "River", select = c("O","H","cond.", "Type","site"))
data_S_S <- subset(Summer_CRD, Type == "Spring", select = c("O","H","cond.", "Type","site"))
data_Wl_S <- subset(Summer_CRD, Type == "Well", select = c("O","H","cond.", "Type","site"))
data_SW_S <- rbind(data_S_S, data_Wl_S)


mix_S= matrix(c(data_W_S$O, data_W_S$H, data_W_S$cond.), ncol=3, nrow=nrow(data_W_S))
colnames(mix_S)= c("d18O","d2H", "conductivity")

## create summer mean + sd for precipitation ## dont need to re-run after outlier removal

#cond_NEON$date<-as.Date(cond_NEON$date, format = "%Y-%m-%d") ##converting dates from factor to dates 
cond_NEON_S<-subset(cond_NEON, date > "2019-06-01" & date < "2019-07-31")

s_names = c("River", "Groundwater", "Precipitation") ##only need to run once, same for all seasons
s_means_S = matrix(c(mean(data_R_S$O), mean(data_SW_S$O), mean(c(O$`O 6`, O$`O 7`)),  mean(data_R_S$H), mean(data_SW_S$H), mean(c(H$`H 6`, H$`H 7`)), mean(data_R_S$cond.), mean(data_SW_S$cond.), mean(cond_NEON_S$precipConductivity)), ncol=3, nrow=3)
s_sds_S = matrix(c(sd(data_R_S$O), sd(data_SW_S$O),sd(c(O$`O 6`, O$`O 7`)) , sd(data_R_S$H), sd(data_SW_S$H), sd(c(H$`H 6`, H$`H 7`)) , sd(data_R_S$cond.), sd(data_SW_S$cond.), sd(cond_NEON_S$precipConductivity)), ncol=3, nrow=3)
grp_S = as.integer(c(1:nrow(data_W_S)))

##run model

simmr_Summer = simmr_load(mixtures=mix_S,
                          source_names=s_names,
                          source_means=s_means_S,
                          source_sds=s_sds_S,
                          group = grp_S)



plot(simmr_Summer,tracers=c(1,2), group = 1:nrow(data_W_S)) ## 
plot(simmr_Summer,group=1:nrow(data_W_S),tracers=c(1,3)) #mix#37(site 84), 12 (site 35), 38(site 101)  ## all v. high conductivity
plot(simmr_Summer,group=1:nrow(data_W_S),tracers=c(2,3))

##need to re run the simmr_in with the outliers removed

###removeing outlier

## now re-run with less wetlands

data_W_S <- data_W_S[ !(data_W_S$site %in% c(35,84,101)), ]
print (data_W_S)


###time to run these mixing model for each wetland!

simmr_out_summer = simmr_mcmc(simmr_Summer)
summary(simmr_out_summer,type='diagnostics')
posterior_predictive(simmr_out_spring)
prior_viz(simmr_out_summer)
summer_results<-summary(simmr_out_summer, group=1:nrow(data_W_S))
stats_s<-summary(simmr_out_summer,type='statistics', group=1:nrow(data_W_S))
#quants_s<-summary(simmr_out_summer,type='quantiles', group=c(1:nrow(data_W_S)))
plot(simmr_out_summer,type='density')
plot(simmr_out_summer,type='boxplot')
plot(simmr_out_summer,type='matrix')


outputs_table_s=matrix(data=NA, ncol=6, nrow=nrow(data_W_S))
for (i in 1:nrow(data_W_S)) {
  outputs_table_s[i,1]= stats_s$statistics[[i]][[2]]
  
}

for (i in 1:nrow(data_W_S)) {
  outputs_table_s[i,2]= stats_s$statistics[[i]][[2,2]]
  
}

for (i in 1:nrow(data_W_S)) {
  outputs_table_s[i,3]= stats_s$statistics[[i]][[3]]
  
}

for (i in 1:nrow(data_W_S)) {
  outputs_table_s[i,4]= stats_s$statistics[[i]][[3,2]]
  
}

for (i in 1:nrow(data_W_S)) {
  outputs_table_s[i,5]= stats_s$statistics[[i]][[4]]
  
}

for (i in 1:nrow(data_W_S)) {
  outputs_table_s[i,6]= stats_s$statistics[[i]][[4,2]]
  
}

colnames(outputs_table_s)= c("mriver","riversd","mgw","gwsd", "mprecip", "precipsd")
#data_W_S$site<-as.character(data_W_S$site)
outputs_table_s= cbind(as.numeric(data_W_S$site), outputs_table_s)
colnames(outputs_table_s)= c("site", "mriver","riversd","mgw","gwsd", "mprecip", "precipsd")
write.csv(outputs_table_s, file = "summer CRD wetland water proportions.csv")

###fall mixing model-----------------------------



Fall_CRD<-read.csv("C:/Users/crrem/Dropbox/documents/UWaterloo/columbia river delta/inputs- raw isotopes/CRD_Fall isotopes_for R with conductivity.csv")


ggplot(Fall_CRD, aes(x=O, y=cond., col=Type))+
  geom_point(size=2)+ theme_classic()

data_W_F <- subset(Fall_CRD, Type == "Wetland", select = c("O","H","cond.", "Type", "site"))
data_R_F <- subset(Fall_CRD, Type == "River", select = c("O","H","cond.", "Type","site"))
data_S_F <- subset(Fall_CRD, Type == "Spring", select = c("O","H","cond.", "Type","site"))
data_Wl_F <- subset(Fall_CRD, Type == "Well", select = c("O","H","cond.", "Type","site"))
data_SW_F <- rbind(data_S_F, data_Wl_F)


mix_F= matrix(c(data_W_F$O, data_W_F$H, data_W_F$cond.), ncol=3, nrow=nrow(data_W_F))
colnames(mix_F)= c("d18O","d2H", "conductivity")

## create fall mean + sd for precipitation ## dont need to re-run after outlier removal

cond_NEON$date<-as.Date(cond_NEON$date, format = "%Y-%m-%d") ##converting dates from factor to dates 
cond_NEON_F<-subset(cond_NEON, date > "2019-08-01" & date < "2019-09-30")

s_names = c("River", "Groundwater", "Precipitation") ##only need to run once, same for all seasons
s_means_F = matrix(c(mean(data_R_F$O), mean(data_SW_F$O), mean(c(O$`O 8`, O$`O 9`)),  mean(data_R_F$H), mean(data_SW_F$H), mean(c(H$`H 8`, H$`H 9`)), mean(data_R_F$cond.), mean(data_SW_F$cond.), mean(cond_NEON_F$precipConductivity)), ncol=3, nrow=3)
s_sds_F = matrix(c(sd(data_R_F$O), sd(data_SW_F$O),sd(c(O$`O 8`, O$`O 9`)) , sd(data_R_F$H), sd(data_SW_F$H), sd(c(H$`H 8`, H$`H 9`)) , sd(data_R_F$cond.), sd(data_SW_F$cond.), sd(cond_NEON_F$precipConductivity)), ncol=3, nrow=3)
grp_F = as.integer(c(1:nrow(data_W_F)))

##run model

simmr_Fall = simmr_load(mixtures=mix_F,
                        source_names=s_names,
                        source_means=s_means_F,
                        source_sds=s_sds_F,
                        group = grp_F)



plot(simmr_Fall,tracers=c(1,2), group = 1:nrow(data_W_F)) ## 21(48), 36 (84), 20(47)
plot(simmr_Fall,group=1:nrow(data_W_F),tracers=c(1,3)) #12 (site 35), 37(101)
plot(simmr_Fall,group=1:nrow(data_W_F),tracers=c(2,3))

##need to re run the simmr_in with the outliers removed

###removeing outlier

## now re-run with less wetlands

data_W_F <- data_W_F[ !(data_W_F$site %in% c(35,47,48,84,101)), ]
print (data_W_F)


###time to run these mixing model for each wetland!

simmr_out_Fall = simmr_mcmc(simmr_Fall)
summary(simmr_out_Fall,type='diagnostics')
posterior_predictive(simmr_out_Fall)
prior_viz(simmr_out_Fall)
Fall_results<-summary(simmr_out_Fall, group=1:nrow(data_W_F))
stats_f<-summary(simmr_out_Fall,type='statistics', group=1:nrow(data_W_F))
#quants_s<-summary(simmr_out_summer,type='quantiles', group=c(1:nrow(data_W_S)))
plot(simmr_out_Fall,type='density')
plot(simmr_out_Fall,type='boxplot')
plot(simmr_out_Fall,type='matrix')


outputs_table_f=matrix(data=NA, ncol=6, nrow=nrow(data_W_F))
for (i in 1:nrow(data_W_F)) {
  outputs_table_f[i,1]= stats_f$statistics[[i]][[2]]
  
}

for (i in 1:nrow(data_W_F)) {
  outputs_table_f[i,2]= stats_f$statistics[[i]][[2,2]]
  
}

for (i in 1:nrow(data_W_F)) {
  outputs_table_f[i,3]= stats_f$statistics[[i]][[3]]
  
}

for (i in 1:nrow(data_W_F)) {
  outputs_table_f[i,4]= stats_f$statistics[[i]][[3,2]]
  
}

for (i in 1:nrow(data_W_F)) {
  outputs_table_f[i,5]= stats_f$statistics[[i]][[4]]
  
}

for (i in 1:nrow(data_W_F)) {
  outputs_table_f[i,6]= stats_f$statistics[[i]][[4,2]]
  
}

colnames(outputs_table_f)= c("mriver","riversd","mgw","gwsd", "mprecip", "precipsd")
data_W_F$site<-as.character(data_W_F$site)
outputs_table_f= cbind(as.numeric(data_W_F$site), outputs_table_f)
colnames(outputs_table_f)= c("site", "mriver","riversd","mgw","gwsd", "mprecip", "precipsd")
write.csv(outputs_table_f, file = "Fall CRD wetland water proportions.csv")

## next up triangle bubble plot
##spring plot---------------------
library(ggtern)
outputs_table_sp<-as.data.frame(outputs_table_sp)


#switch sites to a string
outputs_table_sp$site<-as.character(outputs_table_sp$site)
outputs_table_sp$site <- factor(outputs_table_sp$site, levels = as.integer(outputs_table_sp$site))

##working

spring_plot= ggtern(data=outputs_table_sp,aes(x=mriver,y=mgw, z=mprecip)) +
  theme_bw()+
  theme_showarrows()+
  geom_point(aes(color = site), size=3) +
  geom_text(aes(x=mriver+0.04,y=mgw, z=mprecip),label=outputs_table_sp$site)+
  #geom_point(data=as.data.frame(p_post),aes(x=River,y=Groundwater, z=Precipitation))+
  #geom_mean_ellipse(data=as.data.frame(p_post1),aes(x=River,y=Groundwater, z=Precipitation))
  geom_errorbarL(aes(Lmin=mriver-riversd, Lmax=mriver+riversd, colour= site), alpha=0.5, lwd=0.5)+
  geom_errorbarT(aes(Tmin=mgw+gwsd,Tmax=mgw-gwsd,colour=site),alpha=0.5, lwd=0.5)+
  geom_errorbarR(aes(Rmin=mprecip+precipsd,Rmax=mprecip-precipsd,colour=site),alpha=0.5, lwd=0.5)+
  labs(x="", xarrow= "River",y="", yarrow = "Groundwater", z="", zarrow="Precipitation")+
  theme(legend.position="bottom",
        legend.box="vertical",
        legend.box.just="bottom")


ggsave("CRD_spring_mixing.jpeg", plot=spring_plot)
ggsave("CRD_spring_mixing.pdf", plot=spring_plot)
  
##summer plot---------------------------------------------------------------------------------------------------------------

outputs_table_s<-as.data.frame(outputs_table_s)


#switch sites to a string
outputs_table_s$site<-as.character(outputs_table_s$site)
outputs_table_s$site <- factor(outputs_table_s$site, levels = as.integer(outputs_table_s$site))

summer_plot= ggtern(data=outputs_table_s,aes(x=mriver,y=mgw, z=mprecip)) +
  theme_bw()+
  theme_showarrows()+
  geom_point(aes(color = site), size=3) +
  geom_text(aes(x=mriver+0.04,y=mgw, z=mprecip),label=outputs_table_s$site)+
  #geom_point(data=as.data.frame(p_post),aes(x=River,y=Groundwater, z=Precipitation))+
  #geom_mean_ellipse(data=as.data.frame(p_post1),aes(x=River,y=Groundwater, z=Precipitation))
  geom_errorbarL(aes(Lmin=mriver-riversd, Lmax=mriver+riversd, colour= site), alpha=0.2, lwd=0.5)+
  geom_errorbarT(aes(Tmin=mgw+gwsd,Tmax=mgw-gwsd,colour=site),alpha=0.2, lwd=0.5)+
  geom_errorbarR(aes(Rmin=mprecip+precipsd,Rmax=mprecip-precipsd,colour=site),alpha=0.02, lwd=0.5)+
  labs(x="", xarrow= "River",y="", yarrow = "Groundwater", z="", zarrow="Precipitation")+
  theme(legend.position="bottom",
        legend.justification=c(0.5,0.5),
        legend.direction="horizontal",legend.box="horizontal",
        legend.box.just="top",legend.key.size = unit(0.5, "cm"))



ggsave("CRD_summer_mixing.jpeg", plot=summer_plot)
ggsave("CRD_summer_mixing.pdf", plot=summer_plot)

##Fall plot---------------------------------------------------------------------------------------------------------------

outputs_table_f<-as.data.frame(outputs_table_f)


#switch sites to a string
outputs_table_f$site<-as.character(outputs_table_f$site)
outputs_table_f$site <- factor(outputs_table_f$site, levels = as.integer(outputs_table_f$site))

fall_plot= ggtern(data=outputs_table_f,aes(x=mriver,y=mgw, z=mprecip)) +
  theme_bw()+
  theme_showarrows()+
  geom_point(aes(color = site), size=3) +
  geom_text(aes(x=mriver+0.04,y=mgw, z=mprecip),label=outputs_table_f$site)+
  #geom_point(data=as.data.frame(p_post),aes(x=River,y=Groundwater, z=Precipitation))+
  #geom_mean_ellipse(data=as.data.frame(p_post1),aes(x=River,y=Groundwater, z=Precipitation))
  geom_errorbarL(aes(Lmin=mriver-riversd, Lmax=mriver+riversd, colour= site), alpha=0.3, lwd=0.5)+
  geom_errorbarT(aes(Tmin=mgw+gwsd,Tmax=mgw-gwsd,colour=site),alpha=0.3, lwd=0.5)+
  geom_errorbarR(aes(Rmin=mprecip+precipsd,Rmax=mprecip-precipsd,colour=site),alpha=0.03, lwd=0.5)+
  labs(x="", xarrow= "River",y="", yarrow = "Groundwater", z="", zarrow="Precipitation")+
  theme(legend.position="bottom",
        legend.justification=c(0.5,0.5),
        legend.direction="horizontal",legend.box="horizontal",
        legend.box.just="top",legend.key.size = unit(0.5, "cm"))

ggsave("CRD_fall_mixing.jpeg", plot=fall_plot)
ggsave("CRD_fall_mixing.pdf", plot=fall_plot)
##test area----------------------------------------------------------------------

p_post1<-simmr_out_spring$output$`1`$BUGSoutput$sims.list$p
p_post2<-simmr_out_spring$output$`2`$BUGSoutput$sims.list$p
p_post3<-simmr_out_spring$output$`3`$BUGSoutput$sims.list$p
p_post4<-simmr_out_spring$output$`4`$BUGSoutput$sims.list$p
p_post5<-simmr_out_spring$output$`5`$BUGSoutput$sims.list$p
p_post6<-simmr_out_spring$output$`6`$BUGSoutput$sims.list$p
p_post7<-simmr_out_spring$output$`7`$BUGSoutput$sims.list$p
p_post8<-simmr_out_spring$output$`8`$BUGSoutput$sims.list$p
p_post9<-simmr_out_spring$output$`9`$BUGSoutput$sims.list$p
p_post10<-simmr_out_spring$output$`10`$BUGSoutput$sims.list$p
p_post11<-simmr_out_spring$output$`11`$BUGSoutput$sims.list$p



par(mfrow = c(1,2))
hist(p_post[,1], 20, main = s_names[1])
plot(density(p_post[,1]), main = s_names[1])
abline(v = 1/3)

 
spring_plot= ggtern(data=outputs_table_sp,aes(x=mriver,y=mgw, z=mprecip)) +
              theme_bw()+
  theme_showarrows()+
  geom_point(aes(color = site), size=3) +
  #geom_point(data=as.data.frame(p_post),aes(x=River,y=Groundwater, z=Precipitation))+
  #geom_mean_ellipse(data=as.data.frame(p_post1),aes(x=River,y=Groundwater, z=Precipitation))
  geom_errorbarL(aes(Lmin=mriver-riversd, Lmax=mriver+riversd, colour= site), alpha=0.5, lwd=0.5)+
  geom_errorbarT(aes(Tmin=mgw+gwsd,Tmax=mgw-gwsd,colour=site),alpha=0.5, lwd=0.5)+
  geom_errorbarR(aes(Rmin=mprecip+precipsd,Rmax=mprecip-precipsd,colour=site),alpha=0.5, lwd=0.5)+
  labs(x="", xarrow= "River",y="", yarrow = "Groundwater", z="", zarrow="Precipitation")+
  theme(legend.position="bottom",
        legend.box="vertical",
        legend.box.just="bottom")

library(ggpubr)
  
grid.arrange(spring_plot, spring_plot, spring_plot)






(Larrowlab, Llab, Rarrowlab, Rlab, Tarrowlab, Tlab, Wlab, ggtern-labels, larrowlab, llab, rarrowlab, rlab, tarrowlab, tlab, wlab, zlab)
Change Axis labels and legend titles
theme(legend.position="bottom",
      legend.justification=c(1,1),
      legend.direction="vertical",legend.box="horizontal",
      legend.box.just="top")

ggtern.multi
  
  #stops working
  
test1<-c(outputs_table_sp$mriver)
  
  
  ggtern(data=outputs_table_sp,aes(x=mriver,y=mgw, z=mprecip)) +
    geom_point()+

    
    geom_mean_ellipse(data=outputs_table_sp[1,],aes(x=mriver,y=mgw, z=mprecip))
    
    geom_encircle(alpha=0.2,size=1, expand=0)
    #stat_density_tern(geom = "polygon", alpha = 1/2, aes(x=mriver,y=mgw, z=mprecip,fill = site))
    geom_polygon_closed(data = outputs_table_sp,  # This is also a nice example of how to plot
                 aes(x = mriver-riversd, y = mgw-gwsd, z=mprecip-precipsd, fill = site),  # two superimposed geoms
                 alpha = 1/2) 
    
geom_polygon_closed(mapping = c(mriver-riversd,mriver+riversd,mgw-gwsd,mgw+gwsd,mprecip-precipsd,mprecip+precipsd), data = outputs_table_sp)


p1<- plot_ly(
  output_table_sp, a = ~`% Economically Active`, b = ~`% Young`, c = ~`% Old`,
  color = ~`Planning Region`, type = "scatterternary", colors=~colors,
  size = ~Total,
  text = ~paste('Young:',sep='', round(`% Young`,1),'%',
                '<br>Economically Active:',
                round(`% Economically Active`,1),'%', '<br>Old:',
                round(`% Old`,1),'%','<br>Subzone:', Subzone, hoverinfo="text",
                '<br>Planning Area:', `Planning Area`),
  marker = list(symbol = 'circle', opacity=0.55, sizemode="diameter", sizeref=1.5,
                line = list(width = 2, color = '#FFFFFF')))



























###putting hellinger distance aside for now
install.packages("devtools")
library(devtools)
devtools::install_github("cbrown5/BayeSens")
library(BayeSens)
library(MixSIAR)
devtools::install_github("cbrown5/remixsiar")
library(remixsiar)









alpha <- rep(1, source$n.sources) #default prior values
p_prior <- MCMCpack::rdirichlet(10000, alpha) #draw prior samples 



##figuring out
mix.filename <- system.file("extdata", "killerwhale_consumer.csv", package = "MixSIAR")
mix <- load_mix_data(filename=mix.filename,
                     iso_names=c("d13C","d15N"),
                     factors=NULL,
                     fac_random=NULL,
                     fac_nested=NULL,
                     cont_effects=NULL)
source.filename <- system.file("extdata", "killerwhale_sources.csv", package = "MixSIAR")
blah <- load_source_data(filename=source.filename,
                           source_factors=NULL,
                           conc_dep=FALSE,
                           data_type="means",
                           mix)


discr.filename <- system.file("extdata", "killerwhale_discrimination.csv", package = "MixSIAR")
discr <- load_discr_data(filename=discr.filename, mix)

prior_gulls <- default_prior(simmr_Spring)  
hellinger(p_prior, post_pred)

alpha <- rep(1, 3) #default prior values
p_prior <- MCMCpack::rdirichlet(10000, alpha) #draw prior samples 

par(mfrow = c(1,2))
hist(p_prior[,1], 20, main = s_names[1])
plot(density(p_prior[,1]), main = s_names[1])
abline(v = 1/3)

posterior(simmr_out_spring)
##run this next
p_post<-simmr_out_spring$output$`1`$BUGSoutput$sims.list$p
p_post<-simmr_out_spring$output$BUGSoutput$sims.list$p

par(mfrow = c(1,2))
hist(p_post[,1], 20, main = s_names[1])
plot(density(p_post[,1]), main = s_names[1])
abline(v = 1/3)


p_post <- posterior_predictive(simmr_out_spring)

s_means_Sp


plot_dists(simmr_in, simmr_out, priorcontrol, plotdist = FALSE)
plot_dists(simmr_Spring, simmr_out_spring)
