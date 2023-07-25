> summary(cbind(O$`O 4`, O$`O 5`))

> summary(cbind(H$`H 4`, H$`H 5`))


sp.gw.rain<-cbind(O$`O 4`, O$`O 5`,H$`H 4`, H$`H 5`)
head(sp.gw.rain)

sp.gw.snow<-read.csv("C:/Users/Casey/Downloads/CW_snow_oipc.csv")

sp.gw.river<-data_R_Sp



#----------------------------making the snow isotopes----------

O1 <- raster("C:/Users/Casey/Downloads/IsotopeMaps/IsotopeMaps/O01c.asc") #Jan
O2 <- raster("C:/Users/Casey/Downloads/IsotopeMaps/IsotopeMaps/O02c.asc") #Feb
O3 <- raster("C:/Users/Casey/Downloads/IsotopeMaps/IsotopeMaps/O03c.asc") #March
O11 <- raster("C:/Users/Casey/Downloads/IsotopeMaps/IsotopeMaps/O11c.asc") #Nov
O12 <- raster("C:/Users/Casey/Downloads/IsotopeMaps/IsotopeMaps/O12c.asc") #Dec

#missing projection for some reason so adding that in for each raster##
crs(O1)<-"+proj=longlat +datum=WGS84"
crs(O2)<-"+proj=longlat +datum=WGS84"
crs(O3)<-"+proj=longlat +datum=WGS84"
crs(O11)<-"+proj=longlat +datum=WGS84"
crs(O12)<-"+proj=longlat +datum=WGS84"


winterO<-cbind(O1,O2,O3,O11,O12)

##crop to map extent
winterOc<-winterO
e<-extent(-117,-115.5,50,51.33)

for (i in 1:6) {
  winterOc[[i]]<-crop(winterO[[i]], e)
}

##extract values of o at sampling points and place in a table

Ow<-data.frame(matrix(ncol=6, nrow=nrow(wsite)))
Ow[,1]=wsite$Site.number
names(Ow)[1]<-"site number"

for (i in 1:5) {
  Ow[,i+1]=extract(winterOc[[i]],wsite_coords)
  names(Ow)[i+1]<- paste("O", i)
}

write.csv(Ow, file = "O_winter.csv")
#Ow<-read.csv("O_winter.csv")




H3 <- raster("C:/Users/Casey/Downloads/IsotopeMaps/IsotopeMaps/H03c.asc")
crs(H3)<-"+proj=longlat +datum=WGS84"
spHc<-cbind(r4,r5)
e<-extent(-117,-115.5,50,51.33)

spHc2<-spHc

for (i in 1:2) {
  spHc2[[i]]<-crop(spHc[[i]], e)
}

Hsp<-data.frame(matrix(ncol=2, nrow=nrow(wsite)))
Hsp[,1]=wsite$Site.number
names(Hsp)[1]<-"site number"

for (i in 1:2) {
  Hsp[,i+1]=extract(spHc2[[i]],wsite_coords)
  names(Hsp)[i+1]<- paste("H", i+3)
}
Hsp
#--------------spring gw----------

sp_gw_comp<-read.csv("C:/Users/Casey/Dropbox/columbia river delta/spring_gw_rain.csv")

sp_gw_comp[sp_gw_comp == "rain"] <- "Rain"
sp_gw_comp[sp_gw_comp == "marsnow"] <- "Snow"
sp_gw_comp[sp_gw_comp == "Groundwater"] <- "G.water"

cond_NEON <- read.csv("C:/Users/Casey/Dropbox/columbia river delta/conductivity/NEON cond data for mixing model_csv.csv") 
cond_NEON$date<-as.Date(cond_NEON$date, format = "%Y-%m-%d") ##converting dates from factor to dates 
cond_NEON_Sp<-subset(cond_NEON, date > "2019-04-01" & date < "2019-05-31")


sp_gw_comp$id <- factor(sp_gw_comp$id , levels=c("G.water", "Snow", "Rain", "River"))

mean(cond_NEON_Sp$precipConductivity)
sd(cond_NEON_Sp$precipConductivity)

     
sp_rain_EC<-cond_NEON_Sp$precipConductivity
blah<-c("Rain", "Rain", "Rain", "Rain")
sp_rain_EC<-cbind(sp_rain_EC, blah)
sp_rain_EC<-as.data.frame(sp_rain_EC)
colnames(sp_rain_EC)<-c("EC", "id")

sp_gw_EC<-data_SW_Sp[,3:4]
sp_gw_EC[sp_gw_EC == "Spring"] <- "G.water"
sp_gw_EC[sp_gw_EC == "Well"] <- "G.water"
colnames(sp_gw_EC)<-c("EC", "id")

sp_r_EC<-data_R_Sp[,3:4]
colnames(sp_r_EC)<-c("EC", "id")

sp_EC<-rbind(sp_rain_EC, sp_gw_EC, sp_r_EC)
sp_EC$id <- factor(sp_EC$id , levels=c("G.water", "Rain", "River"))
sp_EC$EC<-as.numeric(sp_EC$EC) 


##this is where I need to make the axes consistent between seasons.

EC<-ggplot(data=sp_EC, aes(y=EC, x=id, fill=id))+
  geom_boxplot(alpha=0.5)+
  geom_jitter(aes(colour = id), alpha=0.5)+
  scale_fill_manual(values=c("#B2ABD2", "#0571B0", "#80CDC1"))+
  scale_colour_manual(values=c("#B2ABD2", "#0571B0", "#80CDC1"))+
  theme_bw()+
  theme(axis.title.x = element_blank())
 
H2_<-
  ggplot(data=sp_gw_comp, aes(y=H2, x=id, fill=id))+
  ylim(-170,-90)+
  geom_boxplot(alpha=0.5)+
  geom_jitter(aes(colour = id), alpha=0.5)+
  scale_fill_manual(values=c("#B2ABD2", "#FDB863", "#0571B0", "#80CDC1"))+
  scale_colour_manual(values=c("#B2ABD2", "#FDB863", "#0571B0", "#80CDC1"))+
  theme_bw()+
  theme(axis.title.x = element_blank())


O18_<-
  ggplot(data=sp_gw_comp, aes(y=O18, x=id, fill=id))+
  geom_boxplot(alpha=0.5)+
  ylim(-25,-10)+
  geom_jitter(aes(colour = id), alpha=0.5)+
  scale_fill_manual(values=c("#B2ABD2", "#FDB863", "#0571B0", "#80CDC1"))+
  scale_colour_manual(values=c("#B2ABD2", "#FDB863", "#0571B0", "#80CDC1"))+
  theme_bw()+
  theme(axis.title.x = element_blank())




#library(ggpubr)
springplt<-ggarrange(O18_, H2_, EC, common.legend = TRUE, legend = "none",
                    align="hv", font.label=list(size=2), ncol=3))

gw
ggsave("gw_comparison.pdf", gw)
ggsave("gw_comparison.png", gw)


##summer--------
Summer_CRD<-read.csv("C:/Users/Casey/Dropbox/columbia river delta/inputs- raw isotopes/June CRD isotopes_for R with conductivity.csv")
cond_NEON_S<-subset(cond_NEON, date > "2019-06-01" & date < "2019-07-31")



S_rain_EC<-cond_NEON_S$precipConductivity
blah<-c("Rain")
S_rain_EC<-cbind(S_rain_EC, blah)
S_rain_EC<-as.data.frame(S_rain_EC)
colnames(S_rain_EC)<-c("EC", "id")

data_W_S <- subset(Summer_CRD, Type == "Wetland", select = c("O","H","cond.", "Type", "site"))
data_R_S <- subset(Summer_CRD, Type == "River", select = c("O","H","cond.", "Type","site"))
data_S_S <- subset(Summer_CRD, Type == "Spring", select = c("O","H","cond.", "Type","site"))
data_Wl_S <- subset(Summer_CRD, Type == "Well", select = c("O","H","cond.", "Type","site"))
data_SW_S <- rbind(data_S_S, data_Wl_S)


S_gw_EC<-data_SW_S[,3:4]
S_gw_EC[S_gw_EC == "Spring"] <- "Groundwater"
S_gw_EC[S_gw_EC == "Well"] <- "Groundwater"
colnames(S_gw_EC)<-c("EC", "id")

S_r_EC<-data_R_S[,3:4]
colnames(S_r_EC)<-c("EC", "id")

S_EC<-rbind(S_rain_EC, S_gw_EC, S_r_EC)
S_EC$id <- factor(S_EC$id , levels=c("Groundwater", "Rain", "River"))
S_EC$EC<-as.numeric(S_EC$EC) 

EC_S<-ggplot(data=S_EC, aes(y=EC, x=id, fill=id))+
  geom_boxplot(alpha=0.5)+
  geom_jitter(aes(colour = id), alpha=0.5)+
  scale_fill_manual(values=c("#B2ABD2", "#0571B0", "#80CDC1"))+
  scale_colour_manual(values=c("#B2ABD2",  "#0571B0", "#80CDC1"))+
  theme_bw()+
  theme(axis.title.x = element_blank())


a<-as.data.frame(O$`O 6`)
colnames(a)<-c("O")
b<-as.data.frame(O$`O 7`)
colnames(b)<-c("O")
S1<-rbind(a,b)
a2<-as.data.frame(H$`H 6`)
colnames(a2)<-c("H")
b2<-as.data.frame(H$`H 7`)
colnames(b2)<-c("H")
S2<-rbind(a2,b2)
S_rain_iso<-cbind(S1,S2,rep("Rain", length=length(S1)))
colnames(S_rain_iso)<-c("O18", "H2", "id")
head(S_rain_iso)
S_river_iso<-cbind(data_R_S[,1:2], data_R_S[,4])
colnames(S_river_iso)<-c("O18", "H2", "id")

S_gw_iso<-cbind(data_SW_S[,1:2], data_SW_S[,4])
colnames(S_gw_iso)<-c("O18", "H2", "id")
S_gw_iso[S_gw_iso == "Spring"] <- "Groundwater"
S_gw_iso[S_gw_iso == "Well"] <- "Groundwater"

S_iso<-rbind(S_river_iso, S_rain_iso,S_gw_iso)

H2_S<-
  ggplot(data=S_iso, aes(y=H2, x=id, fill=id))+
  geom_boxplot(alpha=0.5)+
  ylim(-170,-90)+
  geom_jitter(aes(colour = id), alpha=0.5)+
  scale_fill_manual(values=c("#B2ABD2",  "#0571B0", "#80CDC1"))+
  scale_colour_manual(values=c("#B2ABD2",  "#0571B0", "#80CDC1"))+
  theme_bw()+
  theme(axis.title.x = element_blank())


O18_S<-
  ggplot(data=S_iso, aes(y=O18, x=id, fill=id))+
  geom_boxplot(alpha=0.5)+
  ylim(-25,-10)+
  geom_jitter(aes(colour = id), alpha=0.5)+
  scale_fill_manual(values=c("#B2ABD2",  "#0571B0", "#80CDC1"))+
  scale_colour_manual(values=c("#B2ABD2",  "#0571B0", "#80CDC1"))+
  theme_bw()+
  theme(axis.title.x = element_blank())

Summer<-ggarrange(O18_S, H2_S, EC_S, common.legend = TRUE, legend = "none",
              align="hv", font.label=list(size=2), ncol=3, nrow=1)

##Fall--------


Fall_CRD<-read.csv("C:/Users/Casey/Dropbox/columbia river delta/inputs- raw isotopes/CRD_Fall isotopes_for R with conductivity.csv")
cond_NEON_F<-subset(cond_NEON, date > "2019-08-01" & date < "2019-09-30")
data_W_F <- subset(Fall_CRD, Type == "Wetland", select = c("O","H","cond.", "Type", "site"))
data_R_F <- subset(Fall_CRD, Type == "River", select = c("O","H","cond.", "Type","site"))
data_S_F <- subset(Fall_CRD, Type == "Spring", select = c("O","H","cond.", "Type","site"))
data_Wl_F <- subset(Fall_CRD, Type == "Well", select = c("O","H","cond.", "Type","site"))
data_SW_F <- rbind(data_S_F, data_Wl_F)



F_rain_EC<-cond_NEON_F$precipConductivity
blah<-c("Rain")
F_rain_EC<-cbind(F_rain_EC, blah)
F_rain_EC<-as.data.frame(F_rain_EC)
colnames(F_rain_EC)<-c("EC", "id")

F_gw_EC<-data_SW_F[,3:4]
F_gw_EC[F_gw_EC == "Spring"] <- "Groundwater"
F_gw_EC[F_gw_EC == "Well"] <- "Groundwater"
colnames(F_gw_EC)<-c("EC", "id")

F_r_EC<-data_R_F[,3:4]
colnames(F_r_EC)<-c("EC", "id")

F_EC<-rbind(F_rain_EC, F_gw_EC, F_r_EC)
F_EC$id <- factor(F_EC$id , levels=c("Groundwater", "Rain", "River"))
F_EC$EC<-as.numeric(F_EC$EC) 

EC_F<-ggplot(data=F_EC, aes(y=EC, x=id, fill=id))+
  geom_boxplot(alpha=0.5)+
  geom_jitter(aes(colour = id), alpha=0.5)+
  scale_fill_manual(values=c("#B2ABD2",  "#0571B0", "#80CDC1"))+
  scale_colour_manual(values=c("#B2ABD2",  "#0571B0", "#80CDC1"))+
  theme_bw()+
  theme(axis.title.x = element_blank())


a<-as.data.frame(O$`O 8`)
colnames(a)<-c("O")
b<-as.data.frame(O$`O 9`)
colnames(b)<-c("O")
S1<-rbind(a,b)
a2<-as.data.frame(H$`H 8`)
colnames(a2)<-c("H")
b2<-as.data.frame(H$`H 9`)
colnames(b2)<-c("H")
S2<-rbind(a2,b2)
F_rain_iso<-cbind(S1,S2)
F_rain_iso[,3]<-c("Rain")
colnames(F_rain_iso)<-c("O18", "H2", "id")

F_river_iso<-cbind(data_R_F[,1:2], data_R_F[,4])
colnames(F_river_iso)<-c("O18", "H2", "id")

F_gw_iso<-cbind(data_SW_F[,1:2], data_SW_F[,4])
colnames(F_gw_iso)<-c("O18", "H2", "id")
F_gw_iso[F_gw_iso == "Spring"] <- "Groundwater"
F_gw_iso[F_gw_iso == "Well"] <- "Groundwater"

F_iso<-rbind(F_river_iso, F_rain_iso, F_gw_iso)

H2_F<-
  ggplot(data=F_iso, aes(y=H2, x=id, fill=id))+
  geom_boxplot(alpha=0.5)+
  ylim(-170,-90)+
  geom_jitter(aes(colour = id), alpha=0.5)+
  scale_fill_manual(values=c("#B2ABD2",  "#0571B0", "#80CDC1"))+
  scale_colour_manual(values=c("#B2ABD2", "#0571B0", "#80CDC1"))+
  theme_bw()+
  theme(axis.title.x = element_blank())


O18_F<-
  ggplot(data=F_iso, aes(y=O18, x=id, fill=id))+
  geom_boxplot(alpha=0.5)+
  ylim(-25,-10)+
  geom_jitter(aes(colour = id), alpha=0.5)+
  scale_fill_manual(values=c("#B2ABD2", "#0571B0", "#80CDC1"))+
  scale_colour_manual(values=c("#B2ABD2",  "#0571B0", "#80CDC1"))+
  theme_bw()+
  theme(axis.title.x = element_blank())

Fallplt<-ggarrange(O18_F, H2_F, EC_F, common.legend = TRUE, legend = "none",
          align="hv", ncol=3, nrow=1)




full_plots<-ggarrange(ggparagraph(text="a) Spring"), springplt, ggparagraph(text="b) Summer"), Summer, ggparagraph(text="c) Fall"), Fallplt, common.legend = TRUE, 
                      legend = "none", nrow=6, heights=c(0.15,1,0.15,1,0.15,1))



ggsave(full_plots, file= "full_plots.png", width=230, height=190, units = "mm",bg = 'white')
ggsave(full_plots, file= "full_plots.pdf", width=230, height=190, units = "mm",bg = 'white')
