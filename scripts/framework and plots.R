##Columbia River Delta framework with snow as input and calculated dSSL

limitO<--0.551126046

limitH<--44.92532454

sslO<--6.9

sslH<--79.22847312

pO<--19.38

pH<--146.8

#psO<-

#psH<-

asO<--21.46091285

asH<--164.9161743

#CRD<-read.csv(file.choose())

dataO<-c(limitO,pO,sslO,asO)
dataH<-c(limitH,pH,sslH,asH)
#baseline plot

xlabx=expression(paste(delta^{18}, "O (\u2030 VSMOW)"), line=2, cex=1.5)
ylaby=expression(paste(delta^{2}, "H(\u2030 VSMOW)"), line=-1, cex=1.5)

plot(dataO,dataH, xlab=xlabx, ylab=ylaby, xlim=c(-25, 5), ylim=c(-180,-40),
      pch=c(16), col=c("black"), cex=1.1, cex.axis=1.0, las=1)
lel<-lm(dataH[1:2]~dataO[1:2])
segments(pO,(pO*lel$coeff[2])+lel$coeff[1],limitO,(limitO*lel$coeff[2])+lel$coeff[1], cex=1.25)
abline(a=10,b=8,lwd="2", lty="dotted")
#abline(lel)

#LMWL
LMWLO<-c(-25.93,-24.39,-23.30,-19.31,-17.90,-16.43,-15.51,-15.59,-15.16,-17.38,-23.20,-24.91)
LMWLH<-c(-203.8, -194.0,-182.8,-150.8,-136.2,-122.9,-121.5,-116.4,-115.2,-134.6,-180.0,-191.90)
#points(LMWLO,LMWLH, col="pink", pch=16)
LMWL<-lm(LMWLH~ LMWLO)
abline(a=0.08, b=	7.73, col="grey", lwd=2)




#labels, legends
text((dataO[1]+1),(dataH[1]+1), expression(delta^"*"), cex=1.5)
text((dataO[3]+1.5),(dataH[3]-2), expression(delta[SSL]), cex=1.5)
text((dataO[2]+2),(dataH[2]-1), expression(delta[Snow]), cex=1.5)
#text((dataO[4]),(dataH[4]+4), expression(delta[Ps]), cex=1.5)
text((dataO[4]+1),(dataH[4]), expression(delta[As]), cex=1.5)



text(-13,-80, "GMWL")
#text(-10,-82, "LMWL")
text(-9,-100, "LEL" )

#points(CRD$O, CRD$H)
#summary(CRD)
may_wetlands<-(filter(CRD, Type == "Wetland"))
may_River<-(filter(CRD, Type == "River"))
may_spring<-(filter(CRD, Type == "Spring"))
may_springpond<-(filter(CRD, Type == "Spring Pond"))
may_lake<-(filter(CRD, Type == "Lake"))
may_well<-(filter(CRD, Type == "Well"))
may_snow<-(filter(CRD, Type == "Snowmelt"))

points(may_wetlands$H~ may_wetlands$O, col="pink", pch=16)
points(may_River$H~ may_River$O, col="red", pch=16)
points(may_spring$H~ may_spring$O, col="orange", pch=16)
points(may_springpond$H~ may_springpond$O, col="yellow", pch=16)
points(may_lake$H~ may_lake$O, col="light blue", pch=16)
points(may_well$H~ may_well$O, col="blue", pch=16)


#snow
points(-19.38,-149.77, pch=17)
#sy<-8.211*-19.38 + 9.360
text(-18.5, -149, "snow", cex=0.75)
#rain
points(-11.36,-83.92, pch=17)
#ry<-8.211*-11.36 + 9.360
text(-11, -85, "rain", cex=0.75)

#legend
points(0,- 140,col="pink", pch=16)
points(0, -144,col="red", pch=16)
points(0, -148, col="orange", pch=16)
points(0, -152, col="yellow", pch=16)
points(0, -156, col="light blue", pch=16)
points(0, -160, col="blue", pch=16)
#points(0, -164, pch=1)

text(2,- 140, "Wetland", cex=0.75)
text(2, -144,"River", cex=0.75)
text(2, -148, "Spring", cex=0.75)
text(2.5, -152, "Spring Pond", cex=0.75)
text(2, -156, "Lake", cex=0.75)
text(2, -160, "Well", cex=0.75)
#text(2, -164, "Glacier", cex=0.75)


###june isotopes

June_CRD<-read.csv(file.choose())

jn_wetlands<-(filter(June_CRD, Type == "Wetland"))
jn_River<-(filter(June_CRD, Type == "River"))
jn_spring<-(filter(June_CRD, Type == "Spring"))
jn_springpond<-(filter(June_CRD, Type == "Spring Pond"))
jn_lake<-(filter(June_CRD, Type == "Lake"))
jn_well<-(filter(June_CRD, Type == "Well"))
jn_snow<-(filter(June_CRD, Type == "Snowmelt"))

points(jn_wetlands$H~ jn_wetlands$O, col="pink", pch=16)
points(jn_River$H~ jn_River$O, col="red", pch=16)
points(jn_spring$H~ jn_spring$O, col="orange", pch=16)
points(jn_springpond$H~ jn_springpond$O, col="yellow", pch=16)
points(jn_lake$H~ jn_lake$O, col="light blue", pch=16)
points(jn_well$H~ jn_well$O, col="blue", pch=16)


#glacier
points(June_CRD$O[59], June_CRD$H[59], cex=1)

##fall isotopes

fall_CRD<-read.csv(file.choose())

f_wetlands<-(filter(fall_CRD, Type == "Wetland"))
f_River<-(filter(fall_CRD, Type == "River"))
f_spring<-(filter(fall_CRD, Type == "Spring"))
f_springpond<-(filter(fall_CRD, Type == "Spring Pond"))
f_lake<-(filter(fall_CRD, Type == "Lake"))
f_well<-(filter(fall_CRD, Type == "Well"))
f_snow<-(filter(fall_CRD, Type == "Snowmelt"))

points(f_wetlands$H~ f_wetlands$O, col="pink", pch=16)
points(f_River$H~ f_River$O, col="red", pch=16)
points(f_spring$H~ f_spring$O, col="orange", pch=16)
points(f_springpond$H~ f_springpond$O, col="yellow", pch=16)
points(f_lake$H~ f_lake$O, col="light blue", pch=16)
points(f_well$H~ f_well$O, col="blue", pch=16)



##LEL of glacier to d*

segments(June_CRD$O[59], June_CRD$H[59], limitO, limitH)

points(mean(jn_River$O), mean(jn_River$H), cex=2)

## dI values



dI<-read.csv(file.choose())
may_I_wetlands<-(filter(dI, Type == "Wetland"))

 plot(may_I_wetlands$May_O_I,may_I_wetlands$May_H_I, xlab=xlabx, ylab=ylaby, xlim=c(-26,5), ylim=c(-200,-40), 
      pch=c(16), col=c("pink"), cex=1.1, cex.axis=1.0, las=1)
 lel<-lm(dataH[1:2]~dataO[1:2])
 segments(pO,(pO*lel$coeff[2])+lel$coeff[1],limitO,(limitO*lel$coeff[2])+lel$coeff[1], cex=1.25)
 abline(a=10,b=8,lwd="2", lty="dotted")
 #abline(lel)
  points(dataO, dataH)
 #new LMWL
 abline(a=9.360, b=8.211, lwd=2, col="grey")
abline (LMWL)
points(may_I_wetlands$May_O_I,may_I_wetlands$May_H_I)
points(may_I_wetlands$May_O_I,may_I_wetlands$May_H_I, pch=16, col="pink")

#average spring
#may_I_springs<-filter(dI, Type == "Spring")
points(mean(may_I_springs$May_O_I), mean(may_I_springs$May_H_I), cex=1, col="orange", pch=16)

#labels, legends
text((dataO[1]+1),(dataH[1]+1), expression(delta^"*"), cex=1.5)
text((dataO[3]+1.5),(dataH[3]-2), expression(delta[SSL]), cex=1.5)
text((dataO[2]+2),(dataH[2]-1), expression(delta[Snow]), cex=1.5)
#text((dataO[4]),(dataH[4]+4), expression(delta[Ps]), cex=1.5)
text((dataO[4]+1),(dataH[4]), expression(delta[As]), cex=1.5)



text(-13,-80, "GMWL")
#text(-10,-82, "LMWL")
text(-9,-100, "LEL" )

##sumer dIs

dI_summer<-read.csv(file.choose())
sum_I_wetlands<-(filter(dI_summer, Type == "Wetland"))

plot(sum_I_wetlands$Sum_O_I,sum_I_wetlands$Sum_H_I, xlab=xlabx, ylab=ylaby, xlim=c(-26,5), ylim=c(-200,-40), 
     pch=c(16), col=c("pink"), cex=1.1, cex.axis=1.0, las=1)
lel<-lm(dataH[1:2]~dataO[1:2])
segments(pO,(pO*lel$coeff[2])+lel$coeff[1],limitO,(limitO*lel$coeff[2])+lel$coeff[1], cex=1.25)
abline(a=10,b=8,lwd="2", lty="dotted")
#abline(lel)
points(dataO, dataH)
#new LMWL
abline(a=0.08, b=	7.73, lwd=2, col="grey")
points(sum_I_wetlands$Sum_O_I,sum_I_wetlands$Sum_H_I)
points(sum_I_wetlands$Sum_O_I,sum_I_wetlands$Sum_H_I, pch=16, col="pink")


##calgary LMWL
abline(a=0.08, b=	7.73)

##fall dis
dI_fall<-read.csv(file.choose())
fall_I_wetlands<-(filter(dI_fall, Type == "Wetland"))

plot(fall_I_wetlands$fall_O_I,fall_I_wetlands$fall_H_I, xlab=xlabx, ylab=ylaby, xlim=c(-26,5), ylim=c(-200,-40), 
     pch=c(16), col=c("pink"), cex=1.1, cex.axis=1.0, las=1)
lel<-lm(dataH[1:2]~dataO[1:2])
segments(pO,(pO*lel$coeff[2])+lel$coeff[1],limitO,(limitO*lel$coeff[2])+lel$coeff[1], cex=1.25)
abline(a=10,b=8,lwd="2", lty="dotted")
#abline(lel)
points(dataO, dataH)
#new LMWL
abline(a=0.08, b=	7.73, lwd=2, col="grey")
points(fall_I_wetlands$fall_O_I,fall_I_wetlands$fall_H_I)
points(fall_I_wetlands$fall_O_I,fall_I_wetlands$fall_H_I, pch=16, col="pink")
