library(epicalc)
library(grDevices)
library(lattice)
library(grid)
library(scatterplot3d)
library(arm)
library(sensitivity)
library(Rglpk)
library(maptools)
library(spatstat)
library(sp)
library(maps)
library(mapproj)
library(gplots)
library(ggplot2)
library(qcc)
library(ctv)


social_determinants <- read.csv("Indicadores lima 2005_07_09.csv",stringsAsFactors = FALSE)

s <- split(social_determinants, social_determinants$distrito)
lapply(s, function(x) colMeans(x[, c("idh","pobreza","extr_pobreza")],na.rm = TRUE))

s <- split(social_determinants, social_determinants$year)
lapply(s, function(x) colMeans(x[, c("idh","pobreza","extr_pobreza")],na.rm = TRUE))


aggdata_year <-aggregate(social_determinants$idh, by=list(social_determinants$year), 
                         FUN="mean", na.rm=TRUE)
aggdata_year


aggdata_districts <-aggregate(social_determinants$idh, by=list(social_determinants$distrito), 
                              FUN="mean", na.rm=TRUE)
aggdata_districts



quartz (height=6,width=10)
par(las=2,                        # use perpendicular axis labels
    mar=c(10.1,4.1,4.1,2.1),      # create enough space for long x labels
    mgp=c(8,1,0), cex=0.7         # move x axis legend down to avoid overlap
)
plotmeans(social_determinants$idh ~ social_determinants$distrito, 
          main="Human Development Index (IDH), Lima-Peru",
          ylab="Human Development Index (IDH)",xlab="District",
          lty=1,col=4, barwidth=4, barcol="red",p=0.95,cex.lab=.05,
          cex.axis=0.8,cex=0.6,las = 1,
          data=social_determinants)



social_determinants$district <- reorder(social_determinants$distrito, social_determinants$idh, median, na.rm = TRUE)
quartz(width=10, height=6, pointsize=8)
par(mar=c(5,4,4,4)+3)
boxplot(social_determinants$idh ~ social_determinants$district,axes=FALSE, 
        ylab="Human Development Index (IDH)", xlab="",
        main="",col="grey", ylim=c(0,1))
orderVtr <- levels(social_determinants$district["scores"])
axis(1, at=seq(1, 43, by=1), labels=FALSE)
axis(2, at=seq(0, 1, by=0.1))
text(x = seq(1, 43, by=1), par("usr")[3] - 0.05, srt = 45,
     labels = as.vector(orderVtr), pos = 2, xpd = TRUE)


quartz (height=6,width=10)
xyplot(social_determinants$idh ~ social_determinants$year| social_determinants$distrito , 
       data=social_determinants, ylab="Human Development Index (IDH)", 
       xlab="Year", main="Human Development Index (IDH), Lima-Peru",
       par.settings=list(fontsize=list(text=6, points=6)),xlim=c(2002:2015)) 


El_agustino<-social_determinants[social_determinants$distrito=="El Agustino",]

quartz (height=6,width=10)
par(mar=c(5,4,4,4)+0.1)
plot(El_agustino$year,El_agustino$idh, lwd=2,las=1,col="blue",type="o",xlab="Year",ylab="Incidence rate per 100,000 people", main="El Agustino",
     ylim=c(0,1),xlim=c(2002,2015))
par(new=T)
plot(El_agustino$year,El_agustino$pobreza,ylim=c(0,100), col="red", axes=F,ylab="",type="o",xlab="Year",lwd=2,xlim=c(2002,2015))
par(new=T)
plot(El_agustino$year,El_agustino$extr_pobreza,col="green",ylim=c(0,100),axes=F,type="o",xlab="Year",ylab="",lwd=2,xlim=c(2002,2015))
mtext(side=4,line=2.5,"Pobreza/Pobreza Extrema")
axis(4,las=1)
legend(locator(1),c("Human Development Index (IDH)","Pobreza","Pobreza Extrema"),fill=c("blue","red","green"),cex=0.7)


quartz (height=6,width=10)
par(mar=c(5,4,4,4)+0.1)
plot(El_agustino$year,El_agustino$idh, lwd=2,las=1,col="blue",type="o",xlab="Year",ylab="Incidence rate per 100,000 people", main="El Agustino",
     ylim=c(0,1),xlim=c(2002,2015))
par(new=T)
plot(El_agustino$year,El_agustino$ingreso_familiar,ylim=c(0,1000), col="red", axes=F,ylab="",type="o",xlab="Year",lwd=2,xlim=c(2002,2015))
mtext(side=4,line=2.5,"Nuevos soles")
axis(4,las=1)
legend(locator(1),c("Human Development Index (IDH)","Ingreso Familiar"),fill=c("blue","red"),cex=0.7)




San_Juan_de_Lurigancho<-social_determinants[social_determinants$distrito=="San Juan de Lurigancho",]

quartz (height=6,width=10)
par(mar=c(5,4,4,4)+0.1)
plot(San_Juan_de_Lurigancho$year,San_Juan_de_Lurigancho$idh, lwd=2,las=1,col="blue",type="o",xlab="Year",ylab="Human Development Index (IDH)",
     main="San Juan de Lurigancho",ylim=c(0,1),xlim=c(2002,2015))
par(new=T)
plot(San_Juan_de_Lurigancho$year,San_Juan_de_Lurigancho$pobreza,ylim=c(0,100), col="red", axes=F,ylab="",type="o",xlab="Year",lwd=2,xlim=c(2003,2013))
par(new=T)
plot(San_Juan_de_Lurigancho$year,San_Juan_de_Lurigancho$extr_pobreza,col="green",ylim=c(0,100),axes=F,type="o",xlab="Year",ylab="",lwd=2,xlim=c(2003,2013))
mtext(side=4,line=2.5,"Pobreza/Pobreza Extrema")
axis(4,las=1)
legend(locator(1),c("Human Development Index (IDH)","Pobreza","Pobreza Extrema"),fill=c("blue","red","green"),cex=0.7)


quartz (height=6,width=10)
par(mar=c(5,4,4,4)+0.1)
plot(San_Juan_de_Lurigancho$year,San_Juan_de_Lurigancho$idh, lwd=2,las=1,col="blue",type="o",xlab="Year",ylab="Incidence rate per 100,000 people", main="El Agustino",
     ylim=c(0,1),xlim=c(2002,2015))
par(new=T)
plot(San_Juan_de_Lurigancho$year,San_Juan_de_Lurigancho$ingreso_familiar,ylim=c(0,1000), col="red", axes=F,ylab="",type="o",xlab="Year",lwd=2,xlim=c(2002,2015))
mtext(side=4,line=2.5,"Nuevos soles")
axis(4,las=1)
legend(locator(1),c("Human Development Index (IDH)","Ingreso Familiar"),fill=c("blue","red"),cex=0.7)


