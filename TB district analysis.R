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


tb_districts <- read.csv("DISTRITOS LIMA 04_07_11_12.csv",stringsAsFactors = FALSE)

tab1(tb_districts$distrito, graph=FALSE)
tab1(tb_districts$id_district, graph=FALSE)
tab1(tb_districts$xdr, graph=FALSE)
tab1(tb_districts$mdr, graph=FALSE)


quartz (height=6,width=6.5)
coplot(tb_districts$incidencia ~ tb_districts$year|tb_districts$id_district, type="l", data=tb_districts)

s <- split(tb_districts, tb_districts$distrito)
lapply(s, function(x) colMeans(x[, c("tasa_tbpfp","incidencia")],na.rm = TRUE))

s <- split(tb_districts, tb_districts$year)
lapply(s, function(x) colMeans(x[, c("tasa_tbpfp","incidencia")],na.rm = TRUE))



#las determines the orientation of the numbers on the tick marks;
#cex determines the size of plotting characters (pch);
#cex.lab determines the size of the text labels on the axes;
#cex.axis determines the size of the numbers on the tick marks.

legend.districts<-c(
  )

quartz (height=6,width=10)
par(las=2,                        # use perpendicular axis labels
    mar=c(10.1,4.1,4.1,2.1),      # create enough space for long x labels
    mgp=c(8,1,0), cex=0.7         # move x axis legend down to avoid overlap
)
plotmeans(tb_districts$incidencia ~ tb_districts$distrito, 
          main="Incidence of tuberculosis per 100,000 people (means and confidence intervals), Lima-Peru",
          ylab="Incidence rate per 100,000 people",xlab="District",
          lty=1,col=4, barwidth=4, barcol="red",p=0.95,cex.lab=.05,
          cex.axis=0.8,cex=0.8,las = 1,
          data=tb_districts)

quartz (height=6,width=10)
par(las=2,                        # use perpendicular axis labels
    mar=c(10.1,4.1,4.1,2.1),      # create enough space for long x labels
    mgp=c(8,1,0), cex=0.7         # move x axis legend down to avoid overlap
)
plotmeans(tb_districts$tasa_tbpfp ~ tb_districts$distrito, 
          main="Incidence of smear positive tuberculosis per 100,000 people (means and confidence intervals), Lima-Peru", 
          ylab="Incidence rate per 100,000 people",xlab="District",
          lty=1,col=4, barwidth=4, barcol="red",p=0.95,cex.lab=.05,
          cex.axis=0.8,cex=0.8,las = 1,
          data=tb_districts)


#abline(h=c(130), lty=2, col="red")


quartz (height=6,width=10)
xyplot(tb_districts$incidencia ~ tb_districts$year| tb_districts$distrito , 
       data=tb_districts, ylab="Incidence of tuberculosis per 100,000 people", 
       xlab="Year", main="Incidence of tuberculosis per 100,000 people by districts, Lima-Peru",
       par.settings=list(fontsize=list(text=6, points=6)),xlim=c(2003:2013)) 

quartz (height=6,width=10)
xyplot(tb_districts$tasa_tbpfp ~ tb_districts$year| tb_districts$distrito , 
       data=tb_districts, ylab="Incidence of tuberculosis per 100,000 people", 
       xlab="Year", main="Incidence of smear positive tuberculosis per 100,000 people by districts, Lima-Peru",
       par.settings=list(fontsize=list(text=6, points=6)),xlim=c(2003:2013)) 

####################################################################################
############################### Individuals analysis ###############################  
####################################################################################

El_agustino<-tb_districts[tb_districts$distrito=="El Agustino",]

quartz (height=6,width=10)
plot(El_agustino$year,El_agustino$incidencia, lwd=2,las=1,col="blue",type="o",xlab="Year",ylab="Incidence rate per 100,000 people", main="El Agustino",ylim=c(100,400),xlim=c(2003,2013))
par(new=T)
plot(El_agustino$year,El_agustino$tasa_tbpfp,ylim=c(100,400), col="red", axes=F,ylab="",type="o",xlab="Year",lwd=2,xlim=c(2003,2013))
par(new=T)
plot(El_agustino$year,El_agustino$tasa_morbi,col="green",ylim=c(100,400),axes=F,type="o",xlab="Year",ylab="",lwd=2,xlim=c(2003,2013))
legend(locator(1),c("Incidence of tuberculosis per 100,000 people","Incidence of tuberculosis smear positive per 100,000 people", "Morbidity rate per 100,000 people"),fill=c("blue","red","green"),cex=0.7)


El_agustino<-tb_districts[tb_districts$distrito=="El Agustino",]

quartz (height=6,width=10)
plot(El_agustino$year,El_agustino$incidencia, lwd=2,las=1,col="blue",type="o",xlab="Year",ylab="Incidence rate per 100,000 people", main="El Agustino",ylim=c(100,400),xlim=c(2003,2013))
par(new=T)
plot(El_agustino$year,El_agustino$tasa_tbpfp,ylim=c(100,400), col="red", axes=F,ylab="",type="o",xlab="Year",lwd=2,xlim=c(2003,2013))
par(new=T)
plot(El_agustino$year,El_agustino$tasa_morbi,col="green",ylim=c(100,400),axes=F,type="o",xlab="Year",ylab="",lwd=2,xlim=c(2003,2013))
legend(locator(1),c("Incidence of tuberculosis per 100,000 people","Incidence of smear positive tuberculosis per 100,000 people", "Morbidity rate per 100,000 people"),fill=c("blue","red","green"),cex=0.7)


San_Juan_de_Lurigancho<-tb_districts[tb_districts$distrito=="San Juan de Lurigancho",]

quartz (height=6,width=10)
plot(San_Juan_de_Lurigancho$year,San_Juan_de_Lurigancho$incidencia, lwd=2,las=1,col="blue",type="o",xlab="Year",ylab="Incidence rate per 100,000 people",
     main="San Juan de Lurigancho",ylim=c(100,300),xlim=c(2003,2013))
par(new=T)
plot(San_Juan_de_Lurigancho$year,San_Juan_de_Lurigancho$tasa_tbpfp,ylim=c(100,300), col="red", axes=F,ylab="",type="o",xlab="Year",lwd=2,xlim=c(2003,2013))
mtext(side=4,line=2.5,"Incidence of smear positive tuberculosis per 100,000 people")
par(new=T)
plot(San_Juan_de_Lurigancho$year,San_Juan_de_Lurigancho$tasa_morbi,col="green",ylim=c(100,300),axes=F,type="o",xlab="Year",ylab="",lwd=2,xlim=c(2003,2013))
legend(locator(1),c("Incidence of tuberculosis per 100,000 people","Incidence of smear positive tuberculosis per 100,000 people", "Morbidity rate per 100,000 people"),
       fill=c("blue","red","green"),cex=0.7)



 
