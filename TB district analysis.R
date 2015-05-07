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
tab1(tb_districts$mdr, graph=FALSE)
tab1(tb_districts$xdr, graph=FALSE)


s <- split(tb_districts, tb_districts$distrito)
lapply(s, function(x) colMeans(x[, c("tasa_tbpfp","incidencia")],na.rm = TRUE))

s <- split(tb_districts, tb_districts$year)
lapply(s, function(x) colMeans(x[, c("tasa_tbpfp","incidencia")],na.rm = TRUE))



aggdata_year <-aggregate(tb_districts$mdr, by=list(tb_districts$year), 
                    FUN="sum", na.rm=TRUE)
aggdata_year


aggdata_districts <-aggregate(tb_districts$mdr, by=list(tb_districts$distrito), 
                    FUN="sum", na.rm=TRUE)
aggdata_districts


#las determines the orientation of the numbers on the tick marks;
#cex determines the size of plotting characters (pch);
#cex.lab determines the size of the text labels on the axes;
#cex.axis determines the size of the numbers on the tick marks.


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



#### TB incidence

tb_districts$district <- reorder(tb_districts$distrito, tb_districts$incidencia, median, na.rm = TRUE)
quartz(width=10, height=6, pointsize=8)
par(mar=c(5,4,4,4)+2)
boxplot(tb_districts$incidencia ~ tb_districts$district,axes=FALSE, 
        ylab="Incidence of tuberculosis per 100,000 people", xlab="District",
        main="",col="grey", ylim=c(0,350))
orderVtr <- levels(tb_districts$district["scores"])
axis(1, at=seq(1, 43, by=1), labels=FALSE)
axis(2, at=seq(0, 350, by=50))
text(x = seq(1, 43, by=1), par("usr")[3] - 2, srt = 45,
     labels = as.vector(orderVtr), pos = 2, xpd = TRUE)


#### TB MDR
tb_districts$district11 <- reorder(tb_districts$distrito, tb_districts$mdr, mean, na.rm = TRUE)

quartz(width=10, height=6, pointsize=8)
par(mar=c(5,4,4,4)+2)
boxplot(tb_districts$mdr ~ tb_districts$district11,axes=FALSE, 
        ylab="Number of MDR TB cases", xlab="Year",
        main="",col="grey", ylim=c(0,200))
orderVtr1 <- levels(tb_districts$district11["scores"])
axis(1, at=seq(1, 43, by=1), labels=FALSE)
axis(2, at=seq(0, 200, by=50))
text(x = seq(1, 43, by=1), par("usr")[3] - 2, srt = 45,
     labels = as.vector(orderVtr1), pos = 2, xpd = TRUE)


#abline(h=c(130), lty=2, col="red")

#### TB incidence 

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


#### TB MDR
quartz (height=6,width=10)
xyplot(tb_districts$mdr ~ tb_districts$year| tb_districts$distrito , 
       data=tb_districts, ylab="Number of MDR TB Cases", 
       xlab="Year", main="Number of MDR TB by districts, Lima-Peru",
       par.settings=list(fontsize=list(text=6, points=6)),xlim=c(2003:2013), ylim=c(-10,200))

quartz (height=6,width=10)
xyplot((tb_districts$mdr/tb_districts$new_casos)*100 ~ tb_districts$year| tb_districts$distrito , 
       data=tb_districts, ylab="% MDR TB Cases", 
       xlab="Year", main="% of MDR TB by districts, Lima-Peru",
       par.settings=list(fontsize=list(text=6, points=6)),xlim=c(2003:2013), ylim=c(-1,20)) 


#### TB XDR
quartz (height=6,width=10)
xyplot(tb_districts$xdr ~ tb_districts$year| tb_districts$distrito , 
       data=tb_districts, ylab="Number of XDR TB cases", 
       xlab="Year", main="Number of MDR TB by districts, Lima-Peru",
       par.settings=list(fontsize=list(text=6, points=6)),xlim=c(2003:2013), ylim=c(-1,20))



####################################################################################
############################### Individuals analysis ###############################  
####################################################################################

El_agustino<-tb_districts[tb_districts$distrito=="El Agustino",]

quartz (height=6,width=10)
par(mar=c(5,4,4,4)+0.1)
plot(El_agustino$year,El_agustino$incidencia, lwd=2,las=1,col="blue",type="o",xlab="Year",ylab="Incidence rate per 100,000 people", main="El Agustino",
     ylim=c(0,400),xlim=c(2003,2013))
par(new=T)
plot(El_agustino$year,El_agustino$tasa_tbpfp,ylim=c(0,400), col="red", axes=F,ylab="",type="o",xlab="Year",lwd=2,xlim=c(2003,2013))
par(new=T)
plot(El_agustino$year,El_agustino$tasa_morbi,col="green",ylim=c(0,400),axes=F,type="o",xlab="Year",ylab="",lwd=2,xlim=c(2003,2013))
par(new=T)
plot(El_agustino$year,El_agustino$mdr,col="purple",ylim=c(0,70),axes=F,type="o",xlab="Year",ylab="",lwd=2,xlim=c(2003,2013))
mtext(side=4,line=2.5,"Number of cases")
axis(4,las=1)
par(new=T)
plot(El_agustino$year,El_agustino$xdr,col="blue4",ylim=c(0,70),axes=F,type="o",xlab="Year",ylab="",lwd=2,xlim=c(2003,2013))
legend(locator(1),c("Incidence of tuberculosis per 100,000 people","Incidence of tuberculosis smear positive per 100,000 people",
                    "Morbidity rate per 100,000 people","Number of MDR TB cases", "Number of XDR TB cases"),fill=c("blue","red","green", "purple", "blue4"),cex=0.7)



San_Juan_de_Lurigancho<-tb_districts[tb_districts$distrito=="San Juan de Lurigancho",]

quartz (height=6,width=10)
par(mar=c(5,4,4,4)+0.1)
plot(San_Juan_de_Lurigancho$year,San_Juan_de_Lurigancho$incidencia, lwd=2,las=1,col="blue",type="o",xlab="Year",ylab="Incidence rate per 100,000 people",
     main="San Juan de Lurigancho",ylim=c(0,300),xlim=c(2003,2013))
par(new=T)
plot(San_Juan_de_Lurigancho$year,San_Juan_de_Lurigancho$tasa_tbpfp,ylim=c(0,300), col="red", axes=F,ylab="",type="o",xlab="Year",lwd=2,xlim=c(2003,2013))
par(new=T)
plot(San_Juan_de_Lurigancho$year,San_Juan_de_Lurigancho$tasa_morbi,col="green",ylim=c(0,300),axes=F,type="o",xlab="Year",ylab="",lwd=2,xlim=c(2003,2013))
par(new=T)
plot(San_Juan_de_Lurigancho$year,San_Juan_de_Lurigancho$mdr,col="purple",ylim=c(0,150),axes=F,type="o",xlab="Year",ylab="",lwd=2,xlim=c(2003,2013))
mtext(side=4,line=2.5,"Number of cases")
axis(4,las=1)
par(new=T)
plot(San_Juan_de_Lurigancho$year,San_Juan_de_Lurigancho$xdr,col="blue4",ylim=c(0,70),axes=F,type="o",xlab="Year",ylab="",lwd=2,xlim=c(2003,2013))
legend(locator(1),c("Incidence of tuberculosis per 100,000 people","Incidence of tuberculosis smear positive per 100,000 people",
                    "Morbidity rate per 100,000 people","Number of MDR TB cases", "Number of XDR TB cases"),
       fill=c("blue","red","green", "purple", "blue4"),cex=0.7)


Santa_anita<-tb_districts[tb_districts$distrito=="Santa Anita",]

quartz (height=6,width=10)
par(mar=c(5,4,4,4)+0.1)
plot(Santa_anita$year,Santa_anita$incidencia, lwd=2,las=1,col="blue",type="o",xlab="Year",ylab="Incidence rate per 100,000 people",
     main="Santa Anita",ylim=c(0,300),xlim=c(2003,2013))
par(new=T)
plot(Santa_anita$year,Santa_anita$tasa_tbpfp,ylim=c(0,300), col="red", axes=F,ylab="",type="o",xlab="Year",lwd=2,xlim=c(2003,2013))
par(new=T)
plot(Santa_anita$year,Santa_anita$tasa_morbi,col="green",ylim=c(0,300),axes=F,type="o",xlab="Year",ylab="",lwd=2,xlim=c(2003,2013))
par(new=T)
plot(Santa_anita$year,Santa_anita$mdr,col="purple",ylim=c(0,150),axes=F,type="o",xlab="Year",ylab="",lwd=2,xlim=c(2003,2013))
mtext(side=4,line=2.5,"Number of cases")
axis(4,las=1)
par(new=T)
plot(Santa_anita$year,Santa_anita$xdr,col="blue4",ylim=c(0,70),axes=F,type="o",xlab="Year",ylab="",lwd=2,xlim=c(2003,2013))
legend(locator(1),c("Incidence of tuberculosis per 100,000 people","Incidence of tuberculosis smear positive per 100,000 people",
                    "Morbidity rate per 100,000 people","Number of MDR TB cases", "Number of XDR TB cases"),
       fill=c("blue","red","green", "purple", "blue4"),cex=0.7)



Ate<-tb_districts[tb_districts$distrito=="Ate",]

quartz (height=6,width=10)
par(mar=c(5,4,4,4)+0.1)
plot(Ate$year,Ate$incidencia, lwd=2,las=1,col="blue",type="o",xlab="Year",ylab="Incidence rate per 100,000 people",
     main="Ate Vitarte",ylim=c(0,300),xlim=c(2003,2013))
par(new=T)
plot(Ate$year,Ate$tasa_tbpfp,ylim=c(0,300), col="red", axes=F,ylab="",type="o",xlab="Year",lwd=2,xlim=c(2003,2013))
par(new=T)
plot(Ate$year,Ate$tasa_morbi,col="green",ylim=c(0,300),axes=F,type="o",xlab="Year",ylab="",lwd=2,xlim=c(2003,2013))
par(new=T)
plot(Ate$year,Ate$mdr,col="purple",ylim=c(0,150),axes=F,type="o",xlab="Year",ylab="",lwd=2,xlim=c(2003,2013))
mtext(side=4,line=2.5,"Number of cases")
axis(4,las=1)
par(new=T)
plot(Ate$year,Ate$xdr,col="blue4",ylim=c(0,70),axes=F,type="o",xlab="Year",ylab="",lwd=2,xlim=c(2003,2013))
legend(locator(1),c("Incidence of tuberculosis per 100,000 people","Incidence of tuberculosis smear positive per 100,000 people",
                    "Morbidity rate per 100,000 people","Number of MDR TB cases", "Number of XDR TB cases"),
       fill=c("blue","red","green", "purple", "blue4"),cex=0.7)


La_Victoria<-tb_districts[tb_districts$distrito=="La Victoria",]

quartz (height=6,width=10)
par(mar=c(5,4,4,4)+0.1)
plot(La_Victoria$year,La_Victoria$incidencia, lwd=2,las=1,col="blue",type="o",xlab="Year",ylab="Incidence rLa_Victoria per 100,000 people",
     main="La Victoria",ylim=c(0,400),xlim=c(2003,2013))
par(new=T)
plot(La_Victoria$year,La_Victoria$tasa_tbpfp,ylim=c(0,400), col="red", axes=F,ylab="",type="o",xlab="Year",lwd=2,xlim=c(2003,2013))
par(new=T)
plot(La_Victoria$year,La_Victoria$tasa_morbi,col="green",ylim=c(0,400),axes=F,type="o",xlab="Year",ylab="",lwd=2,xlim=c(2003,2013))
par(new=T)
plot(La_Victoria$year,La_Victoria$mdr,col="purple",ylim=c(0,150),axes=F,type="o",xlab="Year",ylab="",lwd=2,xlim=c(2003,2013))
mtext(side=4,line=2.5,"Number of cases")
axis(4,las=1)
par(new=T)
plot(La_Victoria$year,La_Victoria$xdr,col="blue4",ylim=c(0,70),axes=F,type="o",xlab="Year",ylab="",lwd=2,xlim=c(2003,2013))
legend(locator(1),c("Incidence of tuberculosis per 100,000 people","Incidence of tuberculosis smear positive per 100,000 people",
                    "Morbidity rate per 100,000 people","Number of MDR TB cases", "Number of XDR TB cases"),
       fill=c("blue","red","green", "purple", "blue4"),cex=0.7)




