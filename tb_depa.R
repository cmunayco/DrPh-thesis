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

tb_depa <- read.csv("base 2000-2013.csv",stringsAsFactors = FALSE)

s <- split(tb_depa, tb_depa$depa)
lapply(s, function(x) colMeans(x[, c("t_incidencia","t_tbp_new","sri_atenciones","positividad_sre","tb_mdr","tb_xdr")],na.rm = TRUE))

s <- split(tb_depa, tb_depa$year)
lapply(s, function(x) colMeans(x[, c("t_incidencia","t_tbp_new","sri_atenciones","positividad_sre","tb_mdr","tb_xdr")],na.rm = TRUE))


quartz (height=6,width=10)
par(las=2,                        # use perpendicular axis labels
    mar=c(10.1,4.1,4.1,2.1),      # create enough space for long x labels
    mgp=c(8,1,0), cex=0.7         # move x axis legend down to avoid overlap
)
plotmeans(tb_depa$t_incidencia ~ tb_depa$depa, 
          main="TB incidence rate per 100,000 population, Lima-Peru",
          ylab="TB incidence rate per 100,000 population",xlab="Department",
          lty=1,col=4, barwidth=4, barcol="red",p=0.95,cex.lab=.05,
          cex.axis=0.8,cex=0.6,las = 1,
          data=tb_depa)



tb_depa$depa <- reorder(tb_depa$depa, tb_depa$t_incidencia, median, na.rm = TRUE)
quartz(width=10, height=6, pointsize=8)
par(mar=c(5,4,4,4)+3)
boxplot(tb_depa$t_incidencia ~ tb_depa$depa,axes=FALSE, 
        ylab="TB incidence rate per 100,000 population", xlab="",
        main="",col="grey", ylim=c(0,200))
orderVtr <- levels(tb_depa$depa["scores"])
axis(1, at=seq(1, 25, by=1), labels=FALSE)
axis(2, at=seq(0, 200, by=10))
text(x = seq(1, 25, by=1), par("usr")[3] - 3, srt = 45,
     labels = as.vector(orderVtr), pos = 2, xpd = TRUE)


quartz (height=6,width=10)
xyplot(tb_depa$t_incidencia ~ tb_depa$year| tb_depa$depa , 
       data=tb_depa, ylab="TB incidence rate per 100,000 population", 
       xlab="Year", main="TB incidence rate per 100,000 population, Lima-Peru",
       par.settings=list(fontsize=list(text=6, points=6)),xlim=c(1996:2014)) 
