
#################################################################################
######################### Spatial Analysis  #####################################
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
library(RColorBrewer)
library(classInt)
library(spBayes)
library(SpatialEpi)


tb_districts <- read.csv("DISTRITOS LIMA 04_07_11_12.csv",stringsAsFactors = FALSE)



dep_lima<-readShapeSpatial("lima.shp")
dep_lima_districts<-readShapeSpatial("di_lima.shp")
dep_lima_centroide<-readShapeSpatial("centroide.shp")
prov_lima_districts<-readShapeSpatial("limadistrict.shp", ID="UBIGEO")
prov_lima_centroide<-readShapeSpatial("centroidelima.shp")
results5p<-readShapeSpatial("result_5percent.shp")
results10p<-readShapeSpatial("result_10percent.shp")
results20p<-readShapeSpatial("result_20percent.shp")
results30p<-readShapeSpatial("result_30percent.shp")
results40p<-readShapeSpatial("result_40percent.shp")
results50p<-readShapeSpatial("result_50percent.shp")



#prov_lima_districts@data = prov_lima_districts@data[prov_lima_districts@data$DISTRITO != "BELLAVISTA",]
#prov_lima_districts@data = prov_lima_districts@data[prov_lima_districts@data$DISTRITO != "CARMEN DE LA LEGUA REYNOSO",]
#prov_lima_districts@data = prov_lima_districts@data[prov_lima_districts@data$DISTRITO != "LA PERLA",]
#prov_lima_districts@data = prov_lima_districts@data[prov_lima_districts@data$DISTRITO != "LA PUNTA",]
#prov_lima_districts@data = prov_lima_districts@data[prov_lima_districts@data$DISTRITO != "VENTANILLA",]


quartz (height=6,width=6)
plot(dep_lima)

quartz (height=6,width=6)
plot(dep_lima_districts)

quartz (height=6,width=6)
plot(dep_lima_centroide)

quartz (height=6,width=6)
plot(prov_lima_districts)
plot(results,add=T)


quartz (height=6,width=6)
plot(prov_lima_districts)
plot(prov_lima_centroide,add=T)

quartz (height=6,width=6)
plot(satscanr)

names(prov_lima_districts)
summary(prov_lima_districts)
attributes(prov_lima_districts)

class(prov_lima_districts)
head(prov_lima_districts@data$DISTRITO)


# TB map 2004
tb_2004<-tb_districts[tb_districts$year==2004,]

plotvar <- tb_2004$incidencia
nclr <- 4
plotclr <- brewer.pal(nclr,"BuPu")
class <- classIntervals(plotvar, nclr, style="quantile")
colcode <- findColours(class, plotclr)

which(colcode=="#88419D", arr.ind=T)  # finding position of a variable in a dataset



quartz (height=6,width=5)
plot(prov_lima_districts)
plot(prov_lima_districts, col=colcode, add=T)
title(main="Metropolitan Lima")
title(sub="Incidence of tuberculosis per 100,000 people, 2004")
#     sub="Quantile (Equal-Frequency) Class Intervals")
legend(locator(1), legend=names(attr(colcode, "table")), title="Quantiles",
       fill=attr(colcode, "palette"), cex=0.7, bty="n")
pointLabel(prov_lima_centroide@coords[c(3,10,11,12,15,17,23,32,37,42,43),c(1,2)],
           tb_2004$distrito[c(3,10,11,12,15,17,23,32,37,42,43)], offset = 0, cex = .4)
list(tb_2004$ubigeo,tb_2004$distrito,tb_2004$incidencia)



# TB map 2007
tb_2007<-tb_districts[tb_districts$year==2007,]

plotvar <- tb_2007$incidencia
nclr <- 4
plotclr <- brewer.pal(nclr,"BuPu")
class <- classIntervals(plotvar, nclr, style="quantile")
colcode <- findColours(class, plotclr)

quartz (height=6,width=5)
plot(prov_lima_districts)
plot(prov_lima_districts, col=colcode, add=T)
title(main="Metropolitan Lima")
title(sub="Incidence of tuberculosis per 100,000 people, 2007")
#     sub="Quantile (Equal-Frequency) Class Intervals")
legend(locator(1), legend=names(attr(colcode, "table")), title="Quantiles",
       fill=attr(colcode, "palette"), cex=0.7, bty="n")
pointLabel(prov_lima_centroide@coords, tb_2007$distrito, offset = 0, cex = .4)
list(tb_2007$ubigeo,tb_2007$distrito,tb_2007$incidencia)



# TB map 2012
tb_2012<-tb_districts[tb_districts$year==2012,]

plotvar <- tb_2012$incidencia
nclr <- 4
plotclr <- brewer.pal(nclr,"BuPu")
class <- classIntervals(plotvar, nclr, style="quantile")
colcode <- findColours(class, plotclr)

quartz (height=6,width=5)
plot(prov_lima_districts)
plot(prov_lima_districts, col=colcode, add=T)
title(main="Metropolitan Lima")
title(sub="Incidence of tuberculosis per 100,000 people, 2012")
#     sub="Quantile (Equal-Frequency) Class Intervals")
legend(locator(1), legend=names(attr(colcode, "table")), title="Quantiles",
       fill=attr(colcode, "palette"), cex=0.7, bty="n")
pointLabel(prov_lima_centroide@coords, tb_2012$distrito, offset = 0, cex = .4)

list(tb_2012$ubigeo,tb_2012$distrito,tb_2012$incidencia)





### Results SatScan window 5%
class(results5p)
head(results5p@data$result5p_2)
names(results5p)
summary(results5p)
attributes(results5p)

plotvar <- results5p@data$result5p_2
nclr <- 5
plotclr <- brewer.pal(nclr,"BuPu")
class <- classIntervals(plotvar, nclr, style="quantile")
colcode <- findColours(class, plotclr)

quartz (height=6,width=5)
plot(results5p)
plot(results5p, col=colcode, add=T)
title(main="Metropolitan Lima")
title(sub="Relative Risk (Spatial window 5%)")
#     sub="Quantile (Equal-Frequency) Class Intervals")
legend(locator(1), legend=names(attr(colcode, "table")), title="Quantiles",
       fill=attr(colcode, "palette"), cex=0.7, bty="n")
#pointLabel(results5p@data$result5p_4,results5p@data$result5p_5, results5p@data$DISTRITO, offset = 0, cex = .4)



### Results SatScan window 10%
class(results10p)
head(results10p@data$result10_2)
names(results10p)
summary(results10p)
attributes(results10p)

plotvar <- results10p@data$result10_2
nclr <- 5
plotclr <- brewer.pal(nclr,"BuPu")
class <- classIntervals(plotvar, nclr, style="quantile")
colcode <- findColours(class, plotclr)

quartz (height=6,width=5)
plot(results10p)
plot(results10p, col=colcode, add=T)
title(main="Metropolitan Lima")
title(sub="Relative Risk (Spatial window 10%)")
#     sub="Quantile (Equal-Frequency) Class Intervals")
legend(locator(1), legend=names(attr(colcode, "table")), title="Quantiles",
       fill=attr(colcode, "palette"), cex=0.7, bty="n")
#pointLabel(results10p@data$result5p_4,results10p@data$result5p_5, results10p@data$DISTRITO, offset = 0, cex = .4)



## Load TB Data from principal data
data <- tb_districts[,c(2,5,4)]
head(data)

cases<-data[,c(2)]
population<-data[,c(3)]
## Process geographical information and convert to grid
head(results10p@data)
geo <- results10p@data[,49:50]
geo <- latlong2grid(geo)

## Based on the 16 strata levels, computed expected numbers of disease
n.strata <- 16
expected.cases <- expected(data$population, data$cases, n.strata)


## Set Parameters
pop.upper.bound <- 0.5
n.simulations <- 999
alpha.level <- 0.05
plot <- TRUE


## Kulldorff using Binomial likelihoods
binomial <- kulldorff(geo, cases, population, NULL, pop.upper.bound, n.simulations, alpha.level, plot)
cluster <- binomial$most.likely.cluster$location.IDs.included

## plot
plot(pennLC$spatial.polygon,axes=TRUE)
plot(pennLC$spatial.polygon[cluster],add=TRUE,col="red")
title("Most Likely Cluster")

## Kulldorff using Poisson likelihoods
poisson <- kulldorff(geo, cases, population, expected.cases, pop.upper.bound, n.simulations, alpha.level, plot)
cluster <- poisson$most.likely.cluster$location.IDs.included

## plot
plot(pennLC$spatial.polygon,axes=TRUE)
plot(pennLC$spatial.polygon[cluster],add=TRUE,col="red")
title("Most Likely Cluster Controlling for Strata")













## Load Pennsylvania Lung Cancer Data
data(pennLC)
data <- pennLC$data
head(data)

## Process geographical information and convert to grid
geo <- pennLC$geo[,2:3]
geo <- latlong2grid(geo)

## Get aggregated counts of population and cases for each county
population <- tapply(data$population,data$county,sum)
cases <- tapply(data$cases,data$county,sum)

## Based on the 16 strata levels, computed expected numbers of disease
n.strata <- 16
expected.cases <- expected(data$population, data$cases, n.strata)

## Set Parameters
pop.upper.bound <- 0.5
n.simulations <- 999
alpha.level <- 0.05
plot <- TRUE

## Kulldorff using Binomial likelihoods
binomial <- kulldorff(geo, cases, population, NULL, pop.upper.bound, n.simulations, alpha.level, plot)
cluster <- binomial$most.likely.cluster$location.IDs.included

## plot
plot(pennLC$spatial.polygon,axes=TRUE)
plot(pennLC$spatial.polygon[cluster],add=TRUE,col="red")
title("Most Likely Cluster")

## Kulldorff using Poisson likelihoods
poisson <- kulldorff(geo, cases, population, expected.cases, pop.upper.bound, n.simulations, alpha.level, plot)
cluster <- poisson$most.likely.cluster$location.IDs.included

## plot
plot(pennLC$spatial.polygon,axes=TRUE)
plot(pennLC$spatial.polygon[cluster],add=TRUE,col="red")
title("Most Likely Cluster Controlling for Strata")



data(scotland)
data <- scotland$data

data(NYleukemia)
sp.obj <- NYleukemia$spatial.polygon
population <- NYleukemia$data$population
cases <- NYleukemia$data$cases
centroids <- latlong2grid(NYleukemia$geo[, 2:3])

remove <- NYleukemia$surrounded
add <- NYleukemia$surrounding

population[add] <- population[add] + population[remove]
population <- population[-remove]
cases[add] <- cases[add] + cases[remove]
cases <- cases[-remove]
sp.obj <-
  SpatialPolygons(sp.obj@polygons[-remove], proj4string=CRS("+proj=longlat"))
centroids <- centroids[-remove, ]

## Set parameters
y <- cases
E <- expected(population, cases, 1)
max.prop <- 0.15
shape <- c(2976.3, 2.31)
rate <- c(2977.3, 1.31)
J <- 7
pi0 <- 0.95
n.sim.lambda <- 10^4
n.sim.prior <- 10^5
n.sim.post <- 10^5
## (Uncomment first) Compute output
output <- bayes_cluster(y, E, population, sp.obj, centroids, max.prop,
  shape, rate, J, pi0, n.sim.lambda, n.sim.prior, n.sim.post)
plotmap(output$prior.map$high.area, sp.obj)
plotmap(output$post.map$high.area, sp.obj)
plotmap(output$post.map$RR.est.area, sp.obj, log=TRUE)
barplot(output$pk.y, names.arg=0:J, xlab="k", ylab="P(k|y)")



