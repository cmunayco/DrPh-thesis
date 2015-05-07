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
library(optimx)
###databases

tb_districts <- read.csv("DISTRITOS LIMA 04_07_11_12.csv",stringsAsFactors = FALSE)
social_determinants <- read.csv("Indicadores lima 2005_07_09.csv",stringsAsFactors = FALSE)


df1<-as.data.frame(tb_districts[(tb_districts$year!=2004 & tb_districts$year!=2006 & tb_districts$year!=2008 & 
                                    tb_districts$year!=2009 & tb_districts$year!=2010) ,])
df1<-as.data.frame(df1[,c(2,3,5,11)])


df2<-as.data.frame(social_determinants[(social_determinants$year!=2003 & social_determinants$year!=2004 & 
                                          social_determinants$year!=2006 & social_determinants$year!=2008 & 
                                          social_determinants$year!=2009 & social_determinants$year!=2010 &
                                          social_determinants$year!=2013 & social_determinants$year!=2014) ,])
                                          
df2<-as.data.frame(df2[,c(2,3,4,12,15)])


df<-merge(df1,df2,by=c("distrito","year"),all=TRUE)
                   
hdi2005<-df[df$year==2005,]


hdi2005sort<-hdi2005[order(hdi2005$idh),]
totalp=sum(hdi2005sort$pobtot)
totaltb=sum(hdi2005sort$new_casos)
hdi2005sort$Wpop<-hdi2005sort$pobtot/totalp
hdi2005sort$CWpop<-cumsum(hdi2005sort$Wpop)
hdi2005sort$ridit<-c((0+hdi2005sort$CWpop[1])/2,(hdi2005sort$CWpop[1]+hdi2005sort$CWpop[2])/2,
                     (hdi2005sort$CWpop[2]+hdi2005sort$CWpop[3])/2,(hdi2005sort$CWpop[3]+hdi2005sort$CWpop[4])/2,
                     (hdi2005sort$CWpop[4]+hdi2005sort$CWpop[5])/2,(hdi2005sort$CWpop[5]+hdi2005sort$CWpop[6])/2,
                     (hdi2005sort$CWpop[6]+hdi2005sort$CWpop[7])/2,(hdi2005sort$CWpop[7]+hdi2005sort$CWpop[8])/2,
                     (hdi2005sort$CWpop[8]+hdi2005sort$CWpop[9])/2,(hdi2005sort$CWpop[9]+hdi2005sort$CWpop[10])/2,
                     (hdi2005sort$CWpop[10]+hdi2005sort$CWpop[11])/2,(hdi2005sort$CWpop[11]+hdi2005sort$CWpop[12])/2,
                     (hdi2005sort$CWpop[12]+hdi2005sort$CWpop[13])/2,(hdi2005sort$CWpop[13]+hdi2005sort$CWpop[14])/2,
                     (hdi2005sort$CWpop[14]+hdi2005sort$CWpop[15])/2,(hdi2005sort$CWpop[15]+hdi2005sort$CWpop[16])/2,
                     (hdi2005sort$CWpop[16]+hdi2005sort$CWpop[17])/2,(hdi2005sort$CWpop[17]+hdi2005sort$CWpop[18])/2,
                     (hdi2005sort$CWpop[18]+hdi2005sort$CWpop[19])/2,(hdi2005sort$CWpop[19]+hdi2005sort$CWpop[20])/2,
                     (hdi2005sort$CWpop[20]+hdi2005sort$CWpop[21])/2,(hdi2005sort$CWpop[21]+hdi2005sort$CWpop[22])/2,
                     (hdi2005sort$CWpop[22]+hdi2005sort$CWpop[23])/2,(hdi2005sort$CWpop[23]+hdi2005sort$CWpop[24])/2,
                     (hdi2005sort$CWpop[24]+hdi2005sort$CWpop[25])/2,(hdi2005sort$CWpop[25]+hdi2005sort$CWpop[26])/2,
                     (hdi2005sort$CWpop[26]+hdi2005sort$CWpop[27])/2,(hdi2005sort$CWpop[27]+hdi2005sort$CWpop[28])/2,
                     (hdi2005sort$CWpop[28]+hdi2005sort$CWpop[29])/2,(hdi2005sort$CWpop[29]+hdi2005sort$CWpop[30])/2,
                     (hdi2005sort$CWpop[30]+hdi2005sort$CWpop[31])/2,(hdi2005sort$CWpop[31]+hdi2005sort$CWpop[32])/2,
                     (hdi2005sort$CWpop[32]+hdi2005sort$CWpop[33])/2,(hdi2005sort$CWpop[33]+hdi2005sort$CWpop[34])/2,
                     (hdi2005sort$CWpop[34]+hdi2005sort$CWpop[35])/2,(hdi2005sort$CWpop[35]+hdi2005sort$CWpop[36])/2,
                     (hdi2005sort$CWpop[36]+hdi2005sort$CWpop[37])/2,(hdi2005sort$CWpop[37]+hdi2005sort$CWpop[38])/2,
                     (hdi2005sort$CWpop[38]+hdi2005sort$CWpop[39])/2,(hdi2005sort$CWpop[39]+hdi2005sort$CWpop[40])/2,
                     (hdi2005sort$CWpop[40]+hdi2005sort$CWpop[41])/2,(hdi2005sort$CWpop[41]+hdi2005sort$CWpop[42])/2,
                     (hdi2005sort$CWpop[42]+hdi2005sort$CWpop[43])/2)

hdi2005sort$Whealth<-hdi2005sort$new_casos/totaltb
hdi2005sort$CWhealth<-cumsum(hdi2005sort$Whealth)
hdi2005sort$logridit<-log10(hdi2005sort$ridit)
hdi2005sort$Wi<-sqrt(hdi2005sort$pobtot)
hdi2005sort$XiWi<-hdi2005sort$Wi*hdi2005sort$logridit
hdi2005sort$YiWi<-hdi2005sort$Wi*hdi2005sort$incidencia
fit2005<-lm(hdi2005sort$YiWi~hdi2005sort$Wi + hdi2005sort$XiWi + 0)
summary(fit2005)
hdi2005sort$predict2005<-coef(summary(fit2005))[1,1] + coef(summary(fit2005))[2,1]*hdi2005sort$logridit



hdi2007<-as.data.frame(df[df$year=="2007",])
hdi2007sort<-hdi2007[order(hdi2007$idh),]
totalp=sum(hdi2007sort$pobtot)
totaltb=sum(hdi2007sort$new_casos)
hdi2007sort$Wpop<-hdi2007sort$pobtot/totalp
hdi2007sort$CWpop<-cumsum(hdi2007sort$Wpop)
hdi2007sort$ridit<-c((0+hdi2007sort$CWpop[1])/2,(hdi2007sort$CWpop[1]+hdi2007sort$CWpop[2])/2,
                     (hdi2007sort$CWpop[2]+hdi2007sort$CWpop[3])/2,(hdi2007sort$CWpop[3]+hdi2007sort$CWpop[4])/2,
                     (hdi2007sort$CWpop[4]+hdi2007sort$CWpop[5])/2,(hdi2007sort$CWpop[5]+hdi2007sort$CWpop[6])/2,
                     (hdi2007sort$CWpop[6]+hdi2007sort$CWpop[7])/2,(hdi2007sort$CWpop[7]+hdi2007sort$CWpop[8])/2,
                     (hdi2007sort$CWpop[8]+hdi2007sort$CWpop[9])/2,(hdi2007sort$CWpop[9]+hdi2007sort$CWpop[10])/2,
                     (hdi2007sort$CWpop[10]+hdi2007sort$CWpop[11])/2,(hdi2007sort$CWpop[11]+hdi2007sort$CWpop[12])/2,
                     (hdi2007sort$CWpop[12]+hdi2007sort$CWpop[13])/2,(hdi2007sort$CWpop[13]+hdi2007sort$CWpop[14])/2,
                     (hdi2007sort$CWpop[14]+hdi2007sort$CWpop[15])/2,(hdi2007sort$CWpop[15]+hdi2007sort$CWpop[16])/2,
                     (hdi2007sort$CWpop[16]+hdi2007sort$CWpop[17])/2,(hdi2007sort$CWpop[17]+hdi2007sort$CWpop[18])/2,
                     (hdi2007sort$CWpop[18]+hdi2007sort$CWpop[19])/2,(hdi2007sort$CWpop[19]+hdi2007sort$CWpop[20])/2,
                     (hdi2007sort$CWpop[20]+hdi2007sort$CWpop[21])/2,(hdi2007sort$CWpop[21]+hdi2007sort$CWpop[22])/2,
                     (hdi2007sort$CWpop[22]+hdi2007sort$CWpop[23])/2,(hdi2007sort$CWpop[23]+hdi2007sort$CWpop[24])/2,
                     (hdi2007sort$CWpop[24]+hdi2007sort$CWpop[25])/2,(hdi2007sort$CWpop[25]+hdi2007sort$CWpop[26])/2,
                     (hdi2007sort$CWpop[26]+hdi2007sort$CWpop[27])/2,(hdi2007sort$CWpop[27]+hdi2007sort$CWpop[28])/2,
                     (hdi2007sort$CWpop[28]+hdi2007sort$CWpop[29])/2,(hdi2007sort$CWpop[29]+hdi2007sort$CWpop[30])/2,
                     (hdi2007sort$CWpop[30]+hdi2007sort$CWpop[31])/2,(hdi2007sort$CWpop[31]+hdi2007sort$CWpop[32])/2,
                     (hdi2007sort$CWpop[32]+hdi2007sort$CWpop[33])/2,(hdi2007sort$CWpop[33]+hdi2007sort$CWpop[34])/2,
                     (hdi2007sort$CWpop[34]+hdi2007sort$CWpop[35])/2,(hdi2007sort$CWpop[35]+hdi2007sort$CWpop[36])/2,
                     (hdi2007sort$CWpop[36]+hdi2007sort$CWpop[37])/2,(hdi2007sort$CWpop[37]+hdi2007sort$CWpop[38])/2,
                     (hdi2007sort$CWpop[38]+hdi2007sort$CWpop[39])/2,(hdi2007sort$CWpop[39]+hdi2007sort$CWpop[40])/2,
                     (hdi2007sort$CWpop[40]+hdi2007sort$CWpop[41])/2,(hdi2007sort$CWpop[41]+hdi2007sort$CWpop[42])/2,
                     (hdi2007sort$CWpop[42]+hdi2007sort$CWpop[43])/2)

hdi2007sort$Whealth<-hdi2007sort$new_casos/totaltb
hdi2007sort$CWhealth<-cumsum(hdi2007sort$Whealth)
hdi2007sort$logridit<-log10(hdi2007sort$ridit)
hdi2007sort$Wi<-sqrt(hdi2007sort$pobtot)
hdi2007sort$XiWi<-hdi2007sort$Wi*hdi2007sort$logridit
hdi2007sort$YiWi<-hdi2007sort$Wi*hdi2007sort$incidencia
fit2007<-lm(hdi2007sort$YiWi~hdi2007sort$Wi + hdi2007sort$XiWi + 0)
summary(fit2007)
hdi2007sort$predict2007<-coef(summary(fit2007))[1,1] + coef(summary(fit2007))[2,1]*hdi2007sort$logridit

hdi2011<-as.data.frame(df[df$year=="2011",])
hdi2011sort<-hdi2011[order(hdi2011$idh),]
totalp=sum(hdi2011sort$pobtot)
totaltb=sum(hdi2011sort$new_casos)
hdi2011sort$Wpop<-hdi2011sort$pobtot/totalp
hdi2011sort$CWpop<-cumsum(hdi2011sort$Wpop)
hdi2011sort$ridit<-c((0+hdi2011sort$CWpop[1])/2,(hdi2011sort$CWpop[1]+hdi2011sort$CWpop[2])/2,
                     (hdi2011sort$CWpop[2]+hdi2011sort$CWpop[3])/2,(hdi2011sort$CWpop[3]+hdi2011sort$CWpop[4])/2,
                     (hdi2011sort$CWpop[4]+hdi2011sort$CWpop[5])/2,(hdi2011sort$CWpop[5]+hdi2011sort$CWpop[6])/2,
                     (hdi2011sort$CWpop[6]+hdi2011sort$CWpop[7])/2,(hdi2011sort$CWpop[7]+hdi2011sort$CWpop[8])/2,
                     (hdi2011sort$CWpop[8]+hdi2011sort$CWpop[9])/2,(hdi2011sort$CWpop[9]+hdi2011sort$CWpop[10])/2,
                     (hdi2011sort$CWpop[10]+hdi2011sort$CWpop[11])/2,(hdi2011sort$CWpop[11]+hdi2011sort$CWpop[12])/2,
                     (hdi2011sort$CWpop[12]+hdi2011sort$CWpop[13])/2,(hdi2011sort$CWpop[13]+hdi2011sort$CWpop[14])/2,
                     (hdi2011sort$CWpop[14]+hdi2011sort$CWpop[15])/2,(hdi2011sort$CWpop[15]+hdi2011sort$CWpop[16])/2,
                     (hdi2011sort$CWpop[16]+hdi2011sort$CWpop[17])/2,(hdi2011sort$CWpop[17]+hdi2011sort$CWpop[18])/2,
                     (hdi2011sort$CWpop[18]+hdi2011sort$CWpop[19])/2,(hdi2011sort$CWpop[19]+hdi2011sort$CWpop[20])/2,
                     (hdi2011sort$CWpop[20]+hdi2011sort$CWpop[21])/2,(hdi2011sort$CWpop[21]+hdi2011sort$CWpop[22])/2,
                     (hdi2011sort$CWpop[22]+hdi2011sort$CWpop[23])/2,(hdi2011sort$CWpop[23]+hdi2011sort$CWpop[24])/2,
                     (hdi2011sort$CWpop[24]+hdi2011sort$CWpop[25])/2,(hdi2011sort$CWpop[25]+hdi2011sort$CWpop[26])/2,
                     (hdi2011sort$CWpop[26]+hdi2011sort$CWpop[27])/2,(hdi2011sort$CWpop[27]+hdi2011sort$CWpop[28])/2,
                     (hdi2011sort$CWpop[28]+hdi2011sort$CWpop[29])/2,(hdi2011sort$CWpop[29]+hdi2011sort$CWpop[30])/2,
                     (hdi2011sort$CWpop[30]+hdi2011sort$CWpop[31])/2,(hdi2011sort$CWpop[31]+hdi2011sort$CWpop[32])/2,
                     (hdi2011sort$CWpop[32]+hdi2011sort$CWpop[33])/2,(hdi2011sort$CWpop[33]+hdi2011sort$CWpop[34])/2,
                     (hdi2011sort$CWpop[34]+hdi2011sort$CWpop[35])/2,(hdi2011sort$CWpop[35]+hdi2011sort$CWpop[36])/2,
                     (hdi2011sort$CWpop[36]+hdi2011sort$CWpop[37])/2,(hdi2011sort$CWpop[37]+hdi2011sort$CWpop[38])/2,
                     (hdi2011sort$CWpop[38]+hdi2011sort$CWpop[39])/2,(hdi2011sort$CWpop[39]+hdi2011sort$CWpop[40])/2,
                     (hdi2011sort$CWpop[40]+hdi2011sort$CWpop[41])/2,(hdi2011sort$CWpop[41]+hdi2011sort$CWpop[42])/2,
                     (hdi2011sort$CWpop[42]+hdi2011sort$CWpop[43])/2)

hdi2011sort$Whealth<-hdi2011sort$new_casos/totaltb
hdi2011sort$CWhealth<-cumsum(hdi2011sort$Whealth)
hdi2011sort$logridit<-log10(hdi2011sort$ridit)
hdi2011sort$Wi<-sqrt(hdi2011sort$pobtot)
hdi2011sort$XiWi<-hdi2011sort$Wi*hdi2011sort$logridit
hdi2011sort$YiWi<-hdi2011sort$Wi*hdi2011sort$incidencia
fit2011<-lm(hdi2011sort$YiWi~hdi2011sort$Wi + hdi2011sort$XiWi + 0)
summary(fit2011)
hdi2011sort$predict2011<-coef(summary(fit2011))[1,1] + coef(summary(fit2011))[2,1]*hdi2011sort$logridit


hdi2012<-as.data.frame(df[df$year=="2012",])
hdi2012sort<-hdi2012[order(hdi2012$idh),]
totalp=sum(hdi2012sort$pobtot)
totaltb=sum(hdi2012sort$new_casos)
hdi2012sort$Wpop<-hdi2012sort$pobtot/totalp
hdi2012sort$CWpop<-cumsum(hdi2012sort$Wpop)
hdi2012sort$ridit<-c((0+hdi2012sort$CWpop[1])/2,(hdi2012sort$CWpop[1]+hdi2012sort$CWpop[2])/2,
                     (hdi2012sort$CWpop[2]+hdi2012sort$CWpop[3])/2,(hdi2012sort$CWpop[3]+hdi2012sort$CWpop[4])/2,
                     (hdi2012sort$CWpop[4]+hdi2012sort$CWpop[5])/2,(hdi2012sort$CWpop[5]+hdi2012sort$CWpop[6])/2,
                     (hdi2012sort$CWpop[6]+hdi2012sort$CWpop[7])/2,(hdi2012sort$CWpop[7]+hdi2012sort$CWpop[8])/2,
                     (hdi2012sort$CWpop[8]+hdi2012sort$CWpop[9])/2,(hdi2012sort$CWpop[9]+hdi2012sort$CWpop[10])/2,
                     (hdi2012sort$CWpop[10]+hdi2012sort$CWpop[11])/2,(hdi2012sort$CWpop[11]+hdi2012sort$CWpop[12])/2,
                     (hdi2012sort$CWpop[12]+hdi2012sort$CWpop[13])/2,(hdi2012sort$CWpop[13]+hdi2012sort$CWpop[14])/2,
                     (hdi2012sort$CWpop[14]+hdi2012sort$CWpop[15])/2,(hdi2012sort$CWpop[15]+hdi2012sort$CWpop[16])/2,
                     (hdi2012sort$CWpop[16]+hdi2012sort$CWpop[17])/2,(hdi2012sort$CWpop[17]+hdi2012sort$CWpop[18])/2,
                     (hdi2012sort$CWpop[18]+hdi2012sort$CWpop[19])/2,(hdi2012sort$CWpop[19]+hdi2012sort$CWpop[20])/2,
                     (hdi2012sort$CWpop[20]+hdi2012sort$CWpop[21])/2,(hdi2012sort$CWpop[21]+hdi2012sort$CWpop[22])/2,
                     (hdi2012sort$CWpop[22]+hdi2012sort$CWpop[23])/2,(hdi2012sort$CWpop[23]+hdi2012sort$CWpop[24])/2,
                     (hdi2012sort$CWpop[24]+hdi2012sort$CWpop[25])/2,(hdi2012sort$CWpop[25]+hdi2012sort$CWpop[26])/2,
                     (hdi2012sort$CWpop[26]+hdi2012sort$CWpop[27])/2,(hdi2012sort$CWpop[27]+hdi2012sort$CWpop[28])/2,
                     (hdi2012sort$CWpop[28]+hdi2012sort$CWpop[29])/2,(hdi2012sort$CWpop[29]+hdi2012sort$CWpop[30])/2,
                     (hdi2012sort$CWpop[30]+hdi2012sort$CWpop[31])/2,(hdi2012sort$CWpop[31]+hdi2012sort$CWpop[32])/2,
                     (hdi2012sort$CWpop[32]+hdi2012sort$CWpop[33])/2,(hdi2012sort$CWpop[33]+hdi2012sort$CWpop[34])/2,
                     (hdi2012sort$CWpop[34]+hdi2012sort$CWpop[35])/2,(hdi2012sort$CWpop[35]+hdi2012sort$CWpop[36])/2,
                     (hdi2012sort$CWpop[36]+hdi2012sort$CWpop[37])/2,(hdi2012sort$CWpop[37]+hdi2012sort$CWpop[38])/2,
                     (hdi2012sort$CWpop[38]+hdi2012sort$CWpop[39])/2,(hdi2012sort$CWpop[39]+hdi2012sort$CWpop[40])/2,
                     (hdi2012sort$CWpop[40]+hdi2012sort$CWpop[41])/2,(hdi2012sort$CWpop[41]+hdi2012sort$CWpop[42])/2,
                     (hdi2012sort$CWpop[42]+hdi2012sort$CWpop[43])/2)

hdi2012sort$Whealth<-hdi2012sort$new_casos/totaltb
hdi2012sort$CWhealth<-cumsum(hdi2012sort$Whealth)
hdi2012sort$logridit<-log10(hdi2012sort$ridit)
hdi2012sort$Wi<-sqrt(hdi2012sort$pobtot)
hdi2012sort$XiWi<-hdi2012sort$Wi*hdi2012sort$logridit
hdi2012sort$YiWi<-hdi2012sort$Wi*hdi2012sort$incidencia
fit2012<-lm(hdi2012sort$YiWi~hdi2012sort$Wi + hdi2012sort$XiWi + 0)
summary(fit2012)
hdi2012sort$predict2012<-coef(summary(fit2012))[1,1] + coef(summary(fit2012))[2,1]*hdi2012sort$logridit


# Social gradient
slope_index_of_inequality_hdi2005<-fit2005$coefficients[2]
slope_index_of_inequality_hdi2007<-fit2007$coefficients[2]
slope_index_of_inequality_hdi2011<-fit2011$coefficients[2]
slope_index_of_inequality_hdi2012<-fit2012$coefficients[2]
round(slope_index_of_inequality_hdi2005,2)
round(slope_index_of_inequality_hdi2007,2)
round(slope_index_of_inequality_hdi2011,2)
round(slope_index_of_inequality_hdi2012,2)

mylabel1a= bquote(2005==.(format(slope_index_of_inequality_hdi2005,digits=4))) 
mylabel2a= bquote(2007==.(format(slope_index_of_inequality_hdi2007,digits=4))) 
mylabel3a= bquote(2011==.(format(slope_index_of_inequality_hdi2011,digits=4))) 
mylabel4a= bquote(2012==.(format(slope_index_of_inequality_hdi2012,digits=4))) 


quartz(width=10, height=6, pointsize=10)
plot(hdi2005sort$ridit,hdi2005sort$incidencia, col="red",pch=0,
     ylab="Tasa de incidencia de tuberculosis (por 100,000 hab)", 
     xlab="Gradiente de población entre distritos de Lima provincia según índice de desarrollo humano (IDH)")
points(hdi2007sort$ridit,hdi2007sort$incidencia, col="blue",pch=1,
       ylab="", 
       xlab="")
points(hdi2011sort$ridit,hdi2011sort$incidencia, col="green",pch=2,
       ylab="", 
       xlab="")
points(hdi2012sort$ridit,hdi2012sort$incidencia, col="purple",pch=3,
       ylab="", 
       xlab="")
lines(hdi2005sort$ridit,hdi2005sort$predict2005, col="red", lty=1,
      ylab="", 
      xlab="")
lines(hdi2007sort$ridit,hdi2007sort$predict2007, col="blue", lty=2,
      ylab="", 
      xlab="")
lines(hdi2011sort$ridit,hdi2011sort$predict2011, col="green", lty=3,
      ylab="", 
      xlab="")
lines(hdi2012sort$ridit,hdi2012sort$predict2012, col="purple", lty=4,
      ylab="", 
      xlab="")
legend(locator(1),c("2005","2007","2011","2012"),col=c("red","blue","green","purple"),lty=c(1,2,3,4),pch=c(0,1,2,3),cex = .8)
#text(0.8,150, "Índice de desigualdad de la pendiente (IDP)", col="red")
#text(0.8,140, labels=mylabel1a, col="red")
#text(0.8,130, labels=mylabel2a, col="red")
#text(0.8,120, labels=mylabel3a, col="red")
#text(0.8,110, labels=mylabel4a, col="red")





######################################################################################
######################  Quantiles of Human Development Index  ########################
hdi2005sort$qhdi<-cut(hdi2005sort$idh,quantile(hdi2005sort$idh),include.lowest = TRUE,labels=c("Q1","Q2","Q3","Q4"))
hdi2007sort$qhdi<-cut(hdi2007sort$idh,quantile(hdi2007sort$idh),include.lowest = TRUE,labels=c("Q1","Q2","Q3","Q4"))
hdi2011sort$qhdi<-cut(hdi2011sort$idh,quantile(hdi2011sort$idh),include.lowest = TRUE,labels=c("Q1","Q2","Q3","Q4"))
hdi2012sort$qhdi<-cut(hdi2012sort$idh,quantile(hdi2012sort$idh),include.lowest = TRUE,labels=c("Q1","Q2","Q3","Q4"))

list(hdi2005sort$country,hdi2005sort$qhdi)
list(hdi2007sort$country,hdi2007sort$qhdi)
list(hdi2011sort$country,hdi2011sort$qhdi)
list(hdi2012sort$country,hdi2012sort$qhdi)

qpg2005<-sapply(split(hdi2005sort$pobtot,hdi2005sort$qhdi),sum)
qpg2007<-sapply(split(hdi2007sort$pobtot,hdi2007sort$qhdi),sum)
qpg2011<-sapply(split(hdi2005sort$pobtot,hdi2011sort$qhdi),sum)
qpg2012<-sapply(split(hdi2012sort$pobtot,hdi2012sort$qhdi),sum)

wpopg2005<-c(qpg2005[1]/sum(qpg2005),qpg2005[2]/sum(qpg2005),qpg2005[3]/sum(qpg2005),qpg2005[4]/sum(qpg2005))
wpopg2007<-c(qpg2007[1]/sum(qpg2007),qpg2007[2]/sum(qpg2007),qpg2007[3]/sum(qpg2007),qpg2007[4]/sum(qpg2007))
wpopg2011<-c(qpg2005[1]/sum(qpg2011),qpg2005[2]/sum(qpg2011),qpg2005[3]/sum(qpg2011),qpg2005[4]/sum(qpg2011))
wpopg2012<-c(qpg2012[1]/sum(qpg2012),qpg2012[2]/sum(qpg2012),qpg2012[3]/sum(qpg2012),qpg2012[4]/sum(qpg2012))

hdi2005sort$wpop2005<-ifelse(hdi2005sort$qhdi=="Q1", hdi2005sort$pobtot/qpg2005[1],0)
hdi2005sort$wpop2005<-ifelse(hdi2005sort$qhdi=="Q2", hdi2005sort$pobtot/qpg2005[2],hdi2005sort$wpop2005)
hdi2005sort$wpop2005<-ifelse(hdi2005sort$qhdi=="Q3", hdi2005sort$pobtot/qpg2005[3],hdi2005sort$wpop2005)
hdi2005sort$wpop2005<-ifelse(hdi2005sort$qhdi=="Q4", hdi2005sort$pobtot/qpg2005[4],hdi2005sort$wpop2005)

hdi2007sort$wpop2007<-ifelse(hdi2007sort$qhdi=="Q1", hdi2007sort$pobtot/qpg2007[1],0)
hdi2007sort$wpop2007<-ifelse(hdi2007sort$qhdi=="Q2", hdi2007sort$pobtot/qpg2007[2],hdi2007sort$wpop2007)
hdi2007sort$wpop2007<-ifelse(hdi2007sort$qhdi=="Q3", hdi2007sort$pobtot/qpg2007[3],hdi2007sort$wpop2007)
hdi2007sort$wpop2007<-ifelse(hdi2007sort$qhdi=="Q4", hdi2007sort$pobtot/qpg2007[4],hdi2007sort$wpop2007)

hdi2011sort$wpop2011<-ifelse(hdi2011sort$qhdi=="Q1", hdi2011sort$pobtot/qpg2011[1],0)
hdi2011sort$wpop2011<-ifelse(hdi2011sort$qhdi=="Q2", hdi2011sort$pobtot/qpg2011[2],hdi2011sort$wpop2011)
hdi2011sort$wpop2011<-ifelse(hdi2011sort$qhdi=="Q3", hdi2011sort$pobtot/qpg2011[3],hdi2011sort$wpop2011)
hdi2011sort$wpop2011<-ifelse(hdi2011sort$qhdi=="Q4", hdi2011sort$pobtot/qpg2011[4],hdi2011sort$wpop2011)

hdi2012sort$wpop2012<-ifelse(hdi2012sort$qhdi=="Q1", hdi2012sort$pobtot/qpg2012[1],0)
hdi2012sort$wpop2012<-ifelse(hdi2012sort$qhdi=="Q2", hdi2012sort$pobtot/qpg2012[2],hdi2012sort$wpop2012)
hdi2012sort$wpop2012<-ifelse(hdi2012sort$qhdi=="Q3", hdi2012sort$pobtot/qpg2012[3],hdi2012sort$wpop2012)
hdi2012sort$wpop2012<-ifelse(hdi2012sort$qhdi=="Q4", hdi2012sort$pobtot/qpg2012[4],hdi2012sort$wpop2012)

hdi2005sort$wrate<-hdi2005sort$wpop2005*hdi2005sort$incidencia
hdi2007sort$wrate<-hdi2007sort$wpop2007*hdi2007sort$incidencia
hdi2011sort$wrate<-hdi2011sort$wpop2011*hdi2011sort$incidencia
hdi2012sort$wrate<-hdi2012sort$wpop2012*hdi2012sort$incidencia

meang2005<-sapply(split(hdi2005sort$wrate,hdi2005sort$qhdi),sum)
meang2005

meang2007<-sapply(split(hdi2007sort$wrate,hdi2007sort$qhdi),sum)
meang2007

meang2011<-sapply(split(hdi2011sort$wrate,hdi2011sort$qhdi),sum)
meang2011

meang2012<-sapply(split(hdi2012sort$wrate,hdi2012sort$qhdi),sum)
meang2012



Q1<-c(meang2005[1],meang2007[1],meang2011[1],meang2012[1]) 
Q1<-round(Q1,2)
Q2<-c(meang2005[2],meang2007[2],meang2011[2],meang2012[2]) 
Q2<-round(Q2,2)
Q3<-c(meang2005[3],meang2007[3],meang2011[3],meang2012[3]) 
Q3<-round(Q3,2)
Q4<-c(meang2005[4],meang2007[4],meang2011[4],meang2012[4]) 
Q4<-round(Q4,2)
r<-cbind(Q1,Q2,Q3,Q4)
#r<-t(r)
rownames(r)<-c('2005','2007','2011','2012')
colnames(r)<-c('Q1','Q2','Q3','Q4')
r

##Table 4A. Metrics of country-level inequalities in TB incidence according to social stratifiers and year assessed
regional_mean_rate_hdi2012<-sum(wpopg2012*meang2012)
regional_mean_rate_hdi2011<-sum(wpopg2011*meang2011)
regional_mean_rate_hdi2007<-sum(wpopg2007*meang2007)
regional_mean_rate_hdi2005<-sum(wpopg2005*meang2005)
round(regional_mean_rate_hdi2012,2)
round(regional_mean_rate_hdi2011,2)
round(regional_mean_rate_hdi2007,2)
round(regional_mean_rate_hdi2005,2)

# Bottom-top quartile gap 
absolute_Kuznets_index_hdi2012<-meang2012[1]-meang2012[4]
absolute_Kuznets_index_hdi2011<-meang2011[1]-meang2011[4]
absolute_Kuznets_index_hdi2007<-meang2007[1]-meang2007[4]
absolute_Kuznets_index_hdi2005<-meang2005[1]-meang2005[4]
round(absolute_Kuznets_index_hdi2012,2)
round(absolute_Kuznets_index_hdi2011,2)
round(absolute_Kuznets_index_hdi2007,2)
round(absolute_Kuznets_index_hdi2005,2)

relative_Kuznets_index_hdi2012<-meang2012[1]/meang2012[4]
relative_Kuznets_index_hdi2011<-meang2011[1]/meang2011[4]
relative_Kuznets_index_hdi2007<-meang2007[1]/meang2007[4]
relative_Kuznets_index_hdi2005<-meang2005[1]/meang2005[4]
round(relative_Kuznets_index_hdi2012,2)
round(relative_Kuznets_index_hdi2011,2)
round(relative_Kuznets_index_hdi2007,2)
round(relative_Kuznets_index_hdi2005,2)

mylabel1a= bquote(2012==.(format(absolute_Kuznets_index_hdi2012,digits=4))) 
mylabel2a= bquote(2011==.(format(absolute_Kuznets_index_hdi2011,digits=6))) 
mylabel3a= bquote(2007==.(format(absolute_Kuznets_index_hdi2007,digits=4))) 
mylabel4a= bquote(2005==.(format(absolute_Kuznets_index_hdi2005,digits=4))) 
mylabel5a= bquote(2012==.(format(relative_Kuznets_index_hdi2012,digits=3))) 
mylabel6a= bquote(2011==.(format(relative_Kuznets_index_hdi2011,digits=3))) 
mylabel7a= bquote(2007==.(format(relative_Kuznets_index_hdi2007,digits=3))) 
mylabel8a= bquote(2005==.(format(relative_Kuznets_index_hdi2005,digits=3))) 


quartz(width=10, height=6, pointsize=10)
b<-barplot(t(r),col=c("deepskyblue4","dodgerblue3","dodgerblue","deepskyblue"),beside=T,ylim=c(0,300),
           xlab="Cuartíles del Indice de Desarrollo Humano (IDH)", ylab="Tasa promedio de incidencia de TB (por 100,000 hb)")
legend("topright",c("Q1","Q2","Q3","Q4"),
       col= c("deepskyblue4","dodgerblue3","dodgerblue","deepskyblue"),pch=15,bty="n") 
text(x=c(1.5,2.5,3.5,4.5,6.5,7.5,8.5,9.5,11.5,12.5,13.5,14.5,16.5,17.5,18.5,19.5),
     y=c(t(r[1,1]),t(r[1,2]),t(r[1,3]),t(r[1,4]),t(r[2,1]),t(r[2,2]),t(r[2,3]),t(r[2,4]),t(r[3,1]),t(r[3,2]),t(r[3,3]),t(r[3,4]),t(r[4,1]),t(r[4,2]),t(r[4,3]),t(r[4,4])),
     labels=c(t(r[1,1]),t(r[1,2]),t(r[1,3]),t(r[1,4]),t(r[2,1]),t(r[2,2]),t(r[2,3]),t(r[2,4]),t(r[3,1]),t(r[3,2]),t(r[3,3]),t(r[3,4]),t(r[4,1]),t(r[4,2]),t(r[4,3]),t(r[4,4])),
     cex=1,pos=3)
#text(10,200, "Índice de Kuznets absoluto", col="red")
#text(10,190, labels=mylabel1a, col="red")
#text(10,180, labels=mylabel2a, col="red")
#text(10,170, labels=mylabel3a, col="red")
#text(10,160, labels=mylabel4a, col="red")
#text(10,150, "Índice de Kuznets relativo", col="red")
#text(10,140, labels=mylabel5a, col="red")
#text(10,130, labels=mylabel6a, col="red")
#text(10,120, labels=mylabel7a, col="red")
#text(10,110, labels=mylabel8a, col="red")




## lines and dots graphics
quartz(width=10, height=6, pointsize=12)
plot(r[1,],c(2005,2005,2005,2005),bg=rainbow(4), pch=21, cex=2, lwd=3, lty=1,xlim=c(10,250), ylim=c(2004,2014), yaxt='n', xlab="Tasa promedio de incidencia de TB (por 100,000 hb)", ylab="Años")
ticks = c(2005, 2007, 2011, 2012)
axis(side = 2, at = ticks)
#text(seq(2005, 2013, by=1), par("usr")[1] - 1, labels = as.vector(c("2005", "2007", "2011", "2012")), srt = 45, pos = 2, xpd = TRUE)
from.x <- c(r[1,1], r[1,2], r[1,3],r[1,4])
to.x   <- c(r[1,1], r[1,3], r[1,4])
to.y   <- from.y <- c(2005, 2005, 2005,2005)
segments(x0 = from.x, y0 = from.y, x1 = to.x, y1 = to.y,lwd=2)
par(new=TRUE)
plot(r[2,],c(2007,2007,2007,2007), bg=rainbow(4), pch=21, cex=3, lwd=3, xlim=c(10,250), ylim=c(2004,2014), axes=FALSE, xlab="", ylab="")
from.x <- c(r[2,1], r[2,2], r[2,3],r[2,4])
to.x   <- c(r[2,1], r[2,3], r[2,4])
to.y   <- from.y <- c(2007, 2007, 2007,2007) 
segments(x0 = from.x, y0 = from.y, x1 = to.x, y1 = to.y,lwd=2)
par(new=TRUE)
plot(r[3,],c(2011,2011,2011,2011), bg=rainbow(4), pch=21, cex=3, lwd=3, xlim=c(10,250), ylim=c(2004,2014), axes=FALSE, xlab="", ylab="")
from.x <- c(r[3,1], r[3,2], r[3,3],r[3,4])
to.x   <- c(r[3,1], r[3,3], r[3,4])
to.y   <- from.y <- c(2011, 2011, 2011,2011) 
segments(x0 = from.x, y0 = from.y, x1 = to.x, y1 = to.y,lwd=2)
par(new=TRUE)
plot(r[4,],c(2012,2012,2012,2012), bg=rainbow(4), pch=21, cex=3, lwd=3, xlim=c(10,250), ylim=c(2004,2014), axes=FALSE, xlab="", ylab="") 
from.x <- c(r[4,1], r[4,2], r[4,3],r[4,4])
to.x   <- c(r[4,1], r[4,3], r[4,4])
to.y   <- from.y <- c(2012, 2012, 2012,2012) 
segments(x0 = from.x, y0 = from.y, x1 = to.x, y1 = to.y,lwd=2)
legend("topright",c("Q1","Q2","Q3","Q4"),
       col= rainbow(4),
       bg=rainbow(4), pch=19,bty="n", cex=1, title="Cuartíles del Índice de Desarrollo Humano (IDH)") 




##########################################################################
############################# Concentration curve  #######################

CWpopf2005<-c(0,hdi2005sort$CWpop)
CWhealthf2005<-c(0,hdi2005sort$CWhealth)
CWpopf2007<-c(0,hdi2007sort$CWpop)
CWhealthf2007<-c(0,hdi2007sort$CWhealth)
CWpopf2011<-c(0,hdi2011sort$CWpop)
CWhealthf2011<-c(0,hdi2011sort$CWhealth)
CWpopf2012<-c(0,hdi2012sort$CWpop)
CWhealthf2012<-c(0,hdi2012sort$CWhealth)


ccurve2005<-data.frame(y=CWhealthf2005, x=CWpopf2005)
ccurve2007<-data.frame(y=CWhealthf2007, x=CWpopf2007)
ccurve2011<-data.frame(y=CWhealthf2011, x=CWpopf2011)
ccurve2012<-data.frame(y=CWhealthf2012, x=CWpopf2012)

ccurve_f <- function(k,mydata){
  sum((mydata$y-(exp(mydata$x/(k-mydata$x))-1)/(exp(1/(k-1))-1)))^2
}

ccurve.optx2005 <- optimx(par=-1.5, fn=ccurve_f, mydata=ccurve2005, control=list(all.methods=TRUE, save.failures=TRUE, maxit=2500))
print(ccurve.optx2005)

ccurve.optx2007 <- optimx(par=-1.5, fn=ccurve_f, mydata=ccurve2007, control=list(all.methods=TRUE, save.failures=TRUE, maxit=2500))
print(ccurve.optx2007)

ccurve.optx2011 <- optimx(par=-1.5, fn=ccurve_f, mydata=ccurve2011, control=list(all.methods=TRUE, save.failures=TRUE, maxit=2500))
print(ccurve.optx2011)

ccurve.optx2012 <- optimx(par=-1.5, fn=ccurve_f, mydata=ccurve2012, control=list(all.methods=TRUE, save.failures=TRUE, maxit=2500))
print(ccurve.optx2012)

x<-seq(0,1,0.01)

k<-ccurve.optx2005[1,1]
f<-function(x,k) {
  (exp(x/(k-x))-1)/(exp(1/(k-1))-1)
}

lf02005<-f(x,k)
delta_x_y<-x-lf02005
delta_x_y

##Table 4A. Metrics of country-level inequalities in TB incidence according to social stratifiers and year assessed
# Social gradient
health_concentration_index_hdi2005<-2*sum(delta_x_y)*0.01
round(health_concentration_index_hdi2005,2)


k<-ccurve.optx2007[1,1]
f<-function(x,k) {
  (exp(x/(k-x))-1)/(exp(1/(k-1))-1)
}

lf02007<-f(x,k)

delta_x_y<-x-lf02007
delta_x_y

##Table 4A. Metrics of country-level inequalities in TB incidence according to social stratifiers and year assessed
# Social gradient
health_concentration_index_hdi2007<-2*sum(delta_x_y)*0.01
round(health_concentration_index_hdi2007,2)


k<-ccurve.optx2011[1,1]
f<-function(x,k) {
  (exp(x/(k-x))-1)/(exp(1/(k-1))-1)
}

lf02011<-f(x,k)

delta_x_y<-x-lf02011
delta_x_y

##Table 4A. Metrics of country-level inequalities in TB incidence according to social stratifiers and year assessed
# Social gradient
health_concentration_index_hdi2005<-2*sum(delta_x_y)*0.01
round(health_concentration_index_hdi2005,2)


k<-ccurve.optx2012[1,1]
f<-function(x,k) {
  (exp(x/(k-x))-1)/(exp(1/(k-1))-1)
}

lf02012<-f(x,k)

delta_x_y<-x-lf02012
delta_x_y

##Table 4A. Metrics of country-level inequalities in TB incidence according to social stratifiers and year assessed
# Social gradient
health_concentration_index_hdi2012<-2*sum(delta_x_y)*0.01
round(health_concentration_index_hdi2012,2)


mylabel1b= bquote(2012==.(format(health_concentration_index_hdi2012,digits=2))) 
mylabel2b= bquote(2011==.(format(health_concentration_index_hdi2011,digits=2))) 
mylabel3b= bquote(2007==.(format(health_concentration_index_hdi2007,digits=2))) 
mylabel4b= bquote(2005==.(format(health_concentration_index_hdi2005,digits=2))) 

quartz(width=10, height=6, pointsize=10)
plot(CWpopf2005,CWhealthf2005, col="red",pch=0, xlab="Gradiente de población entre países según índice de desarrollo humano (IDH)", ylab="Número de casos incidentes de TB (acumulado)")
points(CWpopf2007,CWhealthf2007, col="blue",pch=1)
points(CWpopf2005,CWhealthf2011, col="green",pch=2)
points(CWpopf2012,CWhealthf2012, col="purple",pch=3)
lines(x,lf02005,col="red", lty=1)
lines(x,lf02007,col="blue", lty=2)
lines(x,lf02011,col="green", lty=3)
lines(x,lf02012,col="purple", lty=4)
lines(x,x)
legend(locator(1),c("2005","2007","2011","2012"),col=c("red","blue","green","purple"),pch=c(0,1,2,3), lty=c(1,2,3,4),cex = .8)
#text(0.8,0.25, "Índices de concentración de salud (IC)", col="red")
#text(0.8,0.22, labels=mylabel1b, col="red")
#text(0.8,0.18, labels=mylabel2b, col="red")
#text(0.8,0.15, labels=mylabel3b, col="red")
#text(0.8,0.12, labels=mylabel4b, col="red")







write.csv(hdi2012,"hdi2012.csv")
write.csv(hdi2005,"hdi2005.csv")
write.csv(hdi2007,"hdi2007.csv")
write.csv(hdi2005,"hdi2005.csv")






