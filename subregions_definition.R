library(s2dv)
library(maps)
library(raster)
library(terra)
library(ggplot2)
library(maptools)
prudence <- list(
  IP=c(-10,36,3,44),
  FR=c(-5,44,5,50),
  ME=c(2,48,16,55),
  AL=c(5,44,15,48),
  MD=c(3,36,25,44), 
  EA=c(16,44,33,55),
  NAF=c(-10,25,12,36),
  MI=c(25,25,43,42),
  SC=c(5,55,30,70),
  BI=c(-10,50,2,59))

cprod <- list(
  MED=c(-10,35,25,45),
  WE=c(-10,40,5,60),
  CE=c(5,45,30,55),
  NE=c(-10,55,30,65),
  WE=c(30,40,60,60))

data("wrld_simpl")
lats <- seq(25.5,69.6,by=1)
lons <- seq(-14.5,59.5,by=1)

cairo_ps('~/regions_HW_WN.ps',width=8,height=14)
layout(matrix(c(1,2),ncol=1))

PlotEquiMap(array(1,dim=c(45,75)),lat=lats,lon=lons,
            filled.continents = F,
            filled.oceans = 'lightgrey',
            drawleg=F,cols='white',colNA='white',
            country.borders = T,
            boxlim = prudence,
            intylat = 10,
            intxlon = 10,
            boxcol=brewer.pal(10,'Paired'),
            boxlwd=2)
abline(h=seq(30,60,by=10),lty=3,col='darkgrey')
abline(v=seq(-10,60,by=10),lty=3,col='darkgrey')

prudence_pos <- lapply(prudence,function(v){
  xpos <- (v[1]+v[3])/2
  ypos <- (v[4]+v[2])/2
  return(c(xpos,ypos))
})
for (n in 1:length(prudence)){
  reg <- prudence_pos[[n]]
  text(names(prudence)[n],x=reg[1],
       y=reg[2],
       col=brewer.pal(10,'Paired')[n],
       font=2)
}


PlotEquiMap(array(1,dim=c(45,75)),lat=lats,lon=lons,filled.continents = F,
            filled.oceans = 'lightgrey',
            drawleg=F,cols='white',colNA='white',
            country.borders = T,
            boxlim = cprod,
            intylat = 10,
            intxlon = 10,
            boxcol=c('red','cyan','purple',
                     'darkblue','orange'),
            boxlwd=2)

abline(h=seq(30,60,by=10),lty=3,col='darkgrey')
abline(v=seq(-10,60,by=10),lty=3,col='darkgrey')

cprod_pos <- lapply(cprod,function(v){
  xpos <- (v[1]+v[3])/2
  ypos <- (v[4]+v[2])/2
  return(c(xpos,ypos))
})
for (n in 1:length(cprod)){
  reg <- cprod_pos[[n]]
  text(names(cprod)[n],x=reg[1],
       y=reg[2],
       font=2,
       col=c('red','cyan','purple',
             'darkblue','orange')[n])
}
dev.off()




library(s2dv)
library(maps)
library(raster)
library(terra)
library(ggplot2)
library(maptools)
prudence <- list(
  IP=c(-10,36,3,44),
  FR=c(-5,44,5,50),
  ME=c(2,48,16,55),
  AL=c(5,44,15,48),
  MD=c(3,36,25,44), 
  EA=c(16,44,33,55),
  NAF=c(-10,25,12,36),
  MI=c(25,25,43,42),
  SC=c(5,55,30,70),
  BI=c(-10,50,2,59))

cprod <- list(
  MED=c(-10,35,25,45),
  WE=c(-10,40,5,60),
  CE=c(5,45,30,55),
  NE=c(-10,55,30,65),
  WE=c(30,40,60,60))

data("wrld_simpl")
lats <- seq(25.5,69.6,by=1)
lons <- seq(-14.5,59.5,by=1)

cairo_ps('~/regions_HW_WN.ps',width=8,height=14)
layout(matrix(c(1,2),ncol=1))

PlotEquiMap(array(1,dim=c(45,75)),lat=lats,lon=lons,
            filled.continents = F,
            filled.oceans = 'lightgrey',
            drawleg=F,cols='white',colNA='white',
            country.borders = T,
            boxlim = prudence,
            intylat = 10,
            intxlon = 10,
            boxcol=brewer.pal(10,'Paired'),
            boxlwd=2)
abline(h=seq(30,60,by=10),lty=3,col='darkgrey')
abline(v=seq(-10,60,by=10),lty=3,col='darkgrey')

prudence_pos <- lapply(prudence,function(v){
  xpos <- (v[1]+v[3])/2
  ypos <- (v[4]+v[2])/2
  return(c(xpos,ypos))
})
for (n in 1:length(prudence)){
  reg <- prudence_pos[[n]]
  text(names(prudence)[n],x=reg[1],
       y=reg[2],
       col=brewer.pal(10,'Paired')[n],
       font=2)
}


reg <- list(
  IP=c(-10,36,3,44),
  FR=c(-5,44,5,50),
  ME=c(2,48,16,55),
  AL=c(5,44,15,48),
  MD=c(3,36,25,44), 
  EA=c(16,44,33,55),
  NAF=c(-10,25,12,36),
  MI=c(25,25,43,42),
  SC=c(5,55,30,70),
  BI=c(-10,50,2,59),
  MED=c(-10,35,25,45),
  WE=c(-10,40,5,60),
  CE=c(5,45,30,55),
  NE=c(-10,55,30,65),
  WE=c(30,40,60,60),
  ALL=c(-15,25,60,70))



x11()
lats <- seq(24,71,by=1)
lons <- seq(-16,61,by=1)
PlotEquiMap(array(1,dim=c(48,78)),lat=lats,lon=lons,filled.continents = F,
            filled.oceans = 'lightgrey',
            drawleg=F,cols='white',colNA='white',
            country.borders = T,
            boxlim = reg,
            intylat = 10,
            intxlon = 10,
            boxcol=c(rep('red',10),rep('blue',5),'black'),
            boxlwd=2)

abline(h=seq(30,60,by=10),lty=3,col='darkgrey')
abline(v=seq(-10,60,by=10),lty=3,col='darkgrey')

reg_pos <- lapply(reg,function(v){
  xpos <- (v[1]+v[3])/2
  ypos <- (v[4]+v[2])/2
  return(c(xpos,ypos))
})
for (n in 1:length(reg)){
  reg1 <- reg_pos[[n]]
  text(names(reg_pos)[n],x=reg1[1],
       y=reg1[2],
       font=2,
       col=c(rep('red',10),rep('blue',5),'black')[n])
}
dev.off()
