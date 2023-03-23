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



reg <- list(
  NE=c(-10,55,30,65),
  WE=c(-10,40,5,60),
  MED=c(-10,35,25,45),
  NAF=c(-10,25,12,36),
  CE=c(5,45,30,55),
  EE=c(30,40,60,60),
  MI=c(25,25,43,42),
  ALL=c(-15,25,60,70))

cols <- c('#E91E63','#C0392B','#E67E22','#FFC107','#27AE60','#2980B9','#8E44AD','black')

cairo_ps('~/map_regions.ps',height=10,width = 10)
#cairo_ps(file='/work/csp/vt17420/figures_HW_WN_indices/regions/map_regions.ps',height=10,width = 10)
lats <- seq(24,71,by=1)
lons <- seq(-16,61,by=1)
PlotEquiMap(array(1,dim=c(48,78)),lat=lats,lon=lons,filled.continents = F,
            filled.oceans = 'lightgrey',
            drawleg=F,cols='white',colNA='white',
            boxlim = reg,
            intylat = 10,
            intxlon = 10,
            boxcol=cols,
            boxlwd=3)

map('world',add=T,lwd=0.8,col='grey')
abline(h=seq(30,60,by=10),lty=3,col='darkgrey')
abline(v=seq(-10,60,by=10),lty=3,col='darkgrey')
reg_pos <- lapply(reg,function(v){
  xpos <- (v[1]+v[3])/2
  ypos <- (v[4]+v[2])/2
  return(c(xpos,ypos))
})
for (n in 1:(length(reg)-1)){
  reg1 <- reg_pos[[n]]
  text(names(reg_pos)[n],x=reg1[1],
       y=reg1[2],
       font=2,
       cex=1.5,
       col=cols[n])
}
text('ALL',x=45,font=2,cex=1.5,
     y=68)
dev.off()


