library(lutz)
library(s2dv)
library(lubridate)
library(RColorBrewer)
library(easyNCDF)

lons <- seq(-15,60,by=0.25)
lats <- seq(25,70,by=0.25)
timeref <- Sys.time()
map_utc_zones <- array(dim=c(length(lons),length(lats)))
for (lo in 1:length(lons)){
  for (la in 1:length(lats)){
    zone <- tz_lookup_coords(lat=lats[la],lon=lons[lo])
    date_aux <- format(timeref,tz=zone)
    map_utc_zones[lo,la]  <-  hour(timeref)-hour(date_aux)
  }
}
map_utc_zones <- -map_utc_zones +2
metadata <- list( map_utc_zones = list(dim = list(units=NULL)) )
attr(map_utc_zones, 'variables') <- metadata 
names(dim(map_utc_zones)) <- c('lon', 'lat') 
lon <- lons
dim(lon) <- length(lon)
metadata <- list(lon = list(units = 'degrees_east'))
attr(lon, 'variables') <- metadata
lat <- lats
dim(lat) <- length(lat)
names(dim(lat)) <- 'lat'
metadata <- list(lat = list(units = 'degrees_north'))
attr(lat, 'variables') <- metadata
ArrayToNc(list(lon,lat,map_utc_zones), '~/ERA5_mask_UTC_Europe_ocean.nc')
library(maps)
cairo_ps(file='map_utc_summer.ps',width=12,height=8)
layout(matrix(c(1,2),ncol=2),widths=c(8,2))
brk <- seq(-1,5,by=1)
cols <- brewer.pal(n=6,'YlOrRd')
cols <- c('#40D0CC','#41ab5d','#dd3497','#07A5FF','#894CAA','#fe9929')
s2dv::PlotEquiMap(map_utc_zones,lat=lats,lon=lons,filled.oceans ='lightgrey',
                  filled.continents = F,brks=brk,cols=cols,drawleg=F)
map('world',add=T,lwd=0.8)
ColorBar(brks=brk,cols=cols,subsampleg = 0,extra_margin = c(0,0,0,2))
axis(4,at=seq(-1,5,by=1)+2,labels=c('UTC','UTC+1','UTC+2','UTC+3','UTC+4','UTC+5','UTC+6'))
dev.off()
