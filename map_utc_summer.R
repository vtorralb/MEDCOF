library(lutz)
library(s2dv)
library(lubridate)
library(RColorBrewer)
lons <- seq(-14.5,59.5,by=0.05)
lats <- seq(25.5,69.5,by=0.05)
timeref <- Sys.time()
map_utc_zones <- array(dim=c(length(lats),length(lons)))
for (lo in 1:length(lons)){
  for (la in 1:length(lats)){
   zone <- tz_lookup_coords(lat=lats[la],lon=lons[lo])
   date_aux <- format(timeref,tz=zone)
  map_utc_zones[la,lo]  <-  hour(timeref)-hour(date_aux)
  }
}

map2plot <- -map_utc_zones +1
#x11(width=12,height = 8)
cairo_ps(file='map_utc_summer.ps',width=12,height=8)
layout(matrix(c(1,2),ncol=2),widths=c(8,2))
brk <- seq(-3,3,by=1) +1
cols <- brewer.pal(n=6,'YlOrRd')
cols <- c('#40D0CC','#41ab5d','#dd3497','#07A5FF','#894CAA','#fe9929')
s2dv::PlotEquiMap(map2plot,lat=lats,lon=lons,filled.oceans ='lightgrey',filled.continents = F,brks=brk,cols=cols,drawleg=F)
map('world',add=T,lwd=0.8)
ColorBar(brks=brk,cols=cols,subsampleg = 0,extra_margin = c(0,0,0,2))
axis(4,at=seq(1,6),labels=c('UTC','UTC+1','UTC+2','UTC+3','UTC+4','UTC+5'))
dev.off()
