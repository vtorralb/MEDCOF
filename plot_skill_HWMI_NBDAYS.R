library(s2dv)
library(multiApply)
library(easyVerification)
library(RColorBrewer)
library(ncdf4)
library(maps)

mask <- nc_open('/data/csp/vt17420/CLINT_proj/ERA5/ERA5_masks/mask_utc.nc')
lsm <- ncvar_get(mask,'mask')
nc_close(mask)
lsm[which(!is.na(lsm))]=1

cbar_cmcc <-c('#BC01FF','#6600CC','#6666FF','#4292c6',
              '#6baed6','#9ecae1','#c6dbef','#deebf7','#F0F8FF',
              '#f7fbff','#fff5f0','#fee0d2','#fcbba1','#fb6a4a',
              '#cb181d','#993404','#cc6600','#FFCA4D','#FFE24D','yellow')

systems <- c('cmcc-35','dwd-21','ECMWF-5','meteo_france-7','mm')
path1 <- '/data/csp/vt17420/CLINT_proj/C3S/'
skill_scores <- readRDS(paste0(path1,'C3S_mm_ndays_hwmi_seasonal_mean_05_15MJJ_1993_2016.RDS'))

cairo_ps('/work/csp/vt17420/Corr_HWMI_nbday_all_systems.ps',width = 14,height = 6)
#x11(width = 14,height = 8)
lats <- seq(25.5,69.6,by=1)
lons <- seq(-14.5,59.5,by=1)
layout(matrix(c(1:10,11,11,11,11,11),nrow=3,byrow=T),heights=c(4,4,2))
par(oma=c(2,2,2,2))
#layout(matrix(c(1:8,9,9),byrow = T,ncol = 2),heights=c(4,4,4,4,2))
for (n in 1:2){
  for (sys in 1:length(systems)){
    flags <- NA*drop(skill_scores[[sys]][[n]][[1]])[2,,]
    flags[which(drop(skill_scores[[sys]][[n]][[1]])[2,,]> drop(skill_scores[[sys]][[n]][[1]])[4,,])]=1
    maps <- drop(skill_scores[[sys]][[n]][[1]])[2,,]
    PlotEquiMap(maps*t(lsm),lat =lats,lon=lons,drawleg = F,
                brks=seq(-1,1,by=0.1),cols=cbar_cmcc,
                filled.oceans='lightgrey',
                dots = flags,dot_symbol = 15,dot_size = 0.8,
                filled.continents = F,colNA = 'white',
                axelab = T)
    map('world',add=T,col='#525252')
    if (n==1){mtext(side=3,c('CMCC-35','DWD-21', 'ECMWF-5','MF-7','MM')[sys],line=1,font=2)}
    if (sys==1){mtext(side=2,c('HWMI','NDAYSQ90')[n],line=2,font=2)}
  }
}
ColorBar(brks=seq(-1,1,by=0.1),cols=cbar_cmcc,
         extra_margin = c(2,0,0,0),
         subsampleg = 1,
         label_scale = 1.2,
         draw_separators = T,vertical = F)
dev.off()



cairo_ps('/work/csp/vt17420/RPSS_HWMI_nbday_all_systems.ps',width = 14,height = 6)
lats <- seq(25.5,69.6,by=1)
lons <- seq(-14.5,59.5,by=1)
layout(matrix(c(1:10,11,11,11,11,11),nrow=3,byrow=T),heights=c(4,4,2))
par(oma=c(2,2,2,2))
#layout(matrix(c(1:8,9,9),byrow = T,ncol = 2),heights=c(4,4,4,4,2))
for (n in 1:2){
  for (sys in 1:5){
    #sfile <- readRDS(paste0(path1,'C3S_',systems[sys],'_ndays_hwmi_seasonal_mean_05_15MJJ_1993_2016.RDS'))
    #   skill_scores <- readRDS(paste0(path1,'C3S_',systems[sys],'_ndays_hwmi_seasonal_mean_05_15MJJ_1993_2016.RDS'))
    flags <- NA*drop(skill_scores[[sys]][[n]][[2]])[[1]]
    flags[which(skill_scores[[sys]][[n]][[2]][[1]]>(skill_scores[[sys]][[n]][[2]][[2]]*qnorm(0.95)))]=1
    maps <- drop(skill_scores[[sys]][[n]])[[2]][[1]]*lsm
    maps[which(maps < -1)]= -1
    PlotEquiMap(maps,lat =lats,lon=lons,drawleg = F,
                brks=seq(-0.45,0.45,by=0.05),
                cols=cbar_cmcc[2:(length(cbar_cmcc)-1)],
                col_sup=cbar_cmcc[length(cbar_cmcc)],
                col_inf=cbar_cmcc[1],
                filled.oceans = 'lightgrey',
                dots = flags,dot_symbol = 15,dot_size = 0.8,
                filled.continents = F,colNA = 'white',
                axelab = T)
    map('world',add=T,col='#525252')
    if (n==1){mtext(side=3,c('CMCC-35','DWD-21', 'ECMWF-5','MF-7','MM')[sys],line=1,font=2)}
    if (sys==1){mtext(side=2,c('HWMI','NDAYSQ90')[n],line=2,font=2)}
  }
}
ColorBar(brks=seq(-0.45,0.45,by=0.05),
         cols=cbar_cmcc[2:(length(cbar_cmcc)-1)],
         col_sup=cbar_cmcc[length(cbar_cmcc)],
         col_inf=cbar_cmcc[1],
         extra_margin = c(2,0,0,0),
         subsampleg = 1,
         label_scale = 1.2,
         draw_separators = T,vertical = F)
dev.off()
