library(s2dverification)
library(multiApply)
library(easyVerification)
library(RColorBrewer)
library(ncdf4)

mask <- nc_open('/data/csp/vt17420/CLINT_proj/ERA5/ERA5_masks/mask_utc.nc')
lsm <- ncvar_get(mask,'mask')
nc_close(mask)
lsm[which(!is.na(lsm))]=1

cbar_cmcc <-c('#BC01FF','#6600CC','#6666FF','#4292c6',
              '#6baed6','#9ecae1','#c6dbef','#deebf7','#F0F8FF',
              '#f7fbff','#fff5f0','#fee0d2','#fcbba1','#fb6a4a',
              '#cb181d','#993404','#cc6600','#FFCA4D','#FFE24D','yellow')

path <-'/data/csp/vt17420/CLINT_proj/C3S/'
sys <- c('cmcc-35','dwd-21','ECMWF-5','meteo_france-7')
vars <- c('atemp2m_night','tmin','t2m_night','tmax')
vars_names <- c('Tdisconfort','Tmin','Tnight','Tmax')

# vars <- c('t2m','tmin','tmax')  
# vars_names <- c('T2m','Tmin','Tmax')

var <- 1
systems <- c('CMCC-35','DWD-21','ECMWF-5','MF-7','MM')
out <- '/work/csp/vt17420/'
x11(width=12,height=12)
#cairo_ps(paste0(out,'EnsCorr_temperature_vars_05_15MJJA_1993_2016.ps'),width=14,height=12)
# layout(matrix(c(1:12,13,13,13,13),nrow=4,byrow=T),heights=c(4,4,4,4,2))
layout(matrix(c(1:20,21,21,21,21,21),nrow=5,byrow=T),heights=c(4,4,4,4,2))
par(oma=c(2,2,2,2))
for (var in 1:length(vars)){
 sfile <- readRDS(paste0(path,'C3S_mm_individual_',vars[var],'seasonal_mean_05_15MJJ_1993_2016.RDS'))
 for (s in 1:5){
   lats <- sfile$lats
   lons <- sfile$lons
   flags <- NA*drop(sfile[[1]][[s]])[2,,]
   flags[which(drop(sfile[[1]][[s]])[2,,]>drop(sfile[[1]][[s]])[4,,])]=1
   map <- drop(sfile[[1]][[s]])[2,,]
   PlotEquiMap(map*t(lsm),lat =lats,lon=lons,drawleg = F,
               brks=seq(-1,1,by=0.1),cols=cbar_cmcc,
               dots = flags,dot_symbol = 15,dot_size = 0.8,
               filled.continents = F,colNA = 'white',
               axelab = T)
   map('world',add=T,col='#525252')
   if (s==1){mtext(side=2,vars_names[var],line=2,font=2)}
   if (var==1){mtext(side=3,systems[s],line=1,font=2)}
 }
}
ColorBar(brks=seq(-1,1,by=0.1),cols=cbar_cmcc,
        extra_margin = c(2,0,0,0),
        subsampleg = 1,
        label_scale = 0.9,
        draw_separators = T,vertical = F)
dev.off()


cairo_ps(paste0(out,'RPSS_temperature_vars_05_15MJJA_1993_2016.ps'),width=14,height=12)
x11(width=12,height=12)
#layout(matrix(c(1:12,13,13,13,13),nrow=4,byrow=T),heights=c(4,4,4,4,2))
layout(matrix(c(1:20,21,21,21,21,21),nrow=5,byrow=T),heights=c(4,4,4,4,2))
par(oma=c(2,2,2,2))
for (var in 1:length(vars)){
  sfile <- readRDS(paste0(path,'C3S_mm_individual_',vars[var],'seasonal_mean_05_15MJJ_1993_2016.RDS'))
  for (s in 1:length(systems)){
    #var <- 3
    lats <- sfile$lats
    lons <- sfile$lons
    flags <- NA*drop(sfile[[2]][[s]])[[1]]
    flags[which(sfile[[2]][[s]][[1]]>(sfile[[2]][[s]][[2]]*qnorm(0.95)))]=1
    map <- drop(sfile[[2]][[s]])[[1]]
    PlotEquiMap(map*lsm,lat =lats,lon=lons,drawleg = F,
                brks=seq(-0.45,0.45,by=0.05),
                cols=cbar_cmcc[2:(length(cbar_cmcc)-1)],
                col_sup=cbar_cmcc[length(cbar_cmcc)],
                col_inf=cbar_cmcc[1],
                #dots = flags,dot_symbol = 15,dot_size = 0.8,
                filled.continents = F,colNA = 'white',
                axelab = T)
    map('world',add=T,col='#525252')
    if (s==1){mtext(side=2,vars_names[var],line=2,font=2)}
    if (var==1){mtext(side=3,systems[s],line=1,font=2)}
  }
}
ColorBar(brks=seq(-0.45,0.45,by=0.05),
         cols=cbar_cmcc[2:(length(cbar_cmcc)-1)],
         col_sup=cbar_cmcc[length(cbar_cmcc)],
         col_inf=cbar_cmcc[1],
         extra_margin = c(2,0,0,0),
         subsampleg = 1,
         label_scale = 0.9,
         draw_separators = T,vertical = F)
dev.off()
