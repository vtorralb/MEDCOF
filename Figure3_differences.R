library(multiApply)
library(easyVerification)
library(RColorBrewer)
library(ncdf4)
library(CSTools)
library(psych)
library(s2dv)
library(maps)
source('/work/csp/vt17420/hatching.R')

mask <- nc_open('~/mask_utc.nc')
lsm <- ncvar_get(mask,'mask')
nc_close(mask)
lsm[which(!is.na(lsm))]=1



cbar_cmcc_old <-c('#BC01FF','#6600CC','#6666FF','#4292c6',
              '#6baed6','#9ecae1','#c6dbef','#deebf7','#F0F8FF',
              '#f7fbff','#fff5f0','#fee0d2','#fcbba1','#fb6a4a',
              '#cb181d','#993404','#cc6600','#FFCA4D','#FFE24D','yellow')

cbar_cmcc <- c("#BC01FF", "#6600CC", "#6666FF", 
               "#4366D5" ,"#2166AC", "#3885BC" ,"#62A6CD", "#98C8DF" ,"#C4DEEC", 
               "#DAF2FE" ,"#FBD0B9",
               "#F4AA88", "#E27B62" ,"#CB4A42", "#B2182B", "#993404" ,"#cc6600" ,"#FFCA4D", "#FFE426", "#FFFF00")



path <-'/data/csp/vt17420/CLINT_proj/C3S/'
sys <- c('cmcc-35','dwd-21','ECMWF-5','meteo_france-7')
vars <- c('atemp2m_night','tmin','t2m_night','tmax')
vars_names <- c('Tdisconfort','Tmin','Tnight','Tmax')
vars_names <- c('ATN','Tmin','TN','Tmax')
systems <- c('CMCC-35','DWD-21','ECMWF-5','MF-7','MM')
var <- 1

corr_maps <- sig_maps <- array(dim=c(length(vars),length(systems),45,75))
names(dim(corr_maps)) <- c('var','sys','lat','lon')

for (var in 1:length(vars)){
  sfile <- readRDS(paste0(path,'/C3S_mm_individual_',vars[var],'seasonal_mean_05_15MJJ_1993_2016.RDS'))
  for (n in 1:5){
    m1 <- drop(sfile[[1]][[n]])[2,,]
    corr_maps[var,n,,] <- m1 
    sig_maps[var,n,,] <- drop(sfile[[1]][[n]])[4,,]
  }
}


stat_corr <- function(data,n){
  nvars <- length(data)
  res <- array(dim=c(2,nvars-1))
  for (i in 1:(nvars-1)){
    pval <- psych::r.test(n=n,r12=data[i+1],r34=data[1])$p
    cdiff <- data[1]-data[i+1]
    res[,i] <- c(cdiff,pval)
  }
  return(res)
}
corr_diff_maps <- Apply(data=list(x=corr_maps),target_dims = 'var',stat_corr,n=length(1993:2016))[[1]]
#x11(width = 14,height = 10)
cairo_ps(file='/work/csp/vt17420/Correlation_apparent_temperature_night_JJA_1993_2016.ps',width=14,height =10)
mat <- matrix(c(1,1,2,2,3,3,0,4,4,5,5,0,6,6,6,6,6,6),ncol=6,byrow=T)
#cairo_ps('/work/csp/vt17420/correlation_differences_sig_col.ps',width=20,height = 14)
layout(mat,heights = c(4,4,2))
par(oma=c(2,2,2,2))
brk <- seq(-0.5,0.5,by=0.1)
cols1 <- colorRampPalette(brewer.pal(11,'PiYG'))(12)
cols1[6:7] <- c('white','white')
#ColorBar(brks=brk,cols=cols1[2:(length(cols1)-1)],col_inf = cols1[1],col_sup = cols1[length(cols1)])
brk <- seq(-0.5,0.5,by=0.1)
for (n in 1:5){
  flags <- NA*drop(sig_maps[1,n,,])
  flags[which(drop(corr_maps[1,n,,])>drop(sig_maps[1,n,,]))]=1
  PlotEquiMap(corr_maps[1,n,,]*flags,lat=as.vector(sfile$lats),
              lon=as.vector(sfile$lons),drawleg = F,filled.oceans = 'lightgrey',
              toptitle = systems[n],
              brks=seq(-1,1,by=0.1),cols=cbar_cmcc,
              #dots = flags,dot_symbol = 15,dot_size = 1,dot_col='black',
              filled.continents = F,colNA = 'white',
              axelab = T)
  map('world',add=T)
}
ColorBar(brks=seq(-1,1,by=0.1),
         cols=cbar_cmcc,
         #col_inf=cols1[1],col_sup=cols1[length(cols1)],
         extra_margin = c(2,0,0,0),
         subsampleg = 1,
         vertical = F,
         draw_separators = T,
         title = 'Ensemble mean correlation',
         label_scale = 1.2,
         title_scale = 1.2)
dev.off()

names(dim(corr_diff_maps)) <- c('cor','var','sys','lat','lon')
mask_pos <- Apply(corr_diff_maps,target_dims = 'sys',
                  function(x){
                    out <- all(x>0)
                    return(out)})[[1]][1,,,]

mask_neg <- Apply(corr_diff_maps,target_dims = 'sys',
                  function(x){
                    out <- all(x<0)
                    return(out)})[[1]][1,,,]

cairo_ps('/work/csp/vt17420/Fig3_differences_hatching.ps',width=14,height=7)
layout(matrix(c(1,2,3,4,4,4),ncol=3,byrow = T),heights=c(4,1.5))
n <- 5
brk <- seq(-0.5,0.5,by=0.1)
cols2 <- colorRampPalette(brewer.pal(11,'PRGn'))(12)[c(1:5,8:12)]
cols1 <- colorRampPalette(cols2)(12)
# cols1[6:7] <- c('white','white')
  # if(n==1) {mtext(side=2,'Tdisconfort',line=2.5,font=2,cex=1.2)}
for (i in 1:3){
  #x11()
  flags <- NA*drop(corr_diff_maps[2,i,n,,])
  flags[which(drop(corr_diff_maps[2,i,n,,])<= 0.05)]=1
  # map <- drop(Diff[[i]])[n,1,,]
  PlotEquiMap(corr_diff_maps[1,i,n,,],lat=as.vector(sfile$lats),
              lon=as.vector(sfile$lons)
              ,drawleg = F,coast_width = 0.2,filled.oceans = 'white',
              brks=brk,cols=cols1[2:(length(cols1)-1)],
              toptitle =  c('ATn - Tmin', 'ATn - Tn','ATn - Tmax')[i],
              col_inf=cols1[1],col_sup=cols1[length(cols1)],
              #dots = flags,dot_symbol = 15,dot_size = 1,dot_col='magenta',
              filled.continents = F,colNA = 'white',
              axelab = T)
  map('world',add=T,lwd=0.8)
#,line=2.5,font=2,cex=1.2)}
  hatching(lats=sfile$lats,lons=sfile$lons,mask=t(mask_pos[i,,]),col_line='#1DC412',dens=20,lwd_size=1.2)
  hatching(lats=sfile$lats,lons=sfile$lons,mask=t(mask_neg[i,,]),col_line='#B365EE',dens=20,lwd_size=1.2)
  
  # for (la in 1:length(sfile$lats)){
  #   for (lo in 1:length(sfile$lons)){
  #     if (mask_pos[i,la,lo]==TRUE && !is.na(mask_pos[i,la,lo])){
  #       points(x=sfile$lons[lo],y=sfile$lats[la],pch=24,col='#1DC412',cex=0.8)
  #     }
  #     if (mask_neg[i,la,lo]==TRUE && !is.na(mask_neg[i,la,lo])){
  #      points(x=sfile$lons[lo],y=sfile$lats[la],pch=25,col='#B365EE',cex=0.8)
  #     }
  #   }
  # }
}
  ColorBar(brks=brk,
           cols=cols1[2:(length(cols1)-1)],
           col_inf=cols1[1],col_sup=cols1[length(cols1)],
           extra_margin = c(3,0,0,1),
           subsampleg = 1,
           vertical=F,
           draw_separators = T,
           title = 'Correlation difference',
           label_scale = 1.4,
           title_scale = 1.2)
  dev.off()
