library(multiApply)
library(easyVerification)
library(RColorBrewer)
library(ncdf4)
library(CSTools)
library(psych)
library(s2dv)
  
  mask <- nc_open('~/mask_utc.nc')
  lsm <- ncvar_get(mask,'mask')
  nc_close(mask)
  lsm[which(!is.na(lsm))]=1
  
  cbar_cmcc <-c('#BC01FF','#6600CC','#6666FF','#4292c6',
                '#6baed6','#9ecae1','#c6dbef','#deebf7','#F0F8FF',
                '#f7fbff','#fff5f0','#fee0d2','#fcbba1','#fb6a4a',
                '#cb181d','#993404','#cc6600','#FFCA4D','#FFE24D','yellow')
  # 
  # path <-'/data/csp/vt17420/CLINT_proj/C3S/'
  sys <- c('cmcc-35','dwd-21','ECMWF-5','meteo_france-7')
  vars <- c('atemp2m_night','tmin','t2m_night','tmax')
  vars_names <- c('Tdisconfort','Tmin','Tnight','Tmax')
  systems <- c('CMCC-35','DWD-21','ECMWF-5','MF-7','MM')
  var <- 1
  
  corr_maps <- sig_maps <- array(dim=c(length(vars),length(systems),45,75))
  names(dim(corr_maps)) <- c('var','sys','lat','lon')
  
  for (var in 1:length(vars)){
  sfile <- readRDS(paste0('~/C3S_mm_individual_',vars[var],'seasonal_mean_05_15MJJ_1993_2016.RDS'))
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
    
  #x11(width=16,height =14)
  cairo_ps('~/correlation_differences.ps',width=20,height = 14)
  layout(matrix(c(1:20,21,22,22,22),ncol=6),widths = c(4,4,4,4,4,2.5))
  par(oma=c(2,4,2,0))
  cols1 <- colorRampPalette(brewer.pal(11,'PRGn'))(22)
  cols1[11:12] <- c('white','white')
  for (n in 1:5){
    flags <- NA*drop(sig_maps[1,n,,])
    flags[which(drop(corr_maps[1,n,,])>drop(sig_maps[1,n,,]))]=1
    PlotEquiMap(corr_maps[1,n,,],lat=as.vector(sfile$lats),
                lon=as.vector(sfile$lons),drawleg = F,filled.oceans = 'lightgrey',
                toptitle = systems[n],
                brks=seq(-1,1,by=0.1),cols=cbar_cmcc,
                dots = flags,dot_symbol = 15,dot_size = 1,dot_col='black',
                filled.continents = F,colNA = 'white',
                axelab = T)
    map('world',add=T)
    if(n==1) {mtext(side=2,'Tdisconfort',line=2.5,font=2,cex=1.2)}
    for (i in 1:3){
      #x11()
      flags <- NA*drop(corr_diff_maps[2,i,n,,])
      flags[which(drop(corr_diff_maps[2,i,n,,])<= 0.05)]=1
      # map <- drop(Diff[[i]])[n,1,,]
      PlotEquiMap(corr_diff_maps[1,i,n,,],lat=as.vector(sfile$lats),
                  lon=as.vector(sfile$lons)
                  ,drawleg = F,coast_width = 0.8,filled.oceans = 'lightgrey',
                  brks=seq(-1,1,by=0.1)/2,cols=cols1[2:(length(cols1)-1)],
                  col_inf=cols1[1],col_sup=cols1[length(cols1)],
                  dots = flags,dot_symbol = 15,dot_size = 1,dot_col='magenta',
                  filled.continents = F,colNA = 'white',
                  axelab = T)
      map('world',add=T,lwd=0.8)
      if (n==1){mtext(side=2,c('Tdisc - Tmin', 'Tdisc - Tnight','Tdisc - Tmax')[i],line=2.5,font=2,cex=1.2)}
    }
  }
  ColorBar(brks=seq(-1,1,by=0.1),
           cols=cbar_cmcc,
           #col_inf=cols1[1],col_sup=cols1[length(cols1)],
           extra_margin = c(1,0,1,1),
           subsampleg = 2,
           draw_separators = T,
           title = 'Correlation',
           label_scale = 1.4,
           title_scale = 1.2)
  
  
  ColorBar(brks=seq(-1,1,by=0.1)/2,
           cols=cols1[2:(length(cols1)-1)],
           col_inf=cols1[1],col_sup=cols1[length(cols1)],
           extra_margin = c(2,0,0,1),
           subsampleg = 2,
           draw_separators = T,
           title = 'Correlation difference',
           label_scale = 1.4,
           title_scale = 1.2)
  dev.off()
  
  
