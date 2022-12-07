library(ncdf4)
library(RColorBrewer)
library(s2dverification)
library(multiApply)
library(easyVerification)
library(ClimProjDiags)
library(s2dv)

years <- 1993:2016

reg_full <- list(
  IP=c(-10,3,36,44),
  FR=c(-5,5,44,50),
  ME=c(2,16,48,55),
  AL=c(5,15,44,48),
  MD=c(3,25,36,44), 
  EA=c(16,33,44,55),
  NAF=c(-10,12,25,36),
  MI=c(25,43,25,42),
  SC=c(5,30,55,70),
  BI=c(-10,2,50,59),
  MED=c(-10,25,35,45),
  WE=c(-10,5,40,60),
  CE=c(5,30,45,55),
  NE=c(-10,30,55,65),
  WE=c(30,60,40,60),
  ALL=c(-15,60,25,70))


reg <- list(
  NE=c(-10,30,55,65),
  WE=c(-10,5,40,60),
  MED=c(-10,25,35,45),
  NAF=c(-10,12,25,36),
  CE=c(5,30,45,55),
  WE=c(30,60,40,60),
  MI=c(25,43,25,42),
  ALL=c(-15,60,25,70))



systems <- c('cmcc-35','dwd-21','ECMWF-5','meteo_france-7')
nmemb <- c(40,30,25,25)
var <- c('atemp2m_night')
cor_vals <- array(dim=c(length(systems),length(reg),2,2))

########## obs ######################
path <- '/data/csp/vt17420/CLINT_proj/CLINT_products_low/'
HWMI <- NBDAYS <- array(dim=c(length(years),45,75))
for (y in 1:length(years)){
  data_var1 <- data_var2 <- list()
  v1 <- 'nbdaygtpercentpct'
  v2 <- paste0('HWMI_',var,'_Europe_15MJJA_CV_percent90_daymin3_ref1993-2016_0')
  file <- paste0('ERA5_HWMI_',var,'/HWMI_',var,'_Europe_15MJJA_CV_percent90_daymin3_ref1993-2016_0_',years[y],'.nc')
  ncold <- nc_open(paste0(path,file))
  d1 <- ncvar_get(ncold,v1)
  d2 <- ncvar_get(ncold,v2)
  NBDAYS[y,,] <- t(d1)
  HWMI[y,,] <- t(d2)
  nc_close(ncold)
}
names(dim(NBDAYS))<- names(dim(HWMI)) <- c('year','lat','lon')

data_obs <- list(ndays=NBDAYS,hwmi=HWMI)

########## exp ######################

#-------

for (sys in 1:length(systems)){
  HWMI <- NBDAYS <- array(dim=c(nmemb[sys],length(years),45,75))
  for (m in 0:(nmemb[sys]-1)){
    for (y in 1:length(years)){
      yr <- years[y]
      path <- paste0('/data/csp/vt17420/CLINT_proj/C3S/',systems[sys],'/seasonal/HWMI_',var,'/')
      file <- paste0('HWMI_',var,'_Europe_',var,'_15MJJA_CV_percent90_daymin3_ref1993-2016_memb_',m,'_',yr,'.nc')
      v1 <- 'nbdaygtpercentpct'
      v2 <- paste0('HWMI_',var,'_Europe_',var,'_15MJJA_CV_percent90_daymin3_ref1993-2016_memb_',m)
      index <- nc_open(paste0(path,file))
      d1 <- ncvar_get(index,v1)
      d2 <- ncvar_get(index,v2)
      NBDAYS[m+1,y,,] <- t(d1)
      HWMI[m+1,y,,] <- t(d2)
      nc_close(index)
    }
  }
  names(dim(NBDAYS))<- names(dim(HWMI)) <- c('member','year','lat','lon')
  data_exp <- list(ndays=NBDAYS,hwmi=HWMI)
  
  lons <- index$dim$lon$vals
  lats <- index$dim$lat$vals
  map_check <- array(dim=c(2,45,75))
  map_check[1,,] <- apply(data_exp$hwmi[,18,,],c(2,3),mean)
  map_check[2,,] <- data_obs$hwmi[18,,]
  x11()
  PlotLayout('PlotEquiMap',c(2,3),
             var=map_check,
             lat=as.vector(lats),
             lon=as.vector(lons),
             filled.continents=F)
  
  funaux <- function(x,la,lo,rg){
    #print(dim(x))
    reg <- SelBox(x, lat=as.vector(la),
                  lon=as.vector(lo),
                  region=rg)
    reg$data <- MeanDims(reg$data,dims=c('lat','lon'))
    return(reg$data)
  }
  
  for (r in 1:length(reg)){
    data_exp_reg <- lapply(data_exp,funaux,la=lats,lo=lons,rg=reg[[r]])
    data_obs_reg <- lapply(data_obs,funaux,la=lats,lo=lons,rg=reg[[r]])
    c1 <- cor.test(MeanDims(data_exp_reg[[1]],'member'),
                   data_obs_reg[[1]])
    c2 <- cor.test(MeanDims(data_exp_reg[[2]],'member'),
                   data_obs_reg[[2]])
    cor_vals[sys,r,1,] <- c(c1$estimate,c1$p.value)
    cor_vals[sys,r,2,] <- c(c2$estimate,c2$p.value)
  }
}



cbar_cmcc <-c('#BC01FF','#6600CC','#6666FF','#4292c6',
              '#6baed6','#9ecae1','#c6dbef','#deebf7','#F0F8FF',
              '#f7fbff','#fff5f0','#fee0d2','#fcbba1','#fb6a4a',
              '#cb181d','#993404','#cc6600','#FFCA4D','#FFE24D','yellow')

labs <- c('NQ90','WNMI')
for (n in 1:length(labs)){
  cairo_ps(file=paste0('/work/csp/vt17420/figures_HW_WN_indices/regions/corrplot_regions_',labs[n],'.ps'),width=10,height = 6)
  M <- cor_vals[,,n,1]
  pvals <- cor_vals[,,n,2]
  colnames(M) <- names(reg)
  rownames(M) <- c('CMCC-35','DWD-21','ECMWF-5','MF-7')
  colnames(pvals) <- names(reg)
  rownames(pvals) <- c('CMCC-35','DWD-21','ECMWF-5','MF-7')
  p1 <- corrplot(M,col=cbar_cmcc,
                 #p.mat=pvals,
                 #sig.level=0.05,
                 method = 'color',tl.col="black",
                 cl.pos = 'b',
                 number.cex=1.2,
                 #insig = "n",
                 tl.cex = 1.2,
                 cl.cex = 1.2,
                 pch.cex = 1.2,pch.col="white",
                 addgrid.col = 'black', addCoef.col = 'black')
  
  v1 <- as.vector(pvals)
  text(x=p1$corrPos$x[which(v1<=0.05)]+0.3,
       y=p1$corrPos$y[which(v1<=0.05)]+0.1,
       '*',font=2,cex=1.2) 
  dev.off()
}

