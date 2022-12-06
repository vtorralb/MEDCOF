library(ncdf4)
library(RColorBrewer)
library(s2dverification)
library(multiApply)
library(easyVerification)

var <- 'atemp2m_night'
years <- 1993:2016

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
systems <- c('cmcc-35','dwd-21','ECMWF-5','meteo_france-7')
nmemb <- c(40,30,25,25)
#-------
sys <- 2
#for (sys in 1:length(systems)){
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
# lons <- index$dim$lon$vals
# lats <- index$dim$lat$vals
# map_check <- array(dim=c(2,45,75))
# map_check[1,,] <- apply(data_exp$hwmi[,18,,],c(2,3),mean)
# map_check[2,,] <- data_obs$hwmi[18,,]
# x11()
# PlotLayout('PlotEquiMap',c(2,3),
#            var=map_check,
#            lat=lats,
#            lon=lons,
#            filled.continents=F)


skill_fun <- function(ens,obs){
  c1 <- s2dverification::Corr(InsertDim(Apply(ens,target_dims='member',mean)[[1]],1,1),
                              InsertDim(obs,1,1),
                              poscor = 2)
  rpss <- veriApply("EnsRpss",
                    aperm(drop(ens),c(4,3,2,1)),
                    aperm(drop(obs),c(3,2,1)),
                    prob=c(1/3,2/3),
                    tdim=3,
                    ensdim=4)
  output <- list(corr=c1,rpss=rpss)
  return(output)
}

skill_hwmi <- skill_fun(ens=data_exp$hwmi,obs=data_obs$hwmi)
skill_ndays <- skill_fun(ens=data_exp$ndays,obs=data_obs$ndays)
skill_scores <- list(hwmi=skill_hwmi,ndays=skill_ndays)

path1 <- '/data/csp/vt17420/CLINT_proj/C3S/'
saveRDS(skill_scores,paste0(path1,'C3S_',systems[sys],'_ndays_hwmi_seasonal_mean_05_15MJJ_1993_2016.RDS'))
