# Preliminary setup

# cleaning:
rm(list=ls())

# To run this example, install and load Library CSTools
# install.packages('CSTools', 'zeallot', 's2dv','ClimProjDiags')
# load libraries:
library(CSTools)
library(s2dv)
library(ClimProjDiags)
library(zeallot)
#library(RColorBrewer)


# 1) Load data
#In this case, the seasonal forecasts of temperature 
#initialized in May from the CMCC and ECMWF seasonal forecasting systems will 
# be used. 

# define paths:
 # path <- '/home/tr100/CST_Calibration/data_storage'
 # exp_path <- list(
 # list(
 #   name = 'cmcc',
 #   path = paste0(
 #     path,
 #     '/exp/cmcc/sps35/monthly_mean/$VAR_NAME$/$VAR_NAME$_$YEAR$$MONTH$01.nc')),
 # list(
 #   name = 'ecmwf',
 #   path = paste0(
 #     path,
 #     '/exp/ecmwf/seas5/monthly_mean/$VAR_NAME$/$VAR_NAME$_$YEAR$$MONTH$01.nc')))
 # 
 # obs_path <- list(list(
 #   name = 'era5',
 #   path = paste0(
 #     path,
 #     '/recon/ecmwf/era5/monthly_mean/$VAR_NAME$/$VAR_NAME$_$YEAR$$MONTH$.nc')))
 # 

 # c(exp,obs) %<-% CST_Load(var = 'tas', 
 #                          exp=exp_path,
 #                          obs=obs_path,
 #                          sdates = paste0(1993:2016, '0501'),
 #                          storefreq ='monthly',
 #                          leadtimemin = 2, leadtimemax = 4,
 #                          latmin = 27, latmax = 48,
 #                          lonmin = -12, lonmax = 40, output = 'lonlat',nprocs=1)
 
# The seasonal forecasts from the two systems (CMCC_SPS35, ECMWF_SEAS5) 
# and the observational reference (ERA5) are stored in two objects: exp and obs
# CST_Load provides corresponding forecasts and observations
# for a common  period and grid 
#### to be removed #########
data <- readRDS('~/CST_Calibration/CST_Calibration_data_1993_2016.RDS')
exp <- data$exp
obs <- data$obs
#### to be removed #########
print(dim(exp$data))
print(dim(obs$data))

# 2) Seasonal mean
# The seasonal mean of both forecasts and observations
# are computed by averaging over the 'ftime' dimension.
exp$data <- MeanDims(exp$data, 'ftime',na.rm = T)
obs$data <- MeanDims(obs$data, 'ftime',na.rm = T)

# 3) Calibration
# The forecasts from each seasonal prediction system are calibrated 
# with the ERA5 observational reference

# The 
exp_cmcc <- exp_ecmwf <- exp
exp_cmcc$data <- Subset(exp$data,along='dataset',indices=1)
exp_ecmwf$data <- Subset(exp$data,along='dataset',indices=2)


exp_cal_cmcc <- CST_Calibration(exp = exp_cmcc,
                  obs = obs,
                  cal.method = 'mse_min')

exp_cal_ecmwf <- CST_Calibration(exp = exp_ecmwf,
                  obs = obs,
                  cal.method = 'mse_min')

# 4) Visualisation

clim_exp <- MeanDims(exp$data,c('member','sdate'),na.rm = T)
clim_cal_cmcc<- MeanDims(exp_cal_cmcc$data,c('member','sdate'),na.rm = T)
clim_cal_ecmwf <- MeanDims(exp_cal_ecmwf$data,c('member','sdate'),na.rm = T)
clim_obs <- MeanDims(obs$data,c('member','sdate'),na.rm = T)

bias_cmcc <- clim_exp[1,,,drop=F]-clim_obs
bias_ecmwf <- clim_exp[2,,,drop=F]-clim_obs
bias_cmcc_cal<- clim_cal_cmcc-clim_obs
bias_ecmwf_cal <- clim_cal_ecmwf-clim_obs


#cairo_ps('/work3/veronicatorralba/MEDCOF/bias.ps',width = 14,height=8)
#x11(width=12,height=10)
PlotLayout('PlotEquiMap',plot_dims = c("lat", "lon"),
           var=list(bias_cmcc,bias_ecmwf,bias_cmcc_cal,bias_ecmwf_cal),
           lon=exp$lon,lat=exp$lat,drawleg="S",
           titles = c('CMCC-SPS35','ECMWF-SEAS5',
                      'CMCC-SPS35 calibrated','ECMWF-SEAS5 calibrated'),
           filled.continents = F,brks=seq(-5.5,5.5,by=1),
           color_fun = clim.palette("bluered"),
           draw_separators = T,
           bar_label_scale = 1.5,
           width=14,height=8,
           toptitle='Mean bias (°C) in JJA 1993-2016',
           title_scale = 0.8,
           bar_extra_margin = c(2,0,0,0),
           fileout = '~/bias.ps',
           country.borders=TRUE)


# The forecasts from each seasonal prediction system are calibrated 
#  by employing the 
# observational ERA5 dataset as a reference
la <- 3
lo <- 10
x11(width=10,height =12)
par(cex.lab=1.2)
years<- 1993:2016
layout(matrix(c(1,2),ncol=1,byrow=T))
col1 <- adjustcolor( "red", alpha.f = 0.2)
col2 <- adjustcolor( "blue", alpha.f = 0.2)
# raw+calibrated CMCC
boxplot(exp$data[1,,,la,lo],names=years,las=2,col=col1,ylim=c(285,303),
        ylab='Temperature (K)')
boxplot(exp_cal_cmcc$data[1,,,la,lo],names=years,las=2,col=col2,add=T)
lines(years,obs$data[1,1,,la,lo],col='black',lwd=2)
legend("top", fill =c(col1,col2,NA),
       border=c(col1,col2,NA),
       cex=1.5,
       bty='n',
       legend = c("raw","calibrated",'obs'),
       horiz = TRUE, lty = c(NA,NA,1),
       col=c(NA,NA,"black"))
# raw+calibrated ECWMF
boxplot(exp$data[2,,,la,lo],names=years,las=2,
        col=col1,ylim=c(285,303), ylab='Temperature (K)')
boxplot(exp_cal_ecmwf$data[1,,,la,lo],names=years,las=2,
        col=col2,add=T)
lines(obs$data[1,1,,la,lo],col='black',lwd=2)
legend("top", fill =c(col1,col2,NA),
       border=c(col1,col2,NA),
       cex=1.5,
       bty='n',
       legend = c("raw","calibrated",'obs'),
       horiz = TRUE, lty = c(NA,NA,1),
       col=c(NA,NA,"black"))
