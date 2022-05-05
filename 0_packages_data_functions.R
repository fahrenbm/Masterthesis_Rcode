####This script contains all the packages, functions as well as variables and inputdata
###which are necessary for the further scripts

## loading packages
library(raster) 
library(rgdal) 
library(ggplot2) 
library(lubridate)
library(tidyverse)
library(gridExtra)
library(MAP)
library(reshape2)
library(Hmisc)
library(GGally)
library(rgl)
library(viridis)
library(plot3D)
library(ggspatial)
library(weathermetrics)
library(rasterVis)
library(ggrepel)
library(RColorBrewer)
library(ggpubr)
library(ggExtra)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)


##workspace
setwd("C:/Users/moniq/Desktop/Uni/Master/Masterarbeit/Data")


####loading data and creating variables
###spatial variables
e <- extent(-10.125, 35.125, 35.125, 70.125)
lat <- seq(from = 70, to = 35, by = -0.25)
lon <- seq(from = -10, to = 50, by = 0.25)


###basic data
##contains the detected extreme events in binary information of 1 extreme, 0 not extreme
#used for the flood-fill function, which was done in Julia due to competition problems in R
data_raster <- brick("./kdebools.nc")
data_raster_eu <- crop(data_raster, e)

##contains the information of the IDs after the flood-fill what was done in Julia
extreme_raster <- brick("./kdeextremes.nc")
extreme_raster_eu <- crop(extreme_raster, e)
extreme_raster_eu[extreme_raster_eu == 0] <- NA


###time variable, where I just consider May, June, July and August
zax <- getZ(data_raster_eu)
zax <- paste0(year(zax), "_", month(zax), "_", day(zax))
zax_summer <- zax[c(16:30, 62:76, 108:122, 154:168, 200:214, 246:260, 292:306, 338:352, 384:398,
                    430:444, 476:490, 522:536, 568:582, 615:629, 661:675, 707:720)]
extreme_raster_eu <- setZ(extreme_raster_eu, zax)


#cutting the raster just to the summer months May, June, July, August
extreme_raster_eu_summer <-  extreme_raster_eu[[c(16:30, 62:76, 108:122, 154:168, 200:214, 246:260, 292:306, 338:352, 384:398,
                                                  430:444, 476:490, 522:536, 568:582, 615:629, 661:675, 707:720)]]

### Solution for the area development issue
#cutting the two events with the greatest area into two separately events, because of falsely compound
##first event with ID 419428 05.05.2010 - 24.07.2010
ID_1 <- extreme_raster_eu_summer[[1:109]]
ID_1_1 <- extreme_raster_eu_summer[[110:239]]

df_ID <- data.frame(id=419428, V=419429)
subs_ID_1 <- subs(ID_1_1, df_ID, subsWithNA=FALSE)

new_extreme_1 <- stack(ID_1, subs_ID_1)

##second event
ID_2 <- new_extreme_1[[1:230]]
ID_2_1 <- new_extreme_1[[231:239]]

df_ID <- data.frame(id=867262, V=867263)
subs_ID_2 <- subs(ID_2_1, df_ID, subsWithNA=FALSE)

extreme_raster_eu_summer <- stack(ID_2, subs_ID_2)
extreme_raster_eu_summer <- setZ(extreme_raster_eu_summer, zax_summer)


array_eu_summer <- as.array(extreme_raster_eu_summer)

#writeRaster(extreme_raster_eu_summer, "extreme_raster_eu_summer.nc", format = "CDF")

##creating the variable area of the extreme events for calculation of the extent with weighted cell size
#empty array in the dimension of the data, to fill them later with the concerning IDs

area_eu <- area(extreme_raster_eu_summer[[1]])
area_cube <- array(0, dim = c(140,180,239))
for(i in seq_len(239)){
  area_cube[ , , i] <- area_eu[,]
}

### loading the hydrometeorological variables and calculation of anomalies

##Load GPP and cut the variable to the concerning time and spatial dimension
gpp_mte <- brick("./inputdata.nc", varname = "gross_primary_productivity_MTE")
gpp_mte_eu <- crop(gpp_mte, e)
gpp_eu_array <- as.array(gpp_mte_eu)

gpp_mte_eu_summer <- gpp_mte_eu[[c(16:30, 62:76, 108:122, 154:168, 200:214, 246:260, 292:306, 338:352, 384:398,
                                   430:444, 476:490, 522:536, 568:582, 615:629, 661:675, 707:720)]]

##Calculating the mean seasonal cycle of GPP for computing the anomalies
#Calculating the daily means to compute a seasonal cycle
zindx <- substr(zax[1:46], 6, 11)
gpp_mte_day <- stackApply(gpp_mte_eu, zindx, fun= mean)
dim(gpp_mte_day)


#subtracting the mean seasonal cycle from the daily values to obtain anomalies for the whole year
gpp_anom <- gpp_mte_eu
for (d in seq_len(nlayers(gpp_mte_eu))) {
  gpp_anom[[d]] <- gpp_mte_eu[[d]] - gpp_mte_day[[(d - 1) %% 46 + 1]]
}

##Filtering only the study period 
gpp_summer_anom <- gpp_anom[[c(16:30, 62:76, 108:122, 154:168, 200:214, 246:260, 292:306, 338:352, 384:398,
                               430:444, 476:490, 522:536, 568:582, 615:629, 661:675, 707:720)]]

gpp_anom_array <- as.array(gpp_summer_anom)



###2m Temperature load and cut the variable to the concerning time and spatial dimension
temp_2m <- brick("./inputdata.nc", varname = "air_temperature_2m")
temp_2m_eu <- crop(temp_2m, e)
temp_2m_eu_array <- as.array(temp_2m_eu)

temp_2m_eu_summer <- temp_2m_eu[[c(16:30, 62:76, 108:122, 154:168, 200:214, 246:260, 292:306, 338:352, 384:398,
                                   430:444, 476:490, 522:536, 568:582, 615:629, 661:675, 707:720)]]
temp_2m_eu_summer_array <- as.array(temp_2m_eu_summer)

#Calculating the mean seasonal cycle of GPP for computing the anomalies
#Calculating the daily means to compute a seasonal cycle
temp_2m_day <- stackApply(temp_2m_eu, zindx, fun= mean)
dim(temp_2m_day)

#Substrating the mean seasonal cycle from the daily values to obtain anomalies for the whole year
temp_2m_anom <- temp_2m_eu
for (d in seq_len(nlayers(temp_2m_eu))) {
  temp_2m_anom[[d]] <- temp_2m_eu[[d]] - temp_2m_day[[(d - 1) %% 46 + 1]]
}
dim(temp_2m_anom)

##Filtering only the study period
temp_2m_summer_anom <- temp_2m_anom[[c(16:30, 62:76, 108:122, 154:168, 200:214, 246:260, 292:306, 338:352, 384:398,
    430:444, 476:490, 522:536, 568:582, 615:629, 661:675, 707:720)]]

temp_2m_anom_array <- as.array(temp_2m_summer_anom)



##load and cut the soil moisture variable to the concerning time and spatial dimension Soil moisture
s_mois <- brick("./inputdata.nc", varname = "surface_moisture")
s_mois_eu <- crop(s_mois, e)
s_mois_eu_array <- as.array(s_mois_eu)

soimoi_eu_summer <- s_mois_eu[[c(16:30, 62:76, 108:122, 154:168, 200:214, 246:260, 292:306, 338:352, 384:398,
                                 430:444, 476:490, 522:536, 568:582, 615:629, 661:675, 707:720)]]
s_mois_eu_summer_array <- as.array(soimoi_eu_summer)


#Calculating the mean seasonal cycle of GPP for computing the anomalies
#Calculating the daily means to compute a seasonal cycle

s_mois_day <- stackApply(s_mois_eu, zindx, fun= mean)
dim(s_mois_day)


s_mois_anom <- s_mois_eu
for (d in seq_len(nlayers(s_mois_eu))) {
  s_mois_anom[[d]] <- s_mois_eu[[d]] - s_mois_day[[(d - 1) %% 46 + 1]]
}

##Filtering only the study period
s_mois_summer_anom <- s_mois_anom[[c(16:30, 62:76, 108:122, 154:168, 200:214, 246:260, 292:306, 338:352, 384:398,
                          430:444, 476:490, 522:536, 568:582, 615:629, 661:675, 707:720)]]
s_mois_anom_array <- as.array(s_mois_summer_anom)


##computing extreme event IDs and Frequency of the events
eu_extreme <-  table(array_eu_summer)
id_eu_extreme <- as.data.frame(eu_extreme)
id_eu_extreme$array_eu_summer <- as.numeric(as.character(id_eu_extreme$array_eu_summer))


###Functions######
##Area of the event
get_area <- function(w){
  sum_area <- sum(w, na.rm =TRUE)
}

##Area of the raster 
raster_area <- function(raster_m){
  cell_size<-area(raster_m, na.rm=TRUE, weights=FALSE)
  cell_size<-cell_size[!is.na(cell_size)]
  raster_a <-length(cell_size)*median(cell_size)
}

##Spatial location of the event
get_centroid <- function(idx_dim, w){
  coordi <- idx_dim
  coordi[,1] <- lat[coordi[,1]]
  coordi_2 <- as.numeric(coordi[,2])
  coordi[,2] <- lon[coordi_2[]]
  centroid <- numeric(0)
  centroid[1] <- weighted.mean(coordi[,1], w = w)
  centroid[2] <- weighted.mean(coordi[,2], w = w)
  return(centroid)
}

##Weighted mean of the variable
get_mean_w <- function(x, w) {
  mean_w <- weighted.mean(x, w, na.rm = TRUE)
}


##calculating the time of the event
event_time <- function(idxdim){
  time_r <- range(idxdim[,3])
  time_r[3] <- time_r[2] - time_r[1] +1 ##fuer die lenght
  time_r[1] <- zax_summer[time_r[1]]
  time_r_2 <- as.numeric(time_r[2]) 
  time_r[2] <- zax_summer[time_r_2[1]]
  return(time_r)
}


number_events <- function(df_anom){
  n_events <- df_anom%>%
    mutate(month = month(t_start)) %>%
    count(month)
  return(n_events)
}

###Functions for the second approach which was not used for the main analysis
##for the dimension of the max neg gpp anomalie during a certian event
dim_event <- function(dim_id){
  if (sum(dim_id) > 0 ){
    dim_id[,1] <- lat[dim_id[,1]]
    dim_id_2 <- as.numeric(dim_id[,2])
    dim_id[,2] <- lon[dim_id_2[]]
    dim_id_3 <- as.numeric(dim_id[,3])
    dim_id[,3] <- zax_ID[dim_id_3[]]
    return(dim_id)
  }else{
    dim_id <- matrix(NaN, nrow = 1, ncol = 3)
  }  
}


dim_ID_1 <- function(dim_ID) {
  if (sum(dim_ID) > 0 ) {
    dim_1 <- matrix(dim_ID[1,1:3], nrow = 1, ncol = 3)
  } else {
    dim_1 <- matrix(NaN, nrow = 1, ncol = 3)
  }
}


#variables for graphs
world <- ne_countries(scale = "medium", returnclass = "sf")

