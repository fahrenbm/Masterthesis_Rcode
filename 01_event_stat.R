####In this script I am calculating the event based statistics
## Functions and data from the first script are used here

##variables for the for-loop

i <- 1
n <- length(id_eu_extreme$array_eu_summer)
str(id_eu_extreme)

###creating a list for all event based statistics and calcuate the different variables
## The iteration of the For-loop is per ID number
#The variable x_VARNAME contains the anomalies data of the concerning variable at time and space of the ID
#The variable with just the var. name, contains the actual value during the event

extreme_value_list <- list()
for (i in seq_along(id_eu_extreme$array_eu_summer)) {
  cat("\ri =", i, " / ", n)
  event_id <- id_eu_extreme$array_eu_summer[i]
  idx <- as.numeric(which(array_eu_summer == event_id))
  idx_dim <- which(array_eu_summer == event_id, arr.ind=TRUE)
  w <- area_cube[idx]
  x_gpp <- gpp_anom_array[idx]
  x_temp2m <- temp_2m_anom_array[idx]
  x_smois <- s_mois_anom_array[idx]
  gpp <- gpp_eu_array[idx]
  temp2m <- temp_2m_eu_summer_array[idx]
  soilmoi <- s_mois_eu_summer_array[idx]
  stats <- list()
  stats$ID <- event_id
  stats$npoints <- length(idx)
  stats$area <- get_area(w)
  stats$centroid_lat <- get_centroid(idx_dim, w)[1]
  stats$centroid_lon <- get_centroid(idx_dim, w)[2]
  stats$t_start <- event_time(idx_dim)[1] 
  stats$t_end <- event_time(idx_dim)[2] 
  stats$t_length <- event_time(idx_dim)[3] 
  stats$st_layer <- range(idx_dim[,3])[1]
  stats$end_layer <- range(idx_dim[,3])[2]
  stats$GPP <- get_mean_w(gpp, w)
  stats$GPP_anom <- get_mean_w(x_gpp, w)
  stats$GPP_anom_max <- max(x_gpp)
  stats$GPP_anom_min <- min(x_gpp)
  stats$GPP_int <- sum(x_gpp* w*8, na.rm = TRUE)
  stats$temp2m <- get_mean_w(temp2m, w)
  stats$temp2m_anom <- get_mean_w(x_temp2m, w)
  stats$temp2_anom_max <- max(x_temp2m)
  stats$temp2_anom_min <- min(x_temp2m)
  stats$soilmoi <- get_mean_w(soilmoi, w)
  stats$soilmoi_anom <- get_mean_w(x_smois, w)
  stats$soilmoi_anom_max <- max(x_smois)
  stats$soilmoi_anom_min <- min(x_smois)
  extreme_value_list[[i]] <- stats
}

##List to Dataframe

df_extremes <- as.data.frame(do.call(rbind, extreme_value_list))

##changing the column format to nummeric (before undefined) for further analysis
df_extremes$ID <- as.numeric(df_extremes$ID)
df_extremes$npoints <- as.numeric(df_extremes$npoints)
df_extremes$centroid_lat <- as.numeric(df_extremes$centroid_lat)
df_extremes$centroid_lon <- as.numeric(df_extremes$centroid_lon)
df_extremes$st_layer <- as.numeric(df_extremes$st_layer)
df_extremes$end_layer <- as.numeric(df_extremes$end_layer)
df_extremes$area <- as.numeric(df_extremes$area)
df_extremes$GPP <- as.numeric(df_extremes$GPP)
df_extremes$GPP_anom <- as.numeric(df_extremes$GPP_anom)
df_extremes$GPP_anom_min <- as.numeric(df_extremes$GPP_anom_min)
df_extremes$GPP_anom_max <- as.numeric(df_extremes$GPP_anom_max)
df_extremes$GPP_int <- as.numeric(df_extremes$GPP_int)
df_extremes$temp2m_anom <- as.numeric(df_extremes$temp2m_anom)
df_extremes$temp2_anom_max <- as.numeric(df_extremes$temp2_anom_max)
df_extremes$temp2_anom_min <- as.numeric(df_extremes$temp2_anom_min)
df_extremes$temp2m <- as.numeric(df_extremes$temp2m)
df_extremes$temp2m <- kelvin.to.celsius(df_extremes$temp2m)
df_extremes$soilmoi <- as.numeric(df_extremes$soilmoi)
df_extremes$soilmoi_anom <- as.numeric(df_extremes$soilmoi_anom)
df_extremes$soilmoi_anom_max <- as.numeric(df_extremes$soilmoi_anom_max)
df_extremes$soilmoi_anom_min <- as.numeric(df_extremes$soilmoi_anom_min)
df_extremes$t_length <- as.numeric(df_extremes$t_length)
df_extremes$centroid_lat <- as.numeric(df_extremes$centroid_lat)
df_extremes$centroid_lon <- as.numeric(df_extremes$centroid_lon)

## Changing the date column to character for the graphs
df_extremes$t_start <- as.character(df_extremes$t_start)
df_extremes$t_start <- as.Date(df_extremes$t_start, format = "%Y_%m_%d")
df_extremes$t_end <- as.character(df_extremes$t_end)
df_extremes$t_end <- as.Date(df_extremes$t_end, format = "%Y_%m_%d")

##to get the actual length of the event DAS HIER MUSS NOCH ANGEPASST WERDEN
df_neg_gppanom$t_length <- df_neg_gppanom$t_length -1
df_neg_gppanom$t_length <- df_neg_gppanom$t_length *8
df_neg_gppanom$t_length[df_neg_gppanom$t_length == 0] <- 1


df_neg_gppanom[,c(3:5, 11:23)] <- round(df_neg_gppanom[,c(3:5, 11:23)], digits = 2)


##Save the data frame for all events

#saveRDS(df_extremes, "df_extremes_summer_all.Rda")



##Creating the data frame I am working with for the further analysis
#just the 100 largest events 

df_extremes <- df_extremes %>%
  arrange(desc(area)) %>%
  slice(1:100)

# and from the 100 largest events only the events with negative GPP anomalies
df_neg_gppanom <- df_extremes[df_extremes$GPP_anom < 0, ]
df_neg_gppanom$nr <- 1:55

#saveRDS(df_neg_gppanom, "df_extremes_summer.Rda")
