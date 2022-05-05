### Here I computed the monthly averages to analyze possible tendencies
## per month of the entire study period and spatialy 

##creating empty variables with the spatiotemporla extent of each variable
#this will be "filled" than with the concerning IDs per analyzes case
#based on the event stat. (all; only neg. temp & pos soil moisture;  only pos. temp & neg soil moisture;)
gpp_anom_extreme <- gpp_anom_array
gpp_anom_extreme[] <- NA

temp_anom_extreme <- temp_2m_anom_array
temp_anom_extreme[] <- NA

temp_extreme <- temp_2m_eu_summer_array
temp_extreme[] <- NA

soilmoi_anom_extreme <- s_mois_anom_array
soilmoi_anom_extreme[] <- NA

soilmoi_extreme <- s_mois_eu_summer_array
soilmoi_extreme[] <- NA


##month
may <- as.vector(c(1:4, 16:19, 31:34, 46:49, 61:64, 76:79, 91:94, 106:109, 
                   121:124, 136:139, 151:154, 166:169, 181:184, 196:198, 211:213, 226:228))

june <- as.vector(c(5:8, 20:23, 35:38, 50:53, 65:68, 80:83, 95:98, 110:113,
                    125:128, 140:143, 155:158, 170:173, 185:188, 199:202, 214:217, 229:232))

july <- as.vector(c(9:11, 24:27, 39:41, 54:56, 69:71, 84:88, 99:101, 114:116,
                    129:131, 144:147, 159:161, 174:176, 189:191, 203:206, 218:220, 233:235))

august <- as.vector(c(12:15, 28:30, 42:45, 57:60, 72:75, 88:90, 102:105, 117:120, 132:136,
                      148:150, 162:165, 177:180, 192:195, 207:210, 221:225, 236:239))
list_m <- list(may, june, july, august)


#for all 55 events with neg gpp anomalies

for (id in df_neg_gppanom$ID) {
  idx <- as.numeric(which(array_eu_summer == id))
  gpp_anom_extreme[idx] <- gpp_anom_array[idx]
  temp_anom_extreme[idx] <- temp_2m_anom_array[idx]
  temp_extreme[idx] <- temp_2m_eu_summer_array[idx]
  soilmoi_anom_extreme[idx] <- s_mois_anom_array[idx]
  soilmoi_extreme[idx] <- s_mois_eu_summer_array[idx]
  print(id)
}

gpp_anom_extreme_all <- gpp_anom_extreme
temp_anom_extreme_all <- temp_anom_extreme
temp_extreme_all <- temp_extreme
soilmoi_anom_extreme_all <- soilmoi_anom_extreme
##df
df_month_mean <- data.frame(month = 1:4)
df_month_mean$mean_gpp <- NA
df_month_mean$mean_temp <- NA
df_month_mean$mean_soilmoi <- NA

for (m in 1:4) {
  mean_m_gpp <- weighted.mean(gpp_anom_extreme[,,list_m[[m]]], area_cube[,,list_m[[m]]], na.rm = TRUE)
  df_month_mean[df_month_mean$month == m, "mean_gpp"] <- mean_m_gpp
  mean_m_temp <- weighted.mean(temp_anom_extreme[,,list_m[[m]]], area_cube[,,list_m[[m]]], na.rm = TRUE)
  df_month_mean[df_month_mean$month == m, "mean_temp"] <- mean_m_temp
  mean_m_soilmoi <- weighted.mean(soilmoi_anom_extreme[,,list_m[[m]]], area_cube[,,list_m[[m]]], na.rm = TRUE)
  df_month_mean[df_month_mean$month == m, "mean_soilmoi"] <- mean_m_soilmoi
}
df_month_mean["month"][df_month_mean["month"] == 1:4]  <- 5:8


df_month_mean$variable <- "all"



##types of extreme events seperated
##positive temp anomalie and neg soilmoi

ID_event_pos_tempanom_soilanom <- df_neg_gppanom[df_neg_gppanom$soilmoi_anom <0 & df_neg_gppanom$temp2m_anom >0, 1]

gpp_anom_extreme <- gpp_anom_array
gpp_anom_extreme[] <- NA

temp_anom_extreme <- temp_2m_anom_array
temp_anom_extreme[] <- NA

temp_extreme <- temp_2m_eu_summer_array
temp_extreme[] <- NA

soilmoi_anom_extreme <- s_mois_anom_array
soilmoi_anom_extreme[] <- NA

soilmoi_extreme <- s_mois_eu_summer_array
soilmoi_extreme[] <- NA


for (id in ID_event_pos_tempanom_soilanom) {
  idx <- as.numeric(which(array_eu_summer == id))
  gpp_anom_extreme[idx] <- gpp_anom_array[idx]
  temp_anom_extreme[idx] <- temp_2m_anom_array[idx]
  temp_extreme[idx] <- temp_2m_eu_summer_array[idx]
  soilmoi_anom_extreme[idx] <- s_mois_anom_array[idx]
  soilmoi_extreme[idx] <- s_mois_eu_summer_array[idx]
  print(id)
}

gpp_anom_extreme_postemp <- gpp_anom_extreme

df_month_mean_temppos_negsmoi <- data.frame(month = 1:4)
df_month_mean_temppos_negsmoi$mean_gpp <- NA
df_month_mean_temppos_negsmoi$mean_temp <- NA
df_month_mean_temppos_negsmoi$mean_soilmoi <- NA


for (m in 1:4) {
  mean_m_gpp <- weighted.mean(gpp_anom_extreme[,,list_m[[m]]], area_cube[,,list_m[[m]]], na.rm = TRUE)
  df_month_mean_temppos_negsmoi[df_month_mean_temppos_negsmoi$month == m, "mean_gpp"] <- mean_m_gpp
  mean_m_temp <- weighted.mean(temp_anom_extreme[,,list_m[[m]]], area_cube[,,list_m[[m]]], na.rm = TRUE)
  df_month_mean_temppos_negsmoi[df_month_mean_temppos_negsmoi$month == m, "mean_temp"] <- mean_m_temp
  mean_m_soilmoi <- weighted.mean(soilmoi_anom_extreme[,,list_m[[m]]], area_cube[,,list_m[[m]]], na.rm = TRUE)
  df_month_mean_temppos_negsmoi[df_month_mean_temppos_negsmoi$month == m, "mean_soilmoi"] <- mean_m_soilmoi
  
}
df_month_mean_temppos_negsmoi["month"][df_month_mean_temppos_negsmoi["month"] == 1:4]  <- 5:8

df_month_mean_temppos_negsmoi$variable <- "temppos_smoineg"

##negative temp anomalie and pos soilmoi
ID_event_neg_tempanom_soilanom <- df_neg_gppanom[df_neg_gppanom$soilmoi_anom >0 & df_neg_gppanom$temp2m_anom <0, 1]

gpp_anom_extreme <- gpp_anom_array
gpp_anom_extreme[] <- NA

temp_anom_extreme <- temp_2m_anom_array
temp_anom_extreme[] <- NA

temp_extreme <- temp_2m_eu_summer_array
temp_extreme[] <- NA

soilmoi_anom_extreme <- s_mois_anom_array
soilmoi_anom_extreme[] <- NA

soilmoi_extreme <- s_mois_eu_summer_array
soilmoi_extreme[] <- NA

for (id in ID_event_neg_tempanom_soilanom) {
  idx <- as.numeric(which(array_eu_summer == id))
  gpp_anom_extreme[idx] <- gpp_anom_array[idx]
  temp_anom_extreme[idx] <- temp_2m_anom_array[idx]
  temp_extreme[idx] <- temp_2m_eu_summer_array[idx]
  soilmoi_anom_extreme[idx] <- s_mois_anom_array[idx]
  soilmoi_extreme[idx] <- s_mois_eu_summer_array[idx]
  print(id)
}

gpp_anom_extreme_negtemp <- gpp_anom_extreme

df_month_mean_tempneg_possmoi <- data.frame(month = 1:4)
df_month_mean_tempneg_possmoi$mean_gpp <- NA
df_month_mean_tempneg_possmoi$mean_temp <- NA
df_month_mean_tempneg_possmoi$mean_soilmoi <- NA


for (m in 1:4) {
  mean_m_gpp <- weighted.mean(gpp_anom_extreme[,,list_m[[m]]], area_cube[,,list_m[[m]]], na.rm = TRUE)
  df_month_mean_tempneg_possmoi[df_month_mean_tempneg_possmoi$month == m, "mean_gpp"] <- mean_m_gpp
  mean_m_temp <- weighted.mean(temp_anom_extreme[,,list_m[[m]]], area_cube[,,list_m[[m]]], na.rm = TRUE)
  df_month_mean_tempneg_possmoi[df_month_mean_tempneg_possmoi$month == m, "mean_temp"] <- mean_m_temp
  mean_m_soilmoi <- weighted.mean(soilmoi_anom_extreme[,,list_m[[m]]], area_cube[,,list_m[[m]]], na.rm = TRUE)
  df_month_mean_tempneg_possmoi[df_month_mean_tempneg_possmoi$month == m, "mean_soilmoi"] <- mean_m_soilmoi
  
}
df_month_mean_tempneg_possmoi["month"][df_month_mean_tempneg_possmoi["month"] == 1:4]  <- 5:8


df_month_mean_tempneg_possmoi$variable <- "tempneg_smoipos"



df_plot <- rbind(df_month_mean[,c(1:2, 5)], df_month_mean_tempneg_possmoi[,c(1:2, 5)], 
                 df_month_mean_temppos_negsmoi[,c(1:2, 5)])

df_plot_temp <- rbind(df_month_mean[,c(1:3, 5)], df_month_mean_tempneg_possmoi[,c(1:3, 5)], 
                      df_month_mean_temppos_negsmoi[,c(1:3, 5)])

df_plot_soilmoi <- rbind(df_month_mean[,c(1:4, 5)], df_month_mean_tempneg_possmoi[,c(1:4, 5)], 
                         df_month_mean_temppos_negsmoi[,c(1:4, 5)])


gpp_plot <- ggplot()  +
  geom_bar(data = subset(df_plot, variable != "all"), aes( x= month, y = mean_gpp, fill = variable),
           position= position_dodge(width = 0.5), stat = 'identity', width = 0.4) +
  geom_line(data = subset(df_plot, variable == "all"), aes(x = month, y = mean_gpp, col = variable)) +
  scale_color_manual(values = "black", name = "Variable") +
  labs(x = "Month", y = "averaged GPP anomaly") +
  theme(legend.title=element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_fill_manual(values = c("steelblue", 
                               "red"))



temp_plot <- ggplot()  +
  geom_bar(data = subset(df_plot_temp, variable != "all"), aes( x= month, y = mean_temp, fill = variable),
           position= position_dodge(width = 0.5), stat = 'identity', width = 0.4) +
  geom_line(data = subset(df_plot_temp, variable == "all"), aes(x = month, y = mean_temp, col = variable)) +
  scale_color_manual(values = "black", name = "") +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Month", y = "averaged temperature anomaly") +
  theme(legend.title=element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_fill_manual(values = c("steelblue", 
                               "red"))
  

soilmoi_plot <- ggplot()  +
  geom_bar(data = subset(df_plot_soilmoi, variable != "all"), aes( x= month, y = mean_soilmoi, fill = variable),
           position= position_dodge(width = 0.5), stat = 'identity', width = 0.4) +
  geom_line(data = subset(df_plot_soilmoi, variable == "all"), aes(x = month, y = mean_soilmoi, col = variable)) +
  scale_color_manual(values = "black", name = "") +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Month", y = "averaged soil moisture anomaly") +
  theme(legend.title=element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_fill_manual(values = c("steelblue", 
                               "red"))


plot_av_m_va <- ggarrange(gpp_plot, temp_plot, soilmoi_plot, ncol = 2, nrow = 2, 
                          common.legend = TRUE, legend="bottom")


### the area calculation per month of extreme area per case
#therefore at first each of the case variables calculating per month and writing it into the df
# than the next case, step by step

##First following with the extent and followed code after wards going to next variable
#all
area_extreme_raster <- brick(gpp_anom_extreme_all)

##Second
#neg temp pos soilmoi
area_extreme_raster <- brick(gpp_anom_extreme_negtemp)

##Third
#pos temp neg soilmoi
area_extreme_raster <- brick(gpp_anom_extreme_postemp)


extent(area_extreme_raster) <- e



for (m in 1:4) {
  area <- area_extreme_raster[[list_m[[m]]]]
  assign(paste0("area_mean_", m),  mean(area, na.rm = TRUE), globalenv())                              
}
area_mean_5 <- area_mean_1
rm(area_mean_1)
area_mean_6 <- area_mean_2
rm(area_mean_2)
area_mean_7 <- area_mean_3
rm(area_mean_3)
area_mean_8 <- area_mean_4
rm(area_mean_4)

##calculating the area
df_area <- data.frame(month = 1:4)
df_area$all <- NA


df_area$all[1] <- raster_area(area_mean_5)
df_area$all[2] <- raster_area(area_mean_6)
df_area$all[3] <- raster_area(area_mean_7)
df_area$all[4] <- raster_area(area_mean_8)

##second 
#with this area_extreme_raster <- brick(gpp_anom_extreme_negtemp) from above
df_area$negtemppossoi <- NA

df_area$negtemppossoi[1] <- raster_area(area_mean_5)
df_area$negtemppossoi[2] <- raster_area(area_mean_6)
df_area$negtemppossoi[3] <- raster_area(area_mean_7)
df_area$negtemppossoi[4] <- raster_area(area_mean_8)


##Third
#with this area_extreme_raster <- brick(gpp_anom_extreme_postemp) from above
df_area$postemp_negsmoi <- NA
df_area$postemp_negsmoi[1] <- raster_area(area_mean_5)
df_area$postemp_negsmoi[2] <- raster_area(area_mean_6)
df_area$postemp_negsmoi[3] <- raster_area(area_mean_7)
df_area$postemp_negsmoi[4] <- raster_area(area_mean_8)

df_area["month"][df_area["month"] == 1:4]  <- 5:8


df_area <- gather(df_area[,1:4], key = "variable", value = "area", -month)

area_plot <- ggplot()  +
  geom_bar(data = df_area, aes( x= month, y = area, fill = variable),
           position= position_dodge(width = 0.5), stat = 'identity', width = 0.4) +
  scale_color_manual(values = "black", name = "Variable") +
  labs(x = "Month", y = "Area [ha^2]") +
  theme(legend.title=element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_fill_manual(values = c("black", "steelblue", 
                               "red"))




####Raster average per month to be able to identify spatial patterns
#here only for all 55 extreme events

gpp_anom_extreme_raster <- brick(gpp_anom_extreme_all)
extent(gpp_anom_extreme_raster) <- e
mean_gpp_anom_extreme_raster <- mean(gpp_anom_extreme_raster, na.rm = TRUE)
extent(mean_gpp_anom_extreme_raster) <- e

##calculating the varibale per month

for (m in 1:4) {
  anom_gpp <- gpp_anom_extreme_raster[[list_m[[m]]]]
  assign(paste0("anom_gpp_mean_", m),  mean(anom_gpp, na.rm = TRUE), globalenv())                              
}


##rename raster to the right month number
anom_gpp_mean_5 <- anom_gpp_mean_1
rm(anom_gpp_mean_1)
anom_gpp_mean_6 <- anom_gpp_mean_2
rm(anom_gpp_mean_2)
anom_gpp_mean_7 <- anom_gpp_mean_3
rm(anom_gpp_mean_3)
anom_gpp_mean_8 <- anom_gpp_mean_4
rm(anom_gpp_mean_4)

##raster to dataframe to be able to project the data on the map
df_raster_gpp_5 <- as.data.frame(anom_gpp_mean_5, xy = TRUE)  %>% drop_na()
df_raster_gpp_6 <- as.data.frame(anom_gpp_mean_6, xy = TRUE)  %>% drop_na()
df_raster_gpp_7 <- as.data.frame(anom_gpp_mean_7, xy = TRUE)  %>% drop_na()
df_raster_gpp_8 <- as.data.frame(anom_gpp_mean_8, xy = TRUE)  %>% drop_na()
df_raster_gpp <- as.data.frame(mean_gpp_anom_extreme_raster, xy= TRUE)  %>% drop_na()


ggplot(data = world) +
  geom_sf(col = "darkgray") +
  geom_tile(df_raster_gpp, mapping = aes(x = x, y= y, fill= layer), alpha = 0.5)+
  coord_sf(xlim = c(-10, 35), ylim = c(35, 70), expand = FALSE) +
  scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, name = "GPP anom") +
  theme(panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  labs(y = "", x = "")


##temp anomalies

temp_anom_extreme_raster <- brick(temp_anom_extreme_all)
extent(temp_anom_extreme_raster) <- e
mean_temp_anom_extreme_raster <- mean(temp_anom_extreme_raster, na.rm = TRUE)

#calculating the varibale per month
for (m in 1:4) {
  anom_temp <- temp_anom_extreme_raster[[list_m[[m]]]]
  assign(paste0("anom_temp_mean_", m),  mean(anom_temp, na.rm = TRUE), globalenv())                              
}

##rename raster to the right month number
anom_temp_mean_5 <- anom_temp_mean_1
rm(anom_temp_mean_1)
anom_temp_mean_6 <- anom_temp_mean_2
rm(anom_temp_mean_2)
anom_temp_mean_7 <- anom_temp_mean_3
rm(anom_temp_mean_3)
anom_temp_mean_8 <- anom_temp_mean_4
rm(anom_temp_mean_4)

##raster to dataframe to be able to project the data on the map
df_raster_tempanom_5 <- as.data.frame(anom_temp_mean_5, xy = TRUE)  %>% drop_na()
df_raster_tempanom_6 <- as.data.frame(anom_temp_mean_6, xy = TRUE)  %>% drop_na()
df_raster_tempanom_7 <- as.data.frame(anom_temp_mean_7, xy = TRUE)  %>% drop_na()
df_raster_tempanom_8 <- as.data.frame(anom_temp_mean_8, xy = TRUE)  %>% drop_na()
df_raster_tempanom <- as.data.frame(mean_temp_anom_extreme_raster, xy= TRUE)  %>% drop_na()

ggplot(data = world) +
  geom_sf(col = "darkgray") +
  geom_tile(df_raster_tempanom_5, mapping = aes(x = x, y= y, fill=layer), alpha = 0.5)+
  coord_sf(xlim = c(-10, 35), ylim = c(35, 70), expand = FALSE) +
  scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0) +
  theme(panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  labs(y = "", x = "")

##act temp
temp_extreme_raster <- brick(temp_extreme_all)
extent(temp_extreme_raster) <- e
temp_extreme_raster <- kelvin.to.celsius(temp_extreme_raster)
mean_temp_extreme_raster <- mean(temp_extreme_raster, na.rm = TRUE)

#calculating the variable per month
for (m in 1:4) {
  act_temp <- temp_extreme_raster[[list_m[[m]]]]
  assign(paste0("act_temp_mean_", m),  mean(act_temp, na.rm = TRUE), globalenv())                              
}

##rename raster to the right month number
act_temp_mean_5 <- act_temp_mean_1
rm(act_temp_mean_1)
act_temp_mean_6 <- act_temp_mean_2
rm(act_temp_mean_2)
act_temp_mean_7 <- act_temp_mean_3
rm(act_temp_mean_3)
act_temp_mean_8 <- act_temp_mean_4
rm(act_temp_mean_4)

##raster to dataframe to be able to project the data on the map
df_raster_temp_5 <- as.data.frame(act_temp_mean_5, xy = TRUE)  %>% drop_na()
df_raster_temp_6 <- as.data.frame(act_temp_mean_6, xy = TRUE)  %>% drop_na()
df_raster_temp_7 <- as.data.frame(act_temp_mean_7, xy = TRUE)  %>% drop_na()
df_raster_temp_8 <- as.data.frame(act_temp_mean_8, xy = TRUE)  %>% drop_na()
df_raster_temp <- as.data.frame(mean_temp_extreme_raster, xy= TRUE)  %>% drop_na()


cols <- rev(rainbow(7)[-7])
ggplot(data = world) +
  geom_sf(col = "darkgray") +
  geom_tile(df_raster_temp, mapping = aes(x = x, y= y, fill=layer), alpha = 0.5)+
  coord_sf(xlim = c(-10, 35), ylim = c(35, 70), expand = FALSE) +
  scale_fill_gradientn(colours = cols) +
  theme(panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  labs(y = "", x = "")




#anom soil moi
soilmoi_anom_extreme_raster <- brick(soilmoi_anom_extreme_all )
extent(soilmoi_anom_extreme_raster) <- e
mean_soilmoi_extreme_raster <- mean(soilmoi_anom_extreme_raster, na.rm = TRUE)

##calculating the varibale per month
for (m in 1:4) {
  anom_soilmoi <- soilmoi_anom_extreme_raster[[list_m[[m]]]]
  assign(paste0("anom_soilmoi_mean_", m),  mean(anom_soilmoi, na.rm = TRUE), globalenv())                              
}

##rename raster to the right month number
anom_soilmoi_mean_5 <- anom_soilmoi_mean_1
rm(anom_soilmoi_mean_1)
anom_soilmoi_mean_6 <- anom_soilmoi_mean_2
rm(anom_soilmoi_mean_2)
anom_soilmoi_mean_7 <- anom_soilmoi_mean_3
rm(anom_soilmoi_mean_3)
anom_soilmoi_mean_8 <- anom_soilmoi_mean_4
rm(anom_soilmoi_mean_4)

##raster to dataframe to be able to project the data on the map
df_raster_soilmoi_5 <- as.data.frame(anom_soilmoi_mean_5, xy = TRUE)  %>% drop_na()
df_raster_soilmoi_6 <- as.data.frame(anom_soilmoi_mean_6, xy = TRUE)  %>% drop_na()
df_raster_soilmoi_7 <- as.data.frame(anom_soilmoi_mean_7, xy = TRUE)  %>% drop_na()
df_raster_soilmoi_8 <- as.data.frame(anom_soilmoi_mean_8, xy = TRUE)  %>% drop_na()
df_raster_soilmoi <- as.data.frame(mean_soilmoi_extreme_raster, xy= TRUE)  %>% drop_na()



gpp_plot <- ggplot(data = world) +
  geom_sf(col = "darkgray") +
  geom_tile(df_raster_gpp, mapping = aes(x = x, y= y, fill= layer), alpha = 0.5)+
  coord_sf(xlim = c(-10, 35), ylim = c(35, 70), expand = FALSE) +
  scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, name = expression(paste("GPP \nanomaly"~ "[",gCm^-2 ~ d^-1,"]"))) +
  theme(panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  labs(y = "", x = "")


tempanom_plot <-ggplot(data = world) +
  geom_sf(col = "darkgray") +
  geom_tile(df_raster_tempanom, mapping = aes(x = x, y= y, fill=layer), alpha = 0.5)+
  coord_sf(xlim = c(-10, 35), ylim = c(35, 70), expand = FALSE) +
  scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, name = "Temperature \nanomaly [°C]") +
  theme(panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  labs(y = "", x = "")


temp_plot <- ggplot(data = world) +
  geom_sf(col = "darkgray") +
  geom_tile(df_raster_temp, mapping = aes(x = x, y= y, fill=layer), alpha = 0.5)+
  coord_sf(xlim = c(-10, 35), ylim = c(35, 70), expand = FALSE) +
  scale_fill_gradientn(colours = cols, name = "Actual \ntemperature [°C]") +
  theme(panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  labs(y = "", x = "")

soilmoi_plot <- ggplot(data = world) +
  geom_sf(col = "darkgray") +
  geom_tile(df_raster_soilmoi, mapping = aes(x = x, y= y, fill=layer), alpha = 0.5)+
  coord_sf(xlim = c(-10, 35), ylim = c(35, 70), expand = FALSE) +
  scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0, name = "Soil moisture \nanomaly [-]") +
  theme(panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  labs(y = "", x = "")


##per month changing the concerning df in the plots above
month <- grid.arrange(gpp_plot, tempanom_plot, soilmoi_plot, temp_plot, ncol = 2)
#ggsave("maps_anom_var_all.png", plot = month, width = 25, height = 20, units = "cm")
