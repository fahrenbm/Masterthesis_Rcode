##Here I did the final graphs which I included in the thesis 


##dataframe with the 10 highest negative averaged GPP anomaly

#just the colums I need for the analysis
df <- df_neg_gppanom[,c(1, 3:8, 11:12, 15, 16:21)]

##just the ten highest negative averaged GPP anomaly
df <- df %>%
  arrange(GPP_anom)
df <- df[c(1:10),]

#write.csv(df,"C:/Users/moniq/Desktop/Uni/Master/Masterarbeit/Data/extreme_events.csv", row.names = FALSE)

##area development
#here extreme_raster_eu_summer have to be the raster data before cutting the event from script 0
inputimage <- extreme_raster_eu_summer[[106:116]]

tmpfilter <- inputimage == 419428

sum_g_event <- cellStats(tmpfilter, "sum")

date <- zax_summer[106:116]

df_max_a <- as.data.frame(sum_g_event)
df_max_a$date <- date
df_max_a$date <-  as.Date(df_max_a$date, format = "%Y_%m_%d")


p_area <-  df_max_a%>%
  ggplot() + 
  aes(x = date, y = sum_g_event) +
  geom_line() +
  geom_point(aes(x = as.Date("2010-06-14"),y = 40), size = 3, colour = "Red") +
  theme(panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  labs(y = expression(paste("Area" ~ "[",ha^2,"]")), x= "Time")



##basic stat

#integera summed up over the year and reordered reffering to the strength
df_gpp_int <- df_neg_gppanom[c("GPP_int", "t_start")]
df_gpp_int$t_start <- as.character(df_gpp_int$t_start)
df_gpp_int$GPP_int <- as.numeric(df_gpp_int$GPP_int)
df_gpp_int_y <- df_gpp_int%>%
  group_by(year = year(t_start))%>%
  summarise(sum_gppint = sum(GPP_int, na.rm = TRUE))

plot <- df_gpp_int_y %>%
  ggplot() +
  aes(x = reorder(year, sum_gppint), y = sum_gppint/100000) +
  geom_point() +
  theme(panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  labs(y = expression(paste("Total GPP integral"~ "[",10^6 ~ gC,"]")), x= "Year")


#Overview map of location of the vents, with year and GPP results

set.seed(66)
plot <- ggplot(data = world) +
  geom_sf(col = "darkgray") +
  geom_point(data = df_neg_gppanom, aes(x = centroid_lon , y= centroid_lat, size = desc(GPP_int), col = GPP_anom)) +
  geom_text_repel(data = df_neg_gppanom,aes(x = centroid_lon, y = centroid_lat, label = factor(year(t_start))), 
                  size = 2, color = "black", fontface = "bold", 
                  max.overlaps = 70) +
  coord_sf(xlim = c(-10, 40), ylim = c(35, 74), expand = TRUE) +
  labs(y = "", x = "") +
  scale_colour_gradient2(low = "red", mid = "lightgreen", high = "white", 
                         midpoint = 0, name =expression(paste("GPP anomaly"~ "[",gCm^-2 ~ d^-1,"]"))) +
  theme(panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  labs(size = "GPP integral")

ggsave("map_gpp_year.png", plot = plot, width = 30, height = 20, units = "cm")


#var vs var script 01 and 01_01

p <- df_neg_gppanom%>%
  ggplot(aes(y = temp2m, x = soilmoi, colour = GPP_anom, size = desc(GPP_int/1000000)))+
  geom_point() +
  scale_colour_gradient2(low = "red", mid = "lightgreen", high = "white", 
                         midpoint = 0, name = expression(paste("GPP anomaly"~ "[",gCm^-2 ~ d^-1,"]"))) +
  theme(legend.title = element_text(), 
        legend.text = element_text(),
        legend.position = "left",
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  labs(y = "Temperature during Event [°C]", x = "Soil moisture during Event [-]",
       size = expression(paste("GPP integral"~ "[",10^6 ~ gC,"]")))


p_act <- ggMarginal(p, type = "density")


p_1 <- df_neg_gppanom%>%
  ggplot(aes(y = temp2m_anom, x = soilmoi_anom, colour = GPP_anom, size = desc(GPP_int/1000000)))+
  geom_point() +
  scale_colour_gradient2(low = "red", mid = "lightgreen", high = "white", 
                         midpoint = 0, name = expression(paste("GPP anomaly"~ "[",gCm^-2 ~ d^-1,"]"))) +
  theme(legend.title = element_text(), 
        legend.text = element_text(),
        legend.position = "left",
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  labs(y = "Temperature anomaly during Event [°C]", x = "Soil moisture anomaly during Event [-]",
       size = expression(paste("GPP integral"~ "[",10^6 ~ gC,"]")))

p_anom <- ggMarginal(p_1, type = "density")




plot <- df_neg_gppanom%>%
  ggplot(aes(y = soilmoi_anom, x = GPP_anom, col = temp2m_anom))+
  geom_point() +
  scale_color_gradient2(low = "blue", mid = "gray", high = "red", 
                        midpoint = 0, name = "Temperature [°C] \nanomaly") +
  theme(legend.title = element_text(size = 8), 
        legend.text = element_text(size= 8),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  labs(x =expression(paste("GPP anomaly"~ "[",gCm^-2 ~ d^-1,"]")), 
y = "Soil moisture anomaly during Event [-]") 



##tendency script 03

gpp_plot <- ggplot()  +
  geom_bar(data = subset(df_plot, variable != "all"), aes( x= month, y = mean_gpp, fill = variable),
           position= position_dodge(width = 0.5), stat = 'identity', width = 0.4) +
  geom_line(data = subset(df_plot, variable == "all"), aes(x = month, y = mean_gpp, col = variable)) +
  scale_color_manual(values = "black", name = "Extreme Events", labels = "All") +
  labs(x = "Month", y = expression(paste("GPP anomaly"~ "[",gCm^-2 ~ d^-1,"]"))) +
  theme(legend.title= element_text(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_fill_manual(values = c("steelblue", 
                               "red"), name = "",labels = c("neg. temperature & \npos. soil moisture anomalies", 
                                                  "pos. temperature & \nneg. soil moisture anomalies"))



temp_plot <- ggplot()  +
  geom_bar(data = subset(df_plot_temp, variable != "all"), aes( x= month, y = mean_temp, fill = variable),
           position= position_dodge(width = 0.5), stat = 'identity', width = 0.4) +
  geom_line(data = subset(df_plot_temp, variable == "all"), aes(x = month, y = mean_temp, col = variable)) +
  scale_color_manual(values = "black", name = "Extreme Events", labels = "All") +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Month", y = "Temperature anomaly [°C]") +
  theme(legend.title=element_text(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_fill_manual(values = c("steelblue", 
                               "red"), name = "",labels = c("neg. temperature & \npos. soil moisture anomalies", 
                                                                          "pos. temperature & \nneg. soil moisture anomalies"))


soilmoi_plot <- ggplot()  +
  geom_bar(data = subset(df_plot_soilmoi, variable != "all"), aes( x= month, y = mean_soilmoi, fill = variable),
           position= position_dodge(width = 0.5), stat = 'identity', width = 0.4) +
  geom_line(data = subset(df_plot_soilmoi, variable == "all"), aes(x = month, y = mean_soilmoi, col = variable)) +
  scale_color_manual(values = "black", name = "Extreme Events", labels = "All") +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Month", y = "Soil moisture anomaly [-]") +
  theme(legend.title=element_text(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_fill_manual(values = c("steelblue", 
                               "red"), name = "",labels = c("neg. temperature & \npos. soil moisture anomalies", 
                                                                          "pos. temperature & \nneg. soil moisture anomalies"))


area_plot <- ggplot()  +
  geom_bar(data = df_area, aes( x= month, y = area, fill = variable),
           position= position_dodge(width = 0.5), stat = 'identity', width = 0.4) +
  scale_color_manual(values = "black", name = "Variable") +
  labs(x = "Month", y = expression(paste("Area" ~ "[",ha^2,"]"))) +
  theme(legend.title=element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_fill_manual(values = c("black", "steelblue", 
                               "red"), labels = c("All", "neg. temperature & \npos. soil moisture anomalies", 
                    "pos. temperature & \nneg. soil moisture anomalies"))


plot_av_m_va <- ggarrange(gpp_plot, temp_plot, soilmoi_plot, area_plot, ncol = 2, nrow = 2, 
                          common.legend = TRUE, legend="bottom")


##plots of the other approach here you need the scripts from the other approach 04 and 04_1

#df_gppanom_min <- readRDS("df_gppanom_min")


set.seed(66)
anom_plot <- ggplot(data = world) +
  geom_sf(col = "darkgray") +
  geom_point(data = df_gppanom_min, aes(x = centroid_lon , y= centroid_lat, col = gpp_anom)) +
  geom_text_repel(data = df_gppanom_min,aes(x = centroid_lon, y = centroid_lat, label = temp2mC_anom), 
                  size = 2, color = "red", fontface = "bold", 
                  max.overlaps = 70) +
  coord_sf(xlim = c(-10, 40), ylim = c(35, 74), expand = TRUE) +
  labs(y = "", x = "") +
  scale_colour_gradient2(low = "red", mid = "green", high = "green", 
                         name =expression(paste("GPP anomaly"~ "[",gCm^-2 ~ d^-1,"]"))) +
  theme(panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))



temp_neg_gpp <- ggplot(data = world) +
  geom_sf(col = "darkgray") +
  geom_point(data = df_gppanom_min, aes(x = centroid_lon , y= centroid_lat, col = factor(month(time))), alpha=0.5) +
  geom_text_repel(data = df_gppanom_min,aes(x = centroid_lon, y = centroid_lat, label = temp2mC), 
                  size = 2, color = "red", fontface = "bold", 
                  max.overlaps = 70) +
  coord_sf(xlim = c(-10, 40), ylim = c(35, 74), expand = TRUE) +
  labs(y = "", x = "") +
  scale_color_discrete(name = "Month") +
  theme(panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))



min_anom <- df_gppanom_min%>%
  ggplot(aes(y = temp2mC_anom, x = gpp_anom, colour = gpp_anom))+
  geom_point() +
  scale_colour_gradient2(low = "red", mid = "green", high = "green", 
                         midpoint = 0, name = expression(paste("GPP anomaly"~ "[",gCm^-2 ~ d^-1,"]"))) +
  theme(legend.title = element_text(), 
        legend.text = element_text(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  labs(y = "Temperature anomaly [°C]", x = expression(paste("GPP anomaly"~ "[",gCm^-2 ~ d^-1,"]"))) 
