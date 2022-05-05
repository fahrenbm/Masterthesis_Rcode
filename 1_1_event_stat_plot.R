## Plots for the analysis of the event based statistics
##Based on the Dataframe from 01_df_event_stat "df_neg_gppanom"



##first overview plots
#frequency
fre_pix <- id_eu_extreme %>%
  count(Freq)

qplot(id_eu_extreme$Freq, geom="histogram",
      binwidth = 2) + xlim(1, 100) + ylim(0, 1000) + labs(y= "Number", x = "Frequency of events") 


n_mon_ev <- number_events(df_neg_gppanom)

ggplot() +
  geom_line(data = n_mon_ev, aes(x = month, y = n))

##creating overview plots for events with negative gpp anomalies

events_neg <- data.frame(df_neg_gppanom[,c(2,6)])
events_neg$t_start <- events_neg[order(as.Date(events_neg$t_start, format="%m/%d/%Y")),]
events_neg <- events_neg$t_start
events_neg$events <- 1:55
events_neg$t_start <-  as.Date(events_neg$t_start, format = "%Y_%m_%d")
events_neg$npoints <- as.numeric(events_neg$npoints)

## per area of the events, summed up yearly
events_neg %>%
  ggplot() +
  aes(x = events, y = npoints) +
  geom_line() +
  theme(legend.title=element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


events_neg_y <- events_neg %>%
  group_by(year = year(t_start))%>%
  summarise(y_sum = sum(npoints))



events_neg_y %>%
  ggplot() +
  aes(x = reorder(year, -y_sum), y = y_sum) +
  geom_point() +
  labs(y= "Detected extreme pixels", x = "Year") 


##per extreme events

events_neg_y <- events_neg %>%
  group_by(year = year(t_start))

freq_events_y <- events_neg_y %>%
  count(year)

freq_events_y %>%
  ggplot() +
  aes(x = year, y = n) +
  geom_line() +
  labs(y= "Detected extreme events", x = "Year") 

##integral GPP over the yearly sumed up

df_gpp_int <- df_neg_gppanom[c("GPP_int", "t_start")]
df_gpp_int$t_start <- as.character(df_gpp_int$t_start)
df_gpp_int$GPP_int <- as.numeric(df_gpp_int$GPP_int)
df_gpp_int_y <- df_gpp_int%>%
  group_by(year = year(t_start))%>%
  summarise(sum_gppint = sum(GPP_int, na.rm = TRUE))

df_gpp_int_y %>%
  ggplot() +
  aes(x = reorder(year, sum_gppint), y = sum_gppint) +
  geom_point() +
  theme(legend.title=element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

df_gpp_int_y %>%
  ggplot() +
  aes(x= year, y = sum_gppint) +
  geom_line() +
  theme(legend.title=element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


#density plots
density_df <- df_neg_gppanom[,c("temp2m_anom")]
density_df <- df_neg_gppanom[,c("GPP_anom")]
density_df <- df_neg_gppanom[,c("soilmoi_anom")]


density_df <- melt(density_df)
ggplot(density_df, aes(x=value)) + 
  geom_density(alpha = 0.25) +
  labs(x = "GPP anomalies", y = "Density")


temp_dens <- ggplot(density_df, aes(x=value)) + 
  geom_density(alpha = 0.25) +
  labs(x = "2m temperature anomalies", y = "Density")


grid.arrange(gpp_dens, soilmoi_dens, temp_dens, ncol = 2)


##correlation

correlations <- df_neg_gppanom[,c("GPP_anom", "temp2m", "soilmoi")]
correlations <- df_neg_gppanom[,c("GPP_anom", "temp2m_anom", "soilmoi_anom")]


cor(as.matrix(correlations))
ggpairs(correlations)

##overview of where are the events locally
#most of them are located in east europe and scandivan region


ggplot(data = world) +
  geom_sf() +
  geom_point(data = df_neg_gppanom, aes(x = centroid_lon, y= centroid_lat, color = GPP_anom), alpha=0.4) +
  coord_sf(xlim = c(-10, 40), ylim = c(35, 74), expand = FALSE) +
  scale_color_viridis() +
  labs(y = "", x = "") +
  theme(legend.title=element_blank(),
        panel.background = element_blank())






##plots temp2m vs soilmoi and colour different varibales


plot <- df_neg_gppanom%>%
  ggplot(aes(y = temp2m, x = soilmoi, colour = GPP_anom, size = desc(GPP_int)))+
  geom_point() +
  scale_colour_gradient2(low = "red", mid = "lightgreen", high = "white", 
                         midpoint = 0, name = "averaged GPP anomaly") +
  theme(legend.title = element_text(), 
        legend.text = element_text(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  labs(y = "Temperature during Event [°C]", x = "Soil moisture during Event",
       size = "GPP integer") 




df_neg_gppanom%>%
  ggplot(aes(y = temp2m, x = soilmoi, colour = centroid_lat))+
  geom_point() +
  scale_color_gradientn(colors = rainbow(3), name = "Lat") +
  theme(legend.title = element_text(size = 8), 
        legend.text = element_text(size= 8),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))


df_neg_gppanom%>%
  ggplot(aes(y = temp2m, x = soilmoi, col = factor(month(t_start))))+
  geom_point() +
  scale_color_discrete(name = "Month") +
  theme(legend.title = element_text(size = 8), 
        legend.text = element_text(size= 8),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))



df_neg_gppanom%>%
  ggplot(aes(y = temp2m, x = soilmoi, colour = t_length))+
  geom_point() +
  scale_color_gradientn(colors = brewer.pal(n = 7, name = "BuPu"), 
                        limits = c(0,130), breaks = c(10,25,50,75,100), name = "Time") +
  theme(legend.title = element_text(size = 8), 
        legend.text = element_text(size= 7),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))



ggarrange(plot_t_soi_1, plot_t_soi_2, plot_t_soi_3, 
          plot_t_soi_4,plot_t_soi_5, plot_t_soi_6, heights = c(2,2,2,2,2,2),
          widths = c(2,2,2,2,2,2),ncol = 2, nrow= 3)

#ggexport(plot_t_soi_1, plot_t_soi_2, plot_t_soi_3, 
#         plot_t_soi_4,plot_t_soi_5, plot_t_soi_6, ncol = 2, nrow= 3, filename = "test_3.pdf")




##all anom 

plot <- df_neg_gppanom%>%
  ggplot(aes(y = temp2m_anom, x = soilmoi_anom, colour = GPP_anom, size = desc(GPP_int)))+
  geom_point() +
  scale_colour_gradient2(low = "red", mid = "lightgreen", high = "white", 
                         midpoint = 0, name = "averaged GPP anomaly") +
  theme(legend.title = element_text(), 
        legend.text = element_text(),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  labs(y = "Temperature anomaly during Event [°C]", x = "Soil moisture anomaly during Event",
       size = "GPP integer") 



plot <- df_neg_gppanom%>%
  ggplot(aes(y = temp2m_anom, x = soilmoi_anom, colour = centroid_lat, size= desc(GPP_anom)))+
  geom_point() +
  scale_color_gradientn(colors = rainbow(3), name = "Lat") +
  theme(legend.title = element_text(size = 5), 
        legend.text = element_text(size= 5),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))
ggsave("temp_soilmoi_anom_lat.png", plot = plot, width = 30, height = 20, units = "cm")


df_neg_gppanom%>%
  ggplot(aes(y = temp2m_anom, x = soilmoi_anom, col = factor(month(t_start))))+
  geom_point() +
  scale_color_discrete(name = "Month") +
  theme(legend.title = element_text(size = 8), 
        legend.text = element_text(size= 8),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))


df_neg_gppanom%>%
  ggplot(aes(x = soilmoi_anom, y = temp2m_anom, col = GPP_int))+
  geom_point() +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", 
                        midpoint = 0, name = "GPP int") +
  theme(legend.title = element_text(size = 8), 
        legend.text = element_text(size= 8),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))


df_neg_gppanom%>%
  ggplot(aes(y = temp2m_anom, x = GPP_anom, col = GPP_int))+
  geom_point() +
  scale_color_continuous() +
  theme(legend.title = element_text(size = 8), 
        legend.text = element_text(size= 8),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))



df_neg_gppanom%>%
  ggplot(aes(y = soilmoi_anom, x = GPP_anom, col = temp2m_anom))+
  geom_point() +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", 
                        midpoint = 0, name = "Temp anom") +
  theme(legend.title = element_text(size = 8), 
        legend.text = element_text(size= 8),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))





df_neg_gppanom%>%
  ggplot(aes(y = area, x = GPP_anom))+
  geom_point() +
  scale_color_gradientn(colors = rainbow(3), name = "Lat") +
  theme(legend.title = element_text(size = 5), 
        legend.text = element_text(size= 5),
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))
