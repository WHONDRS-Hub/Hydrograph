require(ggplot2)
require(scales)

setwd('C:\\Users\\mcgr323\\projects\\Hydrograph')
discharge_data <- read.csv("S191S_daily_dischrg_data.csv")
pressure_data <- read.csv("S191S_daily_gauge_ht.csv")

#make sure classes are correct
discharge_data$datetime <- as.POSIXct(discharge_data$datetime, format = "%Y-%m-%d", tz = "UTC")
class(discharge_data$datetime) #should be "POSIXct" "POSIXt"

# list of values to loop over
uniq_sites= unique(discharge_data$site)

#set limits for the graphs
lims <- as.POSIXct(strptime(c("2018-06-01 00:00","2019-10-31 00:00"), format = "%Y-%m-%d %H:%M")) 

setwd('C:\\Users\\mcgr323\\projects\\Hydrograph\\Graphs\\Discharge')
# Loop
for (i in uniq_sites) {
temp_plot = ggplot(data= subset(discharge_data, site == i))+
            geom_line(aes(x = datetime, y = mean_daily_discharge), 
                        color = "#09557f",
                        alpha = 0.6,
                        size = 0.9) +
            labs(x = "Date", y = "Daily Mean Discharge (cms)")+
            scale_x_datetime(date_labels = "%b-%y", limits = lims,breaks=date_breaks("1 month"))+
            theme_minimal() +
            theme(axis.text.x=element_text(angle=45))+
            ggtitle(paste0("WHONDRS S19S Site:", i))
ggsave(temp_plot, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
}

#make sure classes are correct
pressure_data$datetime <- as.POSIXct(pressure_data$datetime, format = "%Y-%m-%d", tz = "UTC")
class(pressure_data$datetime) #should be "POSIXct" "POSIXt"

# list of values to loop over
uniq_sites= unique(pressure_data$site)

#set limits for the graphs
lims <- as.POSIXct(strptime(c("2018-06-01 00:00","2019-10-31 00:00"), format = "%Y-%m-%d %H:%M")) 

setwd('C:\\Users\\mcgr323\\projects\\Hydrograph\\Graphs\\Gauge_Ht')
# Loop
for (i in uniq_sites) {
  temp_plot = ggplot(data= subset(pressure_data, site == i))+
    geom_line(aes(x = datetime, y = mean_gauge_ht), 
              color = "tomato2",
              alpha = 0.6,
              size = 0.9) +
    labs(x = "Date", y = "Daily Mean Gauge Ht (m)")+
    scale_x_datetime(date_labels = "%b-%y", limits = lims,breaks=date_breaks("1 month"))+
    theme_minimal() +
    theme(axis.text.x=element_text(angle=45))+
    ggtitle(paste0("WHONDRS S19S Site:", i))
  ggsave(temp_plot, file=paste0("plot_", i,".png"), width = 14, height = 10, units = "cm")
}
