library(dplyr)
library(ggplot2)

# creating through time plots
attacks_df <- read.csv('./Data/shark_attack_df.csv')
names(attacks_df)
head(attacks_df$DATE)
tail(attacks_df$DATE)
dim(attacks_df)
attacks_sum <- vector(mode = "numeric", length = dim(attacks_df)[1])
for ( i in 1:dim(attacks_df)[1]) {
  if (is.na(attacks_df$ISAF.4th.case_no[i]) == F) {
    attacks_sum[i] <- 4
  } else if (is.na(attacks_df$X3rd.case_no[i]) == F) {
    attacks_sum[i] <- 3
  } else if (is.na(attacks_df$ISAF.2nd.case_no[i]) == F) {
    attacks_sum[i] <- 2
  } else if (is.na(attacks_df$ISAF.case_no[i]) == F) {
    attacks_sum[i] <- 1
  } else {
    attacks_sum[i] <- 0
  }
}

attacks_df <- cbind(attacks_df, attacks_sum)
attacks_df$DATE <- as.Date(attacks_df$DATE,format = "%m/%d/%Y")
#attacks_df$Year <- as.Date(attacks_df$Year, format = "%Y", origin = "1990")

# attacks by year bar plot
pdf('./Figures/attack_frequency_barplot.pdf')
p <- ggplot(data = attacks_df, aes(x = Year, y = attacks_sum)) +
  geom_bar(stat = "identity", fill = "lightseagreen") +
  ylab("Number of Attacks") +
  theme_minimal()
p
dev.off()

# creating time series plots
wanted_var_df <- subset(attacks_df, select = -c(1, 3:35, 39, 51:54, 59:74, 
                                                81, 87, 94, 97, 100, 103, 
                                                106, 107, 121))

save(wanted_var_df, file = './Data/wanted_var_df.Rdata')

for (i in 1:dim(wanted_var_df)[2]) {
  wanted_var_df[,i] <- as.numeric(wanted_var_df[,i])
}

wanted_var_list <- names(wanted_var_df)
nice_names <- c("Average Cloudiness from Midnight to Midnight (%)", 
                "Average Cloudiness from Sunrise to Sunset (%)",
                "Average Daily Wind Speed (dm/sec)",
                "Time of Fastest 1 Minute Wind",
                "Multiday Precipitation Total (tenth of mm)",
                "Peak Gust Time",
                "Precipitation (tenth of mm)",
                "Snow (mm)",
                "Snow Depth (mm)",
                "Average Temperature (tenth of °C)",
                "Maximum Temperature (tenth of °C)",
                "Minimum Temperature (tenth of °C)",
                "Temperature at time of Observation (tenth of °C)",
                "Daily Total Sunshine (min)",
                "Fastest 1 Minute Wind Speed (dm/sec)",
                "Fastest 2 Minute Wind Speed (dm/sec)",
                "Fastest 5 Second Wind Speed (dm/sec)",
                "Peak Gust Wind Speed (dm/sec)",
                "Bouy 1 Wind Speed (m/sec)",
              "Bouy 1 Peak 5-8 sec gust speed (m/s) within 2-8 min period",
              "Bouy 1 Significant Wave Height (m)",
                "Bouy 1 Sea Level Pressure (hPa)",
                "Bouy 1 Air Temperature (°C)",
                "Bouy 1 Sea Surface Temperature (°C)",
                "Bouy 2 Wind Speed (m/sec)",
              "Bouy 2 Peak 5-8 sec gust speed (m/s) within 2-8 min period",
                "Bouy 2 Sea Level Pressure (hPa)",
                "Bouy 2 Air Temperature (°C)",
                "Bouy 2 Sea Surface Temperature (°C)",
                "Bouy Average Wind Speed (m/sec)",
        "Bouy Average Peak 5-8 sec gust speed (m/s) within 2-8 min period",
                "Bouy Average Air Temperature (°C)",
                "Bouy Average Sea Surface Temperature (°C)",
                "Tide 1: Time (LST/LDT)",
                "Tide 1: Predicted (m)",
                "Tide 2: Time (LST/LDT)",
                "Tide 2: Predicted (m)",
                "Tide 3: Time (LST/LDT)",
                "Tide 3: Predicted (m)",
                "Tide 4: Time (LST/LDT)",
                "Tide 4: Predicted (m)",
                "Tide 5: Time (LST/LDT)",
                "Tide 5: Predicted (m)",
                "Moon Illumination (%)",
                "NSB Chlorophyll a comb",
                "St Augestine Chlorophyll a comb",
                "Melbourne Beach chlorophyll a comb",
                "NSB chlorophyll a",
                "NSB chlor a log10 rmsd",
                "NSB chlor a log10 bias",
                "St Augestine Chlorophyll a",
                "St Augestine chlor a log10 rmsd",
                "St Augestine chlor a log10 bias",
                "Melbourne Chlorophyll a",
                "Melbourne chlor a log10 rmsd",
                "Melbourne chlor a log10 bias")

year_u <- unique(attacks_df$Year)
year_avg <- matrix(nrow = 32, ncol = 57)
for (i in 1:length(Year)) {
t <- wanted_var_df %>% filter(Year == year_u[i])
t1 <- colMeans(t, na.rm = T)
year_avg[i,] <- t1
}

year_avg <- data.frame(year_avg)
colnames(year_avg) <- wanted_var_list
for_iter <- wanted_var_list[2:57]

pdf('./Figures/through_time_plots.pdf')
for (i in 1:length(for_iter)) {
  wanted_var <- eval(parse(text = paste0('year_avg$', 
                                         for_iter[i])))
  p <- ggplot(data = year_avg) +
    geom_line(aes(x = Year, y = wanted_var, group = 1), 
              color = "blue") +
    xlab("Year") + 
    ylab(nice_names[i]) +
    theme_minimal()
  print(p)
}
dev.off()