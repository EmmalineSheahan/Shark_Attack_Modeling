library(dplyr)
library(dismo)

# move attacks into single column
Attacks_df <- read.csv('./Data/shark_attack_df.csv')
attacks_1 <- subset(Attacks_df, select = -c(12:35))
attacks_2 <- subset(Attacks_df, select = -c(4:11, 20:35))
attacks_3 <- subset(Attacks_df, select = -c(4:19, 28:35))
attacks_4 <- subset(Attacks_df, select = -c(4:27))

wanted_names <- names(attacks_1)
colnames(attacks_2) <- wanted_names
colnames(attacks_3) <- wanted_names
colnames(attacks_4) <- wanted_names

attacks_1 <- attacks_1 %>% filter(!(is.na(ISAF.case_no)))
attacks_2 <- attacks_2 %>% filter(!(is.na(ISAF.case_no)))
attacks_3 <- attacks_3 %>% filter(!(is.na(ISAF.case_no)))
attacks_4 <- attacks_4 %>% filter(!(is.na(ISAF.case_no)))

total_attacks <- rbind(attacks_1, attacks_2, attacks_3, attacks_4)

total_attacks$time_of_attack
total_attacks$St.Augestine.chlor_a

total_attacks_envs <- total_attacks %>% 
  dplyr::select(Average.cloudiness.midnight.to.midnight.from.manual.observations..percent.,
         Bouy.2.Sea.surface.temperature..Celsius., 
         Bouy.1.Sea.surface.temperature..Celsius., Bouy.1.Wind.speed..m.s.,
         Bouy.2.Wind.speed..m.s., St.Augestine.chlor_a, Melbourne.chlor_a)

Presence <- rep(1, times = 240)
total_attacks_envs <- cbind(Presence, total_attacks_envs)

for(i in 1:8) {
  total_attacks_envs[,i] <- as.numeric(total_attacks_envs[,i])
}

avg_bouy_temp <- rowMeans(total_attacks_envs[,c('Bouy.2.Sea.surface.temperature..Celsius.',
                                               'Bouy.1.Sea.surface.temperature..Celsius.')], 
                          na.rm = T)
avg_chlor <- rowMeans(total_attacks_envs[,c('St.Augestine.chlor_a',
                                           'Melbourne.chlor_a')],
                      na.rm = T)
avg_wind_speed <- rowMeans(total_attacks_envs[,c("Bouy.1.Wind.speed..m.s.",
                                               "Bouy.2.Wind.speed..m.s.")])

total_attacks_envs <- total_attacks_envs %>% dplyr::select(Presence,
  Average.cloudiness.midnight.to.midnight.from.manual.observations..percent.)

total_attacks_envs <- cbind(total_attacks_envs, avg_bouy_temp, avg_chlor, 
                            avg_wind_speed)
total_attacks_envs$avg_bouy_temp[total_attacks_envs$avg_bouy_temp == 'NaN'] <- NA
total_attacks_envs$avg_chlor[total_attacks_envs$avg_chlor == 'NaN'] <- NA
total_attacks_envs$avg_wind_speed[total_attacks_envs$avg_wind_speed == 'NaN'] <- NA
total_attacks_envs <- na.omit(total_attacks_envs)

# creating absences or zeroes
non_attacks <- Attacks_df %>% filter(is.na(ISAF.case_no))
non_attacks_envs <- non_attacks %>% dplyr::select(Average.cloudiness.midnight.to.midnight.from.manual.observations..percent.,
                                                  Bouy.2.Sea.surface.temperature..Celsius., 
                                                  Bouy.1.Sea.surface.temperature..Celsius., Bouy.1.Wind.speed..m.s.,
                                                  Bouy.2.Wind.speed..m.s., St.Augestine.chlor_a, Melbourne.chlor_a)

for(i in 1:7) {
  non_attacks_envs[,i] <- as.numeric(non_attacks_envs[,i])
}

avg_bouy_temp <- rowMeans(non_attacks_envs[,c('Bouy.2.Sea.surface.temperature..Celsius.',
                                                'Bouy.1.Sea.surface.temperature..Celsius.')], 
                          na.rm = T)
avg_chlor <- rowMeans(non_attacks_envs[,c('St.Augestine.chlor_a',
                                            'Melbourne.chlor_a')],
                      na.rm = T)
avg_wind_speed <- rowMeans(non_attacks_envs[,c("Bouy.1.Wind.speed..m.s.",
                                               "Bouy.2.Wind.speed..m.s.")])

non_attacks_envs <- non_attacks_envs %>% dplyr::select(Average.cloudiness.midnight.to.midnight.from.manual.observations..percent.)

non_attacks_envs <- cbind(non_attacks_envs, avg_bouy_temp, avg_chlor, avg_wind_speed)
non_attacks_envs$avg_bouy_temp[non_attacks_envs$avg_bouy_temp == 'NaN'] <- NA
non_attacks_envs$avg_chlor[non_attacks_envs$avg_chlor == 'NaN'] <- NA
non_attacks_envs$avg_wind_speed[non_attacks_envs$avg_wind_speed == 'NaN'] <- NA
non_attacks_envs <- na.omit(non_attacks_envs)

Presence <- rep(0, times = length(non_attacks_envs$avg_chlor))
non_attacks_envs <- cbind(Presence, non_attacks_envs)
attacks_envs_final <- rbind(total_attacks_envs, non_attacks_envs)

save(attacks_envs_final, file = './Data/attacks_envs_final.Rdata')
