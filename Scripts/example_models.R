library(dplyr)
library(mgcv)
library(dismo)

# reading in data
load('./Data/attacks_envs_final.Rdata')
load('./Data/wanted_var_df.Rdata')
Attacks_df <- read.csv('./Data/shark_attack_df.csv')

# variable covariance
names(attacks_envs_final)
for_cov <- attacks_envs_final[,2:5]

cov_table <- cov(for_cov)
View(cov_table)

cor_table <- cor(for_cov)
View(cor_table)

# none of the four example variables are very strongly correlated

# variable and Presence Scatter plots
xlabs <- c("Average Cloudiness (%)", "Average Bouy Temp (°C)",
           "Average Chlorophyll", "Average Wind Speed (m/s)")
pdf('./Figures/example_cor_plots.pdf')
for (i in 1:length(xlabs)) {
  plot(for_cov[,i], attacks_envs_final$Presence, type = 'p', 
       xlab = xlabs[i], ylab = "Attacks")
}
dev.off()

# y variable is a factor, so some kind of logistic regression is necessary,
# but I'm going to try out a few things just cause

# Standardizing each variable so they're on the same scale
standard_vals <- matrix(nrow = 3016, ncol = 4)
for (i in 1:length(xlabs)) {
  standard_vals[,i] <- for_cov[,i]/max(for_cov[,i])
}

standard_vals <- data.frame(standard_vals)
colnames(standard_vals) <- c("Cloudiness", "Bouy_Temp",
                             "Chlorophyll", "Wind_Speed")
Presence <- attacks_envs_final[,1]
standard_vals <- cbind(Presence, standard_vals)

# Simple linear regression
attacks_lm <- glm(Presence ~ Cloudiness + Bouy_Temp + 
                   Chlorophyll + Wind_Speed, data = standard_vals)
summary(attacks_lm)
plot(attacks_lm)

AIC(attacks_lm)

# Logistic regression
attacks_log <- glm(Presence ~., family = binomial(link = 'logit'),
                   data = standard_vals)
summary(attacks_log)
plot(attacks_log)

AIC(attacks_log)

# Logistic gam
attacks_gam <- gam(Presence ~ s(Cloudiness, k = 71) + 
                     s(Bouy_Temp, k = 71) +
                     s(Chlorophyll, k = 71) + s(Wind_Speed, k = 71), 
                   family = binomial(link = "logit"), data = standard_vals)

summary(attacks_gam)
gam.check(attacks_gam)
AIC(attacks_gam)

# logistic gamm with non significant variables removed
attacks_gam <- gam(Presence ~  s(Bouy_Temp) + 
                     s(Wind_Speed), 
                   family = binomial(link = "logit"), data = standard_vals)

summary(attacks_gam)
gam.check(attacks_gam)
AIC(attacks_gam)

# maxent
needed_x <- standard_vals[,2:5]
needed_p <- standard_vals[,1]
attacks_maxent <- maxent(x = needed_x, p = needed_p)
