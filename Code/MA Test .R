library(ggplot2)


# 2021_yk_490

# 2011_nk_34

#2015_yk_380

#2019_yk_070

#2020_kk_022

#2020_kk_070


# YK_490
yk_490<- read.csv("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Data/Processed/Trim_Locations/2021_yk_490_trimLocations.csv") 

# Add a 60 point moving average, a 80 point moving average, and a 40 point moving average 

yk_490$Iso_MA_20 <- rollmean(yk_490$Iso, 20, fill = NA)
yk_490$Iso_MA_60 <- rollmean(yk_490$Iso, 60, fill = NA)
yk_490$Iso_MA_80 <- rollmean(yk_490$Iso, 80, fill = NA)
yk_490$Iso_MA_40 <- rollmean(yk_490$Iso, 40, fill = NA)

df <- data.frame(Microns = yk_490$Microns, Iso = yk_490$Iso)
k <- floor(100 * (nrow(df)^(2/9)))    

# # Adjustable parameters
# k_value <- 200  # Controls complexity (higher = more flexible, lower = smoother)
gamma_value <- .6 # Controls smoothing penalty (lower = more sensitive, higher = smoother)

# Compute GAM-smoothed values
df <- data.frame(Microns = yk_490$Microns, Iso = yk_490$Iso)

model <- gam(Iso ~ s(Microns, bs = "tp", k = k_value), gamma = gamma_value, data = df)
              
yk_490$Iso_GAM <- predict(model, newdata = data.frame(Microns = yk_490$Microns))       


# Plot Iso vs Microns with moving average
ggplot(yk_490, aes(x= Microns, y= Iso)) +
  geom_errorbar(aes(ymin = .7092 - .0005, ymax = .7092 + .0005), color = "orange", alpha = .04, linewidth = 1.4) +
  geom_point(alpha = .2) +
  geom_line(aes(x = Microns, y = Iso_MA_20), color = "#FFCCCC", linewidth = 1.7) +  # Light pink
  geom_line(aes(x = Microns, y = Iso_MA_40), color = "#FF9999", linewidth = 1.7) +  # Light red
  geom_line(aes(x = Microns, y = Iso_MA_60), color = "#CC3333", linewidth = 1.7) +  # Medium red
  geom_line(aes(x = Microns, y = Iso_MA_80), color = "#800000", linewidth = 1.7) +  # Dark red
  geom_line(aes(x = Microns, y = Iso_GAM), color = "blue", linewidth = 1.7) +  # Light pink+ 
  geom_hline(yintercept = .7092, color = "orange", alpha = .5, linewidth = 1.4) +
  # Add an error bar +- .0005
  theme_minimal() +
  labs(title = "2021_yk_490", x = "Microns", y = "Isotopic Value") +
  theme(plot.title = element_text(hjust = 0.5))






# Nk_34
nk_34<- read.csv("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Data/Processed/Trim_Locations/2011_nk_34_trimLocations.csv")



# Add a 60 point moving average, a 80 point moving average, and a 40 point moving average

nk_34$Iso_MA_20 <- rollmean(nk_34$Iso, 20, fill = NA)
nk_34$Iso_MA_60 <- rollmean(nk_34$Iso, 60, fill = NA)
nk_34$Iso_MA_80 <- rollmean(nk_34$Iso, 80, fill = NA)
nk_34$Iso_MA_40 <- rollmean(nk_34$Iso, 40, fill = NA)

df <- data.frame(Microns = nk_34$Microns, Iso = nk_34$Iso)
k <- floor(100 * (nrow(df)^(2/9)))

# # Adjustable parameters
# k_value <- 200  # Controls complexity (higher = more flexible, lower = smoother)
gamma_value <- .6 # Controls smoothing penalty (lower = more sensitive, higher = smoother)

# Compute GAM-smoothed values
df <- data.frame(Microns = nk_34$Microns, Iso = nk_34$Iso)

model <- gam(Iso ~ s(Microns, bs = "tp", k = k_value), gamma = gamma_value, data = df)

nk_34$Iso_GAM <- predict(model, newdata = data.frame(Microns = nk_34$Microns))

# Compute a gam the old way 

k <- floor(15 * (nrow(df)^(2/9)))                  
gamma_value <- 1.4

model <- gam(Iso ~ s(Microns, bs = "tp", k = k), gamma = gamma_value, data = df)
nk_34$Iso_GAM_original <- predict(model, newdata = data.frame(Microns = nk_34$Microns))


# Plot Iso vs Microns with moving average

ggplot(nk_34, aes(x= Microns, y= Iso)) +
  geom_point(alpha = .2) +
  geom_line(aes(x = Microns, y = Iso_MA_20), color = "#FFCCCC", linewidth = 1.7) +  # Light pink
  geom_line(aes(x = Microns, y = Iso_MA_40), color = "#FF9999", linewidth = 1.7) +  # Light red
  geom_line(aes(x = Microns, y = Iso_MA_60), color = "#CC3333", linewidth = 1.7) +  # Medium red
  geom_line(aes(x = Microns, y = Iso_MA_80), color = "#800000", linewidth = 1.7) +  # Dark red
  geom_line(aes(x = Microns, y = Iso_GAM), color = "blue", linewidth = 1.7) +  # blue
  geom_line(aes(x = Microns, y = Iso_GAM_original), color = "dodgerblue", linewidth = 1.7) +  # green
  geom_hline(yintercept = .7091, color = "orange", alpha = .5, linewidth = 1.4) +
  theme_minimal() +
  labs(title = "2011_nk_34", x = "Microns", y = "Isotopic Value") +
  theme(plot.title = element_text(hjust = 0.5))


# filter to between microns 450 and 1200
nk_34_cut <- nk_34[nk_34$Microns > 450 & nk_34$Microns < 1200,]


ggplot(nk_34_cut, aes(x= Microns, y= Iso)) +
  geom_point(alpha = .2) +
  geom_line(aes(x = Microns, y = Iso_MA_20), color = "#FFCCCC", linewidth = 1.7) +  # Light pink
  geom_line(aes(x = Microns, y = Iso_MA_40), color = "#FF9999", linewidth = 1.7) +  # Light red
  geom_line(aes(x = Microns, y = Iso_MA_60), color = "#CC3333", linewidth = 1.7) +  # Medium red
  geom_line(aes(x = Microns, y = Iso_MA_80), color = "#800000", linewidth = 1.7) +  # Dark red
  geom_line(aes(x = Microns, y = Iso_GAM), color = "blue", linewidth = 1.7) +  # blue
  geom_line(aes(x = Microns, y = Iso_GAM_original), color = "dodgerblue", linewidth = 1.7) +  # green
  geom_hline(yintercept = .7091, color = "orange", alpha = .5, linewidth = 1.4) +
  theme_minimal() +
  labs(title = "2011_nk_34", x = "Microns", y = "Isotopic Value") +
  theme(plot.title = element_text(hjust = 0.5))




#Yk_380
yk_380<- read.csv("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Data/Processed/Trim_Locations/2015_yk_380_trimLocations.csv")

# Add a 60 point moving average, a 80 point moving average, and a 40 point moving average
yk_380$Iso_MA_20 <- rollmean(yk_380$Iso, 20, fill = NA)
yk_380$Iso_MA_60 <- rollmean(yk_380$Iso, 60, fill = NA)
yk_380$Iso_MA_80 <- rollmean(yk_380$Iso, 80, fill = NA)
yk_380$Iso_MA_40 <- rollmean(yk_380$Iso, 40, fill = NA)

df <- data.frame(Microns = yk_380$Microns, Iso = yk_380$Iso)
k <- floor(100 * (nrow(df)^(2/9)))

# # Adjustable parameters
# k_value <- 200  # Controls complexity (higher = more flexible, lower = smoother)
gamma_value <- .5 # Controls smoothing penalty (lower = more sensitive, higher = smoother)

# Compute GAM-smoothed values
df <- data.frame(Microns = yk_380$Microns, Iso = yk_380$Iso)

model <- gam(Iso ~ s(Microns, bs = "tp", k = k_value), gamma = gamma_value, data = df)

yk_380$Iso_GAM <- predict(model, newdata = data.frame(Microns = yk_380$Microns))

# Compute a gam the old way

k <- floor(15 * (nrow(df)^(2/9)))
gamma_value <- 1.4

model <- gam(Iso ~ s(Microns, bs = "tp", k = k), gamma = gamma_value, data = df)
yk_380$Iso_GAM_original <- predict(model, newdata = data.frame(Microns = yk_380$Microns))


# Plot Iso vs Microns with moving average

ggplot(yk_380, aes(x= Microns, y= Iso)) +
  geom_point(alpha = .2) +
  geom_line(aes(x = Microns, y = Iso_MA_20), color = "#FFCCCC", linewidth = 1.7) +  # Light pink
  geom_line(aes(x = Microns, y = Iso_MA_40), color = "#FF9999", linewidth = 1.7) +  # Light red
  geom_line(aes(x = Microns, y = Iso_MA_60), color = "#CC3333", linewidth = 1.7) +  # Medium red
  geom_line(aes(x = Microns, y = Iso_MA_80), color = "#800000", linewidth = 1.7) +  # Dark red
  geom_line(aes(x = Microns, y = Iso_GAM), color = "blue", linewidth = 1.7) +  # blue
  geom_line(aes(x = Microns, y = Iso_GAM_original), color = "dodgerblue", linewidth = 1.7) +  # green
  geom_hline(yintercept = .7091, color = "orange", alpha = .5, linewidth = 1.4) +
  theme_minimal() +
  labs(title = "2015_yk_380", x = "Microns", y = "Isotopic Value") +
  theme(plot.title = element_text(hjust = 0.5))


#2019_yK_070

yk_070<- read.csv("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Data/Processed/Trim_Locations/2019_yk_070_trimLocations.csv")

# Add a 60 point moving average, a 80 point moving average, and a 40 point moving average
yk_070$Iso_MA_20 <- rollmean(yk_070$Iso, 20, fill = NA)
yk_070$Iso_MA_60 <- rollmean(yk_070$Iso, 60, fill = NA)
yk_070$Iso_MA_80 <- rollmean(yk_070$Iso, 80, fill = NA)
yk_070$Iso_MA_40 <- rollmean(yk_070$Iso, 40, fill = NA)

df <- data.frame(Microns = yk_070$Microns, Iso = yk_070$Iso)
k <- floor(100 * (nrow(df)^(2/9)))

# # Adjustable parameters
# k_value <- 200  # Controls complexity (higher = more flexible, lower = smoother)
gamma_value <- .5 # Controls smoothing penalty (lower = more sensitive, higher = smoother)

# Compute GAM-smoothed values
df <- data.frame(Microns = yk_070$Microns, Iso = yk_070$Iso)

model <- gam(Iso ~ s(Microns, bs = "tp", k = k), gamma = gamma_value, data = df)

yk_070$Iso_GAM <- predict(model, newdata = data.frame(Microns = yk_070$Microns))

# Compute a gam the old way

k <- floor(15 * (nrow(df)^(2/9)))
gamma_value <- .5

model <- gam(Iso ~ s(Microns, bs = "tp", k = k), gamma = gamma_value, data = df)

yk_070$Iso_GAM_original <- predict(model, newdata = data.frame(Microns = yk_070$Microns))

#

# Plot Iso vs Microns with moving average

ggplot(yk_070, aes(x= Microns, y= Iso)) +
  geom_point(alpha = .2) +
  geom_line(aes(x = Microns, y = Iso_MA_20), color = "#FFCCCC", linewidth = 1.7) +  # Light pink
  geom_line(aes(x = Microns, y = Iso_MA_40), color = "#FF9999", linewidth = 1.7) +  # Light red
  geom_line(aes(x = Microns, y = Iso_MA_60), color = "#CC3333", linewidth = 1.7) +  # Medium red
  geom_line(aes(x = Microns, y = Iso_MA_80), color = "#800000", linewidth = 1.7) +  # Dark red
  geom_line(aes(x = Microns, y = Iso_GAM), color = "blue", linewidth = 1.7) +  # blue
  geom_line(aes(x = Microns, y = Iso_GAM_original), color = "dodgerblue", linewidth = 1.7) +  # green
  geom_hline(yintercept = .7091, color = "orange", alpha = .5, linewidth = 1.4) +
  geom_vline(xintercept = yk_070$marine_start[1], color = "orange", alpha = .5, linewidth = 1.4) +
  theme_minimal() +
  labs(title = "2019_yk_070", x = "Microns", y = "Isotopic Value") +
  theme(plot.title = element_text(hjust = 0.5))


### 2020_kk_070

kk_070<- read.csv("/Users/benjaminmakhlouf/Research_repos/Western_Ak_otolith_stock_discrimination/Data/Processed/Trim_Locations/2020_kk_070_trimLocations.csv")

# Add a 60 point moving average, a 80 point moving average, and a 40 point moving average
kk_070$Iso_MA_20 <- rollmean(kk_070$Iso, 20, fill = NA)
kk_070$Iso_MA_60 <- rollmean(kk_070$Iso, 60, fill = NA)
kk_070$Iso_MA_80 <- rollmean(kk_070$Iso, 80, fill = NA)
kk_070$Iso_MA_40 <- rollmean(kk_070$Iso, 40, fill = NA)

df <- data.frame(Microns = kk_070$Microns, Iso = kk_070$Iso)

k <- floor(100 * (nrow(df)^(2/9)))

# # Adjustable parameters
# k_value <- 200  # Controls complexity (higher = more flexible, lower = smoother)
gamma_value <- .5 # Controls smoothing penalty (lower = more sensitive, higher = smoother)

# Compute GAM-smoothed values
df <- data.frame(Microns = kk_070$Microns, Iso = kk_070$Iso)

model <- gam(Iso ~ s(Microns, bs = "tp", k = k), gamma = gamma_value, data = df)

kk_070$Iso_GAM <- predict(model, newdata = data.frame(Microns = kk_070$Microns))

#cmpute a gam the old way

k <- floor(15 * (nrow(df)^(2/9)))
gamma_value <- .75

model <- gam(Iso ~ s(Microns, bs = "tp", k = k), gamma = gamma_value, data = df)

kk_070$Iso_GAM_original <- predict(model, newdata = data.frame(Microns = kk_070$Microns))

#plot

ggplot(kk_070, aes(x= Microns, y= Iso)) +
  geom_point(alpha = .2) +
  # geom_line(aes(x = Microns, y = Iso_MA_20), color = "#FFCCCC", linewidth = 1.7) +  # Light pink
  # geom_line(aes(x = Microns, y = Iso_MA_40), color = "#FF9999", linewidth = 1.7) +  # Light red
  # geom_line(aes(x = Microns, y = Iso_MA_60), color = "#CC3333", linewidth = 1.7) +  # Medium red
  # geom_line(aes(x = Microns, y = Iso_MA_80), color = "#800000", linewidth = 1.7) +  # Dark red
  # geom_line(aes(x = Microns, y = Iso_GAM), color = "blue", linewidth = 1.7) +  # blue
  geom_line(aes(x = Microns, y = Iso_GAM_original), color = "dodgerblue", linewidth = 1.7) +  # green
  geom_hline(yintercept = .7091, color = "orange", alpha = .5, linewidth = 1.4) +
  geom_vline(xintercept = kk_070$marine_start[1], color = "orange", alpha = .5, linewidth = 1.4) +
  theme_minimal() +
  labs(title = "2020_kk_070", x = "Microns", y = "Isotopic Value") +
  theme(plot.title = element_text(hjust = 0.5))


# Calculate the number of reads per micron on average 
readspermicron<- nrow(kk_070)/kk_070$Microns[nrow(kk_070)]
readspermicron_nk34<- nrow(nk_34)/nk_34$Microns[nrow(nk_34)]
readspermicron_yk_070<- nrow(yk_070)/yk_070$Microns[nrow(yk_070)]

