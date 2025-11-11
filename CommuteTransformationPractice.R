# Data description
# 1. date - R's numeric date format
# 2. direction - AM and PM (Going to office and vice versa)
# 3. first_mile_min - walk or bike time from home to metro.
# 4. metro_wait_min
# 5. metro_ride_min - Normally 30.
# 6. last_mile_min - walk or bike from metro to office or home.
# 7. jam - metro delays.
# 8. used_bike
# 9. total_min - first + wait + ride + last

# Read the CSV
commute_data = read.csv("Open Source Data/commute_log.csv")
View(commute_data)

# To compare original visuals with the transformed visuals
commute_data_t = commute_data

# Getting data summary and a few visuals
summary(commute_data)

# Keeping only numeric variables
library(corrplot)
library(RColorBrewer)
commute_data_numeric_columns = sapply(commute_data, is.numeric)
commute_data_numeric = commute_data_numeric[,-1]
commute_data_numeric = cor(commute_data[, commute_data_numeric_columns])
# Correlation matrix
corrplot(commute_data_numeric, type = "upper", addCoef.col = "red", order="hclust", col = brewer.pal(11, name = "RdYlBu"))

# Visuals before adding transformations
hist(commute_data$first_mile_min,
     main = "First mile time (min)",
     xlab = "minutes")
hist(commute_data$metro_wait_min, main = "Metro wait (min)", xlab = "minutes")
hist(commute_data$metro_ride_min, main = "Metro ride (min)", xlab = "minutes")
hist(commute_data$last_mile_min,  main = "Last mile (min)",  xlab = "minutes")
hist(commute_data$total_min,      main = "Total commute (min)", xlab = "minutes")

# Applying transformations

source("transformations.R")

# first_mile_min
# Left Skewed, tail thins out for smaller values.
# reflect + log transformation, min max scaling and normalisation test using Shapiro Wick

first_mile_reflect <- negation(commute_data$first_mile_min)
first_mile_log     <- log1p(first_mile_reflect)
first_mile_scaled  <- min_max_scale(first_mile_log)

shapiro.test(first_mile_scaled)

qqnorm(first_mile_scaled)
qqline(first_mile_scaled, col="red")

library(tseries)
jarque.bera.test(first_mile_scaled)

# wait time
wait_scaled <- min_max_scale(commute_data$metro_wait_min)
shapiro.test(wait_scaled)
jarque.bera.test(wait_scaled)
qqnorm(wait_scaled)
qqline(wait_scaled, col = "red")

# metro ride time
# Extremely uneven, not normal distribution. Best to apply piece wise transformation. We will use mode (30)
ride_extra <- ifelse(commute_data$metro_ride_min <= 30,
                     0,
                     commute_data$metro_ride_min - 30)
ride_extra_log <- log1p(ride_extra)
ride_extra_z <- z_score(ride_extra_log)
shapiro.test(ride_extra_z)
jarque.bera.test(ride_extra_z)

# last_mile_min (looks normal in histogram)
last_mile_z <- z_score(commute_data$last_mile_min)

shapiro.test(last_mile_z)
jarque.bera.test(last_mile_z)

qqnorm(last_mile_z)
qqline(last_mile_z, col = "red")
