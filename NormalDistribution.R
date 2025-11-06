# reproducibility
set.seed(226156802)

# setting up our data
normalData <- rnorm(1000, mean = 50, sd = 10)

# visualising the data to check for normal distribution
hist(normalData, main = "Histogram of Data", xlab = "Values", col = "lightblue", border = "white")


# Shapiro Wilk Test 
shapiro.test(normalData)

# Kolmogorov Smirnov Test
ks.test(normalData, "pnorm", mean = mean(normalData), sd = sd(normalData))

# Jarque Bera Test
# install.packages("tseries")
library(tseries)
jarque.bera.test(normalData)

# Standarisation (Z-Scores)
mean_val = mean(normalData)
sd_val = sd(normalData)

standardisedData <- (normalData-mean_val)/sd_val

hist(standardisedData, main = "Sandardised Data (Z-Scores)", col = "lightgreen")

# testing standardised data
shapiro.test(standardisedData)
jarque.bera.test(standardisedData)


# Setting up a non normal data
skewedData <- rexp(1000, rate = 1)
hist(skewedData, main="Skewed Data")
shapiro.test(skewedData)
jarque.bera.test(skewedData)