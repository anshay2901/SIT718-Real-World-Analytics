# NHANES package contains a subset of real data from the official CDC National Health and Nutrition Examination Survey (NHANES).
# install.packages("NHANES")

# Load it into R
library(NHANES)

data("NHANES")

# Check if it works
head(NHANES)



data <- NHANES

# Just an example — if you had BMI data for two groups:
# younger adults
age1 <- rnorm(200, mean = 25, sd = 3)  
# older adults
age2 <- rnorm(200, mean = 28, sd = 5)  


bmi <- c(age1, age2)
group <- c(rep("18-35", 200), rep("36-65", 200))

# Plot overall histogram
hist(bmi, col = "lightblue",
     main = "Overall BMI Distribution",
     xlab = "BMI")

# Cross-sectioned histograms
hist(age1, col = "skyblue", main = "BMI (18–35)", xlab = "BMI")
hist(age2, col = "pink", main = "BMI (36–65)", xlab = "BMI")

# Calculate spread
mean(bmi)
sd(bmi)
