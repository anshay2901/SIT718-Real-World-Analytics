## Import Volley Data set

volley = read.table("Data/volley.txt")
View(volley)

str(volley)
summary(volley)

volley_original = volley

## Apply Transformations

# Feature changes to Column 1
volley[,1] = (volley[,1] - min(volley[,1]))/(max(volley[,1]) - min(volley[,1]))

# Normalisation (Z-Score) to Column 2
volley[,2] = (volley[,2] - mean(volley[,2]))/sd(volley[,2])

## Visualisations

# Column 1
par(mfrow=c(1,2))
plot(volley_original[,1], main = "Column 1 (Before Scaling)", pch=16, col="red")
hist(volley_original[,1], main="Histogram Before Scaling", col="red")

par(mfrow=c(1,2))
plot(volley[,1], main = "Column 1 (After Min-Max Scaling)", pch=16, col="blue")
hist(volley[,1], main="Histogram After Scaling", col="blue")

# Column 2
par(mfrow=c(1,2))
plot(volley_original[,2], main = "Column 2 (Before Normalisation)", pch=16, col="red")
hist(volley_original[,2], main="Histogram Before Normalisation", col="red")

par(mfrow=c(1,2))
plot(volley[,2], main = "Column 2 (After Z-Score Normalisation)", pch=16, col="blue")
hist(volley[,2], main="Histogram After Normalisation", col="blue")


summary(volley_original[,1])
summary(volley[,1])

summary(volley_original[,2])
summary(volley[,2])

