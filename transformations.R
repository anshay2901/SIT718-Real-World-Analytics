# Negation

# Creating raw i/p
rawData = runif(100, 10, 50)
# Visualisation
plot(rawData, main = "Before Negation", col = "red", cex = 2, pch = 42)
# Inverting
transformedData = 50-rawData+10
# Visualisation
plot(transformedData, main = "After Negation", col = "red", cex = 2, pch = 42)