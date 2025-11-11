## Negation

# # Creating raw i/p
# rawData = runif(100, 10, 50)
# # Visualisation
# plot(rawData, main = "Before Negation", col = "red", cex = 2, pch = 42)
# # Inverting
# transformedData = 50-rawData+10
# # Visualisation
# plot(transformedData, main = "After Negation", col = "red", cex = 2, pch = 42)

negation <- function(x) {
  if (!is.numeric(x)) stop("Input must be numeric.")
  if (length(x) < 2) stop("Vector must contain at least 2 values.")
  if (any(is.na(x))) stop("Vector contains NA values.")
  
  max(x, na.rm = TRUE) + min(x, na.rm = TRUE) - x
}

## Normalisation

# Feature Scaling (min-max scaling)
min_max_scale <- function(x) {
  # Input Validation
  if(!is.numeric(x)) stop("Inputs must be numeric!")
  if(length(x) < 2) stop("Vector must contain atleast 2 values!")
  if(any(is.na(x))) stop("Vector contains NA values.")
  
  min = min(x)
  max = max(x)
  
  if(min == max) stop("All Values are same! Cannot Scale!")
  
  (x-min)/(max - min)
}

# Standardisation (z - score)
z_score <- function(x) {
  # Input Validation
  if(!is.numeric(x)) stop("Inputs must be numeric!")
  if(length(x) < 2) stop("Vector must contain atleast 2 values!")
  if(any(is.na(x))) stop("Vector contains NA values.")
  
  s = sd(x)
  if(s == 0) stop("Standard deviation is zero. Cannot standardise.")
  
  normalised = (x - mean(x)) / s
}

# Modified Standardisation to [0.05, 0.95]
scaled_normal <- function(x) {
  # Input Validation
  if (!is.numeric(x)) stop("Input must be numeric.")
  if (length(x) < 2) stop("Vector must contain at least 2 values.")
  if (any(is.na(x))) stop("Vector contains NA values.")
  
  s <- sd(x)
  if (s == 0) stop("Standard deviation is zero. Cannot scale.")
  
  normalised = 0.15 * ((x - mean(x)) / s) + 0.5
}

# Rank Scaling
percentile_scale <- function(x) {
  if (!is.numeric(x)) stop("Input must be numeric.")
  if (any(is.na(x))) stop("Vector contains NA values.")
  if (length(x) < 2) stop("Vector must contain at least 2 values.")
  
  rank(x, ties.method = "average") / length(x)
}

# visual test (apply any above method)
plot_transform <- function(x, f, main_before = "Before", main_after = "After") {
  # Input validation
  if (!is.numeric(x)) stop("x must be numeric!")
  if (!is.function(f)) stop("f must be a function!")
  if (length(x) < 2) stop("x must have at least 2 values!")
  if (any(is.na(x))) stop("x contains NA values!")
  
  # Apply transformation
  transformed <- f(x)
  
  # Set up side-by-side plots
  par(mfrow = c(1, 2))
  
  # Before plot
  plot(x,main = main_before,col = "red",pch = 16,cex = 1.5)
  
  # After plot
  plot(transformed,main = main_after,col = "blue",pch = 16,cex = 1.5)
  
  # Reset plotting window
  par(mfrow = c(1, 1))
  
  # Return transformed data
  return(transformed)
}