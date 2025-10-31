# Learning R-Programming Language


# sumArray: returns the sum of a numeric vector.
sumArray <- function(array.of.numbers) {
  
  # Guard: if not numeric, warn and exit
  if (!is.numeric(array.of.numbers)) {
    warning("Input must be numeric.")
  }
  
  # Initialize result
  result <- 0;
  
  # Iterate i from 1 to length of input
  for(i in c(1:length(array.of.numbers))) {
    # Add current element (skip N/As to avoid NA result)
    if (!is.na(array.of.numbers[i])) result <- result + array.of.numbers[i]
  }
  
  result
  
}



# equationXY (f): computes (x^2 + y^2) / (x + y)
equationXY <- function(x, y) {
  
  # Validate numeric inputs
  if (!is.numeric(x) || !is.numeric(y)) {
    warning("x and y must be numeric.")
  }
  
  # Special case FIRST: f(0,0) = 0
  if (x == 0 && y == 0) return(0)
  
  # Then guard against division by zero
  if ((x + y) == 0) {
    warning("Undefined: (x + y) equals 0, denominator would be zero.")
    return(NA_real_)
  }
  
  ((x^2)+(y^2))/(x+y)
}


# Custom function to calculate arithmetic mean using sum() and length()
calculateMean <- function(numbers) {
  
  if (!is.numeric(numbers)) stop("Input must be numeric.")  # validate input
  
  if (length(numbers) == 0) stop("Input vector is empty.")  # guard: no data
  
  total_sum <- sum(numbers, na.rm = TRUE)   # total of all values
  count <- length(numbers)                  # number of values
  total_sum / count                         # mean = sum / count
}


