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
  # validate input
  if (!is.numeric(numbers)) stop("Input must be numeric.")
  if (length(numbers) == 0) stop("Input vector is empty.")
  
  total_sum <- sum(numbers, na.rm = TRUE)   # Guard against NA inputs
  count <- length(numbers)  
  
  # mean = sum / count
  total_sum / count                         
}

# Custom function to calculate median
calcMedian <- function(num_values) {
  # validate input
  if (!is.numeric(num_values)) stop("Input must be numeric.")
  if (length(num_values) == 0) stop("Input vector is empty.")
  
  # Store before computing the middle value
  num_values <- sort(num_values)  # <--- add this line right after Step 1
  
  # Step 3: find the middle based on even/odd length
  n <- length(num_values)
  if (n %% 2 != 0) {
    # Odd count: return the single middle value
    return(num_values[(n + 1) / 2])
  } else {
    # Even count: average of the two middle values
    mid1 <- n / 2
    mid2 <- mid1 + 1
    return((num_values[mid1] + num_values[mid2]) / 2)
  }
}

# Custom function to calculate the mode (numeric) (most frequent value)
calcModeNumeric <- function(values) {
  
  # Check the input 
  if (missing(values)) stop("Please provide a numeric vector.")
  if (!is.numeric(values)) stop("The input must be numeric.")
  if (length(values) == 0) stop("The input vector is empty.")
  if (all(is.na(values))) stop("All values are NA. Cannot calculate mode.")
  
  # Remove missing values
  values <- values[!is.na(values)]
  
  # variables to track counts
  mode_values <- c()      
  highest_count <- 0      

  for (i in 1:length(values)) {
    current_value <- values[i]
    count_value <- sum(values == current_value)
    
    # If a new highest count is found, reset mode_values
    if (count_value > highest_count) {
      highest_count <- count_value
      mode_values <- current_value
    }
    # If another value has the same count, add it (avoid duplicates)
    else if (count_value == highest_count && !(current_value %in% mode_values)) {
      mode_values <- c(mode_values, current_value)
    }
  }
  
 
  return(mode_values)
}


# Geometric Mean Function
geometricMean <- function(x, na.rm = TRUE) {
  # validate input
  if (!is.numeric(x)) stop("Input must be numeric.")
  if (length(x) == 0) stop("Input vector is empty.")
   
  # remove NA values if Na.rm = TRUE
  if (na.rm) x <- x[!is.na(x)]
  if (length(x) == 0) stop("All values are NA after removing missing values.")
  
  # check for non-positive values (log() undefined for <= 0)
  if (any(x <= 0)) {
    warning("Geometric mean is undefined for non-positive values. Returning NA.")
    return(NA_real_)
  }
  
  # compute geometric mean safely
  return(exp(mean(log(x))))
}

# Harmonic Mean Function
HM <- function(x) {
  # validate input
  if (!is.numeric(x)) stop("Input must be numeric.")
  if (length(x) == 0) stop("Input vector is empty.")
  if (any(x <= 0, na.rm = TRUE)) return(NA)
  length(x) / sum(1/x)
}

# Power Mean Function
PM <- function(x, p, na.rm = TRUE) {
  # Validate Inputs
  if(!is.numeric(x)) stop("x must be numeric.")
  if(!is.numeric(p) || length(p) != 1) stop("p must be a single number")
  if(na.rm) x <- x[!is.na(x)]
  if(length(x) == 0) stop("x is empty after removing NAs")
  
  # limits: p = +inf or -inf
  if(is.infinite(p)) return(if(p>0) max(x) else min(x))
  
  # If GM
  if(p == 0) {
    if(any(x <= 0)) stop("Geometric Mean needs all x > 0.")
    return(exp(mean(log(x))))
  }
  
  # For - p, also require x > 0
  if (p < 0 && any(x <= 0)) stop("For p < 0, all x must be > 0.")
  
  # General Power Mean
  (mean(x^p))^{1/p}
}
