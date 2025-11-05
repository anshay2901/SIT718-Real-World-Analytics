# Probabilities for each category
brands <- c("Apple", "Samsung", "Pixel")
prob <- c(0.45, 0.35, 0.20)  
# number of customers surveyed
n <- 10  

# Let’s find the probability that 5 choose Apple, 3 choose Samsung, and 2 choose Pixel.
result <- dmultinom(x = c(5, 3, 2), prob = prob)
print(result)

# Simulate multinomial expm 10000 times
set.seed(123)
sim <- t(rmultinom(n = 10000, size = n, prob = prob))
head(sim)

# Visualize Apple Choices
apple_counts <- sim[, 1]  # first column = Apple counts
hist(apple_counts, breaks = 10, col = "skyblue",
     main = "Distribution of Apple Choices (n = 10)",
     xlab = "Number of people choosing Apple")
# Check mean and variance
mean(apple_counts)
var(apple_counts)

# Visualize Samsung Choices
samsumng_counts <- sim[, 2]  # second column = Samsung counts
hist(samsumng_counts, breaks = 10, col = "green",
     main = "Distribution of Samsung Choices (n = 10)",
     xlab = "Number of people choosing Samsung")
# Check mean and variance
mean(samsumng_counts)
var(samsumng_counts)