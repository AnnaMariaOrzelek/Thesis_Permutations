# This code file allows to generate the results from Section 2.1 + 2.2, regarding the t-tests.

# First load the function in the environment, the results are genrated afterwards.

# The seed is reset before everything that involves randomness to ensure the reproducibility of the results.

library(ggplot2)
library(gridExtra)

# Function to perform t-test for several scenarios
all.t.tests <- function(sample.size1 = 100, sample.size2 = 100, num.iterations = 1000, num.permutations = 10, outlier = 25) {
  
    p.values.original = numeric()
    p.values.permuted = numeric()
    p.values.permuted.outlier = numeric()
    p.values.bootstrap = numeric()
    p.values.permuted.bootstrap = numeric()
  
  
  
  for (i in 1:num.iterations) {
    # Generate new samples for each iteration
    
    sample1 <- rnorm(sample.size1, mean = 0, sd = 1)
    sample2 <- c(rnorm(sample.size2 - 1, mean = 0, sd = 1), outlier)  # Adding an outlier to sample2
    sample3 <- rnorm(sample.size2, mean = 0, sd = 1)
    
    # t-test on original data
    t.test.result <- t.test(sample1, sample2)
    p.values.original <- c(p.values.original, t.test.result$p.value)
    
    # t-test on permuted data without outlier
    p.values.permuted.i <- numeric()
    for (j in 1:num.permutations) {
      permuted.data <- sample(c(sample1, sample3))
      permuted.sample1 <- permuted.data[1:sample.size1]
      permuted.sample2 <- permuted.data[(sample.size1 + 1):length(permuted.data)]
      p.values.permuted.i <- c(p.values.permuted.i, t.test(permuted.sample1, permuted.sample2)$p.value)
    }
    p.values.permuted <- c(p.values.permuted, p.values.permuted.i)
    
    # t-test on permuted data with outlier
    p.values.permuted.outlier.i <- numeric()
    for (j in 1:num.permutations) {
     permuted.data.outlier <- sample(c(sample1, sample2))
     permuted.sample1.outlier <- permuted.data.outlier[1:sample.size1]
     permuted.sample2.outlier <- permuted.data.outlier[(sample.size1 + 1):length(permuted.data.outlier)]
     p.values.permuted.outlier.i <- c(p.values.permuted.outlier.i, t.test(permuted.sample1.outlier, permuted.sample2.outlier)$p.value)
    }
    p.values.permuted.outlier <- c(p.values.permuted.outlier, p.values.permuted.outlier.i)
    
    # t-test on bootstrappd data
    bootstrap.sample1 <- sample(sample1, replace = TRUE)
    bootstrap.sample2 <- sample(sample3, replace = TRUE)
    p.values.bootstrap <- c(p.values.bootstrap, t.test(bootstrap.sample1, bootstrap.sample2)$p.value)
    
    #t-test on permuted then bootstrapped data
    permuted.data2 <- sample(c(sample1, sample3))
    permuted.sample3 <- permuted.data2[1:sample.size1]
    permuted.sample4 <- permuted.data2[(sample.size1 + 1):length(permuted.data2)]
    bootstrap.sample3 <- sample(permuted.sample3, replace = TRUE)
    bootstrap.sample4 <- sample(permuted.sample4, replace = TRUE)
    p.values.permuted.bootstrap <- c(p.values.permuted.bootstrap, t.test(bootstrap.sample3, bootstrap.sample4)$p.value)
    
  }
  
  # Plot the p-values
  plot.original.permuted <- ggplot(data = data.frame(p.values.permuted), aes(x = p.values.permuted)) +
      geom_histogram(binwidth = 0.05, fill = "white", color = "#009E73") +  
      labs(title = "p-values of t-tests on permuted data", x = "p-value", y = "Frequency") 
    
  plot.original.outlier <- ggplot(data = data.frame(p.values.original), aes(x = p.values.original)) +
    geom_histogram(binwidth = 0.05, fill = "white", color = "coral") +  
    labs(title = "p-values of t-tests on original data with outlier", x = "p-value", y = "Frequency")
  
  plot.original.permuted.outlier <- ggplot(data = data.frame(p.values.permuted.outlier), aes(x = p.values.permuted.outlier)) +
    geom_histogram(binwidth = 0.05, fill = "white", color = "#009E73") +  
    labs(title = "p-values of t-tests on permuted data with outlier", x = "p-value", y = "Frequency")
  
  plot.bootstrap <- ggplot(data = data.frame(p.values.bootstrap), aes(x = p.values.bootstrap)) +
    geom_histogram(binwidth = 0.05, fill = "white", color = "coral") +  
    labs(title = "p-values of t-tests on bootstrapped data", x = "p-value", y = "Frequency")
  
  
  plot.permuted.bootstrap <- ggplot(data = data.frame(p.values.permuted.bootstrap), aes(x = p.values.permuted.bootstrap)) +
    geom_histogram(binwidth = 0.05, fill = "white", color = "#009E73") +  
    labs(title = "p-values of t-tests on permuted and bootstrapped data", x = "p-value", y = "Frequency")
  
 grid.arrange(plot.original.permuted, plot.original.outlier, plot.original.permuted.outlier, plot.bootstrap,
               plot.permuted.bootstrap, ncol = 2)
  
  assign("p.values.orignial.outlier", p.values.original, envir = .GlobalEnv)
  assign("p.values.bootstrap", p.values.bootstrap, envir = .GlobalEnv)
  
}

# Run function
set.seed(123)
all.t.tests(sample.size1 = 100, sample.size2 = 100, num.permutations = 10, outlier = 25) #produces figures 1-5

summary(p.values.orignial.outlier) #result section 2.1
sum(p.values.bootstrap <= 0.05) #result section 2.2













