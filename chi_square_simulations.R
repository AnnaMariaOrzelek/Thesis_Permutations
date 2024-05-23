# This code file allows to generate the results from Sections 3.2, 3.4, 5.1 and 5.2, regarding the chi-square test.

# First load all the functions in the environment, the results are genrated afterwards.

# The seed is reset before everything that involves randomness to ensure the reproducibility of the results.

library(gridExtra)
library(ggplot2)
library(reshape2)

# function to generate data with two-categorical explanatory variables
generate.data.2cat <- function(x1.proba, x2.proba, x3.proba, x4.proba, sample.size, beta = NULL) {
  
  
  x1 <- rbinom(sample.size, 1, x1.proba)
  x2 <- rbinom(sample.size, 1, x2.proba)
  x3 <- rbinom(sample.size, 1, x3.proba)
  x4 <- rbinom(sample.size, 1, x4.proba)
  
  if(is.null(beta)) {
    
    y <- rbinom(sample.size, 1, 0.5)
  }
  
  else {
    X <- cbind(1, x1, x2, x3, x4)
    linpred <- as.matrix(X) %*% matrix(beta, ncol = 1)
    y <- as.factor(rbinom(sample.size, size = 1, prob = plogis(linpred)))
  }
  
  return(data.frame(y, x1, x2, x3, x4))
}


# funtion to generate data with 3 categorical explanatory variables
generate.data.3cat <- function(sample.size, x1.proba, x2.proba, x3.proba, x4.proba, beta = NULL) {
  
  # Generate random values for each explanatory variable
  x1 <- sample(0:2, size = sample.size, replace = TRUE, prob = c(1-(x1.proba[1]+x1.proba[2]), x1.proba[2], x1.proba[1]))
  x2 <- sample(0:2, size = sample.size, replace = TRUE, prob = c(1-(x2.proba[1]+x2.proba[2]), x2.proba[2], x2.proba[1]))
  x3 <- sample(0:2, size = sample.size, replace = TRUE, prob = c(1-(x3.proba[1]+x3.proba[2]), x3.proba[2], x3.proba[1]))
  x4 <- sample(0:2, size = sample.size, replace = TRUE, prob = c(1-(x4.proba[1]+x4.proba[2]), x4.proba[2], x4.proba[1]))
  
  if(is.null(beta)) {
    y <- rbinom(sample.size, 1, 0.5)
  }
  
  else {
    X <- cbind(1, x1, x2, x3, x4)
    linpred <- as.matrix(X) %*% matrix(beta, ncol = 1)
    y <- as.factor(rbinom(sample.size, size = 1, prob = plogis(linpred)))
  }
  
  # Return the generated variables as a data frame
  return(data.frame(y, x1, x2, x3, x4))
}


# function to count how often x1 is two-categorical
count.x1.2cat <- function(sample.size, x1.proba, x2.proba, x3.proba, x4.proba, beta = NULL, simulations = 10000) {
  count.x1.two.categories <- 0
  
  for (i in 1:simulations) {
    data <- generate.data.3cat(sample.size, x1.proba, x2.proba, x3.proba, x4.proba, beta)
    
    # Check if x1 has only two unique categories
    if (length(unique(data$x1)) == 2) {
      count.x1.two.categories <- count.x1.two.categories + 1
    }
  }
  
  return(count.x1.two.categories)
}

set.seed(123)
count <- count.x1.2cat(sample.size = 1000, x1.proba = c(0.003, 0.1), x2.proba = c(0.1, 0.2),
                       x3.proba = c(0.15, 0.3), x4.proba = c(0.33, 0.33), beta = c(1, 0.5, -0.5, 0.5, -0.5))
count



# run chi-square tests between each explanatory variables and the outcome, extract p-value
run.chi.square.pval <- function(my.data, correct = TRUE) {
  p.values <- numeric()
  
  for (i in 2:ncol(my.data)) {
    p.val <- chisq.test(my.data[, 1], my.data[, i], correct = correct)$p.value
    
    p.values <- c(p.values, p.val)
  }
  
  result.df.pval <- data.frame(Column = colnames(my.data)[-1], p.value = p.values)
  return(result.df.pval)
  
}



chi.sqr <- function(x1.proba, x2.proba, x3.proba, x4.proba, sample.size = 1000, num.simulations = 10000, beta = NULL,
                    num.permutations = 5, correct = TRUE, data.gen.function = c("generate.data.3cat", "generate.data.2cat")) {
  
  pval.x1 <- numeric()
  pval.x2 <- numeric()
  pval.x3 <- numeric()
  pval.x4 <- numeric()
  lowest.pval <- numeric()
  
  pval.x1.p <- numeric()
  pval.x2.p <- numeric()
  pval.x3.p <- numeric()
  pval.x4.p <- numeric()
  lowest.pval.p <- numeric()
  
  
  for (i in 1:num.simulations) {
    
    # Generate data using the specified function
    data <- do.call(data.gen.function, list(sample.size = sample.size, x1.proba = x1.proba, x2.proba = x2.proba,
                                            x3.proba = x3.proba, x4.proba = x4.proba, beta = beta))
    
    # run chi.sqr and store results
    
    pval.df <- run.chi.square.pval(data, correct = correct)
    pval.x1 <- c(pval.x1, pval.df[1,2])
    pval.x2 <- c(pval.x2, pval.df[2,2])
    pval.x3 <- c(pval.x3, pval.df[3,2])
    pval.x4 <- c(pval.x4, pval.df[4,2])
    
    
  
    
    #find the variable with the lowest p-values
    lowest.pvalue <- which.min(pval.df$p.value)
    lowest.pval <- c(lowest.pval, lowest.pvalue)
    
    for (j in 1:num.permutations) {
    data.p <- data 
    data.p$y <- sample(data$y, replace = FALSE)
      
    
    pval.df.p <- run.chi.square.pval(data.p, correct = correct)
    pval.x1.p <- c(pval.x1.p, pval.df.p[1,2])
    pval.x2.p <- c(pval.x2.p, pval.df.p[2,2])
    pval.x3.p <- c(pval.x3.p, pval.df.p[3,2])
    pval.x4.p <- c(pval.x4.p, pval.df.p[4,2])
    
    
    lowest.pvalue.p <- which.min(pval.df.p$p.value)
    lowest.pval.p <- c(lowest.pval.p, lowest.pvalue.p)
    
    }
    
  }
  
 
  
  # Create data for plotting 
  
  data.pval <- data.frame(X1 = pval.x1, X2 = pval.x2, X3 = pval.x3, X4 = pval.x4)
  data.pval.long <- reshape2::melt(data.pval)
  
  data.pval.permuted <- data.frame(X1 = pval.x1.p, X2 = pval.x2.p, X3 = pval.x3.p, X4 = pval.x4.p)
  data.pval.permuted.long <- reshape2::melt(data.pval.permuted)
  
  
  #count significant p-values
  significant.x1 <- sum(pval.x1 <= 0.05)
  significant.x2 <- sum(pval.x2 <= 0.05)
  significant.x3 <- sum(pval.x3 <= 0.05)
  significant.x4 <- sum(pval.x4 <= 0.05)
  
  significant.pvalues <- data.frame(x1 = significant.x1, x2 = significant.x2, x3 = significant.x3, x4 = significant.x4)
  
  #count significant p-values permuted
  significant.x1.p <- sum(pval.x1.p <= 0.05)
  significant.x2.p <- sum(pval.x2.p <= 0.05)
  significant.x3.p <- sum(pval.x3.p <= 0.05)
  significant.x4.p <- sum(pval.x4.p <= 0.05)
  
  significant.pvalues.p <- data.frame(x1 = significant.x1.p, x2 = significant.x2.p, x3 = significant.x3.p, x4 = significant.x4.p)
  
  # plot the results
  
  plot.pval <- ggplot(data.pval.long, aes(x = variable, y = value)) +
  geom_boxplot(color = "coral") +
  labs(title = "p-values of chi-square tests on original data", x = "Variable", y = "p-value")

  plot.pval.permuted <- ggplot(data.pval.permuted.long, aes(x = variable, y = value)) +
    geom_boxplot(color = "#009E73") +
    labs(title = "p-values of chi-square tests on permuted data", x = "Variable", y = "p-value") 
  
  grid.arrange(plot.pval, plot.pval.permuted, ncol = 1)


     cat("\nCount lowest p-value:\n")
     print((table(lowest.pval))/(num.simulations/100))
     
     cat("\nCount significant p-values:\n")
     print((significant.pvalues)/(num.simulations/100), row.names = FALSE)
     
     cat("\nCount lowest p-value permuted:\n")
     print((table(lowest.pval.p))/((num.simulations*num.permutations)/100))
     
     cat("\nCount significant p-values permuted:\n")
     print((significant.pvalues.p)/((num.simulations*num.permutations)/100), row.names = FALSE)
  
  
}

##### Generation of the results #####

### Section 3.2

set.seed(123)
data.2cat <- generate.data.2cat(0.05, 0.15, 0.3, 0.5, sample.size = 1000) 
lapply(data.2cat[, c("y", "x1", "x2", "x3", "x4")], table) #results of Table 1

### Section 3.4

# 2-categorical explanatory variables, with Yate's correction, Figure 6+7, Table 2+3
set.seed(123)
chi.sqr(x1.proba = 0.05, x2.proba = 0.15, x3.proba = 0.3, x4.proba = 0.5, sample.size = 1000,
        num.simulations = 10000, num.permutations = 5, beta = c(1, -0.5, 0.5, -0.5, 0.5), correct = TRUE,
        data.gen.function = "generate.data.2cat") 

# 2-categorical explanatory variables, without Yate's correction, Figure 8+9, Table 4
set.seed(123)
chi.sqr(x1.proba = 0.05, x2.proba = 0.15, x3.proba = 0.3, x4.proba = 0.5, sample.size = 1000,
        num.simulations = 10000, num.permutations = 5, beta = c(1, -0.5, 0.5, -0.5, 0.5), correct = FALSE,
        data.gen.function = "generate.data.2cat")


# 3-categorical explanatory variables, figure 10+11, Table 5
set.seed(123)
chi.sqr(sample.size = 1000, x1.proba = c(0.05, 0.1), x2.proba = c(0.1, 0.2),
        x3.proba = c(0.15, 0.3), x4.proba = c(0.33, 0.33), beta = c(1, 0.5, -0.5, 0.5, -0.5), num.simulations = 10000,
        num.permutations = 5, correct = FALSE, data.gen.function = "generate.data.3cat") 
        
# Count number of 2-categorical x1 variables with the probability settings of the next two simulations
set.seed(123)
count <- count.x1.2cat(sample.size = 1000, x1.proba = c(0.003, 0.1), x2.proba = c(0.1, 0.2),
                       x3.proba = c(0.15, 0.3), x4.proba = c(0.33, 0.33), beta = c(1, 0.5, -0.5, 0.5, -0.5))
count

# 2/3-categorical explanatory variables, with Yate's correction, Figure 12+13, Table 6
set.seed(123)
chi.sqr(sample.size = 1000, x1.proba = c(0.003, 0.1), x2.proba = c(0.1, 0.2),
        x3.proba = c(0.15, 0.3), x4.proba = c(0.33, 0.33), beta = c(1, 0.5, -0.5, 0.5, -0.5), num.simulations = 10000,
        num.permutations = 5, correct = TRUE, data.gen.function = "generate.data.3cat")
        

# 2/3-categorical explanatory variables, without Yate's correction, Figure 14+15, Table 7
set.seed(123)
chi.sqr(sample.size = 1000, x1.proba = c(0.003, 0.1), x2.proba = c(0.1, 0.2),
        x3.proba = c(0.15, 0.3), x4.proba = c(0.33, 0.33), beta = c(1, 0.5, -0.5, 0.5, -0.5), num.simulations = 10000,
        num.permutations = 5, correct = FALSE, data.gen.function = "generate.data.3cat")



### Section 5.1
set.seed(123)
data3 <- generate.data.2cat(0.05, 0.15, 0.3, 0.5, sample.size = 1000, beta = c(3, 0.5, -0.5, 0.5, -0.5))
table(data3$y) #check how unevenly Y is distributed with those settings

# 2-categorical explanatory variables, with Yate's correction, y very unevenly distributed, Figure 23, Table 13 
set.seed(123)
chi.sqr(x1.proba = 0.05, x2.proba = 0.15, x3.proba = 0.3, x4.proba = 0.5, sample.size = 1000,
        num.simulations = 10000, num.permutations = 5, beta = c(3, -0.5, 0.5, -0.5, 0.5), correct = TRUE,
        data.gen.function = "generate.data.2cat")


### Section 5.2
# 2-categorical explanatory variables, without Yate's correction, only one random permutation, Table 14
set.seed(123)
chi.sqr(x1.proba = 0.05, x2.proba = 0.15, x3.proba = 0.3, x4.proba = 0.5, sample.size = 1000,
        num.simulations = 10000, num.permutations = 1, beta = c(1, -0.5, 0.5, -0.5, 0.5), correct = FALSE,
        data.gen.function = "generate.data.2cat")

