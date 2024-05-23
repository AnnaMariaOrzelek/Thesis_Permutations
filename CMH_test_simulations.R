# This code file allows to generate the results from Section 4.3 and 5.1, regarding the Cochran-Mantel-Haenszel test

# First load all the functions in the environment, the results are genrated afterwards.

# The seed is reset before everything that involves randomness to ensure the reproducibility of the results.

library(gridExtra)
library(ggplot2)
library(reshape2)

# function to generate 2-categorical data
generate.data <- function(x1.proba, x2.proba, x3.proba, x4.proba, sample.size, beta = NULL) {
  
  
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


# when having two variables (y and x), split into three parts and create contingency table
process.data <- function(data, indices) {
  split.data <- split(data[indices, ], cut(seq_along(indices), breaks = 3, labels = FALSE))
  table.list <- lapply(split.data, function(part) table(part$y, part$x))
  # Combine tables into an array
  array.data <- array(unlist(table.list), dim = c(2, 2, 3))
  return(array.data)
}

# Define the function
mantel.haenszel.test <- function(num.simulations = 10000, sample.size = 1000, x1.proba, x2.proba, x3.proba, x4.proba, 
                                 num.permutations = 5, beta = NULL, correct = TRUE) {
  
  p.value.counts <- c(x1 = 0, x2 = 0, x3 = 0, x4 = 0)
  p.value.counts.p <- c(x1 = 0, x2 = 0, x3 = 0, x4 = 0)
  p.values.df <- data.frame(x1 = numeric(num.simulations), x2 = numeric(num.simulations), x3 = numeric(num.simulations), x4 = numeric(num.simulations))
  p.values.df.p <- data.frame(x1 = numeric(), x2 = numeric(), x3 = numeric(), x4 = numeric())
  
  
  for (iteration in 1:num.simulations) {
    data <- generate.data(x1.proba, x2.proba, x3.proba, x4.proba, sample.size, beta = beta)
    

    n.rows <- nrow(data)
    indices <- sample(1:n.rows, n.rows)
    
    # Create a list to store test data for each variable
    test.data.list <- list()
    
    # Loop through each variable and split it into three parts + contingency table
    for (i in 1:4) {
      variable.name <- paste0("x", i)
      # Extract data for the variable
      variable.data <- data[, c(1, i + 1)]
      test.data.list[[variable.name]] <- process.data(variable.data, indices)
    }
    
    # Perform Mantel-Haenszel test for each variable
    p.values <- sapply(test.data.list, function(test.data) {
      mantelhaen.test(test.data, correct = correct)$p.value
    })
    
    # Find the variable with the lowest p-value and increase the counter
    lowest.p.variable <- names(p.values)[which.min(p.values)]
    p.value.counts[lowest.p.variable] <- p.value.counts[lowest.p.variable] + 1
    
    p.values.df[iteration, ] <- p.values
    
    for (j in 1:num.permutations) {
      data.p <- data 
      data.p$y <- sample(data$y, replace = FALSE)
      
      test.data.list.p <- list()
      
      for (i in 1:4) {
        variable.name <- paste0("x", i)
        # Extract data for the variable
        variable.data <- data.p[, c(1, i + 1)]
        test.data.list.p[[variable.name]] <- process.data(variable.data, indices)
        
      }
        # Perform Mantel-Haenszel test for each variable
        p.values.p <- sapply(test.data.list.p, function(test.data) {
          mantelhaen.test(test.data, correct = correct)$p.value
        })
        
        
        
        # Find the variable with the lowest p-value and increase the counter
        lowest.p.variable.p <- names(p.values.p)[which.min(p.values.p)]
        p.value.counts.p[lowest.p.variable.p] <- p.value.counts.p[lowest.p.variable.p] + 1
        
        p.values.df.p <- rbind(p.values.df.p, p.values.p)
    }
  }
  names(p.values.df.p) <- c("x1", "x2", "x3", "x4")
  #count significant p-values
  significant.x1 <- sum(p.values.df$x1 <= 0.05)
  significant.x2 <- sum(p.values.df$x2 <= 0.05)
  significant.x3 <- sum(p.values.df$x3 <= 0.05)
  significant.x4 <- sum(p.values.df$x4 <= 0.05)
  
  significant.pvalues <- data.frame(x1 = significant.x1, x2 = significant.x2, x3 = significant.x3, x4 = significant.x4)
  
  #count significant p-values after permutation checks
  significant.x1.p <- sum(p.values.df.p$x1 <= 0.05)
  significant.x2.p <- sum(p.values.df.p$x2 <= 0.05)
  significant.x3.p <- sum(p.values.df.p$x3 <= 0.05)
  significant.x4.p <- sum(p.values.df.p$x4 <= 0.05)
  
  significant.pvalues.p <- data.frame(x1 = significant.x1.p, x2 = significant.x2.p, x3 = significant.x3.p, x4 = significant.x4.p)
  
  data.pval.long <- reshape2::melt(p.values.df)
  data.pval.long.p <- reshape2::melt(p.values.df.p)
  
  plot.pval <- ggplot(data.pval.long, aes(x = variable, y = value)) +
    geom_boxplot(color = "coral") +
    labs(title = "p-values of CMH tests on original data", x = "Variable", y = "p-value")
  
  plot.pval.p <- ggplot(data.pval.long.p, aes(x = variable, y = value)) +
    geom_boxplot(color = "#009E73") +
    labs(title = "p-values of CMH tests on permuted data", x = "Variable", y = "p-value")
  
  grid.arrange(plot.pval, plot.pval.p, ncol = 1)
  
  cat("\nCount lowest p-values not permuted:\n")
  print(p.value.counts/(num.simulations/100))
  cat("\nCount significant p-values not permuted:\n")
  print((significant.pvalues)/(num.simulations/100), row.names = FALSE)
  cat("\nCount lowest p-values permuted:\n")
  print(p.value.counts.p/((num.simulations*num.permutations)/100))
  cat("\nCount significant p-values permuted:\n")
  print((significant.pvalues.p)/((num.simulations*num.permutations)/100), row.names = FALSE)
  assign("pvals.mh.test.p", p.values.df.p, envir = .GlobalEnv)
  
  
}

### Section 4.3
# With Yate's correction, Tables 11 + 12, Figures 19 + 21 
set.seed(123) 
mantel.haenszel.test(num.simulations = 10000, sample.size = 1000, x1.proba = 0.05, x2.proba = 0.15, x3.proba = 0.3, x4.proba = 0.5,
                               num.permutations = 5, beta = c(1, -0.5, 0.5, -0.5, 0.5), correct = TRUE)

summary(pvals.mh.test.p$x1)

# Without Yate's correction, Figures 20 + 22, Tables 11 + 12
set.seed(123) 
mantel.haenszel.test(num.simulations = 10000, sample.size = 1000, x1.proba = 0.05, x2.proba = 0.15, x3.proba = 0.3, x4.proba = 0.5,
                     num.permutations = 5, beta = c(1, -0.5, 0.5, -0.5, 0.5), correct = FALSE)


# With Yate's correction, uneven y groups, Figure 25, Table 15 
set.seed(123) 
mantel.haenszel.test(num.simulations = 10000, sample.size = 1000, x1.proba = 0.05, x2.proba = 0.15, x3.proba = 0.3, x4.proba = 0.5,
                     num.permutations = 5, beta = c(3, -0.5, 0.5, -0.5, 0.5), correct = TRUE)


