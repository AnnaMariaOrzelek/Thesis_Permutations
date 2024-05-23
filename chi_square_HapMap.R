# This code file allows to generate the results from Section 3.5, regarding the analysis on the HapMap data

# First load everything until line 182 in the environment, the results are genrated afterwards.

# The seed is reset before everything that involves randomness to ensure the reproducibility of the results.


library(ggplot2)
library(gridExtra)

load("my.data.selected.imputed.RData")

# count 2 categorical variables and add "2cat" to their name for easier plotting later
number.2cat <- character(0)
for (i in 1:ncol(my.data.selected.imputed)) {
   unique.categories <- length(unique(my.data.selected.imputed[, i]))
   
   if (unique.categories == 2) {
     number.2cat <- c(number.2cat, names(my.data.selected.imputed)[i])
     names(my.data.selected.imputed)[i] <- paste0(names(my.data.selected.imputed)[i], ".2cat")
   }
 }

selected.variables <- names(my.data.selected.imputed) #character with all variable names

# rename dataset
data <- my.data.selected.imputed

#### create frequency variable
relative.frequency <- c()

# Calculate relative frequency of the lesser frequent category
for (i in 2:length(selected.variables)) {
  variable.name <- selected.variables[i]
  frequencies <- table(data[, variable.name])
  
  
  total.observations <- sum(frequencies)
  
  relative.frequency[i-1] <- max(frequencies[-which.max(frequencies)]) / total.observations
}


### convert data to numeric
data <- data.frame(lapply(data, function(x) {
  if(is.factor(x)) {
    as.numeric(x)
  } else {
    x
  }
}))

# rename outcome variable
names(data)[1] = "y"



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


chisq.HapMap <- function(original.data, num.permutations, relative.frequency, correct = TRUE) {
  
  pvalues.original <- run.chi.square.pval(original.data, correct = correct)
  
  results.original <- cbind(Column = colnames(original.data)[-1], pvalues.original$p.value, relative.frequency)
  colnames(results.original)[2] <- "p.value"
 
  results.permuted <- data.frame()
  
  
  for (i in 1:num.permutations) {
    permuted.data <- original.data
    permuted.data$y <- sample(original.data$y, replace = FALSE)
    
    # Run chi-square test on permuted data
    
    pvalues.permuted <- run.chi.square.pval(permuted.data, correct = correct)
    
    statistics.pval.permuted <- cbind(Column = colnames(original.data)[-1], pvalues.permuted$p.value, relative.frequency)
    
    
    results.permuted <- rbind(results.permuted, statistics.pval.permuted)
    
  
  }
  
  colnames(results.permuted)[2] <- "p.value"
  results.original <- as.data.frame(results.original)
  # Splitting the datasets based on number of categories 
  original.2cat <- results.original[grepl(".2cat$", results.original$Column), ]
  original.3cat <- results.original[!grepl(".2cat$", results.original$Column), ]
  
  permuted.2cat <- results.permuted[grepl(".2cat$", results.permuted$Column), ]
  permuted.3cat <- results.permuted[!grepl(".2cat$", results.permuted$Column), ]
  
  
  original.2cat$p.value <- as.numeric(original.2cat$p.value)
  original.3cat$p.value <- as.numeric(original.3cat$p.value)
  permuted.2cat$p.value <- as.numeric(permuted.2cat$p.value)
  permuted.3cat$p.value <- as.numeric(permuted.3cat$p.value)
  original.2cat$relative.frequency <- as.numeric(original.2cat$relative.frequency)
  original.3cat$relative.frequency <- as.numeric(original.3cat$relative.frequency)
  permuted.2cat$relative.frequency <- as.numeric(permuted.2cat$relative.frequency)
  permuted.3cat$relative.frequency <- as.numeric(permuted.3cat$relative.frequency)
  
  plot.original.2cat <- ggplot(original.2cat, aes(x = relative.frequency, y = p.value)) +
    geom_point(color = "coral", size =1, alpha = 0.3) +
    labs(title = "p-values of chi-square test on original data, 2-categorical", x = "Relative frequency", y = "p-value") +
    ylim(c(0, 1)) +
    theme_minimal() +
    theme(plot.title = element_text(size = 10))
  
  plot.original.3cat <- ggplot(original.3cat, aes(x = relative.frequency, y = p.value)) +
    geom_point(color = "coral", size =1, alpha = 0.3) +
    labs(title = "p-values of chi-square test on original data, 3-categorical", x = "Relative frequency", y = "p-value") +
    ylim(c(0, 1)) +
    theme_minimal() +
    theme(plot.title = element_text(size = 10))
  
  
  plot.permuted.2cat <- ggplot(permuted.2cat, aes(x = relative.frequency, y = p.value)) +
    geom_point(color = "#009E73", size =1, alpha = 0.3) +
    labs(title = "p-values of chi-square test on permuted data, 2-categorical", x = "Relative frequency", y = "p-value") +
    ylim(c(0, 1)) +
    theme_minimal() +
    theme(plot.title = element_text(size = 10))
  
  
  plot.permuted.3cat <- ggplot(permuted.3cat, aes(x = relative.frequency, y = p.value)) +
    geom_point(color = "#009E73", size =1, alpha = 0.3) +
    labs(title = "p-values of chi-square test on permuted data, 3-categorical", x = "Relative frequency", y = "p-value") +
    ylim(c(0, 1)) +
    theme_minimal() +
    theme(plot.title = element_text(size = 10))
  
  
  # summary statistics 
  cat("Summary Statistics p-value - Original Data 2cat:\n")
  print(summary(original.2cat$p.value))
  
  cat("\nSummary Statistics p-value  - Original Data 3cat:\n")
  print(summary(original.3cat$p.value))
  
  cat("\nSummary Statistics p-value - Permuted Data 2cat:\n")
  print(summary(permuted.2cat$p.value))
  
  cat("\nSummary Statistics p-value - Permuted Data 3cat:\n")
  print(summary(permuted.3cat$p.value))
  
  cat("\nProportion of significant p-values 2cat original:\n")
  print(mean(original.2cat$p.value<=0.05))
  
  cat("\nProportion of significant p-values 3cat original:\n")
  print(mean(original.3cat$p.value<=0.05))
  
  cat("\nProportion of significant p-values 2cat permuted:\n")
  print(mean(permuted.2cat$p.value<=0.05))
  
  cat("\nProportion of significant p-values 3cat permuted:\n")
  print(mean(permuted.3cat$p.value<=0.05))
  
  grid.arrange(
    plot.original.2cat, plot.original.3cat,
    plot.permuted.2cat, plot.permuted.3cat,
    ncol = 2
  )
  
}

### Section 3.5

# Chi-square tests HapMap data with Yate's correction, Table 8+9, Figure 16+17
set.seed(123)
chisq.HapMap(original.data = data, num.permutations = 5, relative.frequency = relative.frequency, correct = TRUE)



# Chi-square tests HapMap data without Yate's correction, Table 8+9, Figure 18+24
set.seed(123)
chisq.HapMap(original.data = data, num.permutations = 5, relative.frequency = relative.frequency, correct = FALSE)



