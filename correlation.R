pause <- function () {
  cat("Warning: Please source this file in the same directory as big-data..csv file\n")
  cat("Press <Enter> to continue and find B.Susanto's similarity metrics...")
  readline()
  invisible()
}

par(mfrow = c(2,2))

# Use Spearman's correlation to find most similar & dissimilar user
find.cor <- function(m = matrix, usr_index = user) {
  bestcor.spearman <- -1
  worstcor.spearman <- 1
  bestcorrow.spearman <- NULL
  worstcorrow.spearman <- NULL
  
  # Make a vector of all names excluding user's
  names <- c(rownames(use_matrix)[seq(from = 1, to = usr_index - 1)])
  names <- append(names, c(rownames(use_matrix)[seq(from = usr_index+1, to = 42)]))
  
  # Make a vector of all correlation to make histogram
  all.cor <- c()
    
  for (i in 1:nrow(m)) {
    # If same user, continue
    if (i == usr_index ) {
      next 
    } 
    
    this.cor = cor(m[i,], m[usr_index,], method = "spearman")
    
    if (this.cor > bestcor.spearman) {
      bestcor.spearman = this.cor
      bestcorrow.spearman = i
    }
    if (this.cor < worstcor.spearman) {
      worstcor.spearman = this.cor
      worstcorrow.spearman = i
    }
    all.cor <- append(all.cor, this.cor)
  }
  
  cat("Using the Spearman Correlation Method, ", rownames(m)[usr_index], "Is Most Correlated With ", rownames(m)[bestcorrow.spearman], 
      " With Correlation Value of ", bestcor.spearman, "And Is Worst Correlated With ", rownames(m)[worstcorrow.spearman],
      " With Correlation Value of ", worstcor.spearman, "\n\n")
  
  # Make a data frame of all names with user's correlation
  hist.cor <- data.frame(names, all.cor)
  h <- hist(all.cor, plot = TRUE, col = "purple", xlab = "Spearman's Correlation Index"
            , main = "Distribution of Correlation")
  return(invisible(h))
}

# Use Euclidean Distance to find the most similar and dissimilar user
find.dist <- function(m = matrix, usr_index = user) {
  bestdist <- 100
  worstdist <- -100
  bestdistrow <- NULL
  worstdistrow <- NULL
  
  # Make a vector of all correlation to make histogram
  all.dist <- c()
  
  for (i in 1:nrow(m)) {
    # If same user, continue
    if (i == usr_index ) {
      next 
    } 
    
    this.dist <- dist(rbind(m[i,], m[usr_index,]))
    if ( this.dist < bestdist ) {
      bestdist = this.dist
      bestdistrow = i;
    }
    if ( this.dist > worstdist ) {
      worstdist = this.dist
      worstdistrow = i;
    }
    all.dist <- append(all.dist,  this.dist)
  }
  cat("Using the Euclidean Distance Method, ", rownames(m)[usr_index], "Is Most Correlated With ", rownames(m)[bestdistrow], 
      " With Distance of ", bestdist, "And Is Worst Correlated With ", rownames(m)[worstdistrow],
      " With Distance of ", worstdist, "\n")
  
  h <- hist(all.dist, plot = TRUE, col = "purple", xlab = "Euclidean Distance"
            , main = "Distribution of Distance")
  
  return(invisible(h))
}


bg.surv <- read.csv( file = "big-data-survey-2014-fall-interests.csv") # Exports Doc
rownames( bg.surv ) <- bg.surv[,1] # Adds row names
use_matrix <- as.matrix( bg.surv[,c(8:12,14:21)] ) # Makes a matrix of col 8 - 12, leaving col 13
pause()
find.cor(m = use_matrix, usr_index = 20)
find.dist(m = use_matrix, usr_index = 20)

