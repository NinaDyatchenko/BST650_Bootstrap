# Input: data, statistic, n=NULL
# Output: value of the statistic

BootThe <- function(data, stat, n = NULL){
 # Check NA in the data
# Remove missing data
  
  if ((is.matrix(data) == TRUE) || (is.data.frame(data) == TRUE)) {
    
    data <- na.omit(data)
    if (is.null(n)){
      n <- nrow(data)
    }
    if (nrow(data) >= n){
      sampID <- sample(nrow(data), n, replace = TRUE)
      sampX  <- data[sampID, ]
    } else {
      stop("Error")
    }
    
  } else if ((is.atomic(data) == TRUE) || (is.list(data) == TRUE)) {
    
    data <- data[!is.na(data)]
    if (is.null(n)) {
      n <- length(data)
    }
  
    if (length(data) >= n) {
      sampX <- sample(data, n, replace = TRUE)
    } else {
      stop("Error")
    }
    
  }
  
  stat(sampX)
  
}

x<- c(1, 2, 3, 4, 5)
BootThe(x, mean)
BootThe(x, sd)

df <- data.frame(
  a=rnorm(10),
  b=rnorm(10),
  c=rnorm(10),
  d=rnorm(10)
)
BootThe(df[,1], mean)

BootThe(df, colMeans)
BootThe(as.matrix(df), cov)

