### Part 1
#Look through the rest of the `lapply()` call that we didn't finish this morning. 
Create a function called `CheckCoverage()` with the following arguments:
  
  - `x`: a vector
- `n`: the size of the random sample to draw from `x`
- `alpha`: the tolerance for a Type-I error. This should default to `0.05`.

This function should return a single logical value indicating whether the 
$1- \alpha$-level confidence interval contains the true value of the mean. 
Test your function to ensure it works as intended.

ChechCoverage<- function(x, n, alpha=0.05){

  # Step 0: Clean the Data
   x <- x[!is.na(x)] # Remove missing values
  
  # Step 1: The "Truth" (null hypothesis)
  xMu <- mean(x, na.rm = TRUE) # "true" mean, remove N/A
  
  # Step 2: randomly select n value
  sampX <- sample(x, size = n, replace = FALSE)
  
  # Step 3: Construct a 95% CI
  t_mod <- t.test(
    x = sampX,
    mu = xMu,
    alternative = "two.sided",
    conf.level = 1-alpha
  )
  
  # Step 4: Test if the population mean is contained in the CI
  CI <- t_mod$conf.int
  (CI[1] < xMu) && (xMu < CI[2])
}  
wcgs_df <- read.csv("C:/Users/Nina/Desktop/SASUniversityEdition/myfolders/EPH703 Tulay/wcgs.csv", header = TRUE, stringsAsFactors = FALSE)
wcgs_df$BMI <- (wcgs_df$weight * 703) / (wcgs_df$height ^ 2)

head(wcgs_df)

ChechCoverage(x=wcgs_df$BMI, n=10) #TRUE- f-n workes

### Part 2
Apply the `CheckCoverage()` function you wrote in Part 1 to find the coverage probability after 10,000 replicates 
(HINT: use the `replicate()` function instead of `lapply()`; see `?replicate` for more information) 
for sample sizes of 10, 30, and 50 for the following measurements:
  
- `arcus`
- `height`
- `ncigs`
- `BMI` (we calculated this column ourselves)

#BMI
CovProbBMI10<- replicate(10000, ChechCoverage(x=wcgs_df$BMI, n=10))
#Coverage Probability:
mean(CovProbBMI10)

CovProbBMI30<- replicate(10000, ChechCoverage(x=wcgs_df$BMI, n=30))
mean(CovProbBMI30)

CovProbBMI50<- replicate(10000, ChechCoverage(x=wcgs_df$BMI, n=50))
mean(CovProbBMI50)


#Arcus
CovProbArcus10<- replicate(10000, ChechCoverage(x=wcgs_df$arcus, n=10))
mean(CovProbArcus10)

CovProbArcus30<- replicate(10000, ChechCoverage(x=wcgs_df$arcus, n=30))
mean(CovProbArcus30)

CovProbArcus50<- replicate(10000, ChechCoverage(x=wcgs_df$arcus, n=50))
mean(CovProbArcus50)


#Height
CovProbHeight10<- replicate(10000, ChechCoverage(x=wcgs_df$height, n=10))
mean(CovProbHeight10)

CovProbHeight30<- replicate(10000, ChechCoverage(x=wcgs_df$height, n=30))
mean(CovProbHeight30)

CovProbHeight50<- replicate(10000, ChechCoverage(x=wcgs_df$height, n=50))
mean(CovProbHeight50)


#Number of cigarets
CovProbNCigs10<- replicate(10000, ChechCoverage(x=wcgs_df$ncigs, n=10))
mean(CovProbNCigs10)

CovProbNCigs30<- replicate(10000, ChechCoverage(x=wcgs_df$ncigs, n=30))
mean(CovProbNCigs30)

CovProbNCigs50<- replicate(10000, ChechCoverage(x=wcgs_df$ncigs, n=50))
mean(CovProbNCigs50)


### Part 3
Inspect the histograms or densities of the four above measurements of interest. 
Can you draw any conclusions about the coverage results you saw in Part 2 and the shapes of these four measurements?

hist(wcgs_df$BMI) #looks rather notmal
hist(wcgs_df$arcus) #doesn't look normal, binary
hist(wcgs_df$height) # looks normal
hist(wcgs_df$ncigs) #heavily skewed to the right

A confidence interval implies that all data points in the interval are normally distributed around the mean. 
In a non-normal or skewed data distibusions, a confidence interval will also not be normally distributed.
Thus, for the non-npormal distributions, the CI should be wider to be able to capture 95% of data points. 



