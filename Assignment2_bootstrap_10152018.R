# Bootstrap Estimate from WCGS data
# Nina Dyatchenko
# 2018-10-15

wcgs_df <- read.csv(file = "C:/Users/Nina/Desktop/SASUniversityEdition/myfolders/EPH703 Tulay/wcgs.csv", header = TRUE, stringsAsFactors = FALSE)
wcgs_df

head(wcgs_df)

###########Discriptive stats ##########

summary(wcgs_df)


# Add a BMI column #
wcgs_df$BMI <- (wcgs_df$weight * 703) / (wcgs_df$height ^2)
hist(wcgs_df$BMI)

# True mean BMI

mean(wcgs_df$BMI) #24.52
 

### 100 Random Samples of Size 10 ###########

experimentN10_logi <- sapply(1:100, function(i){
  
  # Step 1: randomly select 10 BMIs
  sampBMI <- sample(wcgs_df$BMI, size = 10, replace = F)
  
  # Step 2 and 3: constract CI at 95% and compare to actual mean 24.52
  t_mod<- t.test(sampBMI, mu=24.51837, 
                 alternative = two.sided, 
                 conf.level = 0.95)
  
  # Step 4: extract the fdecision from the model output

  
})