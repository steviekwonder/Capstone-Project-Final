### Some goals:
## Ultimately, the goal is to select schools that provide affordable tuition for those coming form low
## income backgrounds. Just as important is how well the university prepares these students for success 
## after graduation, which could be gaged by percentage of post-graduates earning above $25,000 a year.
## Hence, we want less positive correlations between net price and post grad earnings.
## We want less significant correlations between loans and post grad earnings. 
## We want less significant correlations between post grad debt and earnings. 
## We want smaller percentage of people needing loans.
## We want smaller average net price tuition overall.

## Load the data frame. 
  college_info <- read.csv("~/Desktop/GRE Vocab Data Analytics/Most-Recent-Cohorts-Scorecard-Elements.csv")

## 1. Cut down data frame to variables you need. 
  library(dplyr)
  college_edit <- college_info %>%
    select(INSTNM, STABBR, CONTROL, PCTFLOAN, RELAFFIL, GT_25K_P6, NPT41_PUB, NPT41_PRIV, GRAD_DEBT_MDN_SUPP)
  
  for(i in 1:3) {
    college_edit[,i] <- as.character(college_edit[,i])
  }
  
  for(i in 4:9) {
    college_edit[,i] <- as.numeric(as.character(college_edit[,i]))
  }
  
## 2. Rename the CONTROL factors  
  college_edit$CONTROL <- factor(college_edit$CONTROL)
  levels(college_edit$CONTROL) <- c("public", "non_profit", "for_profit")
  college_edit$CONTROL <- as.character(college_edit$CONTROL)
  
## 3. Make one column for net price of lower income students, regardless of whether the university is public, private non-profit or private for-profit.
  NETPRICE <- 1:7703
  
  i <- 1
  while(i < 7704) {
    if(is.na(college_edit$NPT41_PUB[i]) == FALSE) {
      NETPRICE[i] <- college_edit$NPT41_PUB[i]
    }
    else{
      NETPRICE[i] <- college_edit$NPT41_PRIV[i]
    }
    i <- i + 1
  }
  
  college_edit <- cbind(college_edit, NETPRICE)
  college_edit <- subset(college_edit, select = -c(NPT41_PUB, NPT41_PRIV)) # get rid of the original net price columns
  
## 4. The average percentage of student population receiving some sort of loan. Let's observe the frequency  
## distributions and the average loan percentages for public, private non-profit and private for-profit universities.  
  library(ggplot2)
  ggplot(college_edit, aes(x = PCTFLOAN, col = CONTROL)) + geom_freqpoly() + facet_grid(.~CONTROL) 

  loans_by_CONTROL <- college_edit %>%
                          group_by(CONTROL) %>%
                          summarise(avgLoan <- mean(PCTFLOAN, na.rm = TRUE)) # 32% for public, 56.3% non- profit, 62% for profit

## 5. The average net price of institutions for those of lower income bracket of 0-$30000. Let's observe the frequency  
## distributions and the average net price for public, private non-profit and private for-profit universities.
  netPrice_by_CONTROL <- college_edit %>%
                            group_by(CONTROL) %>% 
                            summarise(avgNetPrice <- mean(NETPRICE, na.rm = TRUE)) # $8012.64 for public, $16751.61 for non-profit, $16744.82 for for-profit
    
  ggplot(college_edit, aes(x = NETPRICE, col = CONTROL)) + geom_freqpoly() + facet_grid(.~CONTROL) 
    
## 6. Divide up dataset into public, private non-profit and private for-profit.
  college_public <- subset(college_edit, college_edit$CONTROL == "public")
  college_public <- subset(college_public, select = -RELAFFIL) 
        
  college_nonProfit <- subset(college_edit, college_edit$CONTROL == "non_profit")

  college_forProfit <- subset(college_edit, college_edit$CONTROL == "for_profit") 
  college_forProfit <- subset(college_forProfit, select = -RELAFFIL)
        
## 7. What about earning annual salaries above $25,000 after graduation?  geom_freqpoly() 
  ggplot(college_edit, aes(x = GT_25K_P6)) + geom_freqpoly() + facet_grid(.~CONTROL)

## 8a. Correlations for public, private non-profit and private for-profit universities. The correlations that I care about are 
## between post grad earnings and loans, net price and post grad earnings, and post grad earnings and college debt.   
 
  # public universities
    cor(college_public[,4:7], use = "pairwise.complete.obs") # 0.58 between GT_25K_P6 and PCTFLOAN,  
                                                             # 0.44 between NETPRICE and GT_25K_P6,
                                                             # 0.46 between GT_25K_P6 and GRAD_DEBT
                                                    
  # private non-profit
    cor(college_nonProfit[,4:8], use = "pairwise.complete.obs") # 0.2 between PCTFLOAN and GT_25K, 
                                                                # 0.22 between NETPRICE and GT_25K,
                                                                # 0.17 between GT_25K_P6 and GRAD_DEBT
                                                          
  # private for-profit
    cor(college_forProfit[,4:7], use = "pairwise.complete.obs") # 0.28 between PCTFLOAN and GT_25K, 
                                                                # 0.49 between NETPRICE and GT_25K, 
                                                                # 0.58 between GT_25K_P6 and GRAD_DEBT
## 8b. All the universities combined. 
 cor(college_edit[,4:8], use = "pairwise.complete.obs") # 0.13 between GT_25K_P6 and PCTFLOAN
                                                        # 0.14 between NETPRICE and GT_25K_P6
                                                        # 0.48 between GT_25K_P6 and GRAD_DEBT
 
  # Linear Regression
    modelOne <- lm(GT_25K_P6 ~ GRAD_DEBT_MDN_SUPP + PCTFLOAN + RELAFFIL, data = college_edit)
    modelTwo <- lm(GT_25K_P6 ~ NETPRICE, data = college_edit)
    modelThree <- lm(GRAD_DEBT_MDN_SUPP ~ NETPRICE*PCTFLOAN, data = college_edit)
 
  # Cross validation for these regressions.
    install.packages("DAAG")
    library(DAAG)
    set.seed(1234)
    MSE <- cv.lm(college_edit, modelOne, m=10)
    cv.lm(college_edit, modelTwo, m=5)
 
## 9. Dplyr methods: With the chaining method, reduce the list to universities with dersired qualities: 
## We want universities with net price noticably lower than the national average.
## We want universities with education debt less than $20,000, since a $25,000 college debt is considered payable for those earning 30 to 40K a year.
## We want to have successful graduates so GT_25K_P6 over 70% or 0.7. Then observe the correlations for each set.
        
  # public universities
    topPubChoices  <-  college_public %>%
                         filter(NETPRICE < 7000 & GRAD_DEBT_MDN_SUPP < 20000 & GT_25K_P6 > 0.7) %>%
                         arrange(desc(GT_25K_P6)) 
  
    cor(topPubChoices[,4:7], use = "pairwise.complete.obs") # 0.024 between GT_25K_P6 and PCTFLOAN,  
                                                            # 0.33 between NETPRICE and GT_25K_P6, 
                                                            # 0.89 between GRAD_DEBT and GT_25K
  # private non-profit universities
    topNonProfitChoices  <-  college_nonProfit %>%
                               filter(NETPRICE < 12000 & GRAD_DEBT_MDN_SUPP < 20000 & GT_25K_P6 > 0.7) %>%
                               arrange(desc(GT_25K_P6)) 
     
    cor(topNonProfitChoices[,4:8], use = "pairwise.complete.obs") # -0.065 between PCTFLOAN and GT_25K
                                                                  # 0.176 between NETPRICE and GT_25K,
                                                                  # -0.25 between GRAD_DEBT and GT_25K
  # private for-profit universities
    topForProfitChoices <- college_forProfit %>%
                             filter(NETPRICE < 12000 & GRAD_DEBT_MDN_SUPP < 20000 & GT_25K_P6 > 0.7) %>%
                             arrange(desc(GT_25K_P6))
      
    cor(topForProfitChoices[,4:7], use = "pairwise.complete.obs") # -0.19 between PCTFLOAN and GT_25K
                                                                  # -0.28 between NETPRICE and GT_25K,
                                                                  # 0.3 between GRAD_DEBT and GT_25K
 
## 10. Combine the public, private non-profit and private for-profit universities with our desired qualities.
## Then, let's observe correlations and linear models. 
  topChoices <- merge(topPubChoices, topNonProfitChoices, all = TRUE)
  topChoices <- merge(topChoices, topForProfitChoices, all = TRUE)
  topChoices <- topChoices %>%
     arrange(desc(GT_25K_P6))
  # correlations
    cor(topChoices[,4:8], use = "pairwise.complete.obs") # -0.07 between PCTFLOAN and GT_25K
                                                         # 0.26 between NETPRICE and GT_25K
                                                         # -0.07 between GT_25K and GRAD_DEBT
  # linear regressions
    modelFour <- lm(GT_25K_P6 ~ GRAD_DEBT_MDN_SUPP + PCTFLOAN + RELAFFIL, data = topChoices)
    modelFive <- lm(GT_25K_P6 ~ NETPRICE, data = topChoices)
    modelSix <- lm(GRAD_DEBT_MDN_SUPP ~ NETPRICE*PCTFLOAN, data = topChoices)

  # Observe the new average net prices and loan percentages.
    newNetPrice_by_CONTROL <- topChoices %>%
                                group_by(CONTROL) %>%
                                summarise(newAvgNetPrice <- mean(NETPRICE, na.rm = TRUE))

    newLoans_by_CONTROL <- topChoices %>%
                             group_by(CONTROL) %>%
                             summarise(newAvgLoans <- mean(PCTFLOAN, na.rm = TRUE))

### Conclusion: 
## Here are some interestings trends in our new data sets. First off, while there are more private for-profit
## universities than public and private non-profit universities in the United States, there are more private
## non-profit universities that cater to low income students. Secondly, for the private for-profit universities
## the correlation between net price and high post grad earnings is actually sort of negatively correlated.
## Overall, these schools portrayed less stronger correlations which is what we wanted and desired
## for all the correlations we were interested in. Some suggestions for those interested in delving further into 
## what I was doing. Since most of the schools in our new data set are private non-profits and big name schools,
## assessing average standard test scores like the SAT and ACT (provided by the original data set) and 
## maybe looking at other data sets that observe student study habits in underresourced schools. Another idea is 
## to look into percentage of first generation students that are attending schools to better understand how 
## well universities are reaching out to those demographics.     