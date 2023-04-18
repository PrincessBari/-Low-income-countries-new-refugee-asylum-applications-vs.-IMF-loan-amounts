# I created this R code for a data analysis project as a part of my master's program. My goal was to
# explore the correlations between "low-income" countries' combined new government and UNHCR
# refugee asylum applications filed in the years 2011-2013 and their IMF loan amounts for 
# those years. 

# The numbers in the corresponding csv file named "Low income countries info.csv" were taken 
# from https://www.unhcr.org/refugee-statistics/download/?url=UVaZC9. For each year, I combined the "N"
# refugee asylum "application type" (which stands for "New") and from the "authority" of "G" (for government) 
# and "U" (for UNHCR). (The explanations of these acronyms can be found here: 
# https://www.unhcr.org/refugee-statistics/methodology/data-content/.) I also gathered GDP info from
# https://data.worldbank.org/indicator/NY.GDP.MKTP.CD?end=2014&start=2011 and figured out which countries
# were classified as "low-income" by visiting 
# https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups

library(dplyr)
library(tidyverse)
library(ggplot2)
library(corrplot)
library(PerformanceAnalytics)
library(ggpubr)

refs1 <- read.csv("[filepath]/file.csv",header=TRUE,colClasses="character")
cols <- 2:13
refs1[,cols] <- lapply(refs1[,cols],function(refs1){as.numeric(gsub(",", "", refs1))})
# code from https://intellipaat.com/community/13744/how-to-read-data-when-some-numbers-contain-commas-as-thousand-separator
# One of many ways on how to transform columns being read as "<chr>" due to commas
# and transform them to numeric. (Column 2-7 were being read as <chr> while 8-13 were
# being understood as <dbl>

glimpse(refs1)

# gdp_2011_not_abbrev <- refs1 %>% mutate(X2011.GDP.in.billions.of.US.dollars = X2011.GDP.in.billions.of.US.dollars * 1000000000) 

# example from gapminder exercise: **** gp_pop_mil_added  <- gapminder %>% mutate(pop_mil = pop/1000000)

refs1_ref_apps_combined <- refs1_gdp_over_imf_added %>% mutate(X2011_ref_apps = X2011.government.new.refugee.applications + X2011.UNHCR.new.refugee.applications,
                                                               X2012_ref_apps = X2012.government.new.refugee.applications + X2012.UNHCR.new.refugee.applications,
                                                               X2013_ref_apps = X2013.government.new.refugee.applications + X2013.UNHCR.new.refugee.applications)

glimpse(refs1_ref_apps_combined)

refs1_imf_mil <- refs1_ref_apps_combined %>% mutate(X2011_imf_loan_millions = X2011.IMF.loan / 1000000, 
                                                         X2012_imf_loan_millions = X2012.IMF.loan / 1000000,
                                                         X2013_imf_loan_millions = X2013.IMF.loan / 1000000)

glimpse(refs1_imf_mil)
head(refs1_imf_mil)
GGally::ggpairs(data = refs1_imf_mil, columns = 17:22) # correlation matrix for 
# 2011-2013 refugee applications vs 2011-2013 IMF loans

# If I want to run specific columns ***data_Subset <- data %>% select(cols I need)***
# GGally::ggpairs(data_Subset)

#----------------
# Checking summaries of the columns I'll be looking into:

summary(refs1_imf_mil$X2011_ref_apps)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 110.0   726.8  1456.5  5696.1  4045.0 52138.0 

summary(refs1_imf_mil$X2012_ref_apps)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 107     513    1226    4415    3076   36910 

summary(refs1_imf_mil$X2013_ref_apps)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 161.0   718.8  1576.5  6334.7  5681.5 55998.0 

summary(refs1_imf_mil$X2011.IMF.loan)
#   Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 2.774e+07 1.574e+08 2.437e+08 3.405e+08 3.681e+08 1.271e+09 

summary(refs1_imf_mil$X2012.IMF.loan)
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 2.800e+07 1.725e+08 2.605e+08 3.526e+08 3.715e+08 1.293e+09 

summary(refs1_imf_mil$X2013.IMF.loan)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 3.210e+07 1.789e+08 2.633e+08 3.756e+08 3.694e+08 1.496e+09 


#---- New refugee applications, including outliers:

# New refugee applications for 2011, including outliers 
ggplot(data = refs1_imf_mil) +
  geom_point(mapping = aes(x = refs1_imf_mil$X, y = refs1_imf_mil$X2011_ref_apps)) +
  labs(title = "Low-Income Countries' New Government and UNHCR Refugee Applications in 2011",
       x = "Low-Income Countries", y = "New Refugee Applications") +
  theme_update() +
  theme(axis.text.x = element_text(angle = 90))
# Zimbabwe is a big outlier in 2011 with its combined government and UNHCR new refugee 
# applications at 52,124

# New refugee applications for 2012, including outliers 
ggplot(data = refs1_imf_mil) +
  geom_point(mapping = aes(x = refs1_imf_mil$X, y = refs1_imf_mil$X2012_ref_apps)) +
  labs(title = "Low-Income Countries' New Government and UNHCR Refugee Applications in 2012",
       x = "Low-Income Countries", y = "New Refugee Applications") +
  theme_update() +
  theme(axis.text.x = element_text(angle = 90))

# New refugee applications for 2013, including outliers 
ggplot(data = refs1_imf_mil) +
  geom_point(mapping = aes(x = refs1_imf_mil$X, y = refs1_imf_mil$X2013_ref_apps)) +
  labs(title = "Low-Income Countries' New Government and UNHCR Refugee Applications in 2013",
       x = "Low-Income Countries", y = "New Refugee Applications") +
  theme_update() +
  theme(axis.text.x = element_text(angle = 90))

#--- ggplot visualization of IMF loan amounts abbreviated in millions of US dollars, including outliers:

# ggplot visualization of IMF loan amounts abbreviated in millions of US dollars for 2011, including outliers
ggplot(data = refs1_imf_mil) +
  geom_point(mapping = aes(x = refs1_imf_mil$X, y = refs1_imf_mil$X2011_imf_loan_millions)) +
  labs(title = "Low-Income Countries' IMF Loan Credit Amounts in Millions of US Dollars in 2011",
       x = "Low-Income Countries", y = "IMF Loan Amounts in US Dollars") +
  theme_update() +
  theme(axis.text.x = element_text(angle = 90))

# ggplot visualization of IMF loan amounts abbreviated in millions of US dollars for 2012, including outliers
ggplot(data = refs1_imf_mil) +
  geom_point(mapping = aes(x = refs1_imf_mil$X, y = refs1_imf_mil$X2012_imf_loan_millions)) +
  labs(title = "Low-Income Countries' IMF Loan Credit Amounts in Millions of US Dollars in 2012",
       x = "Low-Income Countries", y = "IMF Loan Amounts in US Dollars") +
  theme_update() +
  theme(axis.text.x = element_text(angle = 90))

# ggplot visualization of IMF loan amounts abbreviated in millions of US dollars for 2013, including outliers
ggplot(data = refs1_imf_mil) +
  geom_point(mapping = aes(x = refs1_imf_mil$X, y = refs1_imf_mil$X2013_imf_loan_millions)) +
  labs(title = "Low-Income Countries' IMF Loan Credit Amounts in Millions of US Dollars in 2013",
       x = "Low-Income Countries", y = "IMF Loan Amounts in US Dollars") +
  theme_update() +
  theme(axis.text.x = element_text(angle = 90))

# -----Checking for outliers via boxplot visualizations:

# checking for outliers via boxplot visualizations for new refugee application amounts for 2011
ggplot(data = refs1_imf_mil, mapping = aes(x = refs1_imf_mil$X2011_imf_loan_millions, 
                                           y = refs1_imf_mil$X2011_ref_app)) +
  geom_boxplot() +
  labs(title = "Checking for Outliers of Low-Income Countries' New Refugee Applications in 2011",
       x = "IMF Loans in Millions of US Dollars", y = "New Government and UNHCR Refugee Applications")

# checking for outliers via boxplot visualizations for new refugee application amounts for 2012
ggplot(data = refs1_imf_mil, mapping = aes(x = refs1_imf_mil$X2012_imf_loan_millions, 
                                           y = refs1_imf_mil$X2012_ref_app)) +
  geom_boxplot() +
  labs(title = "Checking for Outliers of Low-Income Countries' New Refugee Applications in 2012",
       x = "IMF Loans in Millions of US Dollars", y = "New Government and UNHCR Refugee Applications")

# checking for outliers via boxplot visualizations for new refugee application amounts for 2013
ggplot(data = refs1_imf_mil, mapping = aes(x = refs1_imf_mil$X2013_imf_loan_millions, 
                                                y = refs1_imf_mil$X2013_ref_app)) +
  geom_boxplot() +
  labs(title = "Checking for Outliers of Low-Income Countries' New Refugee Applications in 2013",
       x = "IMF Loans in Millions of US Dollars", y = "New Government and UNHCR Refugee Applications")

# ------

# checking for outliers via boxplot visualizations for IMF loan amounts for 2011
ggplot(data = refs1_imf_mil, mapping = aes(x = refs1_imf_mil$X2011_ref_app , 
                                           y = refs1_imf_mil$X2011_imf_loan_millions)) +
  geom_boxplot() +
  labs(title = "Checking for Outliers of Low-Income Countries' IMF Loan Amounts in 2011",
       x = "New Government and UNHCR Refugee Applications", y = "IMF Loans in Millions of US Dollars")

# checking for outliers via boxplot visualizations for IMF loan amounts for 2012
ggplot(data = refs1_imf_mil, mapping = aes(x = refs1_imf_mil$X2012_ref_app , 
                                           y = refs1_imf_mil$X2012_imf_loan_millions)) +
  geom_boxplot() +
  labs(title = "Checking for Outliers of Low-Income Countries' IMF Loan Amounts in 2012",
       x = "New Government and UNHCR Refugee Applications", y = "IMF Loans in Millions of US Dollars")

# checking for outliers via boxplot visualizations for IMF loan amounts for 2013
ggplot(data = refs1_imf_mil, mapping = aes(x = refs1_imf_mil$X2013_ref_app , 
                                           y = refs1_imf_mil$X2013_imf_loan_millions)) +
  geom_boxplot() +
  labs(title = "Checking for Outliers of Low-Income Countries' IMF Loan Amounts in 2013",
       x = "New Government and UNHCR Refugee Applications", y = "IMF Loans in Millions of US Dollars")

#------- checking again for OUTLIERS via code and pasting those numbers into boxplot titles:

# ------------For 2011 ref apps
# looking at the shape of my data to see if it's normalized
hist(refs1_imf_mil$X2011_ref_apps, xlab = 'New Government and UNHCR Refugee Applications for 2011 with Outliers')

# finding the outliers:
boxplot.stats(refs1_imf_mil$X2011_ref_apps)$out
# [1] 25710 12978 19355 16524 52138

out <- boxplot.stats(refs1_imf_mil$X2011_ref_apps)$out
out_ind <- which(refs1_imf_mil$X2011_ref_apps %in% c(out))
out_ind
# [1]  1  2 10 11 30 - these are the outlier rows:
# Aghanistan, Bangladesh, Dem. Rep. of Congo, Ethiopia, Zimbabwe

refs1_ref_apps_combined[out_ind, ]

# boxplot visualization of new refugee applications for 2011 with outliers pasted in
# subtitle:
boxplot(refs1_imf_mil$X2011_ref_apps,
        ylab = "Frequency",
        main = "Boxplot of New Government and UNHCR Refugee Applications for 2011"
)
mtext(paste("Outliers: ", paste(out, collapse = ", ")))

#--------For 2011 IMF loans
# looking at the shape of my data to see if it's normalized
hist(refs1_imf_mil$X2011_imf_loan_millions, xlab = 'New Government and UNHCR Refugee Applications for 2011 with Outliers')

# finding the outliers:
boxplot.stats(refs1_imf_mil$X2011_imf_loan_millions)$out
# [1] 1270.812 1258.404 1099.535

out_2011_imf <- boxplot.stats(refs1_imf_mil$X2011_imf_loan_millions)$out
out_2011_imf_1 <- which(refs1_imf_mil$X2011_imf_loan_millions %in% c(out_2011_imf))
out_2011_imf_1
# 2 10 16

refs1_ref_apps_combined[out_2011_imf_1, ]
# 2       Bangladesh                                                      
# 10 Congo, Dem. Rep                                              
# 16          Kenya 

# boxplot visualization of IMF loans for 2011 with outliers pasted in subtitle:
boxplot(refs1_imf_mil$X2011_imf_loan_millions,
        ylab = "Frequency",
        main = "Boxplot of IMF Loans for 2011"
)
mtext(paste("Outliers: ", paste(out_2011_imf, collapse = ", ")))


#------- Removing outliers for 2011 new refugee applications and IMF loans
# 2011 new ref apps outliers: 1  2 10 11 30 (from line 182):
# Aghanistan, Bangladesh, Dem. Rep. of Congo, Ethiopia, Zimbabwe

# 2011 IMF loan outliers (from line 208):
# # 2       Bangladesh                                                      
# 10 Congo, Dem. Rep                                              
# 16          Kenya 

# redoing the histogram after I remove the outliers 
refs1_no_outliers_2011 <- refs1_imf_mil[-c(1, 2, 10, 11, 16, 30), ]

hist(refs1_no_outliers_2011$X2011_ref_apps) # still right-tailed after taking out outliers
refs1_no_outliers_2011_log <- log(refs1_no_outliers_2011$X2011_ref_apps)
hist(refs1_no_outliers_2011_log) #normalized now

# --- 2011 pearson's correlation test including outliers without using log() function
cor.test(refs1_imf_mil$X2011_ref_apps, 
         refs1_imf_mil$X2011_imf_loan_millions,
         method = "pearson")
# t = 2.112, df = 28, p-value = 0.04374
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.01202441 0.64484028
# sample estimates:
#  cor 
# 0.3706878 

# --- 2011 spearman's correlation test including outliers without using log() function
cor.test(refs1_imf_mil$X2011_ref_apps, 
         refs1_imf_mil$X2011_imf_loan_millions,
         method = "spearman")

# S = 3132, p-value = 0.1035
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#  rho 
# 0.3032258 

# --- 2011 pearson's correlation test including outliers using log() function
cor.test(log(refs1_imf_mil$X2011_ref_apps), 
         log(refs1_imf_mil$X2011_imf_loan_millions),
         method = "pearson")
# t = 2.1827, df = 28, p-value = 0.0376
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.02441319 0.65202230
# sample estimates:
#  cor 
# 0.3813285 

# --- 2011 spearman's correlation test including outliers using log() function
cor.test(log(refs1_imf_mil$X2011_ref_apps), 
         log(refs1_imf_mil$X2011_imf_loan_millions),
         method = "spearman")

# S = 3132, p-value = 0.1035
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#  rho 
# 0.3032258 

# --- 2011 pearson's correlation test without outliers and without using log() function
cor.test(refs1_no_outliers_2011$X2011_ref_apps, 
         refs1_no_outliers_2011$X2011_imf_loan_millions,
         method = "pearson")
# t = -0.13557, df = 22, p-value = 0.8934
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.4273074  0.3789221
# sample estimates:
#  cor 
# -0.02889074 

# --- 2011 spearman's correlation test without outliers and without using log() function
cor.test(refs1_no_outliers_2011$X2011_ref_apps, 
         refs1_no_outliers_2011$X2011_imf_loan_millions,
         method = "spearman")
# S = 2426, p-value = 0.7993
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#  rho 
# -0.05478261 

# --- 2011 pearson's correlation test without outliers and using log() function
cor.test(log(refs1_no_outliers_2011$X2011_ref_apps), 
         log(refs1_no_outliers_2011$X2011_imf_loan_millions),
         method = "pearson")
# t = 0.079307, df = 22, p-value = 0.9375
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.3891447  0.4174557
# sample estimates:
#  cor 
# 0.01690594 

# --- 2011 spearman's correlation test without outliers and using log() function
cor.test(log(refs1_no_outliers_2011$X2011_ref_apps), 
         log(refs1_no_outliers_2011$X2011_imf_loan_millions),
         method = "spearman")
# S = 2426, p-value = 0.7993
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#  rho 
# -0.05478261 

#-------- And checking for OUTLIERS for 2012

# ------------For 2012 ref apps
# looking at the shape of my data to see if it's normalized
hist(refs1_imf_mil$X2012_ref_apps, xlab = 'New Government and UNHCR Refugee Applications for 2012 with Outliers')

# finding the outliers:
boxplot.stats(refs1_imf_mil$X2012_ref_apps)$out
# [1] 36910  9763 16658 13190 21301

out_ref_apps_2012 <- boxplot.stats(refs1_imf_mil$X2012_ref_apps)$out
out_ref_apps_2012_a <- which(refs1_imf_mil$X2012_ref_apps %in% c(out_ref_apps_2012))
out_ref_apps_2012_a
# [1]  1  2 10 11 30 - these are the outlier rows:
# Aghanistan, Bangladesh, Dem. Rep. of Congo, Ethiopia, Zimbabwe

refs1_imf_mil[out_ref_apps_2012_a, ]

# boxplot visualization of new refugee applications for 2011 with outliers pasted in
# subtitle:
boxplot(refs1_imf_mil$X2012_ref_apps,
        ylab = "Frequency",
        main = "Boxplot of New Government and UNHCR Refugee Applications for 2012"
)
mtext(paste("Outliers: ", paste(out_ref_apps_2012, collapse = ", ")))

#--------For 2012 IMF loans
# looking at the shape of my data to see if it's normalized
hist(refs1_imf_mil$X2012_imf_loan_millions, xlab = 'New Government and UNHCR Refugee Applications for 2011 with Outliers')

# finding the outliers:
boxplot.stats(refs1_imf_mil$X2012_imf_loan_millions)$out
# [1] 1212.874 1259.756 1292.973

out_2012_imf <- boxplot.stats(refs1_imf_mil$X2012_imf_loan_millions)$out
out_2012_imf_1 <- which(refs1_imf_mil$X2012_imf_loan_millions %in% c(out_2012_imf))
out_2012_imf_1
# 2 10 16

refs1_imf_mil[out_2012_imf_1, ]
# 2       Bangladesh                                                      
# 10 Congo, Dem. Rep                                              
# 16          Kenya 

# boxplot visualization of IMF loans for 2012 with outliers pasted in subtitle:
boxplot(refs1_imf_mil$X2012_imf_loan_millions,
        ylab = "Frequency",
        main = "Boxplot of IMF Loans in Millions of US Dollars for 2012"
)
mtext(paste("Outliers: ", paste(out_2012_imf, collapse = ", ")))

#------- removing outliers for 2012 new refugee applications and IMF loans
# 2012 new ref apps outliers: # [1]  1  2 10 11 30 (from line 351):
# Aghanistan, Bangladesh, Dem. Rep. of Congo, Ethiopia, Zimbabwe

# 2012 IMF loan outliers (from line 208):
# # 2       Bangladesh                                                      
# 10 Congo, Dem. Rep                                              
# 16          Kenya 

# redoing the histogram after I remove the outliers 
refs1_no_outliers_2012 <- refs1_imf_mil[-c(1, 2, 10, 11, 16, 30), ]

hist(refs1_no_outliers_2012$X2012_ref_apps) # still right-tailed after taking out outliers
refs1_no_outliers_2012_log <- log(refs1_no_outliers_2012$X2012_ref_apps)
hist(refs1_no_outliers_2012_log) #normalized now

# --- 2012 pearson's correlation test including outliers without using log() function
cor.test(refs1_imf_mil$X2012_ref_apps, 
         refs1_imf_mil$X2012_imf_loan_millions,
         method = "pearson")
# t = 1.9564, df = 28, p-value = 0.06046
# alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  -0.0154138  0.6285258
# sample estimates:
#  cor 
# 0.3467811 

# --- 2012 spearman's correlation test including outliers without using log() function
cor.test(refs1_imf_mil$X2012_ref_apps, 
         refs1_imf_mil$X2012_imf_loan_millions,
         method = "spearman")
# S = 2496, p-value = 0.01457
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#  rho 
# 0.4447164 

# --- 2012 pearson's correlation test including outliers using log() function
cor.test(log(refs1_imf_mil$X2012_ref_apps), 
         log(refs1_imf_mil$X2012_imf_loan_millions),
         method = "pearson")
# t = 2.4267, df = 28, p-value = 0.02192
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.0665944 0.6756592
# sample estimates:
#  cor 
# 0.4168623 

# --- 2012 spearman's correlation test including outliers using log() function
cor.test(log(refs1_imf_mil$X2012_ref_apps), 
         log(refs1_imf_mil$X2012_imf_loan_millions),
         method = "spearman")
# S = 2496, p-value = 0.01457
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#  rho 
# 0.4447164 

# --- 2012 pearson's correlation test without outliers and without using log() function
cor.test(refs1_no_outliers_2012$X2012_ref_apps, 
         refs1_no_outliers_2012$X2012_imf_loan_millions,
         method = "pearson")
# t = 0.4716, df = 22, p-value = 0.6419
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.3161125  0.4839092
# sample estimates:
#  cor 
# 0.1000413 

# --- 2012 spearman's correlation test without outliers and without using log() function
cor.test(refs1_no_outliers_2012$X2012_ref_apps, 
         refs1_no_outliers_2012$X2012_imf_loan_millions,
         method = "spearman")
# S = 1946, p-value = 0.471
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#  rho 
# 0.153913 

# --- 2012 pearson's correlation test without outliers and using log() function
cor.test(log(refs1_no_outliers_2012$X2012_ref_apps), 
         log(refs1_no_outliers_2012$X2012_imf_loan_millions),
         method = "pearson")
# t = 0.42213, df = 22, p-value = 0.677
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.3255311  0.4758277
# sample estimates:
#  cor 
# 0.08963646 

# --- 2012 spearman's correlation test without outliers and using log() function
cor.test(log(refs1_no_outliers_2012$X2012_ref_apps), 
         log(refs1_no_outliers_2012$X2012_imf_loan_millions),
         method = "spearman")
# S = 1946, p-value = 0.471
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#  rho 
# 0.153913 

#-------- And checking for OUTLIERS for 2013

# ------------For 2013 ref apps
# looking at the shape of my data to see if it's normalized
hist(refs1_imf_mil$X2013_ref_apps, xlab = 'New Government and UNHCR Refugee Applications for 2013 with Outliers')

# finding the outliers:
boxplot.stats(refs1_imf_mil$X2013_ref_apps)$out
# [1] 31964 13656 17927 55998 17060

out_ref_apps_2013 <- boxplot.stats(refs1_imf_mil$X2013_ref_apps)$out
out_ref_apps_2013_a <- which(refs1_imf_mil$X2013_ref_apps %in% c(out_ref_apps_2013))
out_ref_apps_2013_a
# [1]  1  2 10 22 30 - these are the outlier rows:

refs1_imf_mil[out_ref_apps_2013_a, ]
# 1      Afghanistan                                                                     
# 2       Bangladesh                                                                        
# 10 Congo, Dem. Rep                                                                    
# 22         Myanmar                                                                       
# 30        Zimbabwe 

# boxplot visualization of new refugee applications for 2013 with outliers pasted in
# subtitle:
boxplot(refs1_imf_mil$X2013_ref_apps,
        ylab = "Frequency",
        main = "Boxplot of New Government and UNHCR Refugee Applications for 2013"
)
mtext(paste("Outliers: ", paste(out_ref_apps_2013, collapse = ", ")))

#--------For 2013 IMF loans
# looking at the shape of my data to see if it's normalized
hist(refs1_imf_mil$X2013_imf_loan_millions, xlab = 'New Government and UNHCR Refugee Applications for 2013 with Outliers')

# finding the outliers:
boxplot.stats(refs1_imf_mil$X2013_imf_loan_millions)$out
# [1] 1496.420 1262.281 1470.880  753.401

out_2013_imf <- boxplot.stats(refs1_imf_mil$X2013_imf_loan_millions)$out
out_2013_imf_1 <- which(refs1_imf_mil$X2013_imf_loan_millions %in% c(out_2013_imf))
out_2013_imf_1
# 2 10 16 27

refs1_imf_mil[out_2013_imf_1, ]
# 2       Bangladesh   
# 10 Congo, Dem. Rep  
# 16          Kenya  
# 27        Tanzania 

# boxplot visualization of IMF loans for 2013 with outliers pasted in subtitle:
boxplot(refs1_imf_mil$X2013_imf_loan_millions,
        ylab = "Frequency",
        main = "Boxplot of IMF Loans in Millions of US Dollars for 2013"
)
mtext(paste("Outliers: ", paste(out_2013_imf, collapse = ", ")))

#------- removing outliers for 2013 new refugee applications and IMF loans
# 2013 new ref apps outliers: (from line 508):
# 1      Afghanistan                                                                     
# 2       Bangladesh                                                                        
# 10 Congo, Dem. Rep                                                                    
# 22         Myanmar                                                                       
# 30        Zimbabwe 

# 2013 IMF loan outliers (from line 536):
# 2       Bangladesh   
# 10 Congo, Dem. Rep  
# 16          Kenya  
# 27        Tanzania 

# redoing the histogram after I remove the outliers 
refs1_no_outliers_2013 <- refs1_imf_mil[-c(1, 2, 10, 16, 22, 27, 30), ]

hist(refs1_no_outliers_2013$X2013_ref_apps) # still right-tailed after taking out outliers
refs1_no_outliers_2013_log <- log(refs1_no_outliers_2013$X2013_ref_apps)
hist(refs1_no_outliers_2013_log) #normalized now

# --- 2013 pearson's correlation test including outliers without using log() function
cor.test(refs1_imf_mil$X2013_ref_apps, 
         refs1_imf_mil$X2013_imf_loan_millions,
         method = "pearson")
# t = 1.2404, df = 28, p-value = 0.2251
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.1438803  0.5437784
# sample estimates:
#  cor 
# 0.2282187 

# --- 2013 spearman's correlation test including outliers without using log() function
cor.test(refs1_imf_mil$X2013_ref_apps, 
         refs1_imf_mil$X2013_imf_loan_millions,
         method = "spearman")
# S = 2636, p-value = 0.02393
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#  rho 
# 0.4135706 

# --- 2013 pearson's correlation test including outliers using log() function
cor.test(log(refs1_imf_mil$X2013_ref_apps), 
         log(refs1_imf_mil$X2013_imf_loan_millions),
         method = "pearson")
# t = 2.2641, df = 28, p-value = 0.03151
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.03857679 0.66009752
# sample estimates:
#  cor 
# 0.3933788 

# --- 2013 spearman's correlation test including outliers using log() function
cor.test(log(refs1_imf_mil$X2013_ref_apps), 
         log(refs1_imf_mil$X2013_imf_loan_millions),
         method = "spearman")
# S = 2636, p-value = 0.02393
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#  rho 
# 0.4135706 

# --- 2013 pearson's correlation test without outliers and without using log() function
cor.test(refs1_no_outliers_2013$X2013_ref_apps, 
         refs1_no_outliers_2013$X2013_imf_loan_millions,
         method = "pearson")
# t = 2.8257, df = 21, p-value = 0.01013
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.1437574 0.7703885
# sample estimates:
#  cor 
# 0.524858 

# --- 2013 spearman's correlation test without outliers and without using log() function
cor.test(refs1_no_outliers_2013$X2013_ref_apps, 
         refs1_no_outliers_2013$X2013_imf_loan_millions,
         method = "spearman")
# S = 1414, p-value = 0.162
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#  rho 
# 0.3013834 

# --- 2013 pearson's correlation test without outliers and using log() function
cor.test(log(refs1_no_outliers_2013$X2013_ref_apps), 
         log(refs1_no_outliers_2013$X2013_imf_loan_millions),
         method = "pearson")
# t = 1.2282, df = 21, p-value = 0.233
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.1716481  0.6063686
# sample estimates:
#  cor 
# 0.2588701 

# --- 2013 spearman's correlation test without outliers and using log() function
cor.test(log(refs1_no_outliers_2013$X2013_ref_apps), 
         log(refs1_no_outliers_2013$X2013_imf_loan_millions),
         method = "spearman")
# S = 1414, p-value = 0.162
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#  rho 
# 0.3013834 


#--------- looking at QQPLOTs with QQLINEs also to check the shape of my data

#----qqnorm for 2011 data refugee applications including outliers
qqnorm(refs1_imf_mil$X2011_ref_apps) # qqplot of X2011_ref_apps
qqline(refs1_imf_mil$X2011_ref_apps)

ref2011log <- log(refs1_imf_mil$X2011_ref_apps)
qqnorm(ref2011log) # now the qqplot for X2011_ref_apps has become normalized with log()
qqline(ref2011log)

hist(ref2011log, xlab = 'New Government and UNHCR Refugee Applications for 2011 without outliers') 
#transformed histogram for X2011_ref_apps

#----qqnorm for 2011 IMF loans including outliers
qqnorm(refs1_imf_mil$X2011_imf_loan_millions)
qqline(refs1_imf_mil$X2011_imf_loan_million)

imf2011log <- log(refs1_imf_mil$X2011_imf_loan_million)
qqnorm(imf2011log) # now the qqplot has become normalized with log()
qqline(imf2011log)

hist(ref2011log, xlab = 'IMF Loans for 2011 without outliers') 
#transformed histogram 

#---qqnorm for 2011 data refugee applications without outliers
qqnorm(refs1_no_outliers_2011$X2011_ref_apps) 
qqline(refs1_no_outliers_2011$X2011_ref_apps)

ref_no_outliers_2011_log <- log(refs1_no_outliers_2011$X2011_imf_loan_million)
qqnorm(ref_no_outliers_2011_log) # now the qqplot has become normalized with log()
qqline(ref_no_outliers_2011_log)

hist(ref_no_outliers_2011_log, xlab = 'New Government and UNHCR Refugee Applications for 2011 without outliers') 
#transformed histogram 

#---qqnorm for 2011 IMF loans without outliers
qqnorm(refs1_no_outliers_2011$X2011_imf_loan_millions) 
qqline(refs1_no_outliers_2011$X2011_imf_loan_millions)

ref_no_outliers_imf_2011_log <- log(refs1_no_outliers_2011$X2011_imf_loan_million)
qqnorm(ref_no_outliers_imf_2011_log) # now the qqplot has become normalized with log()
qqline(ref_no_outliers_imf_2011_log)

hist(ref_no_outliers_imf_2011_log, xlab = 'IMF Loans for 2011 without outliers') 
#transformed histogram 

#----qqnorm for 2012 data refugee applications including outliers
qqnorm(refs1_imf_mil$X2012_ref_apps) # qqplot of X2012_ref_apps
qqline(refs1_imf_mil$X2012_ref_apps)

ref2012log <- log(refs1_imf_mil$X2012_ref_apps)
qqnorm(ref2012log) # now the qqplot for X2012_ref_apps has become normalized with log()
qqline(ref2012log)

hist(ref2012log, xlab = 'New Government and UNHCR Refugee Applications for 2012') 
#transformed histogram for X2012_ref_apps

#----qqnorm for 2012 IMF loans including outliers
qqnorm(refs1_imf_mil$X2012_imf_loan_millions)
qqline(refs1_imf_mil$X2012_imf_loan_million)

imf2012log <- log(refs1_imf_mil$X2012_imf_loan_million)
qqnorm(imf2012log) # now the qqplot has become normalized with log()
qqline(imf2012log)

hist(ref2012log, xlab = 'IMF Loans for 2012') 
#transformed histogram 

#---qqnorm for 2012 data refugee applications without outliers
qqnorm(refs1_no_outliers_2012$X2012_ref_apps) 
qqline(refs1_no_outliers_2012$X2012_ref_apps)

ref_no_outliers_2012_log <- log(refs1_no_outliers_2012$X2012_ref_apps)
qqnorm(ref_no_outliers_2012_log) # now the qqplot has become normalized with log()
qqline(ref_no_outliers_2012_log)

hist(ref_no_outliers_2012_log, xlab = 'New Government and UNHCR Refugee Applications for 2012 without outliers') 
#transformed histogram 

#---qqnorm for 2012 IMF loans without outliers
qqnorm(refs1_no_outliers_2012$X2012_imf_loan_millions) 
qqline(refs1_no_outliers_2012$X2012_imf_loan_millions)

ref_no_outliers_imf_2012_log <- log(refs1_no_outliers_2012$X2012_imf_loan_million)
qqnorm(ref_no_outliers_imf_2012_log) # now the qqplot has become normalized with log()
qqline(ref_no_outliers_imf_2012_log)

hist(ref_no_outliers_imf_2012_log, xlab = 'IMF Loans for 2012 without outliers') 
#transformed histogram 

#----qqnorm for 2013 data refugee applications including outliers
qqnorm(refs1_imf_mil$X2013_ref_apps) 
qqline(refs1_imf_mil$X2013_ref_apps)

ref2013log <- log(refs1_imf_mil$X2013_ref_apps)
qqnorm(ref2013log) 
qqline(ref2013log)

hist(ref2013log, xlab = 'New Government and UNHCR Refugee Applications for 2013 without outliers') 
#transformed histogram 

#----qqnorm for 2013 IMF loans including outliers
qqnorm(refs1_imf_mil$X2013_imf_loan_millions)
qqline(refs1_imf_mil$X2013_imf_loan_millions)

imf2013log <- log(refs1_imf_mil$X2013_imf_loan_million)
qqnorm(imf2013log) # now the qqplot has become normalized with log()
qqline(imf2013log)

hist(ref2013log, xlab = 'IMF Loans for 2013 without outliers') 
#transformed histogram 

#---qqnorm for 2013 data refugee applications without outliers
qqnorm(refs1_no_outliers_2013$X2013_ref_apps) 
qqline(refs1_no_outliers_2013$X2013_ref_apps)

ref_no_outliers_2013_log <- log(refs1_no_outliers_2013$X2013_imf_loan_millions)
qqnorm(ref_no_outliers_2013_log) # now the qqplot has become normalized with log()
qqline(ref_no_outliers_2013_log)

hist(ref_no_outliers_2013_log, xlab = 'New Government and UNHCR Refugee Applications for 2013 without outliers') 
#transformed histogram 

#---qqnorm for 2013 IMF loans without outliers
qqnorm(refs1_no_outliers_2013$X2013_imf_loan_millions) 
qqline(refs1_no_outliers_2013$X2013_imf_loan_millions)

ref_no_outliers_imf_2013_log <- log(refs1_no_outliers_2013$X2013_imf_loan_million)
qqnorm(ref_no_outliers_imf_2013_log) # now the qqplot has become normalized with log()
qqline(ref_no_outliers_imf_2013_log)

hist(ref_no_outliers_imf_2013_log, xlab = 'IMF Loans for 2013 without outliers') 
#transformed histogram 
#---------
# Here's some model code for plotting text on a graph:

nameplot1 <- ggplot(refs1_2011_no_ref_gdpimf_outliers, aes(X2011_gdp_over_imf, X2011_ref_apps, 
                                                            label = rownames(refs1_2011_no_ref_gdpimf_outliers)))
nameplot1 + geom_text(aes(label = refs1_2011_no_ref_gdpimf_outliers$X)) 
 
# Remember that if I want to make sure the text on the graph don't overlap, use the 
# check_overlap function, and if not then erase it:
nameplot3 + geom_text(aes(label = refs1_no_outliers_2011$X), check_overlap = TRUE) 

nameplot2 <- ggplot(refs1_imf_mil, aes(y = log(X2011_ref_apps), x = log(X2011_imf_loan_millions))) + 
  #geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  labs(title = "Low-Income Countries' New Refugee Applications vs. IMF Loans in 2011",
       x = "Low-Income Countries' IMF Loans in Millions of US Dollars", 
       y = "New Government and UNHCR Refugee Applications")

nameplot2 + geom_text(aes(label = refs1_imf_mil$X), check_overlap = TRUE)
            
            
nameplot3 <- ggplot(refs1_no_outliers_2011, aes(y = X2011_ref_apps, x = X2011_imf_loan_millions)) + 
  #geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  labs(title = "Low-Income Countries' New Refugee Applications vs. IMF Loans in 2011 Without Outliers",
       x = "IMF Loans in Millions of US Dollars", 
       y = "New Government and UNHCR Refugee Applications")

nameplot3 + geom_text(aes(label = refs1_no_outliers_2011$X), check_overlap = TRUE) 

# the same visualization with the linear regression model equation and associated 
# r-squared value presented on the graph:
nameplot3 + geom_text(aes(label = refs1_no_outliers_2011$X), check_overlap = TRUE) +
  stat_regline_equation(label.x = 450, label.y = 6000, size = 6) +
  stat_cor(aes(label=..rr.label..), label.x = 450, label.y = 5700, size = 6)

# Note that the regression model equation that appears on the above visualization is
# "y = 1900 - 0.41x" rather than "y = 1898.75 - 0.4126x" because the function 
# stat_regline_equation() rounds to two significant digits, which is hard-coded into the
# function. However, according to 
# https://stackoverflow.com/questions/66177005/im-using-stat-regline-equation-with-ggscatter-is-there-a-way-to-specify-the-si,
# "you can enter trace(ggpubr:::.stat_lm, edit = TRUE) into the console and modify the 
# function's code in the pop-up window. The lines you want to change are lines 13-14."
# In any case, if we wanted to see how the linear model from line 825 was built:
lm_2011_ref_imf_no_outliers <- lm(refs1_no_outliers_2011$X2011_ref_apps ~ refs1_no_outliers_2011$X2011_imf_loan_millions, 
                      data = refs1_no_outliers_2011)
# remember: y axis goes first, then ~ x axis
lm_2011_ref_imf_no_outliers
# Coefficients:
# (Intercept)  refs1_no_outliers_2011$X2011_imf_loan_millions  
# 1898.7510                                         -0.4126  

# This means that the formula for 2011 is 
# ***Refugee Applications = 1898.751 + (-0.4126 * IMF loan in millions of US dollars) 
# This is the slope and intercept, of the 'm' and 'b' in the formula y = b + mx

#--------

nameplot4 <- ggplot(refs1_no_outliers_2012, aes(y = X2012_ref_apps, x = X2012_imf_loan_millions)) + 
  #geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  labs(title = "Low-Income Countries' New Refugee Applications vs. IMF Loans in 2012 Without Outliers",
       x = "IMF Loans in Millions of US Dollars", 
       y = "New Government and UNHCR Refugee Applications")

nameplot4 + geom_text(aes(label = refs1_no_outliers_2012$X), check_overlap = TRUE) 

# the same visualization with the linear regression model equation and associated 
# r squared value presented on the graph:
nameplot4 + geom_text(aes(label = refs1_no_outliers_2012$X), check_overlap = TRUE) +
  stat_regline_equation(label.x = 450, label.y = 4300, size = 6) +
  stat_cor(aes(label=..rr.label..), label.x = 450, label.y = 4100, size = 6)

??stat_regline_equation


# Note that the regression model equation that appears on the above visualization is
# "y = 1200 + 1.1x" rather than "y = 1165.733 + 1.069x" because the function 
# stat_regline_equation() rounds to two significant digits, which is hard-coded into the
# function. However, according to 
# https://stackoverflow.com/questions/66177005/im-using-stat-regline-equation-with-ggscatter-is-there-a-way-to-specify-the-si,
# "you can enter trace(ggpubr:::.stat_lm, edit = TRUE) into the console and modify the 
# function's code in the pop-up window. The lines you want to change are lines 13-14."
# In any case, if we wanted to see how the linear model from line 825 was built:
lm_2012_ref_imf_no_outliers <- lm(refs1_no_outliers_2012$X2012_ref_apps ~ refs1_no_outliers_2012$X2012_imf_loan_millions, 
                                  data = refs1_no_outliers_2012)
# remember: y axis goes first, then ~ x axis
lm_2012_ref_imf_no_outliers
# Coefficients:
# (Intercept)  refs1_no_outliers_2012$X2012_imf_loan_millions  
# 1165.733                                           1.069  

# This means that the formula for 2011 is 
# ***Refugee Applications = 1165.733 + (1.069 * IMF loan in millions of US dollars) 
# This is the slope and intercept, of the 'm' and 'b' in the formula y = b + mx

#----

nameplot5 <- ggplot(refs1_no_outliers_2013, aes(y = X2013_ref_apps, x = X2013_imf_loan_millions)) + 
  #geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  labs(title = "Low-Income Countries' New Refugee Applications vs. IMF Loans in 2013 Without Outliers",
       x = "IMF Loans in Millions of US Dollars", 
       y = "New Government and UNHCR Refugee Applications")

nameplot5 + geom_text(aes(label = refs1_no_outliers_2013$X), check_overlap = TRUE) 

# the same visualization with the linear regression model equation and associated 
# r squared value presented on the graph:
nameplot5 + geom_text(aes(label = refs1_no_outliers_2013$X), check_overlap = TRUE) +
  stat_regline_equation(label.x = 350, label.y = 8200, size = 6) +
  stat_cor(aes(label=..rr.label..), label.x = 350, label.y = 7800, size = 6)

??stat_regline_equation


# Note that the regression model equation that appears on the above visualization is
# "y = -460 + 12x" rather than "y = -456.03 + 12.49x" because the function 
# stat_regline_equation() rounds to two significant digits, which is hard-coded into the
# function. However, according to 
# https://stackoverflow.com/questions/66177005/im-using-stat-regline-equation-with-ggscatter-is-there-a-way-to-specify-the-si,
# "you can enter trace(ggpubr:::.stat_lm, edit = TRUE) into the console and modify the 
# function's code in the pop-up window. The lines you want to change are lines 13-14."
# In any case, if we wanted to see how the linear model from line 825 was built:
lm_2013_ref_imf_no_outliers <- lm(refs1_no_outliers_2013$X2013_ref_apps ~ refs1_no_outliers_2013$X2013_imf_loan_millions, 
                                  data = refs1_no_outliers_2013)
# remember: y axis goes first, then ~ x axis
lm_2013_ref_imf_no_outliers
# Coefficients:
# (Intercept)  refs1_no_outliers_2013$X2013_imf_loan_millions  
# -456.03                                           12.49  

# This means that the formula for 2011 is 
# ***Refugee Applications = -456.03 + (12.49 * IMF loan in millions of US dollars) 
# This is the slope and intercept, of the 'm' and 'b' in the formula y = b + mx

#---------

??ggqqplot
ggpubr::ggqqplot(refs1_no_outliers_2011$X2011_ref_apps, ylab = "2011 Refugee Applications without Outliers")
ggpubr::ggqqplot(refs1_imf_mil$X2011_ref_apps, ylab = "2011 Refugee Applications")
# Visual inspection of the data normality using Q-Q plots (quantile-quantile plots)
# Q-Q plot draws the correlation between a given sample and the normal distribution
# from http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r

-------- # Other ggplot ideas 
  
# ggplot of new refugee applications vs. IMF loans in 2011 using refs1_imf_mil:
# (for this one, the colors are hard to differentiate)
ggplot(refs1_imf_millions, aes(x = refs1_imf_mil$X2011_imf_loan_millions, y = refs1_imf_mil$X2011_ref_apps, 
                               color = refs1_imf_mil$X)) +
  geom_point(size = 2) +
  #geom_smooth(method=lm, se=FALSE) +
  stat_smooth(method = "lm", se = FALSE) +
  labs(title = "Low-Income Countries' New Refugee Applications vs. IMF Loans in 2011",
       x = "IMF Loans in Millions of US Dollars", y = "New Government and UNHCR Refugee Applications in 2011",
       col="Low-Income Countries")


# 2011 ggplot with linear model of ref apps and imf loans without 7 outliers
ggplot(refs1_no_outliers_2011, aes(y = X2011_ref_apps, x = X2011_imf_loan_millions)) + 
  geom_point(mapping = aes(color = X)) +
  stat_smooth(method = "lm") +
  labs(title = "Low-Income Countries' New Refugee Applications vs IMF Loans in 2011",
       x = "IMF Loans in Millions of US Dollars", 
       y = "New Government and UNHCR Refugee Applications",
       col = "Low-income countries")
# the gray shading is our confidence interval for our regression line. Says we're
# 95% confident that our true regression line lies within this shaded region

