################################################
#####    MULTIVARIATE DATA AND ANALYSIS    #####
#####               EXERCISES              #####
################################################

##### QUESTION #1
# Find the correlation matrix and covariance 
# matrix of the age, IQ and weight variables 
# in the hypo data after filling in the missing 
# values with mean replacements (i.e. the mean 
# of that column for the existing data).

hypo <- data.frame(age=c(21,43,22,86,60,16,NA,43,22,80),
                   IQ=c(120,NA,135,150,92,130,150,NA,84,70),
                   weight=c(150,160,135,140,110,110,120,120,105,100))

##### QUESTION #2
# Create and examine both the normal probability 
# plots of each variable in the archaeology data 
# and the chi-square plot of the data. Do the 
# plots suggest anything unusual about the data?

# load the data, check it out
data("pottery", package = "HSAUR2")

##### QUESTION #3
# Convert this covariance matrix into the 
# corresponding correlation matrix:

# 3.8778  2.8110	3.1480	3.5062
# 2.8110	2.1210	2.2669	2.5690
# 3.1480	2.2669	2.6550	2.8341
# 3.5062	2.5690	2.8341	3.2352


