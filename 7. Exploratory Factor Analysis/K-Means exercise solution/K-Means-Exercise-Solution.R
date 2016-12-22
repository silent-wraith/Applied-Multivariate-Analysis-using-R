###################################
#####    CLUSTER  ANALYSIS    #####
####  K-MEANS EXERCISE SOLUTION ###
###################################

# Apply k-means to the crime rate data after 
# standardizing each variable by its standard 
# deviation. Compare the results with those 
# demonstrated live in the online session 
# found by standardizing by a variable's range.

# Will need library 'MVA'
library("MVA")

# Cluster Analysis is an exploratory technique.

# Cluster analysis is a generic term for 
# a wide range of numerical methods with
# common goal of uncovering or discovering 
# groups or clusters of observations that
# are homogeneous, separate from other groups. 

#####  CLUSTER ANALYSIS

#####   K-Means Clustering

### SEE SLIDES

# Algorithms designed to search for
# minimum values of the clustering
# criterion by rearranging existing 
# partitions and keeping the new one
# only if it provides an improvement.

# These algorithms do not guarantee
# finding the global minimum of the
# criterion.

# Essential steps in algorithm are:

# 1) Find some initial partition of the
# individuals into the required number
# of groups.

# 2) Calculate the change in the
# clustering criterion produced by
# "moving" each individual from its own
# cluster to another cluster.

# 3) Make the change that leads to the
# the greatest improvement in the value
# of the clustering criterion.

# 4) Repeat steps (2) and (3) until no
# move of an individual causes the
# clustering criterion to improve.

### Clustering the States of the USA on
### the Basis of Crime Rate Profiles

# Rates of different types of crime per
# 100,000 residents of the 50 states
# for year 1986. See data in slides.

`crime` <-
structure(c(2, 2.2, 2, 3.6, 3.5, 4.6, 10.7, 5.2, 5.5, 5.5, 6,
8.9, 11.3, 3.1, 2.5, 1.8, 9.2, 1, 4, 3.1, 4.4, 4.9, 9, 31, 7.1,
5.9, 8.1, 8.6, 11.2, 11.7, 6.7, 10.4, 10.1, 11.2, 8.1, 12.8,
8.1, 13.5, 2.9, 3.2, 5.3, 7, 11.5, 9.3, 3.2, 12.6, 5, 6.6, 11.3,
8.6, 4.8, 14.8, 21.5, 21.8, 29.7, 21.4, 23.8, 30.5, 33.2, 25.1,
38.6, 25.9, 32.4, 67.4, 20.1, 31.8, 12.5, 29.2, 11.6, 17.7, 24.6,
32.9, 56.9, 43.6, 52.4, 26.5, 18.9, 26.4, 41.3, 43.9, 52.7, 23.1,
47, 28.4, 25.8, 28.9, 40.1, 36.4, 51.6, 17.3, 20, 21.9, 42.3,
46.9, 43, 25.3, 64.9, 53.4, 51.1, 44.9, 72.7, 31, 28, 24, 22,
193, 119, 192, 514, 269, 152, 142, 90, 325, 301, 73, 102, 42,
170, 7, 16, 51, 80, 124, 304, 754, 106, 41, 88, 99, 214, 367,
83, 208, 112, 65, 80, 224, 107, 240, 20, 21, 22, 145, 130, 169,
59, 287, 135, 206, 343, 88, 106, 102, 92, 103, 331, 192, 205,
431, 265, 176, 235, 186, 434, 424, 162, 148, 179, 370, 32, 87,  
184, 252, 241, 476, 668, 167, 99, 354, 525, 319, 605, 222, 274, 
408, 172, 278, 482, 285, 354, 118, 178, 243, 329, 538, 437, 180,
354, 244, 286, 521, 401, 103, 803, 755, 949, 1071, 1294, 1198,
1221, 1071, 735, 988, 887, 1180, 1509, 783, 1004, 956, 1136,
385, 554, 748, 1188, 1042, 1296, 1728, 813, 625, 1225, 1340,
1453, 2221, 824, 1325, 1159, 1076, 1030, 1461, 1787, 2049, 783,
1003, 817, 1792, 1845, 1908, 915, 1604, 1861, 1967, 1696, 1162,
1339, 2347, 2208, 2697, 2189, 2568, 2758, 2924, 2822, 1654, 2574,
2333, 2938, 3378, 2802, 2785, 2801, 2500, 2049, 1939, 2677, 3008,
3090, 2978, 4131, 2522, 1358, 2423, 2846, 2984, 4373, 1740, 2126,
2304, 1845, 2305, 3417, 3142, 3987, 3314, 2800, 3078, 4231, 3712,
4337, 4074, 3489, 4267, 4163, 3384, 3910, 3759, 164, 228, 181,
906, 705, 447, 637, 776, 354, 376, 328, 628, 800, 254, 288, 158,
439, 120, 99, 168, 258, 272, 545, 975, 219, 169, 208, 277, 430,
598, 193, 544, 267, 150, 195, 442, 649, 714, 215, 181, 169, 486,
343, 419, 223, 478, 315, 402, 762, 604, 328), .Dim = c(51L, 7L
), .Dimnames = list(c("ME", "NH", "VT", "MA", "RI", "CT", "NY",
"NJ", "PA", "OH", "IN", "IL", "MI", "WI", "MN", "IA", "MO", "ND",
"SD", "NE", "KS", "DE", "MD", "DC", "VA", "WV", "NC", "SC", "GA",
"FL", "KY", "TN", "AL", "MS", "AR", "LA", "OK", "TX", "MT", "ID",
"WY", "CO", "NM", "AZ", "UT", "NV", "WA", "OR", "CA", "AK", "HI"
), c("Murder", "Rape", "Robbery", "Assault", "Burglary", "Theft",
"Vehicle")))

# roll it all up into a dataframe
# Note all data is numeric
crime <- as.data.frame(crime)

# We look at a scatterplot matrix
plot(crime, pch = ".", cex = 3)

# one of the cities is an outlier, at
# least in terms of murder rate, is DC:
subset(crime, Murder > 15)

# Is DC only high in murder? Or in other
# crimes as well?

# we call the scatterplot again, labelling DC
plot(crime, pch = c(".", "+")[(rownames(crime) == "DC") + 1], cex = 1.5)

# We apply K-Means clustering to the crime
# rate data after removing the outlier, DC.

crime2 <- crime[-24,]

# But if we first calculate the variances
# of the crime rates for the different types
# of crimes we find the following:
sapply(crime2, var)

# The variances are very different, so using
# K-means on the raw data would not be
# sensible; so we standardize each variables.
# But how? By its standard deviation.
crime_sd <- scale(crime2, 
                  center = TRUE, 
                  scale = TRUE)

# all variables have variance of one
cov(crime_sd)

# The variances are now very similar, so
# we proceed with clustering the data.

### DATA SCALED BY STANDARD DEVIATION

# The "elbow" occurs at two groups, so we
# look at the 2-group solution:
kmeans(crime_sd, centers = 2)$centers
?kmeans
# We plot the two-group solution in the
# space of the first two principal components
# of the correlation matrix of the data.

crimesd_pca <- prcomp(crime_sd)
plot(crimesd_pca$x[, 1:2], 
     pch = kmeans(crime_sd, 
                  centers = 2)$cluster)

### What we said first time:

### This time it looks similar, the scales
### on the axes have changed, that's all

# The two groups are created essentially 
# on the basis of the first principal 
# component score, which is a weighted
# average of the crime rates. Perhaps all 
# the cluster analysis is doing here is 
# dividing into two parts a homogenous 
# set of data.