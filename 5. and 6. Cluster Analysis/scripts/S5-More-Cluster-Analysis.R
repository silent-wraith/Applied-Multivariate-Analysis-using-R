###################################
#####    CLUSTER  ANALYSIS    #####
###################################

###################################
#####  Create Measure data (again)
###################################
measure <-
  structure(list(V1 = 1:20, 
                 V2 = c(34L, 37L, 38L, 36L, 38L, 
                        43L, 40L, 38L, 40L, 41L, 
                        36L, 36L, 34L, 33L, 36L, 
                        37L, 34L, 36L, 38L, 35L), 
                 V3 = c(30L, 32L, 30L, 33L, 29L, 
                        32L, 33L, 30L, 30L, 32L, 
                        24L, 25L, 24L, 22L, 26L, 
                        26L, 25L, 26L, 28L, 23L), 
                 V4 = c(32L, 37L, 36L, 39L, 33L, 
                        38L, 42L, 40L, 37L, 39L, 
                        35L, 37L, 37L, 34L, 38L, 
                        37L, 38L, 37L, 40L, 35L)), 
            .Names = c("V1", "V2", "V3", "V4"), 
            class = "data.frame", 
            row.names = c(NA, -20L))
measure <- measure[,-1]
names(measure) <- c("chest", "waist", "hips")
measure$gender <- gl(2, 10)
levels(measure$gender) <- c("male", "female")

# Will need library 'MVA'
library("MVA")

# Cluster Analysis is an exploratory technique.

# Cluster analysis is a generic term for 
# a wide range of numerical methods with
# common goal of uncovering or discovering 
# groups or clusters of observations that
# are homogeneous, separate from other groups. 

# Clustering techniques serve to formalize
# what human observers do so well in two or
# three dimensions. 

# Look at the following scatterplot.
# The conclusion that there are three
# natural groups or clusters of dots comes
# with little conscious effort or thought. 

# Bivariate data showing presence 
# of three clusters

library("mvtnorm") # This package
# computes multivariate normal and 
# t probabilities,quantiles, random 
# deviates and densities.

?rmvnorm

# creating three centers on 55 points
dat <- rbind(rmvnorm(25, mean = c(3,3)),
             rmvnorm(20, mean = c(10,8)),
             rmvnorm(10, mean = c(20,1)))

# plots them
plot(abs(dat), xlab = expression(x[1]), 
     ylab = expression(x[2]))

# We will talk about three types of clustering
# methods: agglomerative hierarchical 
# techniques, k-means, and model-based

### Agglomerative Hierarchical Clustering
# This class of clustering methods produces 
# a hierarchical classification of data.

# The data are not partitioned into a 
# particular number of classes or groups at 
# a single step. Instead the classification 
# consists of a series of partitions that 
# may run from a single "cluster" containing 
# all individuals to n clusters, each 
# containing a single individual. 

# Agglomerative hierarchical clustering 
# techniques produce partitions by a series 
# of successive fusions of the n individuals 
# into groups. With such methods, fusions, 
# once made, are irreversible, so that when 
# an agglomerative algorithm has placed
# two individuals in the same group they 
# cannot subsequently appear in different
# groups. 

# Since all agglomerative hierarchical 
# techniques ultimately reduce the data to a
# single cluster containing all the individuals,
# the investigator seeking the solution must
# decide which division to choose.

# But before the process can begin, an 
# inter-individual distance matrix or
# similarity matrix needs to be calculated. 
# There are many ways to calculate
# distances or similarities between pairs 
# of individuals, but here we only deal
# with Euclidean distance measures.

### Agglomerative Hierarchical 
### Clustering Procedure
# produces a series of partitions of the data,
# Pn, Pn-1,..., P1. The first, Pn, consists of
# n single-member clusters, and the last, P1,
# consists of a single group containing all
# n individuals.

# All methods operate similarly:
# (START) Clusters C1, C2,..., Cn each
# containing a single individual.
# (1) Find the nearest pair of distinct
# clusters, say Ci and Cj, merge them,
# delete Cj, and decrease number of
# clusters by one.
# (2) If the number of clusters equals
# one, then stop; otherwise return to (1).

# But first need an inter-individual
# distance matrix or similarity matrix so
# we use Euclidean distance.

#######################################
### Inter-cluster distance measures
#######################################

# single linkage minimizes distance;
# is invariant under monotone transformations
# of original inter-individual distances
# because is based on rank of distance.

# complete linkage maximizes distance;
# is invariant under monotone transformations
# of original inter-individual distances
# because is based on rank of distance.

# group average linkage takes average;
# is not invariant under monotone transforms.

# Three plots to show three techniques to
# determine inter-cluster distance measures
# in Agglomerative Hierarchical Clustering

# First plot is single linkage (closest) demo
par(mfrow=c(1,3))
set.seed(29)
x1 <- c(0.7, 0.8, 0.85, 0.9, 1.1, 1, 0.95);x1
# makes x1 vector twice as long
x <- c(x1, x1 + 1.5);x
# set.seed(29) gives us same y1 each time
y1 <- sample(x1);y1
# makes y vector twice as long
y <- c(y1, y1 + 1);y
# plot x and y
plot(x, y, main = "single")
# draw line to-from closest points
# single linkage minimizes distance
lines(c(1, 0.7 + 1.5), c(1.1, 0.7 + 1), 
      col = "grey")

# 2nd plot is complete linkage (farthest) demo
set.seed(29)
x1 <- c(0.7, 0.8, 0.85, 0.9, 1.1, 1, 0.95);x1
x <- c(x1, x1 + 1.5);x
# set.seed(29) gives us same y1 each time
y1 <- sample(x1);y1 
y <- c(y1, y1 + 1);y
# draw line to-from farthest points
# complete linkage maximizes distance
plot(x, y, main = "complete")
lines(c(0.7, 2.5), c(0.7, 1.1 + 1), 
      col = "grey")

# 3rd plot is average linkage
set.seed(29)
x1 <- c(0.7, 0.8, 0.85, 0.9, 1.1, 1, 0.95)
x <- c(x1, x1 + 1.5)
# set.seed(29) gives us same y1 each time
y1 <- sample(x1)    
y <- c(y1, y1 + 1)
# draw lines from first 7 points in x
# to each of points 8-14 in y and vice-versa
# graphically depicting average linkage.
plot(x, y, main = "average")
for (i in 1:7) {
    for (j in 8:14) {
      lines(x[c(i, j)], 
            y[c(i, j)], 
            col = rgb(0.1, 0.1, 
                      0.1, 0.1))
    }
}
par(mfrow=c(1,1))

#####  CLUSTER ANALYSIS

###########################################
### Application of three clustering methods
### To calculate Euclidean distances on
### Unstandardized Body Measurements
###########################################
# use dist() from previous MDS examples
# gives us a distance matrix between rows
# (individuals)
dm <- dist(measure[, c("chest", 
                       "waist", 
                       "hips")])
round(dm, 2)
?hclust
###########################################
### Hierarchical Clustering
### Plots for Single, Complete, 
### Average Linkages
###########################################
# use hclust() function to 
# plot example dendograms
plot(cs <- hclust(dm, method = "single"))
plot(cc <- hclust(dm, method = "complete"))
plot(ca <- hclust(dm, method = "average"))

# So below we show the cluster solutions for
# measure data. The top row gives the cluster
# dendograms along with the cutoff used to
# derive the classes presented (in the space
# of the two principal components) in the
# bottom row:

# Note single is long and straggly (problem)
# While complete and average are similar
# and place men and women into separate
# clusters.

body_pc <- princomp(dm, cor = TRUE)
layout(matrix(1:6, nr = 2), height = c(2, 1))
plot(cs <- hclust(dm, method = "single"), 
     main = "Single")
abline(h = 3.8, col = "lightgrey")
xlim <- range(body_pc$scores[,1])
plot(body_pc$scores[,1:2], type = "n", 
     xlim = xlim, ylim = xlim,
     xlab = "PC1", ylab = "PC2")
lab <- cutree(cs, h = 3.8)
text(body_pc$scores[,1:2], labels = lab, 
     cex=0.6)
plot(cc <- hclust(dm, method = "complete"), 
     main = "Complete")
abline(h = 10, col = "lightgrey")
plot(body_pc$scores[,1:2], type = "n", 
     xlim = xlim, ylim = xlim,
     xlab = "PC1", ylab = "PC2")
lab <- cutree(cc, h = 10)  
text(body_pc$scores[,1:2], labels = lab, 
     cex=0.6)     
plot(ca <- hclust(dm, method = "average"), 
     main = "Average")
abline(h = 7.8, col = "lightgrey")
plot(body_pc$scores[,1:2], type = "n", 
     xlim = xlim, ylim = xlim,
     xlab = "PC1", ylab = "PC2")
lab <- cutree(ca, h = 7.8)                             
text(body_pc$scores[,1:2], labels = lab, 
     cex=0.6)     

# We now need to consider how we select 
# specific partitions of the data (i.e.,
# a solution with a particular number of 
# groups) from these dendrograms. The
# answer is that we "cut" the dendrogram 
# at some height and this will give a
# partition with a particular number of 
# groups. 

# How do we choose where to cut or, in other
# words, how do we decide on a particular 
# number of groups that is optimal for the 
# data? 

# Because we know that these data consist 
# of measurements on ten men and ten women, 
# we will look at the two-group solutions 
# from each method that are obtained by 
# cutting the dendrograms at suitable 
# heights. 

# We can display and compare the three 
# solutions graphically by plotting the 
# first two principal component scores 
# of the data, labelling the points to 
# identify the cluster solution of one 
# of the methods by using:

# single linkage:
body_pc <- princomp(dm, cor = TRUE)
xlim <- range(body_pc$scores[,1])
plot(body_pc$scores[,1:2], 
     type = "n", 
     xlim = xlim, 
     ylim = xlim)
lab <- cutree(cs, h = 3.8)
text(body_pc$scores[,1:2], 
     labels = lab, 
     cex = 0.6)


# complete linkage:
body_pc <- princomp(dm, cor = TRUE)
xlim <- range(body_pc$scores[,1])
plot(body_pc$scores[,1:2], type = "n", 
     xlim = xlim, ylim = xlim,
     xlab = "PC1", ylab = "PC2")
lab <- cutree(cc, h = 10)  
text(body_pc$scores[,1:2], labels = lab, 
     cex=0.6) 
# The resulting plots are shown in the 
# lower part of the previous figure. 

# The plots of dendrograms and principal 
# components scatterplots are combined 
# into a single diagram. 

# The plot associated with the single linkage 
# solution immediately demonstrates one of the
# problems with using this method, and
# is a phenomenon known as chaining, which 
# refers to the tendency to incorporate 
# intermediate points between clusters into 
# an existing cluster rather than initiating
# a new one. 

# As a result, single linkage solutions 
# often contain long "straggly" clusters that 
# do not give a useful description of the data. 

# The two-group solutions from complete linkage
# and average linkage are similar and in 
# essence place the men (observations 1 to 10)
# together in one cluster and the women 
# (observations 11 to 20) in the other.

### Clustering Jet Fighters

#### SEE SLIDE

# values of six variables for 22 US fighter
# aircraft. The variables are as follows:

# FFD: first flight date, in months after 
# January 1940;

# SPR: specific power, proportional to power 
# per unit weight;

# RGF: flight range factor;

# PLF: payload as a fraction of gross 
# weight of aircraft;

# SLF: sustained load factor;

# CAR: a binary variable that takes 
# the value 1 if the aircraft can land 
# on a carrier and 0 otherwise.

jet <-
structure(list(V1 = c(82L, 89L, 101L, 107L, 
                      115L, 122L, 127L, 137L, 
                      147L, 166L, 174L, 175L, 
                      177L, 184L, 187L, 189L, 
                      194L, 197L, 201L, 204L, 
                      255L, 328L), 
               V2 = c(1.468, 1.605, 2.168, 2.054, 
                      2.467, 1.294, 2.183, 2.426, 
                      2.607, 4.567, 4.588, 3.618, 
                      5.855, 2.898, 3.880, 0.455, 
                      8.088, 6.502, 6.081, 7.105, 
                      8.548, 6.321), 
               V3 = c(3.3, 3.64, 4.87, 4.72, 4.11, 
                      3.75, 3.97, 4.65, 3.84, 4.92, 
                      3.82, 4.32, 4.53, 4.48, 5.39, 
                      4.99, 4.5, 5.2, 5.65, 5.4, 4.2, 
                      6.45), 
               V4 = c(0.166, 0.154, 0.177, 0.275, 
                      0.298, 0.15, 0, 0.117, 0.155, 
                      0.138, 0.249, 0.143, 0.172, 
                      0.178, 0.101, 0.008, 0.251, 
                      0.366, 0.106, 0.089, 0.222, 
                      0.187), 
               V5 = c(0.1, 0.1, 2.9, 1.1, 1, 0.9, 
                      2.4, 1.8, 2.3, 3.2, 3.5, 2.8, 
                      2.5, 3, 3, 2.64, 2.7, 2.9, 2.9, 
                      3.2, 2.9, 2),
               V6 = c(0L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 
                      0L, 1L, 0L, 0L, 1L, 0L, 1L, 0L, 
                      1L, 1L, 1L, 1L, 0L, 1L)), 
          .Names = c("V1", "V2", "V3", "V4", "V5", "V6"), 
          class = "data.frame", 
          row.names = c(NA, -22L))                                                   
colnames(jet) <- c("FFD", "SPR", "RGF", "PLF", "SLF", "CAR")
rownames(jet) <- c("FH-1", "FJ-1", "F-86A", "F9F-2", "F-94A", 
                   "F3D-1", "F-89A", "XF10F-1", "F9F-6", 
                   "F-100A", "F4D-1", "F11F-1", "F-101A", 
                   "F3H-2", "F-102A", "F-8A", "F-104B",
                   "F-105B", "YF-107A", "F-106A", "F-4B", 
                   "F-111A")
jet$CAR <- factor(jet$CAR, labels = c("no", "yes"))

# take a look at data
jet

# We apply complete linkage to the data 
# but using only variables two to five.
# And given that the variables are on 
# very different scales, we standardize
# them to unit variance before clustering. 

# The required R code for standardization
# and clustering is as follows:
?scale
X <- scale(jet[, c("SPR", "RGF", "PLF", "SLF")],
           center = FALSE, scale = TRUE) 
dj <- dist(X)
plot(cc <- hclust(dj), 
     main = "Jets clustering")
cc

# The resulting dendrogram in suggests the 
# presence of two groups of fighters. In the 
# next plot, the data are plotted in the space 
# of the first two principal components of 
# the correlation matrix of the relevant
# variables (SPR to SLF). And the points are 
# labelled by cluster Number for the two-group 
# solution and the colors used are the values 
# of the CAR variable. 

# The two-group solution largely corresponds 
# to planes that can and cannot land on a
# carrier.

pr <- prcomp(dj)$x[, 1:2]
plot(pr, pch = (1:2)[cutree(cc, k = 2)],
     col = c("black", "darkgrey")[jet$CAR], 
     xlim = range(pr) * c(1, 1.5))
legend("topright", col = c("black", 
                           "black", 
                           "darkgrey", 
                           "darkgrey"), 
       legend = c("1 / no", "2 / no", 
                  "1 / yes", "2 / yes"), 
       pch = c(1:2, 1:2), 
       title = "Cluster / CAR", 
       bty = "n")


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

# But if we first calculate the variances
# of the crime rates for the different types
# of crimes we find the following:
sapply(crime, var)

# The variances are very different, so using
# K-means on the raw data would not be
# sensible; so we standardize each variables.
# But how? By its range.
rge <- sapply(crime, function(x) diff(range(x)))
crime_s <- sweep(crime, 2, rge, FUN = "/")
sapply(crime_s, var)

# The variances are now very similar, so
# we proceed with clustering the data.

# First we plot within-group sums-of-squares
# for one- and six-group solutions to see if
# there is any indication of the 'best' 
# number of groups:
n <- nrow(crime_s)
wss <- rep(0, 6)
wss[1] <- (n - 1) * sum(sapply(crime_s, var))
for (i in 2:6)
    wss[i] <- sum(kmeans(crime_s,
                         centers = i)$withinss)
plot(1:6, wss, type = "b", 
     xlab = "Number of groups",
     ylab = "Within groups sum of squares")

# The "elbow" occurs at two groups, so we
# look at the 2-group solution:
kmeans(crime_s, centers = 2)$centers * rge
?kmeans
# We plot the two-group solution in the
# space of the first two principal components
# of the correlation matrix of the data.

# The two groups are created essentially 
# on the basis of the first principal 
# component score, which is a weighted
# average of the crime rates. Perhaps all 
# the cluster analysis is doing here is 
# dividing into two parts a homogenous 
# set of data.
crime_pca <- prcomp(crime_s)
plot(crime_pca$x[, 1:2], 
     pch = kmeans(crime_s, 
                  centers = 2)$cluster)

#################################################
#####    BEGIN in SESSION 6 on April 29     #####
#################################################

### Pottery data from HSAUR2 package

# One more set of hierarchical and k-means 
# clustering analysis data consists of the
# results of chemical analysis on Romano-
# British pottery made in three different 
# regions (region 1 contains kiln 1,
# region 2 contains kilns 2 and 3, and 
# region 3 contains kilns 4 and 5). 

# One question that might be posed about these
# data is whether the chemical profiles of each 
# pot suggest different types of pots and if
# any such types are related to kiln or region. 

# We are interested in: Whether, on the basis 
# of their chemical compositions, the pots can 
# be divided into distinct groups, and how
# these groups relate to the kiln site.

data("pottery", package = "HSAUR2")
head(pottery)
nrow(pottery)
ncol(pottery)
str(pottery) # all numeric except kiln

# Data on Romano-British pottery. We begin by 
# computing the Euclidean distance matrix for
# standardized measurements of the 45 pots. 

# The resulting 45 x 45 matrix can be inspected 
# graphically by using an image plot, obtained
# with the function levelplot available in the 
# package lattice.

# Such a plot associates each cell of the 
# dissimilarity matrix with a color or gray
# value. We choose a very dark grey for cells 
# with distance zero (ie. the diagonal elements 
# of the dissimilarity matrix) and pale values
# for cells with greater Euclidean distance. 

# We see that there are at least three distinct 
# groups with small inter-cluster differences
# (the dark rectangles), whereas much larger 
# distances can be observed for all other cells.

pottery_dist <- dist(pots <- scale(pottery[, colnames(pottery) != "kiln"], 
                                   center = FALSE))
library("lattice")
levelplot(as.matrix(pottery_dist), 
          xlab = "Pot Number",
          ylab = "Pot Number")

# same plot in just grays
trellis.par.set(standard.theme(color = FALSE))
plot(levelplot(as.matrix(pottery_dist), 
               xlab = "Pot Number", 
               ylab = "Pot Number",
     scales = list(x = list(draw = FALSE), 
                   y = list(draw = FALSE))))

# We plot the within-groups sum of squares 
# for one to six group k-means solutions
# to see if we can get any indication of 
# the number of groups. Again, the plot 
# leads to the relatively clear conclusion 
# that the data contain three clusters.

n <- nrow(pots)
wss <- rep(0, 6)
wss[1] <- (n - 1) * sum(sapply(pots, var))
for (i in 2:6)
    wss[i] <- sum(kmeans(pots,
                         centers = i)$withinss)
plot(1:6, wss, type = "b", 
     xlab = "Number of groups",   
     ylab = "Within groups sum of squares")

###################################################
### Construct three series of partitions using
### single, complete, and average linkage
### hierarchical clustering with function hclust()
###################################################

# Function hclust() uses dissimilarity matrix and
# method argument to specify how distance between
# two clusters is assessed.
pottery_single <- hclust(pottery_dist, 
                         method = "single")
pottery_complete <- hclust(pottery_dist, 
                           method = "complete")
pottery_average <- hclust(pottery_dist, 
                          method = "average")
layout(matrix(1:3, ncol = 3))
# plot method draws a dendogram; the three dendograms
# all indicate that three clusters fit the data best (!)
# I think mainly 'average linkage' shows that
plot(pottery_single, main = "Single Linkage",
     sub = "", xlab = "")
plot(pottery_complete, main = "Complete Linkage",
     sub = "", xlab = "")
plot(pottery_average, main = "Average Linkage",
     sub = "", xlab = "")
layout(matrix(1, ncol = 1))

###################################################
### From the pottery_average object representing
### the average linkage hierarchical clustering,
### we derive the three-cluster solution by
### cutting the dendrogram at a height of four.
###################################################

# We are interested in a comparison of the kiln
# sites at which the pottery was found
pots_pca <- prcomp(pots)
plot(pots_pca$x[, 1:2], 
     pch = kmeans(pots, centers = 3)$cluster)

# The contingency table shows that cluster 1 
# contains all pots found at kiln site number
# one, cluster 2 contains all pots from kiln 
# sites numbers two and three, and cluster three
# collects the ten pots from kiln sites four 
# and five.

# The five kiln sites are from three different 
# regions: region 1 contains just kiln
# one, region 2 contains kilns two and three, 
# and region 3 contains kilns four and five. 

# So the clusters found actually correspond 
# to pots from three different regions.

set.seed(29)
pottery_cluster <- kmeans(pots, centers = 3)$cluster
xtabs(~ pottery_cluster + kiln, data = pottery)

###################################################
######         Classifying Exoplanets        ######
###################################################

# Let's take a look at 3-dimensional planets data:
data("planets", package = "HSAUR2")
# might need to install this
library("scatterplot3d") 
layout(matrix(1:1, ncol = 1))
scatterplot3d(log(planets$mass), 
              log(planets$period),
              log(planets$eccen+ifelse(planets$eccen == 0, 0.001, 0)),
              type = "h", angle = 55, pch = 16,
              y.ticklabs = seq(0, 10, by = 2),
              y.margin.add = 0.1, scale.y = 0.7,
              xlab = "log(mass)", 
              ylab = "log(period)",
              zlab = "log(eccen)")

# Are looking at 3D scatterplot of logarithms 
# of the three variables available for each of 
# the exoplanets.

# The diagram gives no clear indication of 
# distinct clusters in the data but nevertheless 
# we continue to investigate this possibility 
# by applying k-means clustering with the 
# kmeans() function in R.

# kmeans() minimizes the total within-group 
# sum of squares over all variables.

# However, deciding on the 'optimal' number 
# of groups is often difficult. An informal 
# approach is to plot the within-group sum 
# of squares for each partition and looking
# for an 'elbow' in the resulting curve.

###################################################
### The three planet variables are on very different
### scales so they first need to be standardized
### somehow - we use the range of each here.
###################################################
# computes range:
rge <- apply(planets,2,max)-apply(planets,2,min)
# divides all the values by their range:
planet.dat <- sweep(planets,2,rge,FUN = "/")
planet.dat

# 101 rows:
n <- nrow(planet.dat);n
# initialize wss variable
wss <- rep(0, 10);wss
# puts the sum of the variances in the first element:
wss[1] <- (n - 1)*sum(apply(planet.dat,2,var))
wss

# apply kmeans()
for (i in 2:10)
  wss[i] <- sum(kmeans(planet.dat,
                       centers = i)$withinss)
plot(1:10, wss, type = "b", xlab = "Number of groups",
     ylab = "Within groups sum of squares")

# are not looking for lowest point necessarily,
# rather are looking for 'steepest' drop...nothing
# convincing, but have 'little elbows' at 3- and 
# 5-group solutions.

####################################
###   So we find the number of   ###
###   planets in each group      ###
####################################
planet_kmeans3 <- kmeans(planet.dat, centers = 3)
table(planet_kmeans3$cluster)

#  1  2  3 
# 14 34 53 

###################################################
### We create a function to compute the centers
### of the clusters for the untransformed data
###################################################
ccent <- function(cl) {
  f <- function(i) colMeans(planets[cl == i,])
  x <- sapply(sort(unique(cl)), f)
  colnames(x) <- sort(unique(cl))
  return(x)
}

###################################################
### And then we apply it to the three-cluster
### solution obtained by k-means
###################################################
ccent(planet_kmeans3$cluster)

#                 1           2           3
# mass     10.56786   2.9276471   1.6710566
# period 1693.17201 616.0760882 427.7105892
# eccen     0.36650   0.4953529   0.1219491

###################################################
### And we apply it here to the five-cluster
### solution obtained by k-means
###################################################
planet_kmeans5 <- kmeans(planet.dat, centers = 5)
table(planet_kmeans5$cluster)

#  1  2  3  4  5 
# 35 30  4 18 14 

ccent(planet_kmeans5$cluster)

#                  1          2        3           4            5
# mass     1.7448571   1.743533    2.115   3.4916667   10.8121429
# period 552.3494286 176.297374 3188.250 638.0220556 1318.6505856
# eccen    0.2939143   0.049310    0.110   0.6032778    0.3836429

# And I quote: Interpretation of both the three- 
# five-cluster solutions clearly requires a detailed
# knowlege of astronomy. But the mean vectors of the
# three-group solution imply a relatively large class
# of Jupiter-sized planets with small periods and
# small eccentricities, a smaller class of massive
# planets with moderate periods and large eccentricities,
# and a very small class of large planets with extreme
# periods and moderate eccentricities.

###################################################
### Model-based Clustering in R
###################################################

# mclust() selects both the most appropriate model
# for the data AND the optimal number of groups
# based on the values of the Bayesian Information
# Criterion (BIC) computed over several models.

library("mclust")
?Mclust
planet_mclust <- Mclust(planet.dat)

###################################################
### Examine plot of BIC values
###################################################
plot(planet_mclust, 
     planet.dat, what = "BIC", col = "black",
     ylab = "-BIC", ylim = c(0, 350))

# Different plotting symbols refer to different
# model assumptions about the shape of clusters:
# EII: spherical, equal volume
# VII: spherical, unequal volume
# EEI: diagonal, equal volume and shape
# VEI: diagonal, varying volume, equal shape
# EVI: diagonal, equal volume, varying shape
# VVI: diagonal, varying volume and shape ** BEST HERE **
# EEE: ellipsoidal, equal volume, shape and distribution
# EEV: ellipsoidal, equal volume and equal shape
# VEV: ellipsoidal, equal shape
# VVV: ellipsoidal, varying volume, shape and orientation

###################################################
### Examine plot of BIC values
###################################################

# BIC selects model VVI with three clusters as best
# solution as can be seen from the print output:
print(planet_mclust)

###################################################
### Can show this solution graphically as a 
### scatterplot matrix.
###################################################
clPairs(planet.dat,
        classification = planet_mclust$classification,
        symbols = 1:3, col = "black")

###################################################
### Here is a clustering solution in the three-
### dimensional space
###################################################
scatterplot3d(log(planets$mass), log(planets$period),
              log(planets$eccen + ifelse(planets$eccen == 0, 0.001, 0)),
              type = "h", angle = 55, scale.y = 0.7,
              pch = planet_mclust$classification,
              y.ticklabs = seq(0, 10, by = 2), y.margin.add = 0.1,
              xlab = "log(mass)", ylab = "log(period)",
              zlab = "log(eccen)")

###################################################
### The number of planets in each cluster and the
### the mean vectors of the three clusters for
### the untransformed data can now be inspected:
###################################################
table(planet_mclust$classification)
ccent(planet_mclust$classification)

# These numbers are different from the solution we
# determined using k-means

#  1  2  3 
# 19 41 41 

# Cluster 1 consists of planets about the same size
# as Jupiter with very short periods and eccentricities
# (similar to the first cluster of k-means solution).
# Cluster 2 consists of slightly larger planets with
# moderate periods and large eccentricities, and
# cluster 3 contains the very large planets with very
# large periods. 

# The final two clusters do not match those found by
# the k-means approach.

#                   1           2            3
#   mass   1.16652632   1.5797561    6.0761463
# period   6.47180158 313.4127073 1325.5310048
#  eccen   0.03652632   0.3061463    0.3704951

### More on Model-Based Clustering

# The hierarchical and k-means clustering methods 
# are based largely on heuristic but intuitively 
# reasonable procedures.

# They are not based on formal models for cluster 
# structure in the data, making problems such as
# deciding between methods, estimating the number
# of clusters, etc, very difficult.

# There can be no formal inference. Not really
# problematic since is an "exploratory" tool for
# data analysis.

# Here we describe an approach to clustering that 
# postulates a formal statistical model for the 
# population from which the data are sampled, a 
# model that assumes that this population consists 
# of a number of subpopulations (the "clusters"), 
# each having variables with a different 
# multivariate probability density function, 
# resulting in what is known as a finite mixture 
# density for the population as a whole. 

# By using finite mixture densities as models for 
# cluster analysis, the clustering problem becomes 
# that of estimating the parameters of parameters of
# the assumed mixture and then using the estimated 
# parameters to calculate the posteriod probabilities
# of cluster membership. And determining the number 
# of clusters reduces to a model selection problem 
# for which objective procedures exist.

# Finite mixture densities often provide a sensible
# statistical model for the clustering process,
# and cluster analyses based on finite mixture
# models are also know as "model-based" clustering
# methods.

# Finite mixture modeling can be seen as a form
# of latent variable analysis with "subpopulation"
# being a latent categorical variable and the
# latent classes being described by the different
# components of the mixture density. So they are
# sometimes called "latent class cluster analysis"

### Gastroenterologist's Questionnaire

# To illustrate the use of the finite mixture 
# approach to cluster analysis, we apply it to data
# that arise from a study of what gastroenterologists
# in Europe tell their cancer patients.

# A questionnaire was sent to about 600 
# gastroenterologists in 27 European countries
# asking what they would tell a patient with newly 
# diagnosed cancer of the colon, and his or her
# spouse, about the diagnosis. 

# The respondent gastroenterologists were asked to 
# read a brief case history and then to answer
# six questions with a yes/no answer. The questions 
# were as follows:

# Q1: Would you tell this patient that he/she has 
# cancer, if he/she asks no questions?

# Q2: Would you tell the wife/husband that the 
# patient has cancer (In the patient's absence)?

# Q3: Would you tell the patient that he or she 
# has a cancer, if he or she directly asks you 
# to disclose the diagnosis. (During surgery the 
# surgeon notices several small metastases in the 
# liver.)

# Q4: Would you tell the patient about the 
# metastases (supposing the patient asks to be told 
# the results of the operation)?

# Q5: Would you tell the patient that the condition 
# is incurable?

# Q6: Would you tell the wife or husband that the 
# operation revealed metastases?

# Load in the Country data
cnt <- c("Iceland", "Norway", "Sweden", 
         "Finland", "Denmark", "UK", "Eire",
         "Germany", "Netherlands", "Belgium", 
         "Switzerland", "France", "Spain",
         "Portugal", "Italy", "Greece", "Yugoslavia", 
         "Albania", "Bulgaria", "Romania",
         "Hungary", "Czechia", "Slovakia", "Poland", 
         "CIS", "Lithuania", "Latvia", "Estonia")
?expand.grid

# define the thomson questionnaire
thomson <- expand.grid(answer = factor(c("no", "yes")),
                       question = factor(paste("Q", 1:6, sep = "")),
                       country = factor(cnt, levels = cnt))

# data associated with frequency 
thomson$Freq <- c(
  0, 5, 0, 5, 0, 4, 0, 5, 0, 5, 0, 5,
  1, 6, 1, 5, 0, 6, 0, 5, 0, 4, 1, 4,
  0, 11, 4, 7, 0, 7, 0, 11, 5, 5, 3, 6,
  0, 6, 2, 4, 0, 6, 0, 6, 1, 5, 2, 4,
  1, 12, 4, 9, 0, 12, 3, 9, 7, 4, 6, 7,
  7, 12, 2, 16, 0, 20, 1, 19, 9, 10, 0, 17,
  0, 1, 1, 2, 0, 3, 2, 0, 2, 0, 0, 3,
  0, 14, 0, 13, 0, 13, 2, 12, 11, 2, 1, 13,
  0, 8, 0, 8, 0, 8, 1, 7, 2, 5, 1, 7,
  2, 0, 0, 2, 0, 2, 1, 1, 2, 0, 0, 2,
  0, 5, 0, 5, 0, 4, 2, 2, 5, 0, 0, 4,
  7, 3, 1, 7, 3, 5, 8, 2, 10, 0, 1, 7,
  11, 1, 0, 12, 2, 8, 5, 6, 11, 0, 0, 11,
  5, 1, 0, 6, 2, 4, 3, 3, 6, 0, 0, 6,
  8, 7, 0, 15, 1, 13, 9, 6, 13, 2, 0, 15,
  7, 1, 0, 8, 3, 5, 7, 1, 8, 0, 0, 7,
  11, 4, 0, 15, 7, 8, 11, 4, 15, 0, 0, 14,
  3, 2, 2, 3, 3, 2, 3, 2, 3, 3, 2, 3,
  3, 0, 0, 3, 2, 1, 3, 0, 3, 0, 0, 3,
  7, 0, 0, 6, 6, 1, 6, 1, 6, 1, 0, 7,
  4, 1, 0, 5, 1, 4, 5, 0, 5, 0, 0, 5,
  18, 2, 0, 20, 17, 3, 20, 0, 20, 0, 0, 20,
  13, 0, 1, 14, 14, 0, 16, 0, 13, 0, 15, 0,
  18, 0, 0, 19, 13, 5, 17, 2, 17, 0, 0, 19,
  7, 0, 1, 6, 5, 2, 7, 0, 7, 0, 1, 6,
  8, 0, 0, 8, 8, 0, 8, 0, 8, 0, 0, 8,
  5, 0, 0, 5, 5, 0, 5, 0, 5, 0, 0, 5,
  2, 2, 0, 3, 0, 3, 3, 0, 3, 0, 0, 3)

# we tabulate the answers
ttab <- xtabs(Freq ~ country + answer + question, data = thomson)
ttab

# turn it into a proportions table
thomsonprop <- prop.table(ttab, c(1,3))[,"yes",]
thomsonprop

# Finally, we plot all of the data here
plot(1:(22 * 6), rep(-1, 22 * 6), 
     ylim = c(-nlevels(thomson$country), -1), 
     type = "n",
     axes = FALSE, xlab = "", 
     ylab = "")
for (q in 1:6) {   
  tmp <- ttab[,,q]
  xstart <- (q - 1) * 22 + 1
  y <- -rep(1:nrow(tmp), 
            rowSums(tmp))
  x <- xstart + unlist(sapply(rowSums(tmp),function(i) 1:i))
  pch <- unlist(apply(tmp, 1,function(x) c(rep(19, x[2]), rep(1, x[1]))))
  points(x, y, pch = pch)
}
axis(2, at = -(1:nlevels(thomson$country)), 
     labels = levels(thomson$country),
     las = 2, tick = FALSE, line = 0)
mtext(text = paste("Question", 1:6), 
      3, at = 22 * (0:5), adj = 0)

# Applying the finite mixture approach to the 
# proportions of 'yes' answers for each question 
# for each country computed from these data using 
# the R code utilizing functionality offered by 
# package mclust
library("mclust")
(mc <- Mclust(thomsonprop))

# where thomsonprob is the matrix of proportions 
# of "yes" answers to questions Q1-Q6 in the 
# different countries (ie, the proportion of filled 
# dots in) available from the MVA add-on package.

# We examine the resulting plot of BIC values
# for the gastroenterologists questionnaire
plot(mc, thomsonprop, 
     what = "BIC", 
     col = "black")

# The plot symbols and abbreviations refer to 
# different model assumptions about the shapes of
# clusters as given in Table 6.4 (see slides)

# The BIC criterion selects model VEV 
# (ellipsoidal, equal shape) and three clusters as 
# the optimal solution.

# We illustrate the three-cluster solution graphically
cl <- mc$classification
nm <- unlist(sapply(1:3, 
                    function(i) names(cl[cl == i])))
ttab <- ttab[nm,,]
plot(1:(22 * 6), rep(-1, 22 * 6), 
     ylim = c(-nlevels(thomson$country), -1), 
     type = "n",
     axes = FALSE, 
     xlab = "", 
     ylab = "")
for (q in 1:6) {   
  tmp <- ttab[,,q]
  xstart <- (q - 1) * 22 + 1
  y <- -rep(1:nrow(tmp), 
            rowSums(tmp))
  x <- xstart + unlist(sapply(rowSums(tmp), 
                              function(i) 1:i))
  pch <- unlist(apply(tmp, 1, 
                      function(x) c(rep(19, x[2]), 
                                    rep(1, x[1]))))
  points(x, y, pch = pch)
}
axis(2, at = -(1:nlevels(thomson$country)), 
     labels = dimnames(ttab)[[1]],
     las = 2, tick = FALSE, line = 0)
mtext(text = paste("Question", 1:6), 
      3, at = 22 * (0:5), adj = 0)
abline(h = -cumsum(table(cl))[-3] - 0.5, 
       col = "grey")
text(-c(0.75, 0.75, 0.75), 
     -cumsum(table(cl)) + table(cl)/2,
     label = paste("Cluster", 1:3), 
     srt = 90, pos = 1)

# The first cluster consists of countries in which 
# the large majority of respondents gave "yes" answers 
# to questions 1; 2; 3; 4, and 6 and about half
# also gave a "yes" answer to question 5. This 
# cluster includes all the Scandinavian countries 
# the UK, Iceland, Germany, the Netherlands, and
# Switzerland. 

# In the second cluster, the majority of respondents
# answer "no" to questions 1; 4, and 5 
# and "yes" to questions 2; 3 and 6; in these countries
# it appears that the clinicians do not mind giving 
# bad news to the spouses of patients but not to
# the patients themselves unless they are directly 
# asked by the patient about his or her condition. 
# This cluster contains Catholic countries such as
# Spain, Portugal, and Italy. 

# In cluster three, the large majority of respondents
# answer "no" to questions 1; 3; 4, and 5 and again 
# a large majority answer "yes" to questions 2 and 6. 

# In these countries, very few clinicians appear
# to be willing to give the patient bad news even 
# if asked directly by the patient about his or 
# her condition.

### Displaying Clustering Solutions Graphically

# Plotting cluster solutions in the space of the 
# first few principal components as illustrated
# earlier is often a useful way to display clustering
# solutions, but other methods of displaying clustering 
# solutions graphically are also available.

# Leische (2010) describes several graphical displays
# that can be used to visualise cluster analysis 
# solutions. The basis of a number of these graphics 
# is the shadow value, s(x), of each multivariate 
# observation, x, which is a measure of how close
# each observation is to its cluster centroid. If 
# s(x) is close to zero, the observation is close
# to its cluster centroid; if s(x) is close to one, 
# then the shadow value to equidistant from its
# cluster centroid and the next closest cluster's
# centroid.

# Lots of cluster values approaching one indicate
# a lot of cluster overlap. "Neighborhood Plots"
# show the degree to which two cluster centroids
# overlap where the thickness of the lines joining
# the centroid indicate the degree of overlap
# between pairs of clusters.

# Neighborhood plot of k-means five-cluster
# solution for bivariate data containing
# three clusters
library("flexclust")
library("mvtnorm")
set.seed(290875)
x <- rbind(rmvnorm(n = 20, mean = c(0, 0), 
                   sigma = diag(2)),
           rmvnorm(n = 20, mean = c(3, 3), 
                   sigma = 0.5 * diag(2)),
           rmvnorm(n = 20, mean = c(7, 6), 
                   sigma = 0.5 * (diag(2) + 0.25)))
k <- cclust(x, k = 5, save.data = TRUE)
plot(k, hull = FALSE, col = rep("black", 5), 
     xlab = "x", ylab = "y")

# Here we note a lot of apparent overlap between
# the clusters 1 and 5; and the clusters 2 and 4.

# Neighborhood plot of k-means three-cluster
# solution for pottery data. Here we can see that 
# there is a three-cluster solution but that clusters
# 2 and 3 apparently do overlap
k <- cclust(pots, k = 3, save.data = TRUE)
plot(k, project = prcomp(pots), 
     hull = FALSE, col = rep("black", 3),
     xlab = "PC1", ylab = "PC2")     

# A further graphic for displaying clustering 
# solutions is known as a stripes plot. This graphic is
# a simple but often effective way of visualising the 
# distance of each point from its closest and second 
# closest cluster centroids. For each cluster, 
# k = 1, . . ., K, a stripes plot has a rectangular 
# area that is vertically divided into K smaller 
# rectangles, with each smaller rectangle, i, 
# containing information about distances of the 
# observations in cluster i from the centroid
# of that cluster along with the corresponding 
# information about observations that have 
# cluster i as their second closest cluster.

# Stripes plot of k-means solution for artificial data. 
# The set of data generated is meant to contain five
# relatively distinct clusters.

# Looking first at the rectangle for cluster one, 
# we see that observations in clusters two and 
# three have the cluster one centroid as their 
# second closest. These observations form the two
# other stripes within the rectangle. 

# Observations in cluster three are further away 
# from cluster one, but a number of observations 
# in cluster three have a distance to the centroid 
# of cluster one similar to those observations that
# belong to cluster one. 

# Overall though, the stripes plot in suggests that
# the five-cluster solution matches quite well the 
# actual structure in the data.

set.seed(912345654)
x <- rbind(matrix(rnorm(100, sd = 0.5), ncol= 2 ),
           matrix(rnorm(100, mean =4, 
                        sd = 0.5), ncol = 2),
           matrix(rnorm(100, mean =7, 
                        sd = 0.5), ncol = 2),
           matrix(rnorm(100, mean =-1.0, 
                        sd = 0.7), ncol = 2),
           matrix(rnorm(100, mean =-4.0, 
                        sd = 1.0), ncol = 2))
c5 <- cclust(x, 5, save.data = TRUE)
stripes(c5, type = "second", col = 1)



# Stripes plot of k-means solution for artificial data.
# Here the stripes plot for the k-means five-group
# k-means five-group solution suggests that the 
# clusters in this solution are not well separated,
# implying perhaps that the five-group solution is 
# not appropriate for the data in this case.
set.seed(912345654)
x <- rbind(matrix(rnorm(100, sd = 2.5), ncol = 2),
           matrix(rnorm(100, mean = 3, 
                        sd = 0.5), ncol = 2),
           matrix(rnorm(100, mean = 5, 
                        sd = 0.5), ncol = 2),
           matrix(rnorm(100, mean = -1.0, 
                        sd = 1.5), ncol = 2),
           matrix(rnorm(100, mean = -4.0, 
                        sd = 2.0), ncol = 2))
c5 <- cclust(x, 5, save.data = TRUE)
stripes(c5, type = "second", col = 1)

# Stripes plot of three-group k-means 
# solution for artificial data. Here we see for the
# pottery data the plot confirms the three-group
# structure of the data.
set.seed(15)
c5 <- cclust(pots, 
             k = 3, 
             save.data = TRUE)
stripes(c5, type = "second", 
        col = "black")

# All the information in a stripes plot is also 
# available from a neighbourhood plot, but the former
# is dimension independent and may work well even
# for high-dimensional data where projections to 
# two dimensions lose a lot of information about
# the structure in the data. Neither neighbourhood 
# graphs nor stripes plots are infallible, but both 
# offer some help in the often diffcult task of
# evaluating and validating the solutions from a 
# cluster analysis of a set of data.
                                                                 