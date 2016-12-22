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

# Get rid of DC
crime_s <- crime_s[-24,]

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

### Pottery data from HSAUR2 package

# Our last set of clustering analysis data
# consists of the results of chemical analysis on
# Romano-British pottery made in three different 
# regions (region 1 contains kiln 1, region 2 
# contains kilns 2 and 3, and region 3 contains 
# kilns 4 and 5). 

# One question that might be posed about these
# data is whether the chemical profiles of each 
# pot suggest different types of pots and if
# any such types are related to kiln or region. 

# We are interested in: Whether, on the basis of their
# chemical compositions, the pots can be divided into
# distinct groups, and how these groups relate to the
# kiln site.

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
# with distance zero (i.e., the diagonal elements 
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
plot(1:6, wss, type = "b", xlab = "Number of groups",   
     ylab = "Within groups sum of squares")

###################################################
### Construct three series of partitions using
### single, complete, and average linkage
### hierarchical clustering with function hclust()
###################################################

# Function hclust() uses dissimilarity matrix and
# method argument to specify how distnace between
# two clusters is assessed.
pottery_single <- hclust(pottery_dist, method = "single")
pottery_complete <- hclust(pottery_dist, method = "complete")
pottery_average <- hclust(pottery_dist, method = "average")
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
# and region 3 contains kilns fourand five. 

# So the clusters found actually correspond 
# to pots from three different regions.

set.seed(29)
pottery_cluster <- kmeans(pots, centers = 3)$cluster
xtabs(~ pottery_cluster + kiln, data = pottery)
