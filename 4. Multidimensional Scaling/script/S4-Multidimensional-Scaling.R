###############################################
#####      Multidimensional Scaling       #####
###############################################

# Take a look at description in wikipedia

# we need the MVA package
library("MVA")

# Aim of MDS is to produce maps of data. MDS is
# applied to distance matrices which are derived
# from the matrix X rather than operating on
# the matrix itself.

# More broadly, are talking about dissimilarity
# or similarity matrices.

# Does not have to be some metric of distance.
# Differences in judgement, for example.

# 'Similarities' often scaled to lie in [0, 1]

# Term 'proximity' often used to encompass
# both dissimilarity and similarity ratings.

# 4.2 Models for proximity data

# Models are fitted to proximities in order 
# to clarify, display, and help understand
# and explain any structure or pattern 
# amongst the observed or calculated proximities.

# We are dealing with spatial models with these
# examples, although they can be tree models
# and also hybrid models.

### Spatial Models for Proximities: 
### Multidimensional Data

# A spatial representation of a proximity matrix 
# consists of a set of n m-dimensional coordinates, 
# each one of which represents one of the n units 
# in the data. The coordinates are found by 
# minimizing some measure of fit between the 
# distances implied by the coordinates and the 
# observed proximities. 

# We seek a geometrical model in which the
# larger the observed distance or dissimilarity 
# between two units (or the smaller their 
# similarity), the further apart should be the 
# points representing them in the model.

# Usually talking about Euclidean distances.

# Finding the best-fitting set of coordinates
# and the appropriate value of m needed to 
# adequately represent the observed proximities
# is the aim of the many methods of MDS
# that have been proposed.

# MDS is a data reduction technique as 
# goal is to find a set of points in low 
# dimension that approximate the higher
# dimensional data represented by the
# original proximity matrix.

# we will consider two methods: 
# 1) classical Multidimensional scaling; and 
# 2) non-metric multidimensional scaling.

### Classical Multidimensional Scaling

# Classical scaling seeks to represent a proximity
# matrix by a simple geometrical model or map. 
# Such a model is characterized by a set of points
# x1, x2, . . . . xn, in m dimensions, each point 
# representing one of the units of interest, and 
# a measure of the distance between pairs of
# points.

# Objective is to determine both dimensionality,
# m, of the model, and the n m-dimensional 
# coordinates, x1, x2, . . . , xn, so that
# the model gives a "good fit" for the observed 
# proximities. Fit will often be judged by some
# numerical index that measures how well the 
# proximities and the distances in the
# geometrical model match.

### Examples of Classical MDS
# A simple (unexplained) example
# Then 'real' skulls data example

?dist

###############################################
# Separate Example from R documentation
require(graphics)

# create random number 5 x 20 matrix
x <- matrix(rnorm(100), nrow=5)
x

# apply the distance function
# get a 5 x 5 matrix-looking object
dist(x)

# some ways to manipulate it
dist(x, diag = TRUE)
dist(x, upper = TRUE)

# coerce it to be a matrix
m <- as.matrix(dist(x))
m

# check structure
str(m)

# coerce it to be a distance object
d <- as.dist(m)
d

# check structure
str(d)
################ End of Separate Example #########

# Return to MVA Topic Material per se
# A simple example

# Small set of multivariate data X

X <- matrix(c(
  3, 5, 6, 1, 4, 2, 0, 0, 7, 2,
  4, 1, 2, 1, 7, 2, 4, 6, 6, 1,
  4, 1, 0, 1, 3, 5, 1, 4, 5, 4,
  6, 7, 2, 0, 6, 1, 1, 3, 1, 3,
  1, 3, 6, 3, 2, 0, 1, 5, 4, 1), nrow = 10)
X

# Get the associated matrix of Euclidean distances

(D <- dist(X))

# To apply classical scaling to matrix, we use
# cmdscale() function to do the scaling

# classic multidimensional scaling function
?cmdscale

################ Separate Example
require(graphics)

# original data
loc <- cmdscale(eurodist)
loc

# note loc is a two-dimensional soltuion
x <- loc[, 1]
y <- -loc[, 2] # reflect so North is at the top
## note asp = 1, to ensure Euclidean distances are represented correctly
plot(x, y, type = "n", 
     xlab = "", ylab = "", 
     asp = 1, axes = FALSE,
     main = "cmdscale(eurodist)")
text(x, y, rownames(loc), cex = 0.6)

########################### End Separate Example

### Return to MVA Content Specifically

# k is maximum dimension of the space in
# which the data are to be represented in
cmdscale(D, k = 9, eig = TRUE)

# Note that as q = 5 in this example, 
# eigenvalues seven to nine are essentially zero.
# Only the first five columns of points represent 
# the Euclidean distance matrix.

# We need to confirm that the five-dimensional 
# solution achieves complete recovery of the
# observed distance matrix. We can do this 
# by comparing original distances with those 
# calculated from the five-dimensional
# scaling solution coordinates:

max(abs(dist(X) - dist(cmdscale(D, k = 5))))

# confirms all distances very close to zero

# We can also check the duality of classical 
# scaling of Euclidean distances and PCA
# by comparing coordinates of five-dimensional 
# scaling solution with the first five principal 
# component scores obtained by applying PCA
# to the covariance matrix of the original data:

max(abs(prcomp(X)$x) - abs(cmdscale(D, k = 5)))

# also all very close to zero

# What if distances are not Euclidean?:

# Manhattan distances:
X_m <- cmdscale(dist(X, method = "manhattan"), 
                k = nrow(X) - 1, eig = TRUE)

# Find criteria Pm1 and Pm2 from eigenvalues
(X_eigen <- X_m$eig)

# Note some eigenvalues are negative

# Here is Pm1
cumsum(abs(X_eigen)) / sum(abs(X_eigen))

# Here is Pm2
cumsum(X_eigen^2) / sum(X_eigen^2)

# Are looking for values of at least 0.8. The
# authors say "the values of both criteria suggest
# that 3-dimensional solution seems to fit well."

### Second Classical Example: Skulls Data

data("skulls", package = "HSAUR2")

# Our next example of the use of classical multi-
# dimensional scaling will involve the 'skulls'
# dataset. These data show four measurements on
# male Egyptian skulls from five epochs. The 
# measurements are:

# mb: maximum breadth of the skull;
# bh: basibregmatic height of the skull;
# bl: basialiveolar length of the skull; and
# nh: nasal height of the skull.

# We shall calculate Mahalanobis distances 
# between each pair of epochs using the
# mahalanobis() function and apply classical 
# scaling to the resulting distance matrix.

# In this calculation, we shall use the estimate 
# of the assumed common covariance matrix S:

# S = 29S1 + 29S2 + 29S3 + 29S4 + 29S5 / 149

# where S are the covariance matrices of the 
# data in each epoch. We shall then use the
# first two coordinate values to provide a map 
# of the data showing the relationships between 
# epochs. The R code is:

# produces a list with covariance matrix
# for each epoch as component:
skulls_var <- tapply(1:nrow(skulls), skulls$epoch, 
                     function(i) var(skulls[i,-1]))

# initializes common covariance matrix var:
S <- 0

# creates common covariance matrix S
for (v in skulls_var) S <- S + 29 * v
(S <- S / 149)

# finds center of each measurement (mb, bh, bl, nh)
# for each epoch
skulls_cen <- tapply(1:nrow(skulls), skulls$epoch, 
    function(i) apply(skulls[i,-1], 2, mean))
skulls_cen

# There appears to be a change in the "shape"
# of the skulls over time, with maximum breadth
# (column 1) increasing and basialiveaolar 
# length (column 3) decreasing.

# create a matrix out of each components, each
# mean measurement for the five measurements
# by epoch (take a look)
skulls_cen <- matrix(unlist(skulls_cen), 
    nrow = length(skulls_cen), byrow = TRUE)
skulls_cen

# There appears to be a change in the "shape"
# of the skulls over time, with maximum breadth
# (column 1) increasing and basialiveaolar 
# length (column 3) decreasing.

# compute the mahalanobis distances:
skulls_mah <- apply(skulls_cen, 1, 
    function(cen) mahalanobis(skulls_cen, cen, S))
skulls_mah

# run classic MDS function on distances
cmdscale(skulls_mah, k = nrow(skulls_mah) - 1, 
         eig = TRUE)$eig

# only first two dimensions have eigenvalues
# that are positive

# runs it on two dimensions
skulls_mds <- cmdscale(skulls_mah)

# draw a scatterplot of two-dimensional solution
# from classical MDS applied to Mahalanobis
# distances:
lim <- range(skulls_mds) * 1.2
plot(skulls_mds, xlab = "Coordinate 1", 
     ylab = "Coordinate 2",
     xlim = lim, ylim = lim, type = "n")
text(skulls_mds, labels = levels(skulls$epoch), 
     cex = 0.7)

# plot shows that scaling solution for skulls
# data is unidimensional, with this single
# dimension time-ordering the five epochs.

# BECAUSE (again):
# There appears to be a change in the "shape"
# of the skulls over time, with maximum breadth
# (column 1) increasing and basialiveaolar 
# length (column 3) decreasing.

##### Non-Metric Multidimensional Scaling

# In some psychological work and in market 
# research, proximity matrices arise from asking
# humans to make judgements about the similarity 
# or dissimilarity of objects or stimuli of
# interest. When collecting such data, some
# realistically believe subjects are only able 
# to give "ordinal" judgements.

# For example, when comparing a range of colors 
# they might specify that one color is brighter
# than another, but not how much brighter.

# So developed a method of MD scaling that uses
# only the rank order of the proximities to 
# produce a spatial representation of them. 

# Such a method performs a monotonic trans-
# formation of the observed proximity matrix.

# derived coordinates will remain the same if 
# the numerical values of the observed proxi-
# mities are changed but the rank order is not.

# the method is that the coordinates in the 
# spatial representation of the observed
# proximities give rise to fitted distances, 
# dij, and that these distances are
# related to a set of numbers which we will 
# call 'disparities', ^dij, by the formula
# dij = ^dij + eij , where the eij are error 
# terms representing errors of measurement
# plus distortion errors arising because the 
# distances do not correspond to a configuration
# in the particular number of dimensions chosen. 

# The disparities are monotonic with the observed 
# proximities and, subject to this constraint,
# resemble the fitted distances as closely as 
# possible.

### Example Non-metric MD Scaling
## House of Representatives Voting

# Set of data that shows the number of times 
# 15 congressmen from New Jersey voted 
# differently in the House of Representatives
# on 19 environmental bills. Abstentions are 
# not recorded, but two congressmen abstained
# more frequently than the others, Sandman 
# (nine abstentions) and Thompson (six). 

# The question of interest is whether party 
# affiliations can be detected in the data.

library("MASS")
data("voting", package = "HSAUR2")

# look at data: R-Republican; D-Democrat
# is a symmetrical matrix...shows number of 
# times each congressman voted differently
# on 19 environmental bills
voting

# apply non-metric scaling with function
# isoMDS() from MASS package
voting_mds <- isoMDS(voting)

# two dimensional solution:
voting_mds

# we plot the two-dimensional solution:
x <- voting_mds$points[,1]
y <- voting_mds$points[,2]
plot(x, y, xlab = "Coordinate 1", 
     ylab = "Coordinate 2",
     xlim = range(voting_mds$points[,1])*1.2, 
     type = "n")
text(x, y, labels = colnames(voting), cex = 0.6)

# Resulting plot suggests that voting behavior
# is essentially along party lines, but with
# more variation among Republicans.

# Voting behavior of Rinaldo (R) seems closer
# to his Democratic colleagues than to voting
# behavior of other Republicans

# Quality of MD scaling can be assessed informally
# by plotting original dissimilarities and the
# distances obtained for an MD scaling in a
# scatterplot, a so-called Shepard diagram.

# Ideally, points fall on the bisecting line.
# In our case, there are some deviations.

voting_sh <- Shepard(voting[lower.tri(voting)],
                     voting_mds$points)
plot(voting_sh, pch = ".", xlab = "Dissimilarity",
     ylab = "Distance", xlim = range(voting_sh$x), 
     ylim = range(voting_sh$x))
lines(voting_sh$x, voting_sh$yf, type = "S")

### Non-Metric Example: WWII Leaders Judgements

# Get spatial representations of the judgements
# of the dissimilarities in ideology of a number
# of world leaders and politicians prominent
# during WWII

# They made judgements on a nine-point scale with
# anchor points of 1 ("very similar) and 
# 9 ("very dissimilar")

WWIIleaders <- c(
3,
4, 6, 
7, 8, 4, 
3, 5, 6, 8,
8, 9, 3, 9, 8, 
3, 2, 5, 7, 6, 7,
4, 4, 3, 5, 6, 5, 4,  
8, 9, 8, 9, 6, 9, 8, 7,       
9, 9, 5, 4, 7, 8, 8, 4, 4,
4, 5, 5, 4, 7, 2, 2, 5, 9, 5,
7, 8, 2, 4, 7, 8, 3, 2, 4, 5, 7)
tmp <- matrix(0, ncol = 12, nrow = 12)
tmp[upper.tri(tmp)] <- WWIIleaders
tmp <- tmp + t(tmp)
rownames(tmp) <- colnames(tmp) <- c("Hitler", 
                                    "Mussolini", 
                                    "Churchill",
                                    "Eisenhower", 
                                    "Stalin", 
                                    "Attlee", 
                                    "Franco", 
                                    "De Gaulle", 
                                    "Mao Tse-Tung",
                                    "Truman", 
                                    "Chamberlin", 
                                    "Tito")
WWIIleaders <- as.dist(tmp)

# show subjective distances in judgements
# on a nine-point scale
WWIIleaders

# we apply non-metric MD scaling to these distances
(WWII_mds <- isoMDS(WWIIleaders))

# We plot the two-dimensional solution
x <- WWII_mds$points[,1]
y <- WWII_mds$points[,2]
lim <- range(c(x, y)) * 1.2
plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2",
     xlim = lim, ylim = lim, type = "n")
text(x, y, labels = labels(WWIIleaders), cex = 0.7)

# We conclude:

# three fascists (Mussolini, Franco, Hitler) group
# together as do three British PMs (Attlee, Churchill,
# Chamberlin)

# Eisenhower somewhat related to British than to Truman.

# Stalin and Mao are isolated

# de Gaulle is placed in the center of the MDS solution.

