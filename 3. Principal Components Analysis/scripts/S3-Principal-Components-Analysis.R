###############################################
#####    PRINCIPAL COMPONENTS ANALYSIS    #####
###############################################
library("MVA")
library("lattice")

### DEMONSTRATE THAT PRINCIPAL COMPONENTS
### OF COVARIANCE MATRIX CAN DIFFER FROM
### PC EXTRACTED FROM CORRELATION MATRIX

### Create the blood correlation matrix
### this is upper triangular matrix
bc <- c(
 0.290,           
 0.202,  0.415,       
-0.055,  0.285,  0.419,       
-0.105, -0.376, -0.521, -0.877,      
-0.252, -0.349, -0.441, -0.076,  0.206,
-0.229, -0.164, -0.145,  0.023,  0.034,  0.192,
 0.058, -0.129, -0.076, -0.131,  0.151,  0.077,  0.423)

# These are the given standard deviations
# of the eight variables:
blood_sd <- c(rblood = 0.371, 
              plate = 41.253,  
              wblood = 1.935,
              neut = 0.077, 
              lymph = 0.071, 
              bilir = 4.037,
              sodium = 2.732, 
              potass = 0.297)

# create correlation matrix 
# of blood data
?diag

# initializes blood_corr data structure
# as a matrix
blood_corr <- diag(length(blood_sd))/2

# Coerces bc to be upper triangular matrix
blood_corr[upper.tri(blood_corr)]<-bc  

# adds the transposed lower trian. matrix
blood_corr<-blood_corr+t(blood_corr)

# calculates covariance matrix
blood_cov<-blood_corr*outer(blood_sd,
                            blood_sd, 
                            # multiplies them
                            "*")
?outer
# Blood Correlation Matrix
blood_corr

# Standard Deviations of eight variables
blood_sd

# Apply PCA to both corr and cov matrices
blood_pcacov<-princomp(covmat=blood_cov)
summary(blood_pcacov, 
        loadings = TRUE)
blood_pcacor<-princomp(covmat=blood_corr)
summary(blood_pcacor, 
        loadings=TRUE)

# Calculate Principal Components Scores
# Scree diagram:
plot(blood_pcacor$sdev^2, 
     xlab = "Component number",
     ylab = "Component variance", 
     type = "l", main = "Scree diagram")

# Log(eigenvalue) diagram:
plot(log(blood_pcacor$sdev^2), 
     xlab = "Component number",
     ylab = "log(Component variance)", 
     type="l",
     main = "Log(eigenvalue) diagram")

##### EXAMPLES OF PCA APPLICATIONS

## First one is for only two variables
## to illustrate a point

## Head lengths of first and second sons

# The data give the head lengths and head breadths 
# (in millimetres) for each of the first two adult sons 
# in 25 families. Here we shall use only the head
# lengths; the head breadths will be used later. 

"headsize" <-
  matrix(c(191, 195, 181, 183, 176, 208, 189, 197, 188, 192, 179, 183, 174, 190, 188, 163, 195, 186, 181, 175, 192, 174,
           176, 197, 190, 155, 149, 148, 153, 144, 157, 150, 159, 152, 150, 158, 147, 150, 159, 151, 137, 155, 153,
           145, 140, 154, 143, 139, 167, 163, 179, 201, 185, 188, 171, 192, 190, 189, 197, 187, 186, 174, 185, 195,
           187, 161, 183, 173, 182, 165, 185, 178, 176, 200, 187, 145, 152, 149, 149, 142, 152, 149, 152, 159, 151,
           148, 147, 152, 157, 158, 130, 158, 148, 146, 137, 152, 147, 143, 158, 150)
         , nrow = 25, ncol = 4
         ,  dimnames = list(character(0)
                            , c("head1", "breadth1", "head2", "breadth2")))

# headsize is a matrix now
x <- headsize

# so we make it a dataframe
headsize <- as.data.frame(headsize)

# The mean vector and covariance matrix of the head 
# length measurements are found using
head_dat <- headsize[, c("head1", "head2")]
colMeans(head_dat)

# here is covariance matrix
cov(head_dat)

# The principal components of these data, extracted 
# from covariance matrices can be found using:
head_pca <- princomp(x = head_dat)
head_pca

# creates the loadings
print(summary(head_pca), loadings = TRUE)
?summary.princomp
# and they are y1 =  0.693(x1) + 0.721(x2)
# and          y2 = -0.721(x1) + 0.693(x2)
# with variances 167.77 and 28.33

# First principal component accounts for a proportion
# 167.77 / (167.77+28.33) = 0.86 of total variance in the
# original variables.

# Note total variance of the principal components is 196.10
# which should be (and is) equal to the total variance
# of the original variables (95.29 = 100.81 = 196.10)

# How interpret two derived components? The first component
# is essentially the sum of the head lengths of the two 
# sons, and the second component is the difference in 
# head lengths. 

# To calculate an individual's score on a component, 
# we multiply the variable values minus the appropriate 
# mean by the loading for the variable and add these
# values over all variables.

# We can illustrate this calculation using the data
# for the first family, where the head length of the 
# first son is 191 mm and for the second son 179 mm. 

# The score for this family on the first principal
# component is calculated as
# 0.693 x (191-185.72) + 0.721 x (179-183.84)=0.169;

# and on the second component the score is
# -0.721 x (191-185.72) + 0.693 x (179-183:84)= -7.61

# The variance of the first principal components scores 
# will be 167.77, and the variance of the second
# principal component scores will be 28.33.

# 183.84 is colMean of head2 (x2)
# -0.721 is x1 loading on y2 (PC2)
#  0.693 is x2 loading on y2 (PC2)
#  0.693 is also x1 loading on y1 (PC1)
# 185.72 is colMean of head1 (x1)
#  0.721 is x2 loading on y1 (PC1)
a1 <- 183.84-0.721*185.72/0.693
b1 <-   0.721/0.693
a2 <- 183.84-(-0.693*185.72/0.721)
b2 <-  -0.693/0.721

# plot head lengths 1st, 2nd sons, axes correspond
# to principal components of sample covariance matrix
plot(head_dat, xlab = "First son's head length (mm)",
     ylab = "Second son's head length")

# plot first PC (more or less sum of head lengths)
abline(a1, b1)

# plot second PC (more or less diffs of head lengths)
abline(a2, b2, lty = 2)

# Note: PCs are orthogonal

# Plot the scores for each family on PC1 and PC2
# They are unrelated
xlim <- range(head_pca$scores[,1])
plot(head_pca$scores, 
     xlim = xlim, ylim = xlim)

# Plot the scores for each family on PC1 and PC2
# They are unrelated even with narrower range
# of values on PC2
xlim <- range(head_pca$scores[,2])
plot(head_pca$scores, 
     xlim = xlim, ylim = xlim)

### Olympic Heptathlon Results
### First pentathlon for women held Germany 1928

## shot put, long jump, 100m, high jump, javelin
data("heptathlon",package="HSAUR2")

## Want to score everything in same direction
## so that "large" values are indicative of a
## "better" performance

## Have to reverse the 
## values of some events
heptathlon$hurdles <- with(heptathlon, 
                           max(hurdles)-hurdles)
heptathlon$run200m <- with(heptathlon, 
                           max(run200m)-run200m)
heptathlon$run800m <- with(heptathlon, 
                           max(run800m)-run800m)

# don't want score column
score <- which(colnames(heptathlon) == "score")

# as is typical of R, we do some pre-processing

# calculate the correlation coefficients
# between the ten events
round(cor(heptathlon[,-score]), 2)

# note javelin is not correlated with
# any other event really

# some highly positively correlated (high jump
# and hurdles), some moderately (high jump and 
# shot)

# Plot scatterplot matrix
plot(heptathlon[,-score])

# Same plot with finer points
plot(heptathlon[,-score], 
     pch = ".", cex = 1.5)

# all events except javelin there is an outlier
# who performs poorly than other athletes...
# competitor from Papua New Guinea (PNG) who
# finished last in the competition in terms of 
# points scored

# except for javeline, where she is third highest

# so we remove her and look at correlation and
# scatterplot matrices again

# We remove PNG participant
# grep() searches for a string 'PNG'
heptathlon <- heptathlon[-grep("PNG", 
                               rownames(heptathlon)),]

# still do not want score
score <- which(colnames(heptathlon) == "score")

# look at correlations again
round(cor(heptathlon[,-score]), 2)

# Note javelin correlations increase considerably

# Further, there is an overall large change
# in the correlation matrix by booting PNG so
# we will extract principal components from'
# correlation matrix after PNG removal

# New scatterplot matrix
plot(heptathlon[,-score], 
     pch = ".", cex = 5)

# change options to show only
# two decimal points
op <- options(digits = 2)

# Now extract principal components
# from correlation matrix
heptathlon_pca <- prcomp(heptathlon[, -score], 
                         scale = TRUE)
print(heptathlon_pca)
?prcomp
# note are using prcomp() 
# perhaps for scale argument (see below)
?princomp
?prcomp

# summary used for further inspection 
# of details
summary(heptathlon_pca)

# change options back to 4 digits
op <- options(digits = 4)

# Linear combination for first principal
# component is (first column above)
a1 <- heptathlon_pca$rotation[,1]
a1

# We see that the hurdles and long jump 
# events receive the highest weight but
# the javelin result is less important. 
# For computing the first principal comp-
# onent, the data need to be rescaled 
# appropriately. The center and the scaling
# used by prcomp internally can be extracted 
# from the heptathlon_pca by:
center <- heptathlon_pca$center
scale <- heptathlon_pca$scale

# We can apply the scale function to the 
# data and multiply it with the loadings 
# matrix in order to compute the first 
# principal component score for each
# competitor:
hm <- as.matrix(heptathlon[,-score])
drop(scale(hm, center = center, 
           scale = scale) %*% 
     heptathlon_pca$rotation[,1])

# or, by extracting the first from all pre-
# computed principal components:
predict(heptathlon_pca)[,1] # same

# The first two components account for 75% 
# of the variance. A barplot of each compo-
# nent's variance shows how the first two 
# components dominate (with PNG removed).
plot(heptathlon_pca, main = "")

# The correlation between the score given 
# to each athlete by the standard scoring
# system used for the heptathlon and the 
# first principal component score can be
# found from
cor(heptathlon$score, heptathlon_pca$x[,1])

# + or - coefficient not important due to
# arbitrariness of signs of coefficients
# defining the first principal component;
# is magnitude of coefficient that is
# important

# This implies that the first principal 
# component is in good agreement with the
# score assigned to the athletes by offcial 
# Olympic rules; a scatterplot of the official
# score and the first principal component is:                                                                  it is the magnitude of the correlation that is important.)
plot(heptathlon$score, heptathlon_pca$x[,1])

##### The biplot

# The biplot is a graphical representation 
# of the information in an n x p data matrix.
# "bi" reflects that the technique displays 
# in a single diagram the variances and 
# covariances of the variables.

# Here is biplot for the heptathlon data 
# omitting PNG competitor.

# Shows that the winner of the gold medal, 
# Jackie Joyner-Kersee, accumulates majority
# of her points from the three events long jump, 
# hurdles, and 200 m.

# Shows competitors plotted on their relative
# positions for PC1 and PC2

biplot(heptathlon_pca, col=c("gray", "black"))

# Can also see from the biplot that 
# results of the 200 m, the hurdles
# and the long jump are highly correlated,
# as are javelin and high jump, and that 800 m
# time has relatively small correlation.

# The first component largely separates the 
# competitors by their overall score, with the
# second indicating which are their best events; 
# for example, John, Choubenkova, and Behmer 
# are placed near the end of the vector, 
# representing the 800 m event because this is,
# relatively speaking, the event in which they
# give their best performance. Similarly Yuping, 
# Scheider, and Braun can be seen to do well
# in the high jump.
