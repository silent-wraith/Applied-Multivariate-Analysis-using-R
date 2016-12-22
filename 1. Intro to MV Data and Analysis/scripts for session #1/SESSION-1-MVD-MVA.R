################################################
#####    MULTIVARIATE DATA AND ANALYSIS    #####
################################################

# We use data in MVA package:
library("MVA")

# 'Just in case':
set.seed(280875)

# Multivariate data arise when researchers record 
# the values of several random variables on a 
# number of subjects or objects or perhaps one 
# of a variety of other things (we will use the 
# general term\units") in which they are interested,
# leading to a vector-valued or multidimensional 
# observation for each.

# Most multivariate data sets can be represented 
# in the same way, namely in a rectangular format
# known from spreadsheets, in which the elements 
# of each row correspond to the variable values 
# of a particular unit in the data set and the
# elements of the columns correspond to the 
# values taken by a particular variable.

### SEE SLIDE

# Although in some cases where multivariate data 
# have been collected it may make sense to isolate
# each variable and study it separately, in the 
# main it does not. Because the whole set of 
# variables is measured on each unit, the variables
# will be related to a greater or lesser degree. 
# Consequently, if each variable is analysed in 
# isolation, the full structure of the data may 
# not be revealed.

# Multivariate statistical analysis is the 
# simultaneous statistical analysis of a
# collection of variables, which improves upon 
# separate univariate analyses of each variable
# by using information about the relationships 
# between the variables.

# Analysis of each variable separately is very 
# likely to miss uncovering key features of, and
# patterns in, the multivariate data.

### Exploratory versus Inferential

# The units in a set of multivariate data are 
# sometimes sampled from a population of
# interest, but often not. Consequently, there 
# are methods of multivariate analysis that 
# are essentially exploratory and others that
# can be used for statistical inference.

# Exploratory: PCA, MDS, EFA and CA
# Inferential: CFA and SEM

#################################################
##### Types of Variables and Missing Values #####
#################################################

# Create hypothetical MV data on 10 people
# Number of variables q = 7
hypo <-
  structure(list(individual = 1:10, sex = structure(c(2L, 2L, 2L,
    2L, 2L, 1L, 1L, 1L, 1L, 1L), .Label = c("Female", "Male"), class = "factor"),
    age = c(21L, 43L, 22L, 86L, 60L, 16L, NA, 43L, 22L, 80L),
    IQ = c(120L, NA, 135L, 150L, 92L, 130L, 150L, NA, 84L, 70L),
    depression = structure(c(2L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,
    1L, 1L), .Label = c("No", "Yes"), class = "factor"), health = structure(c(3L,
    3L, 1L, 4L, 2L, 2L, 3L, 1L, 1L, 2L), .Label = c("Average",
    "Good", "Very good", "Very poor"), class = "factor"), weight = c(150L,
    160L, 135L, 140L, 110L, 110L, 120L, 120L, 105L, 100L)), .Names = c("individual",
    "sex", "age", "IQ", "depression", "health", "weight"), class = "data.frame", row.names = c(NA, -10L))

# take a look
hypo
str(hypo)

# have missing data, NAs on rows 2, 7 and 8

# Because is a dataframe can extract and
# subset out parts of the data:

hypo[1:2, c("health", "weight")]

# Four types of numeric data (measurements):

# Nominal: Unordered categorical variables. 
# Examples include treatment allocation,
# the sex of the respondent, hair color, 
# presence or absence of depression, etc

# Ordinal: Where there is an ordering but 
# no implication of equal distance between
# the different points of the scale. 
# Examples include social class, self-
# perception of health (each coded from 
# I to V, say), and educational level
# (no schooling, primary, secondary, or 
# tertiary education).

# Interval: Where there are equal 
# differences between successive points 
# on the scale but the position of zero 
# is arbitrary. The classic example is 
# the measurement of temperature using 
# the Celsius or Fahrenheit scales.

# Ratio: The highest level of measurement, 
# where one can investigate the relative
# magnitudes of scores as well as the 
# differences between them. The position
# of zero is fixed. The classic example 
# is the absolute measure of temperature
# (in Kelvin, for example), but other 
# common ones include age, weight, and 
# length.

### MISSING VALUES

# Are a problem. Typical in longitudinal studies
# where people drop out

# Can take 'complete case' analysis route -
# throw out incomplete cases, but at a cost.

# Can examine complete and incomplete data
# separately to make sure responses seem 
# consistent.

# Can impute missing values-multiple imputation,
# are numerous approaches

# Can ignore missing values

##############################################
### Create measure Data
##############################################

# Let's create some datasets and ask some
# questions about the data and see what sort
# of MVA technique might answer the question(s)

# Measure data: chest, waist, and hip
# measurements on a sample of men and women
measure <-
  # first create list of raw measures
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
            # make it a dataframe
            class = "data.frame", 
            # no row names
            row.names = c(NA, -20L))
# put everything but 1:20 measures in measure
measure <- measure[,-1]
# name the columns appropriately
names(measure) <- c("chest", "waist", "hips")
# generate factor levels for new variable
# which we are naming 'gender'
measure$gender <- gl(2, 10)
# name the levels for gender
levels(measure$gender) <- c("male", "female")

# Take a look at data . . .

# look at first six records:
head(measure)
# how many rows?
nrow(measure)
# how many columns?
ncol(measure)
# what is the structure of the data?
str(measure) # all integer except gender

# Some (researchy) questions:

# 1) Could body size and body shape be
#    summarized in some way by combining the
#    three measurements into a single number?

# Might use Principal Components Analysis

# 2) Are there subtypes of body shapes for the
#    men and for the women within which individuals
#    are of similar shapes and between which
#    body shapes differ?

# Might address with Cluster Analysis

# (Need more than 20 observations to do this tho)

### Pottery data from HSAUR2 package

# Our second set of multivariate data consists 
# of the results of chemical analysis on
# Romano-British pottery made in three different 
# regions (region 1 contains kiln 1, region 2 
# contains kilns 2 and 3, and region 3 contains 
# kilns 4 and 5). 

# One question that might be posed about these
# data is whether the chemical profiles of each 
# pot suggest different types of pots and if
# any such types are related to kiln or region. 

# We address in section on Cluster Analysis

data("pottery", package = "HSAUR2")
head(pottery)
nrow(pottery)
ncol(pottery)
str(pottery) # all numeric except kiln

# Our third set of multivariate data involves 
# the examination scores of a large number
# of college students in six subjects. 

# Here the main question of interest might be 
# whether the exam scores reflect some under
# lying trait in a student that cannot be 
# measured directly, perhaps "general intelli-
# gence"? The question could be investigated by
# using exploratory factor analysis

exam <-
  structure(list(subject = 1:5, 
                 math = c(60L, 80L, 53L, 85L, 45L),
                 english = c(70L, 65L, 60L, 79L, 80L), 
                 history = c(75L, 66L, 50L, 71L, 80L), 
                 geography = c(58L, 75L, 48L, 77L, 84L), 
                 chemistry = c(53L,70L, 45L, 68L, 44L), 
                 physics = c(42L, 76L, 43L, 79L, 46L)), 
            .Names = c("subject", "maths", 
                       "english", "history", 
                       "geography", "chemistry", 
                       "physics"),
            class = "data.frame", 
            row.names = c(NA, -5L))

head(exam)
nrow(exam)
ncol(exam)
str(exam) # all 7 variables are integers

# The final set of data we shall consider 
# is from a study of air pollution in cities 
# in the USA. The following variables were 
# obtained for 41 US cities:

#  SO2: SO2 content of air in micrograms per 
# cubic metre;

# temp: average annual temperature in degrees F;

# manu: number of manufacturing enterprises 
# employing 20 or more workers;

# popul: population size (1970) in thousands;

# wind: average annual wind speed mph;

# precip: average annual precipitation in inches;

# predays: average annual days with precip.

data("USairpollution", package = "HSAUR2")
head(USairpollution)
nrow(USairpollution)
ncol(USairpollution)
str(USairpollution) # mix of numeric and integers

# Questions?:
# Maybe how is pollution level as measured by 
# sulphur dioxide concentration related to the 
# six other variables?" 

# Could answer with multiple linear regression, 
# with sulphur dioxide as the response var and
# the remaining six variables being the IVs.

# But in the model underlying multiple regression, 
# only the response is considered a random
# variable; the explanatory variables are 
# strictly assumed to be fixed, not random.

# There is only a single random variable so
# a linear regression is 'multivariable', not
# 'multivariate'

# We are not going to deal with linear models
# at all in this course

##############################################
### COVARIANCES, CORRELATIONS, and DISTANCES
##############################################

# We analyze a multivariate data set using 
# multivariate methods rather than looking 
# at each variable separately using one or
# another familiar univariate method because
# structures or patterns in the data are as
# likely to be implied either by "relationships" 
# between the variables or by the relative
# "closeness" of different units as by their 
# different variable values.

# But, sometimes, by both.

# So we might look for "links" in the columns
# of the data matrix, X, or involving interesting 
# subsets of the units. 

# So how do we quantify these relationships
# between the variables and how do we measure
# 'distances'?

### COVARIANCES

### SEE SLIDES

# The covariance of two random variables is a 
# measure of their linear dependence.

# The population (theoretical) covariance of 
# two random variables, Xi and Xj, is defined 
# by Cov(Xi,Xj) = E(Xi-ui)(Xj-uj) where
# ui=E(Xi) and uj=E(Xj); E is 'expectation'

# If i=j the covariance of a variable with
# itself is its variance. If Xi and Xj are
# independent of each other their covariance
# is necessarily = 0

# In a multivariate dataset with q observed
# variables, there are q variances and
# q(q-1)/2 covariances

# to get covariance matrix for measure data...
# Have to remove gender.
cov(measure[, c("chest", "waist", "hips")])

# Separately for men and women:
cov(subset(measure, gender == "female")[, 
           c("chest", "waist", "hips")])

cov(subset(measure, gender == "male")[, 
           c("chest", "waist", "hips")])

### CORRELATIONS

### SEE SLIDES

# Covariances hard to interpret sometimes because
# of scaling, thus it is often standardized by
# dividing by the product of the standard 
# deviations of the two variables to yield
# the correlation coefficient, pij, where
# pij = COVij/(SDi)(SDj)

# Advantage: Is independent of scales of the two
# variables, is between -1 and +1 and gives a
# measure of the linear relationship of the
# variables Xi and Xj.

# However Xi and Xj could have a nonlinear
# relationship that is not indicated by COR

# If have q variables, there are q(q-1)/2
# distinct correlations

# R function cor() yields sample correlation
# matrix
cor(measure[, c("chest", "waist", "hips")])

### DISTANCE

### SEE SLIDES

# For some multivariate techniques (MDS, CA)
# the concept of distance between the units
# in the data is relevant.

# Given the variable values for two units, 
# unit i and unit j, what is a good measure
# of distance between them to use? 

## Euclidean Distance

# Distances depend on measurements scales so
# should standardize them

# scale() function divides each variable by its 
# standard deviation. Then apply dist() function

?scale
?dist

x <- dist(scale(measure[, 
                        c("chest", 
                          "waist", 
                          "hips")], 
                center = FALSE))
x

# just show first 12 rounded
as.dist(round(as.matrix(x), 2)[1:12, 1:12])
cat("...")

## Multivariate Normal Density Function

# Just as the normal distribution dominates 
# univariate techniques, the multivariate normal
# distribution plays an important role in some 
# multivariate procedures,

# Good to know to know a little about the 
# multivariate density function and how to 
# assess whether or not a set of multivariate 
# data can be assumed to have these properties.

# One property of a multivariate normal density 
# function that is worth mentioning here is that 
# linear combinations of the variables 
# (i.e., y = a1X1 + a2X2 +    + aqXq, where 
# a1; a2; : : : ; aq is a set of scalars) are
# themselves normally distributed 

library("mvtnorm")

# Here we draw a bivariate normal density
# function with correlation p = 0.05
x <- y <- seq(from = -3, to = 3, length = 50)
x
y
dat <- as.matrix(expand.grid(x, y))
dat
# dmvnorm yields density given x and y
d <- dmvnorm(dat, mean = c(0, 0), 
             sigma = matrix(c(1, 0.5, 0.5, 1), 
                            ncol = 2))
d
d <- matrix(d, ncol = length(x))
d
# draws the plot
persp(x = x, y = y, z = d, 
      xlab = "x1", 
      ylab = "x2",
      zlab = "f(x)")

# For many multivariate methods, the 
# assumption of multivariate normality is not 
# critical to the results of the analysis, but
# may need to test for multivariate normality 
# sometimes.

# Can start by assessing each variable separately 
# for univariate normality using a probability plot.
# Such plots are commonly applied in univariate
# analysis and involve ordering the observations 
# and then plotting them against the appropriate
# values of an assumed cumulative distribution
# function.

# There are two basic types of plots for comparing 
# two probability distributions, the probability-
# probability plot and the quantile-quantile plot.

# This code draws cumulative distribution 
# functions and quantiles
x <- seq(from = -3, to = 3, length = 1000)
Fx <- pnorm(x)
Fy <- pnorm(x, mean = -1)
plot(x, Fx, type = "l", axes = FALSE, xlab = "",
     ylab = "Cumulative distribution function") 
lines(x, Fy, type = "l")
x0 <- which.min((x - 1.2)^2)
x05 <- which.min((x + 0.5)^2)
x08 <- which.min((x + 0.9)^2)
xx <- which.min(abs(Fy - Fx[x0]))
arrows(x0 = c(min(x), x[x0], x[xx], 
              x[x08], x[x08], x[x08]),
       y0 = c(Fx[x0], Fx[x0], Fy[xx], 
              0, Fx[x08], Fy[x08]), 
       x1 = c(x[x0], x[x0], x[xx], x[x08], 
              -3, -3), 
       y1 = c(Fx[x0], 0, 0, Fy[x08], 
              Fx[x08], Fy[x08]))
mtext(at = c(x[x08], x[xx], x[x0]), 
      side = 1, line = 1, text =
      c(expression(q), expression(q[2](p)), 
        expression(q[1](p))))
mtext(at = c(0, Fx[x08], Fy[x08], Fx[x0], 1), 
      line = 1, side = 2, text =
      c(0, expression(p[1](q)), 
        expression(p[2](q)), 
        expression(p), 1)) 
box()

# Want to assess body measurement data for
# normality
x <- measure[, c("chest", "waist", "hips")]

# Estimate the means of all three variables
cm <- colMeans(x)
# and the covariance matrix
S <- cov(x)

# Compute differences di for all units in our
# data, so we iterate over rows using apply()
d <- apply(x, MARGIN = 1, function(x) 
           t(x - cm) %*% solve(S) %*% (x - cm))

# Normal probability plots of chest,
# waist, and hip measurements separately:
qqnorm(measure[,"chest"], main = "chest")
qqline(measure[,"chest"])
qqnorm(measure[,"waist"], main = "waist") 
qqline(measure[,"waist"])
qqnorm(measure[,"hips"], main = "hips") 
qqline(measure[,"hips"])

# Sorted distances can now be plotted against
# appropriate quantiles of chi-squared dist.

# Plotting the ordered distances against the
# corresponding quantiles of appropriate chi-sq
# distribution should lead through straight
# line through the origin.
plot(qchisq((1:nrow(x) - 1/2) / nrow(x), df = 3), sort(d),
     xlab = expression(paste(chi[3]^2, " Quantile")), 
     ylab = "Ordered distances")
abline(a = 0, b = 1)

# Do seem to deviate from normality a bit but
# we have so few observations it is hard to tell.

# Use chi-square plot on air pollution data

# Probability plots for each separate variable
layout(matrix(1:8, nc = 2))
# Iterate over all variables with sapply() that
# loops over variable names
sapply(colnames(USairpollution), function(x) {
    qqnorm(USairpollution[[x]], main = x)
    qqline(USairpollution[[x]])
})

# The plots for SO2 concentration and precipitation
# both deviate considerably from linearity, and 
# the plots for manufacturing and population show
# evidence of a number of outliers. But of more 
# importance is the chi-square plot for the data.

# R script is identical to the code used to 
# produce the chi-square plot for the body
# measurement data. 

# The two most extreme points in the plot have
# been labelled with the city names to which 
# they correspond using text().
x <- USairpollution
cm <- colMeans(x)
S <- cov(x)
d <- apply(x, 1, function(x) t(x - cm) %*% solve(S) %*% (x - cm))
plot(qc <- qchisq((1:nrow(x) - 1/2) / nrow(x), df = 7), 
     sd <- sort(d),
     xlab = expression(paste(chi[7]^2, 
                             " Quantile")), 
     ylab = "Ordered distances", 
     xlim = range(qc) * c(1, 1.1))
oups <- which(rank(abs(qc - sd), 
                   ties = "random") > nrow(x) - 3)
text(qc[oups], 
     sd[oups] - 1.5, 
     names(oups))
abline(a = 0, b = 1)
