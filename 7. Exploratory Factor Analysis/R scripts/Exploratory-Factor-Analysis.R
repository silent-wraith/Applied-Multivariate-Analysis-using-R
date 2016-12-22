########################################
###   EXPLORATORY FACTOR ANALYSIS    ###
########################################

# Both "intelligence" and "social class" are 
# what are generally referred to as "latent
# variables (concepts that cannot be measured 
# directly but can be assumed to relate to a 
# number of measurable or manifest variables. 

# The method of analysis most generally used
# to help uncover the relationships between the
# assumed latent variables and the manifest 
# variables is factor analysis. 

# The model on which the method is based is 
# essentially that of multiple regression,
# except now the manifest variables are 
# regressed on the unobservable latent variables
# (often referred to in this context as common 
# factors), so that direct estimation of the
# corresponding regression coefficients 
# (factor loadings) is not possible.
                 
# Are two varieties of factor analysis:

# Exploratory where make no assumptions about
# which manifest variables belong to which
# latent variables; and

# Confirmatory used to test whether a specific 
# model provides an adequate fit for the 
# covariances or correlations between the 
# manifest variables.

## Simple Example of a Factor Analysis Model

## TWO EXAMPLES OF EFA

# Expectations of Life

library("MVA")
library("lattice")
#############

# Create druguse data:
d <-
  c(0.447,          
    0.422, 0.619,       
    0.435, 0.604, 0.583,        
    0.114, 0.068, 0.053, 0.115,        
    0.203, 0.146, 0.139, 0.258, 0.349,   
    0.091, 0.103, 0.110, 0.122, 0.209, 0.221,
    0.082, 0.063, 0.066, 0.097, 0.321, 0.355, 0.201,
    0.513, 0.445, 0.365, 0.482, 0.186, 0.315, 0.150, 0.154,
    0.304, 0.318, 0.240, 0.368, 0.303, 0.377, 0.163, 0.219, 0.534,
    0.245, 0.203, 0.183, 0.255, 0.272, 0.323, 0.310, 0.288, 0.301, 0.302,
    0.101, 0.088, 0.074, 0.139, 0.279, 0.367, 0.232, 0.320, 0.204, 0.368, 0.340,
    0.245, 0.199, 0.184, 0.293, 0.278, 0.545, 0.232, 0.314, 0.394, 0.467, 0.392, 0.511)

druguse <- diag(13) / 2
druguse[upper.tri(druguse)] <- d
druguse <- druguse + t(druguse)

rownames(druguse) <- colnames(druguse) <- c("cigarettes", "beer", "wine", "liquor", "cocaine",
                                            "tranquillizers", "drug store medication", "heroin",
                                            "marijuana", "hashish", "inhalants", "hallucinogenics", "amphetamine")

write.csv(druguse,"druguse.csv")

# Create life data: Life Expectancies
# for Different Countries

"life" <-
  structure(.Data = list(c(63., 34., 38., 59., 56., 62., 50., 65., 56., 69., 65., 64., 56., 60., 61., 49., 59., 63., 59., 65., 65., 64.,
                           64., 67., 61., 68., 67., 65., 59., 58., 57.)
                         , c(51., 29., 30., 42., 38., 44., 39., 44., 46., 47., 48., 50., 44., 44., 45., 40., 42., 44., 44., 48., 48., 63.,
                             43., 45., 40., 46., 45., 46., 43., 44., 46.)
                         , c(30., 13., 17., 20., 18., 24., 20., 22., 24., 24., 26., 28., 25., 22., 22., 22., 22., 23., 24., 28., 26., 21.,
                             21., 23., 21., 23., 23., 24., 23., 24., 28.)
                         , c(13., 5., 7., 6., 7., 7., 7., 7., 11., 8., 9., 11., 10., 6., 8., 9., 6., 8., 8., 14., 9., 7., 6., 8., 10., 8.,
                             8., 9., 10., 9., 9.)
                         , c(67., 38., 38., 64., 62., 69., 55., 72., 63., 75., 68., 66., 61., 65., 65., 51., 61., 67., 63., 68., 67., 68.,
                             68., 74., 67., 75., 74., 71., 66., 62., 60.)
                         , c(54., 32., 34., 46., 46., 50., 43., 50., 54., 53., 50., 51., 48., 45., 49., 41., 43., 48., 46., 51., 49., 47.,
                             47., 51., 46., 52., 51., 51., 49., 47., 49.)
                         , c(34., 17., 20., 25., 25., 28., 23., 27., 33., 29., 27., 29., 27., 25., 27., 23., 22., 26., 25., 29., 27., 25.,
                             24., 28., 25., 29., 28., 28., 27., 25., 28.)
                         , c(15., 6., 7., 8., 10., 14., 8., 9., 19., 10., 10., 11., 12., 9., 10., 8., 7., 9., 8., 13., 10., 9., 8., 10., 11.,
                             10., 10., 10., 12., 10., 11.)
  )
            , class = "data.frame" 
            , names = c("m0", "m25", "m50", "m75", "w0", "w25", "w50", "w75")
            , row.names = c("Algeria", "Cameroon", "Madagascar", "Mauritius", "Reunion", "Seychelles", "South Africa (C)", "South Africa (W)",
                            "Tunisia", "Canada", "Costa Rica", "Dominican Rep.", "El Salvador", "Greenland", "Grenada", "Guatemala",
                            "Honduras", "Jamaica", "Mexico", "Nicaragua", "Panama", "Trinidad (62)", "Trinidad (67)",
                            "United States (66)", "United States (NW66)", "United States (W66)", "United States (67)", "Argentina",
                            "Chile", "Colombia", "Ecuador")
  )

write.csv(life,"life.csv")

# Use formal test for number of factors
# in the maximum likelihood approach

?factanal

#### Demo of factanal from help screen

# There are so many variations on factor analysis 
# that it is hard to compare output from different 
# programs. Further, the optimization in maximum 
# likelihood factor analysis is hard, and many other 
# examples we compared had less good fits than 
# produced by this function

# A little demonstration, v2 is just v1 with noise,
# and same for v4 vs. v3 and v6 vs. v5
# Last four cases are there to add noise
# and introduce a positive manifold (g factor)
v1 <- c(1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,4,5,6)
v2 <- c(1,2,1,1,1,1,2,1,2,1,3,4,3,3,3,4,6,5)
v3 <- c(3,3,3,3,3,1,1,1,1,1,1,1,1,1,1,5,4,6)
v4 <- c(3,3,4,3,3,1,1,2,1,1,1,1,2,1,1,5,6,4)
v5 <- c(1,1,1,1,1,3,3,3,3,3,1,1,1,1,1,6,4,5)
v6 <- c(1,1,1,2,1,3,3,3,4,3,1,1,1,2,1,6,5,4)
m1 <- cbind(v1,v2,v3,v4,v5,v6)
cor(m1)
factanal(m1, factors = 3) # varimax is the default
factanal(m1, factors = 3, rotation = "promax")
# The following shows the g factor as PC1
prcomp(m1) # signs may depend on platform

## formula interface
factanal(~v1+v2+v3+v4+v5+v6, factors = 3,
         scores = "Bartlett")$scores

## a realistic example from Bartholomew (1987, pp. 61-65)
utils::example(ability.cov)

#### Back to MVA Material

# To begin, we use the formal test for the number
# of factors using mle:
sapply(1:3, function(f) factanal(life, factors = f, 
                                 method ="mle")$PVAL)

# Results suggest three-factor solution might be
# adequate to account for observed covariances
# in the data.

# So we go with a three-factor solution
# which is as follows using varimax:
factanal(life, factors = 3, method = "mle")

# Examining the estimated factor loadings, we
# see that the first factor is dominated by 
# life expectancy at birth for both males
# and females; perhaps this factor could be 
# labelled "life force at birth". The second
# reflects life expectancies at older ages,
# and we might lable it "life force amongst 
# the elderly". The third factor from the 
# varimax rotation has its highest loadings 
# for the life expectancies of men aged 50 
# and 75 and in the same vein might be labelled 
# "life force for elderly men". 

# The estimated factor scores are found by:
(scores <- factanal(life, factors = 3, method = "mle",
                    scores = "regression")$scores)

# We use these scores to plot individual
# scatterplots of three factor scores for life
# expectancy data, with points labelled by
# abbreviated country names:

# Plot all three side-by-side in one row
# layout(matrix(c(1,2,3), 1, 3, byrow = TRUE))
cex <- 0.8
plot(scores[,1], scores[,2], 
     type = "n", xlab = "Factor 1", 
     ylab = "Factor 2")
text(scores[,1], scores[,2], 
     abbreviate(rownames(life), 5), cex = cex)
plot(scores[,1], scores[,3], 
     type = "n", xlab = "Factor 1", 
     ylab = "Factor 3")
text(scores[,1], scores[,3], 
     abbreviate(rownames(life), 5), 
     cex = cex)
plot(scores[,2], scores[,3], type = "n", 
     xlab = "Factor 2", ylab = "Factor 3")
text(scores[,2], scores[,3], 
     abbreviate(rownames(life), 5), cex = cex)

# Ordering along the first axis reflects
# life force at birth ranging from Cameroon
# and Madagascar to countries such as the USA. 

# And on the third axis Algeria is prominent 
# because it has high life expectancy amongst 
# men at higher ages, with Cameroon at the 
# lower end of the scale with a low life
# expectancy for men over 50.

### Drug Use by American College Students

# The majority of adult and adolescent Americans
# regularly use psychoactive substances during
# an increasing proportion of their lifetimes.

# Here is data on drug usage rates for 1634
# students in the 7th to 9th grades in 11 schools
# in Los Angeles. Each participant completed a
# questionnaire about the number of times a 
# particular substance had ever been used.

# Asked about:
# cigarettes
# beer
# wine
# liquor
# cocaine
# tranquillizers
# drug store medicine used to get high
# heroin and other opiates
# marijuana
# hashish
# inhalants
# hallucinogenics
# amphetamine stimulants

# Responses were recorded on a five-point scale:
# never tried, only once, a few times, many
# times, and regularly. The correlations 
# between the usage rates of the 13 substances
# are shown in the following plot.

# This is a visualization of correlation
# matrix of drug use. The numbers in the
# cells correspond to 100 times the
# correlation coefficient. The color
# and shape of the plotting symbols also
# correspond to the correlation in this
# cell:

ord <- order.dendrogram(as.dendrogram(hclust(dist(druguse))))  
panel.corrgram <-    
  function(x, y, z, subscripts, at,  
           level = 0.9, label = FALSE, ...) 
  {
    require("ellipse", quietly = TRUE)
    x <- as.numeric(x)[subscripts]   
    y <- as.numeric(y)[subscripts]     
    z <- as.numeric(z)[subscripts]   
    zcol <- level.colors(z, 
                         at = at, 
                         col.regions=grey.colors, ...)   
    for (i in seq(along = z)) {
      ell <- ellipse(z[i], 
                     level = level, 
                     npoints = 50,   
                     scale = c(.2, .2), 
                     centre = c(x[i], y[i]))
      panel.polygon(ell, col = zcol[i], 
                    border = zcol[i], ...)
    }
    if (label)  
      panel.text(x = x, y = y, 
                 lab = 100 * round(z, 2), 
                 cex = 0.8,
                 col = ifelse(z < 0, 
                              "white", 
                              "black"))   
  }    

print(levelplot(druguse[ord, ord], 
                at=do.breaks(c(-1.01, 1.01),20),
                xlab = NULL, 
                ylab = NULL, 
                colorkey=list(space ="top"), 
                scales=list(x=list(rot=90)),
                panel = panel.corrgram, 
                label = TRUE))

# The plot was produced using levelplot() 
# function from the package lattice with a 
# somewhat lengthy panel function.

# The figure depicts each correlation by an 
# ellipse whose shape tends towards a line
# with slope 1 for correlations near 1, to 
# a circle for correlations near zero, and to
# a line with negative slope -1 for negative 
# correlations near -1.

# In addition, 100 times the correlation 
# coefficient is printed inside the ellipse,
# and colorcoding indicates strong negative 
# (dark) to strong positive (light)
# correlations.

# We first try to determine the number of
# factors using maximum likelihood test:
sapply(1:6, function(nf)
  factanal(covmat = druguse, 
           factors = nf, 
           method = "mle", 
           n.obs = 1634)$PVAL)

# Results suggest that only the six-factor
# solution provides an adequate fit. The results
# from the six-factor varimax solution are
# obtained from:

(factanal(covmat = druguse, factors = 6, 
          method = "mle", n.obs = 1634))

# Substances that load highly on the first 
# factor are cigarettes, beer, wine, liquor,
# and marijuana and we might label it 
# "social/soft drug use". 

# Cocaine, tranquillizers, and heroin load
# highly on the second factor-the obvious 
# label for this factor is "hard drug use". 

# Factor three is essentially simply amphetamine 
# use, and factor four hashish use. We will 
# not try to interpret the last two factors,
# even though the formal test for number of 
# factors indicated that a six-factor solution
# was necessary. 

# It may be that we should not take
# the results of the formal test too
# literally; rather, it may be a better 
# strategy to consider the value of k
# indicated by the test to be an upper bound 
# on the number of factors with practical
# importance. Certainly a six-factor solution 
# for a data set with only 13 manifest
# variables might be regarded as not entirely 
# satisfactory, and clearly we would have
# some difficulties interpreting all factors.

# One of the problems is that with the large 
# sample size in this example, even small
# discrepancies between the correlation matrix 
# predicted by a proposed model and the observed
# correlation matrix may lead to rejection of 
# the model.

# One way to investigate this possibility is 
# simply to look at the differences between the
# observed and predicted correlations. We shall 
# do this first for the six-factor model using
# the following R code:
pfun <- function(nf) {
  fa <- factanal(covmat = druguse, factors = nf, 
                 method = "mle", n.obs = 1634)
  est <- tcrossprod(fa$loadings) + diag(fa$uniquenesses)
  ret <- round(druguse - est, 3)
  colnames(ret) <- rownames(ret) <- 
    abbreviate(rownames(ret), 3)
  ret
}

# We call the function with 6 factors:
pfun(6)

# The differences are all very small, 
# underlining that the six-factor model 
# does describe the data very well. Now 
# let us look at the corresponding matrices 
# for the three- and four-factor solutions 
# found in a similar way. Again, in both
# cases the residuals are all relatively 
# small, suggesting perhaps that use of the
# formal test for number of factors leads, 
# in this case, to overfitting. The three-
# factor model appears to provide a 
# perfectly adequate fit for these data.

pfun(3)
pfun(4)

# FACTOR ANALYSIS AND PCA COMPARED

# Factor analysis, like principal components 
# analysis, is an attempt to explain a set
# of multivariate data using a smaller number 
# of dimensions than one begins with, but
# the procedures used to achieve this goal 
# are essentially quite different in the
# two approaches. Some differences between 
# the two are as follows:

# 1) Factor analysis tries to explain the 
# covariances or correlations of the 
# observed variables by means of a few 
# common factors. Principal components
# analysis is primarily concerned with 
# explaining the variance of the observed
# variables.

# 2) If the number of retained components 
# is increased, say from m to m+1, the
# first m components are unchanged. This 
# is not the case in factor analysis,
# where there can be substantial changes 
# in all factors if the number of factors
# is changed.

# 3) The calculation of principal component
# scores is straightforward, but the calcu-
# lation of factor scores is more complex, 
# and a variety of methods have been suggested.

# 4) There is usually no relationship 
# between the principal components of the
# sample correlation matrix and the sample 
# covariance matrix. For maximum likelihood
# factor analysis, however, the results of 
# analyzing either matrix are essentially
# equivalent (which is not true of principal 
# factor analysis).

# Despite these differences, the results 
# from both types of analyses are frequently
# very similar. Certainly, if the specific 
# variances are small, we would expect both
# forms of analyses to give similar results. 
# However, if the specific variances are 
# large, they will be absorbed into all the
# principal components, both retained and
# rejected, whereas factor analysis makes 
# special provision for the.

# Lastly, it should be remembered that both
# principal components analysis and factor
# analysis are similar in one important 
# respect: they are both pointless if the 
# observed variables are almost uncorrelated. 
# In this case, factor analysis has nothing 
# to explain and principal components 
# analysis will simply lead to components 
# that are similar to the original variables.

